  {-
  SockeyeBackendProlog.hs: Backend for generating ECLiPSe-Prolog for Sockeye

  Part of Sockeye

  Copyright (c) 2018, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
  Attn: Systems Group.
-}


{-
  TODO: This currently works on a subset of the parser AST. Ideally, there would be
  a transformation first that:
  * Removes wildcards and replaces it with forall loops (introducing a new variable)
  * Expands natural expression into a seperate definition blocks (introducing new local
    variable for each block)
  * Everytime a range is encountered, it's passed to a natural limit/base range (no more bit ops)
  * Pushes the type of accepted/translated blocks own to the specific blocks, this should
    also merge the translte/convert types into one.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module SockeyeBackendProlog
( compile, compileDirect ) where

import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Maybe (catMaybes)
import Text.Printf
import Control.Exception (throw, Exception)
import Control.Monad.State.Strict

import qualified SockeyeSymbolTable as ST
import qualified SockeyeAST as SAST
import qualified SockeyeParserAST as AST

data PrologBackendException
  =  NYIException String
  deriving(Show)

instance Exception PrologBackendException

{- The structure of the code generator should be very similar to the old Prolog Backend -}
compile :: ST.Sockeye -> SAST.Sockeye -> String
compile symTable ast = "Prolog backend not yet implemented"

compileDirect :: AST.Sockeye -> String
compileDirect = generate_sockeye


{- The Module level state (State counter, temp variable counter etc) and the
 - corresponding Monad
 -}
data ModLevelState = ModLevelState { tmpCount :: Int
                                   , stateCount :: Int
                                   , extraPred :: [String]
                                   }

type ModLevelStateS = State ModLevelState 

init_mod_state :: ModLevelState 
init_mod_state = ModLevelState {tmpCount = 0, stateCount = 0, extraPred = []}

{- get a new (per-module) unique temporary variable -}
get_next_tmp_var :: ModLevelStateS String
get_next_tmp_var = do
    st <- get
    modify (\c -> c { tmpCount = (tmpCount c) + 1})
    return $ "TMP_" ++ (show $ tmpCount st)

get_state_var :: ModLevelStateS String
get_state_var = do
    st <- get
    return $ statevar (stateCount st)

get_next_state_var :: ModLevelStateS String
get_next_state_var = do
    modify (\c -> c { stateCount = (stateCount c) + 1})
    st <- get
    return $ statevar (stateCount st)

{- add an extra predicate to be inserted at the module level -}
push_extra_pred :: String -> ModLevelStateS ()
push_extra_pred pred = do
    st <- get
    modify (\c -> c { extraPred = (extraPred c) ++ [pred]})
    return ()

get_extra_pred :: ModLevelStateS [String]
get_extra_pred = do 
    st <- get
    return $ extraPred st


{- Code Generator, first part, outside the module context -}
generate_sockeye :: AST.Sockeye -> String
generate_sockeye s = let
    files = map snd (Map.toList (AST.files s))
    in concat (map generate_sockeye_file files)

generate_sockeye_file :: AST.SockeyeFile -> String
generate_sockeye_file f = 
    let
        -- gmod :: AST.module -> String
        gmod m = fst $ runState (generate m) init_mod_state
    in 
        concat (map gmod (AST.modules f))


class PrologGenerator a where
    generate :: a -> ModLevelStateS String

gen_node_param_list :: [AST.NodeDeclaration] -> [String]
gen_node_param_list ndl = map AST.nodeName ndl

gen_nat_param_list :: [AST.ModuleParameter] -> [String]
gen_nat_param_list = map local_param_name . map AST.paramName


instance PrologGenerator AST.Module where
  generate m = do
    let name = "add_" ++ AST.moduleName m
    let mi = gen_module_info m
    let p1 = gen_nat_param_list (AST.parameters m)
    let bodyChecks = [predicate "is_list" ["Id"]] ++ map (\x -> predicate "nonvar" [x]) p1

    consts <- mapM generate (AST.constants m)
    nodeDecls <- mapM generate (AST.nodeDecls m)
    let instDecls = []
    bodyDefs <- mapM generate (AST.definitions m)
    -- Collect the accumulated extra pred. Must be inserted before the body defs
    extPred <- get_extra_pred
    let body = pred_join $ bodyChecks ++ consts ++ nodeDecls ++ instDecls ++ extPred ++ bodyDefs
    cS <- get_state_var
    return $ name ++ stringify ([statevar 0, "Id"] ++ p1 ++ [cS]) ++ " :- \n    " ++ body ++ ".\n\n"
    where
      stringify [] = ""
      stringify pp = parens $ intercalate "," pp

instance PrologGenerator AST.NamedConstant where
    generate nc = let
            l = local_param_name $ AST.constName nc
            r = show $ AST.namedConst nc
        in
            return $ l ++ " = " ++ r


instance PrologGenerator AST.PropertyExpr where
  generate (AST.And _ a b) = do
            l <- generate a
            r <- generate b
            return $ predicate "and" [l, r]
  generate (AST.Or _ a b) = do
            l <- generate a
            r <- generate b
            return $ predicate "or" [l, r]
  generate (AST.Not _ a) = do
            l <- generate a
            return $ predicate "not" [l]
  generate (AST.Property _ s) = return $ atom s
  generate (AST.True) = return $ atom "true"
  generate (AST.False) = return $ atom "false"






-- Inside each function we add variable that contains
--  * nodeId
--  * params
--  * constants
-- This will return the name of these variables

local_inst_name :: String -> String
local_inst_name x = "ID_" ++ x

local_param_name :: String -> String
local_param_name x = "P_" ++ x

local_const_name :: String -> String
local_const_name x = "CONST_" ++ x

-- Generates something a la:
-- (ID_RAM) = (['ram', Id])
instance PrologGenerator AST.InstanceDeclaration where
    generate x = return ""
--      do
--        let var = local_nodeid_name $ AST.instName x
--        let decl = list_prepend (doublequotes $ AST.instName x) ("Id")
--        return $ var ++ " = " ++ decl

-- Generates something a la:
-- (ID_RAM, INKIND_RAM, OUTKIND_RAM) = (['ram' | Id], memory, memory)
instance PrologGenerator AST.NodeDeclaration where
    generate x = do
        let nn = AST.nodeName x
        let nn_ref = PlQualifiedRef 
                { propName  = nn
                , propIndex = Nothing
                , instName  = Nothing
                , instIndex = Nothing
                }
        nn_ref_s <- generate nn_ref
        return $ predicate "node_enum" [nn_ref_s, "_"] --eager node enum 
        
-- instance PrologGenerator AST.NodeDeclaration where
--     generate x = do
--         let var = local_nodeid_name $ AST.nodeName x
--         decl_kind_in <- generate (AST.originDomain (AST.nodeType x))
--         decl_kind_out <- generate (AST.targetDomain (AST.nodeType x))
--         let decl_id = list_prepend (doublequotes $ AST.nodeName x) ("Id")
--         let decl_tup = tuple [decl_id, decl_kind_in, decl_kind_out]
-- 
--         -- Build the variable list
--         let pf = AST.nodeName x
--         let var_tup = tuple [local_nodeid_name pf, "INKIND_" ++ pf, "OUTKIND_" ++ pf]
--         return $ pred_join [
--             var_tup ++ " = " ++ decl_tup,
--             predicate "node_enum" [local_nodeid_name pf, "_"], --eager node enum 
--             predicate "nonvar" ["INKIND_" ++ pf],  -- remove singleton var warning
--             predicate "nonvar" ["OUTKIND_" ++ pf]]


{- Intermediate Prolog Representation -}
data PlAddressBlock = PlAddressBlock AST.Address

data PlQualifiedRef = PlQualifiedRef
    { propName  :: String
    , propIndex :: Maybe AST.ArrayIndex
    , instName  :: Maybe String
    , instIndex :: Maybe AST.ArrayIndex
    }

qualify_ref :: AST.UnqualifiedRef -> AST.UnqualifiedRef -> PlQualifiedRef
qualify_ref inst prop = PlQualifiedRef
    { propName  = AST.refName prop
    , propIndex = AST.refIndex prop
    , instName  = Just $ AST.refName inst
    , instIndex = AST.refIndex inst
    }

qualify_self :: AST.UnqualifiedRef -> PlQualifiedRef
qualify_self prop = PlQualifiedRef
    { propName  = AST.refName prop
    , propIndex = AST.refIndex prop
    , instName  = Nothing
    , instIndex = Nothing
    }

qualify_nr :: AST.NodeReference -> PlQualifiedRef
qualify_nr (AST.InternalNodeRef _ nr) = qualify_self nr
qualify_nr (AST.InputPortRef _ inst node) = qualify_ref inst node

data PlNameSpec = PlNameSpec
    { node :: AST.NodeReference
    , addr :: Maybe AST.NaturalExpr -- Nothing if we state something the backend doesnt support
    , prop :: AST.PropertyExpr
    }

data PlPortBindSpec = PlPortBindSpec
    { port :: PlQualifiedRef 
    , boundNode    :: AST.NodeReference
    }

data PlRegionSpec = PlRegionSpec
  { regNode :: AST.UnqualifiedRef
  , regBlock :: PlAddressBlock 
  , regProp :: AST.PropertyExpr
  }
data PlTranslateSpec = PlTranslateSpec
  { src :: PlRegionSpec,
    dst :: PlNameSpec
  }
data PlAcceptSpec = PlAcceptSpec PlRegionSpec

data ModuleInfo = ModuleInfo
  {
    params :: [String],
    node_type :: Map.Map String ST.NodeType
  }

gen_module_info :: AST.Module -> ModuleInfo
gen_module_info x =
  ModuleInfo {
    params = ["Id"] ++ mparams ++ nodes ++ insts,
    node_type = Map.fromList [(AST.nodeName z, AST.nodeType $ z) | z <- AST.nodeDecls x]
  }
  where
    insts = [AST.instName d | d <- AST.instDecls x]
    mparams = (gen_nat_param_list $ AST.parameters x)
    nodes = [AST.nodeName d | d <- AST.nodeDecls x]

add_param :: ModuleInfo -> String -> ModuleInfo
add_param mi s = ModuleInfo { params = (params mi) ++ [s], node_type = node_type mi}

param_str :: ModuleInfo -> String
param_str mi = case params mi of
  [] -> ""
  li -> "," ++ intercalate "," [predicate "param" [p] | p <- li]


generate_conj :: ModuleInfo -> [AST.Definition] -> String
generate_conj mi li = "TODO> NYI"
   -- intercalate ", sss\n" $ concat [snd(gen_body_defs mi inn 0) | inn <- li]
   -- TODO: fix the 0 here


-- generate forall with a explicit variable name
{-
forall_qual :: ModuleInfo -> String -> AST.NaturalSet -> [AST.Definition] -> String
forall_qual mi varName ns body =
  "(" ++
   predicate "iblock_values" [generate ns, it_list] ++ "," ++
   "(" ++
   predicate "foreach" [it_var, it_list]
   ++ param_str mi
   ++ " do \n" ++
   body_str ++ "\n))"
   where
     id_var = "ID_" ++ varName
     it_var = "IDT_" ++ varName
     it_list = "IDL_" ++ varName
     body_str = generate_conj (add_param mi it_var) body
-}

{-
forall_uqr :: ModuleInfo -> AST.UnqualifiedRef -> String -> String
forall_uqr mi ref body_str = case (AST.refIndex ref) of
  Nothing -> printf "(%s = %s, %s)" it_var id_var body_str
  Just ai -> "(" ++
                predicate "iblock_values" [generate ai, it_list] ++ "," ++
                "(" ++
                predicate "foreach" [it_var, it_list]
                ++ param_str mi
                ++ " do " ++
                itid_var ++ " = " ++ list_prepend it_var id_var ++ "," ++
                body_str ++ "))"
  where
    id_var = "ID_" ++ (AST.refName ref)
    it_var = "IDT_" ++ (AST.refName ref)
    itid_var = "IDI_" ++ (AST.refName ref)
    it_list = "IDL_" ++ (AST.refName ref)
-}

{-
gen_index :: AST.UnqualifiedRef -> String
gen_index uqr =
  case (AST.refIndex uqr) of
    Nothing -> local_nodeid_name $ AST.refName uqr
    Just ai -> list_prepend (gen_ai ai) (local_nodeid_name $ AST.refName uqr)
  where
    gen_ai (AST.ArrayIndex _ ws) =  list [gen_wildcard_simple w | w <- ws]
    gen_wildcard_simple (AST.ExplicitSet _ ns) = gen_natural_set ns
    gen_natural_set (ST.NaturalSet _ nrs) = gen_natural_ranges nrs
    gen_natural_ranges [nr] = gen_ns_simple nr
    gen_ns_simple (ST.SingletonRange _ base) = gen_exp_simple base
    gen_exp_simple (AST.Variable _ vn) = "IDT_" ++ vn
    gen_exp_simple (AST.Literal _ int) = show int
-}


-- gen_base_address :: AST.Address -> String
-- gen_base_address (AST.Address _ ws) = gen_single_ws $ ws !! 0
--   where
--     gen_single_ws (AST.ExplicitSet _ ns) = gen_single_ns ns
--     gen_single_ws (AST.Wildcard _)  = "0 /* WILDCARD_NYI */"
--     gen_single_ns (AST.NaturalSet _ nr) = gen_single_nr (nr !! 0)
--     gen_single_nr nr = case nr of
--         AST.SingletonRange _ b -> generate b
--         AST.LimitRange _ b _ -> generate b
--         AST.BitsRange _ b bits -> generate b 


instance PrologGenerator PlAddressBlock where
    generate (PlAddressBlock ab) = generate ab
         -- addrStr <- generate ab
         -- return $ predicate "block" [addrStr]

instance PrologGenerator PlRegionSpec where
    generate as = do
        nodeStr <- generate (regNode as)
        blockStr <- generate (regBlock as)
        propStr <- generate (regProp as)
        return $ predicate "region" [nodeStr, blockStr, propStr]

instance PrologGenerator PlNameSpec where
    generate ns =
        let
            gen_addr (Just x) = generate x
            gen_addr Nothing = return $ "0 /* NYI */"
        in
            do
                nodeStr <- generate (node ns)
                addrStr <- gen_addr (addr ns)
                propStr <- generate (prop ns)
                return $ predicate "name" [nodeStr, addrStr, propStr]

instance PrologGenerator PlTranslateSpec where
    generate ts = do
        srcStr <- generate (src ts)
        dstStr <- generate (dst ts)
        ass <- assert_translate srcStr dstStr
        return ass

instance PrologGenerator PlAcceptSpec where
    generate (PlAcceptSpec reg) = do
        regStr <- generate reg
        ass <- assert_accept regStr
        return ass

instance PrologGenerator PlPortBindSpec where
    generate pb =
        do
            portS <- generate $ port pb
            ndS <- generate $ boundNode pb
            olS <- assert_overlay portS ndS
            return olS


        

instance PrologGenerator AST.Definition where
    generate (AST.Accepts _ n accepts) = do
        let pl_accepts = map (mk_acc n) accepts
        accS <- mapM generate pl_accepts
        return $ pred_join accS
        where
            mk_acc :: AST.UnqualifiedRef -> AST.AddressBlock -> PlAcceptSpec
            mk_acc n ab = 
                let
                    block = PlAddressBlock $ SAST.addresses ab
                    reg = PlRegionSpec { regNode=n
                                       , regBlock=block
                                       , regProp=SAST.properties ab }
                in
                    PlAcceptSpec reg

    generate (AST.Maps _ nd maps) = do
        let pl_trans = concat $ map (mk_trans nd) maps
        pl_trans_s <- mapM generate pl_trans
        return $ pred_join pl_trans_s
        where 
            mk_trans :: AST.UnqualifiedRef -> AST.MapSpec -> [PlTranslateSpec]
            mk_trans n mapspec = 
                let
                    ab = AST.mapAddr mapspec
                    block = PlAddressBlock $ SAST.addresses ab
                    src = PlRegionSpec { regNode=n
                                       , regBlock=block
                                       , regProp=SAST.properties ab }
                    targets = AST.mapTargets mapspec
                in 
                    map (\x -> PlTranslateSpec {src=src, dst=mk_name x}) targets

            mk_name :: AST.MapTarget -> PlNameSpec
            mk_name t = 
                let
                    ab = AST.targetAddr t
                    get_ws (SAST.Address _ [ws]) = Just ws
                    get_ws (SAST.Address _ _) = Nothing
                    get_ns (SAST.ExplicitSet _ ns) = Just ns
                    get_ns (SAST.Wildcard _) = Nothing
                    get_fst (SAST.NaturalSet _ [nr]) = Just nr
                    get_fst (SAST.NaturalSet _ _) = Nothing
                    get_base (AST.SingletonRange _ b) = Just b
                    get_base (AST.LimitRange _ b _) = Just b
                    get_base (_) = Nothing
                    add = (get_base <=< get_fst <=< get_ns <=< get_ws) (SAST.addresses ab)
                in
                    PlNameSpec { node=AST.targetNode t
                               , addr=add
                               , prop=SAST.properties ab }

    generate (AST.BlockOverlays _ src dst bits) = do
        srcS <- generate src
        dstS <- generate dst
        let bitsS = show (bits !! 0)
        res <- assert_configurable srcS bitsS dstS 
        return res

    generate (AST.Instantiates _ idname imodname args) = do
        argsS <- mapM generate args
        idnameS <- generate idname
        res <- add_mod idnameS imodname argsS
        return res

    generate (AST.Binds _ inst binds) = do
        let pl_binds = map mk_bind binds 
        bindsS <- mapM generate pl_binds
        return $ pred_join bindsS
        where 
            mk_bind bind = PlPortBindSpec
                { port = qualify_ref inst (AST.boundPort bind)
                , boundNode = AST.boundNode bind }

    generate (AST.Overlays _ src dst) = do
        srcS <- generate $ qualify_self src
        dstS <- generate $ qualify_nr dst
        olS <- assert_overlay srcS dstS
        return olS

    generate x = throw $ NYIException $ "generate for NYI for " ++ (show x)
-- gen_body_defs mi x i = case x of
--   (AST.Accepts _ n accepts) -> do
-- 
--   (AST.Maps _ _ _) -> gen_translate (map_spec_flatten mi x) (i, [])
--    --(1, [(assert 0 $ predicate "translate"
--     --[generate $ srcNode om, generate $ srcAddr om, generate $ targetNode om, generate $ targetAddr om])
--     -- | om <- map_spec_flatten mi x])
--   (AST.Overlays _ src dest) -> (i+1, [state_add_overlay i (generate src) (generate dest)])
--   (AST.BlockOverlays _ src dst bits) -> gen_blockoverlay (generate src) (generate dst) bits (i, [])
--   -- (AST.Instantiates _ i im args) -> [forall_uqr mi i (predicate ("add_" ++ im) ["IDT_" ++ (AST.refName i)])]
--   (AST.Instantiates _ ii im args) -> (i+1, [ predicate ("add_" ++ im)
--         ([statevar i] ++ [gen_index ii] ++ (map generate args) ++ [statevar (i+1)]) ])
--   -- (AST.Binds _ i binds) -> [forall_uqr mi i $ gen_bind_defs ("IDT_" ++ (AST.refName i)) binds]
--   (AST.Binds _ ii binds) -> gen_bind_defs (gen_index ii) binds (i, [])
--   --(AST.Forall _ varName varRange body) -> (0, [forall_qual mi varName varRange body])
--   (AST.Forall _ varName varRange body) -> throw $ NYIException "forall"
--   (AST.Converts _ _ _ ) -> throw $ NYIException "Converts"
--   where
--     new_ab ab = pack_address_block ab



-- count_num_facts :: ModuleInfo -> AST.Definition -> Integer
-- count_num_facts mi x = case x of
--     (AST.Accepts _ n accepts) -> sum([1 | acc <- accepts])
--     (AST.Maps _ _ _) -> sum([1 | om <- map_spec_flatten mi x])
--     (AST.Overlays _ src dest) -> 1
--     -- (AST.Instantiates _ i im args) -> [forall_uqr mi i (predicate ("add_" ++ im) ["IDT_" ++ (AST.refName i)])]
--     (AST.Instantiates _ _ _ _) -> 1
--     (AST.BlockOverlays _ _ _ bits) -> (toInteger (length bits))
--     -- (AST.Binds _ i binds) -> [forall_uqr mi i $ gen_bind_defs ("IDT_" ++ (AST.refName i)) binds]
--     (AST.Binds _ i binds) -> sum([1 | b <- binds])
--     (AST.Forall _ varName varRange body) -> 0
--     (AST.Converts _ _ _ ) -> 0



instance PrologGenerator AST.UnqualifiedRef where
  generate uqr = generate $ qualify_self uqr

instance PrologGenerator PlQualifiedRef where
    generate qr =
        let
            gen_idx Nothing = return Nothing
            gen_idx (Just x) = do
                xS <- generate x
                return $ Just xS
        in do
            propIndexMS <- gen_idx $ propIndex qr
            let propNameMS = Just $ propName qr
            let instNameMS = instName qr
            instIndexMS <- gen_idx $ instIndex qr
            let els = catMaybes [
                            propIndexMS,
                            propNameMS >>= (Just . doublequotes),
                            instIndexMS,
                            instNameMS >>= (Just . doublequotes)]
            return $ many_list_prepend els "Id"

instance PrologGenerator AST.WildcardSet where
    generate (AST.ExplicitSet _ ns) = generate ns
    generate (AST.Wildcard _) = return "block(0,0) /* WILDCARD NYI */"

instance PrologGenerator AST.ArrayIndex where
    generate (AST.ArrayIndex _ wcs) = do
        wcsS <- mapM generate wcs
        return $ brackets $ intercalate "," wcsS

instance PrologGenerator AST.NodeReference where
    generate (AST.InternalNodeRef _ nn) = generate $ qualify_self nn
    generate (AST.InputPortRef _ inst node) = generate $ qualify_ref inst node 

instance PrologGenerator AST.Domain where
  generate (AST.Memory) = return $ atom "memory"
  generate (AST.Interrupt) = return $ atom "interrupt"
  generate (AST.Power) = return $ atom "power"
  generate (AST.Clock) = return $ atom "clock"

instance PrologGenerator AST.AddressBlock where
  generate ab = generate $ SAST.addresses ab


instance PrologGenerator AST.Address where
    generate (AST.Address _ ws) = do
        wsStr <- mapM generate ws
        return $ wsStr !! 0

instance PrologGenerator AST.NaturalSet where
  generate a = case a of
     AST.NaturalSet _ [nrs] -> generate nrs
     AST.NaturalSet _ _ -> throw $ NYIException $ "MULTIDIM"

instance PrologGenerator AST.NaturalRange where
  generate nr = case nr of
    AST.SingletonRange _ b -> do
        bS <- generate b
        return $ predicate "block" [bS,bS]
    AST.LimitRange _ b l -> do
        bS <- generate b
        lS <- generate l
        return $ predicate "block" [bS,lS]
    AST.BitsRange _ b bits -> do
        bS <- generate b
        bitS <- generate bits
        limitVar <- get_next_tmp_var
        push_extra_pred $ limitVar ++ " is " ++ bS ++ " + 2^" ++ bitS ++ " - 1"
        return $ predicate "block" [bS,limitVar]

instance PrologGenerator AST.NaturalExpr where
    -- TODO insert monad magic here
    generate exp = do
        tmpV <- get_next_tmp_var 
        expS <- genm exp
        push_extra_pred $ tmpV ++ " is " ++ expS
        return tmpV
        where
            genm :: AST.NaturalExpr -> ModLevelStateS String
            genm (SAST.Addition _ a b) = do
                aS <- genm a
                bS <- genm b
                return $ "(" ++ aS ++ ")+(" ++ bS ++ ")"
            genm (SAST.Subtraction _ a b) = do
                aS <- genm a
                bS <- genm b
                return $ "(" ++ aS ++ ")-(" ++ bS ++ ")"
            genm (SAST.Multiplication _ a b) = do
                aS <- genm a
                bS <- genm b
                return $ "(" ++ aS ++ ")*(" ++ bS ++ ")"
            -- and the terminals
            genm x = return $ gen x 

            gen :: AST.NaturalExpr -> String
            gen (SAST.Constant _ v) = local_const_name v
            gen (SAST.Variable _ v) = local_param_name v
            gen (SAST.Parameter _ v) = local_param_name v
            gen (SAST.Literal _ n) = show n
            gen (SAST.Slice _ a bitrange) = "SLICE NYI"
            gen (SAST.Concat _ a b) = "CONCAT NYI"



{- Helper functions -}
atom :: String -> String
atom "" = ""
atom name@(c:cs)
    | isLower c && allAlphaNum cs = name
    | otherwise = quotes name
    where
        allAlphaNum cs = foldl (\acc c -> isAlphaNum c && acc) True cs

predicate :: String -> [String] -> String
predicate name args = name ++ (parens $ intercalate "," args)

struct :: String -> [(String, String)] -> String
struct name fields = name ++ (braces $ intercalate "," (map toFieldString fields))
    where
        toFieldString (key, value) = key ++ ":" ++ value

tuple :: [String] -> String
tuple elems = parens $ intercalate "," elems

list :: [String] -> String
list elems = brackets $ intercalate "," elems

list_prepend :: String -> String -> String
list_prepend a li = brackets $ a ++ " | " ++ li

many_list_prepend :: [String] -> String -> String
many_list_prepend as li = 
    let
        asS = intercalate "," as 
    in
        brackets $ asS ++ " | " ++ li

enclose :: String -> String -> String -> String
enclose start end string = start ++ string ++ end

parens :: String -> String
parens = enclose "(" ")"

brackets :: String -> String
brackets = enclose "[" "]"

braces :: String -> String
braces = enclose "{" "}"

quotes :: String -> String
quotes = enclose "'" "'"

doublequotes :: String -> String
doublequotes = enclose "\"" "\""


-- nat_range_from :: AST.NaturalRange -> String
-- nat_range_from nr = case nr of
--   AST.SingletonRange _ b -> generate b
--   AST.LimitRange _ b _ -> generate b
--   AST.BitsRange _ _ _ -> "BitsRange NOT IMPLEMENTED"
-- 
-- nat_range_to :: AST.NaturalRange -> String
-- nat_range_to nr = case nr of
--   AST.SingletonRange _ b -> generate b
--   AST.LimitRange _ _ l -> generate l
--   AST.BitsRange _ _ _ -> "BitsRange NOT IMPLEMENTED"
-- 
-- -- Params are variables passed into the for body
-- for_body_inner :: [String] -> String -> String -> (Int, AST.NaturalRange)  -> String
-- for_body_inner params itvar body itrange  =
--   let
--     itvar_local = itvar ++ (show $ fst itrange)
--     from = nat_range_from $ (snd itrange)
--     to = nat_range_to $ (snd itrange)
--     for = printf "for(%s,%s,%s)" itvar_local from to :: String
--     paramf x  = printf "param(%s)" x :: String
--     header = intercalate "," ([for] ++ map paramf params)
--     in printf "(%s \ndo\n %s \n)" header body
-- 
-- enumerate = zip [0..]
-- 
-- for_body :: [String] -> String -> AST.NaturalSet -> String -> String
-- for_body params itvar (AST.NaturalSet _ ranges) body =
--   foldl fbi body (enumerate ranges)
--   where
--     fbi = for_body_inner params itvar
--
pred_join :: [String] -> String 
pred_join = intercalate ",\n    "

statevar :: Int -> String
statevar i = printf "S%i" i

add_mod ::  String -> String -> [String] -> ModLevelStateS String
add_mod idname modname args = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate ("add_" ++ modname) ([sIn, idname] ++ args ++ [sOut])

assert_translate ::  String -> String -> ModLevelStateS String
assert_translate src dst = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate "assert_translate" [sIn, src, dst, sOut]

assert_accept ::  String -> ModLevelStateS String
assert_accept reg = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate "assert_accept" [sIn, reg, sOut]

assert_overlay ::  String -> String -> ModLevelStateS String
assert_overlay src dst = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate "assert_overlay" [sIn, src, dst, sOut]

assert_configurable ::   String -> String -> String -> ModLevelStateS String
assert_configurable src bits dst = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate "assert_configurable" [sIn, src, bits, dst, sOut]
