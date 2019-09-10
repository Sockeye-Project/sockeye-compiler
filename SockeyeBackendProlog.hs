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
( compile ) where

import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Maybe (catMaybes)
import Text.Printf
import Control.Exception (throw, Exception)
import Control.Monad.State.Strict

import PrologAST 

data PrologBackendException
  =  NYIException String
  |  AssertException String 
  deriving(Show)

instance Exception PrologBackendException

{- Above body-level generator functions -}
compile :: PlFile -> String
compile (PlFile mods) = intercalate "" (map generate_mod mods) 

generate_mod :: PlModule -> String
generate_mod mod = 
    let
        join_body_pts a b = if blank b then a else a ++ ",\n" ++ b
        name = "add_" ++ (moduleName mod)
        -- constants
        bodyConsts = map generate_const (constants mod)
        -- parameters from sockeye
        pSock = map (sock_var_to_pl . paramName) (parameters mod)
        bodyChecks = [predicate "is_list" ["Id"]] ++ map (\x -> predicate "nonvar" [x]) pSock
        bodyPreamble = bodyConsts ++ bodyChecks
        (bodyDefStr,ctx) = generate_body (init_toplevel_ctx mod) (body mod)
        bodyStr = join_body_pts (pred_join 0 $ bodyPreamble) bodyDefStr
        -- add internal parameters
        pMod = [state_var 0, "Id"] ++ pSock ++ [state_var (stateCount ctx)]
        pModStr = parens $ intercalate "," pMod
    in
        name ++ pModStr ++ " :- \n" ++ bodyStr ++ ".\n\n"
        

generate_body :: CtxState -> PlBody -> (String, CtxState)
generate_body ctx body = runState (generate body) ctx

generate_const :: NamedConstant -> String
generate_const (NamedConstant _ name val) =
    printf "%s is %d" (sock_var_to_pl name) val

{- State and some utilities
 - We have to track 
 - * current state variable counter (for passing on state)  
 - * which variables are actually in scope (for generating forall headers)
 - * indentation level -}
data CtxState = CtxState { stateCount :: Int
                         , scopeVars :: [String] 
                         , indentLevel :: Int
                         }

type SM = State CtxState 
init_toplevel_ctx :: PlModule -> CtxState 
init_toplevel_ctx mod = 
    let
    mod_vars :: PlModule -> [String]
    mod_vars mod = ["Id"] ++ pars
      where
        pars = map sock_var_to_pl (pars_s ++ consts_s)
        pars_s = map paramName (parameters mod)
        consts_s = map constName (constants mod)
    in CtxState { stateCount = 0, scopeVars = mod_vars mod, indentLevel = 0}

-- Create a new child context for a new nesterd forall block
child_ctx :: CtxState -> String -> CtxState
child_ctx parent varName = CtxState { stateCount = (stateCount parent)
                                    , scopeVars = ((scopeVars parent) ++ [varName])
                                    , indentLevel = (indentLevel parent) + 1
                                    }

-- Update the parental context (current) with the context from a child (argument)
child_done :: CtxState -> SM ()
child_done child_ctx = do
    modify (\c -> c { stateCount = (stateCount child_ctx)})
    return ()


get_scope_vars :: SM [String]
get_scope_vars = do
    st <- get
    return $ scopeVars st

get_scope_param_str :: SM (Maybe String)
get_scope_param_str = do
    vars <- get_scope_vars
    return $ pstr vars
    where
        pstr [] = Nothing
        pstr xs = Just $ predicate "param" xs

get_state_var :: SM String
get_state_var = do
    st <- get
    return $ state_var (stateCount st)

get_next_state_var :: SM String
get_next_state_var = do
    modify (\c -> c { stateCount = (stateCount c) + 1})
    st <- get
    return $ state_var (stateCount st)


pred_joinM :: [String] -> SM String
pred_joinM preds = do
    st <- get
    return $ pred_join (indentLevel st) preds

{- Utilities -}

-- Transform a sockeye visible name to a prolog variable
sock_var_to_pl :: String -> String
sock_var_to_pl x = "P_" ++ x

state_var :: Int -> String
state_var i = printf "S_%i" i

int_var :: Int -> String
int_var i = printf "TMP_%i" i


{- Below body-level generator functions -}
class PrologGenerator a where
    generate :: a -> SM String

instance PrologGenerator PlBody where
    generate body = do
        p1 <- mapM generate (extraPred body)
        p2 <- mapM generate (definitions body)
        pS <- pred_joinM (p1++p2)
        return pS

instance PrologGenerator PlDefinition where
    generate (PlAccepts _ reg) = do
        regStr <- generate reg
        ass <- assert_accept regStr
        return ass
    generate (PlTranslate _ src dst) = do
        srcStr <- generate src
        dstStr <- generate dst
        ass <- assert_translate srcStr dstStr
        return ass
    generate (PlOverlays _ node overlays) = do
        nodeStr <- generate node
        overlaysStr <- generate overlays
        ass <- assert_overlay nodeStr overlaysStr
        return ass
    generate (PlBlockOverlays _ node overlays blocksize) = do
        nodeStr <- generate node
        overlaysStr <- generate overlays
        overlaysStr <- generate overlays
        blocksizeStrArr <- mapM generate blocksize
        let blocksizeStr = list blocksizeStrArr
        ass <- assert_configurable nodeStr blocksizeStr overlaysStr
        return ass
    generate (PlInstantiates _ inst instModule arguments) = do
        instS <- generate inst
        argS <- mapM generate arguments
        add_mod instS instModule argS
    generate (PlForall boundVarName varRange body) = do
        boundVarNameS <- generate boundVarName
        varRangeS <- generate varRange
        st <- get
        let (bodyS,child_done_ctx) = generate_body (child_ctx st boundVarNameS) body
        cS <- get_state_var
        -- hdr3 = param(S_2, S_99)
        let hdr3 = Just $ predicate "param" [cS, state_var (stateCount child_done_ctx)]
        _ <- child_done child_done_ctx
        let hdr1 = Just $ "foreach(" ++ boundVarNameS ++ "," ++ varRangeS ++ ")"
        hdr2 <- get_scope_param_str
        let hdr = intercalate "," $ catMaybes [hdr1, hdr2, hdr3]
        footer <- pred_joinM [")"]
        return $ "(" ++ hdr ++ " do \n" ++ bodyS ++ "\n" ++ footer

instance PrologGenerator PlExtraPred where
    generate (PlValues varName valSet) = do
            varNameS <- generate varName
            valSetS <- generate valSet
            return $ predicate "block_values" [valSetS, varNameS]
    generate (PlIsPred varName expr) = do
            varNameS <- generate varName
            exprS <- generate expr
            return $ varNameS ++ " is " ++ exprS
    generate (PlBitsLimit name base bits) = do
            nameS <- generate name
            baseS <- generate base
            bitsS <- generate bits
            return $ nameS ++ " is " ++ baseS ++ " + 2^" ++ bitsS ++ " - 1"


instance PrologGenerator PlImmediate where
    generate (PlImmediateStr s) = return $ doublequotes s
    generate (PlImmediateVar var) = generate var

instance PrologGenerator Integer where
    generate x = return $ show x

instance PrologGenerator PlVar where
    generate (PlIntVar i) = return $ int_var i 
    generate (PlSockVar name) = return $ sock_var_to_pl name

instance PrologGenerator PlRegionSpec where
    generate as = do
        nodeStr <- generate (regNode as)
        blockStr <- generate (regBlock as)
        propStr <- generate (regProp as)
        return $ predicate "region" [nodeStr, blockStr, propStr]

instance PrologGenerator PlQualifiedRef where
    generate (PlQualifiedRef propName propIndex instName instIndex) =
        let
            gen_maybe Nothing = return Nothing
            gen_maybe (Just x) = do
                xS <- generate x
                return $ Just xS
        in do
            propIndexMS <- gen_maybe propIndex
            instIndexMS <- gen_maybe instIndex
            propNameS <-  generate propName
            instNameMS <- gen_maybe instName
            let els = catMaybes [
                            propIndexMS,
                            Just propNameS,
                            instIndexMS,
                            instNameMS]
            return $ many_list_prepend els "Id"

instance PrologGenerator PlNameSpec where
    generate (PlNameSpec node addr prop) =
        do
            nodeStr <- generate node
            addrStr <- generate addr
            propStr <- generate prop
            return $ predicate "name" [nodeStr, addrStr, propStr]

instance PrologGenerator PlMultiDSet where
    generate (PlMultiDSet natSets) =
        do 
            natSetsS <- mapM generate natSets
            return $ list natSetsS
        
instance PrologGenerator PlNaturalSet where
    generate (PlNaturalSet natRanges) =
        do 
            natRangesS <- mapM generate natRanges
            return $ list natRangesS

instance PrologGenerator PlNaturalRange where
    generate (PlNaturalRange base limit) =
        do
            baseS <- generate base
            limitS <- generate limit
            return $ predicate "block" [baseS, limitS]

instance PrologGenerator PropertyExpr where
    generate (And _ a b) = do
            l <- generate a
            r <- generate b
            return $ predicate "and" [l, r]
    generate (Or _ a b) = do
            l <- generate a
            r <- generate b
            return $ predicate "or" [l, r]
    generate (Not _ a) = do
            l <- generate a
            return $ predicate "not" [l]
    generate (Property _ s) = return $ atom s
    generate (PrologAST.True) = return $ atom "true"
    generate (PrologAST.False) = return $ atom "false"

instance PrologGenerator NaturalExpr where
    generate = genm
        where
            genm :: NaturalExpr -> SM String
            genm (Addition _ a b) = do
                aS <- genm a
                bS <- genm b
                return $ "(" ++ aS ++ ")+(" ++ bS ++ ")"
            genm (Subtraction _ a b) = do
                aS <- genm a
                bS <- genm b
                return $ "(" ++ aS ++ ")-(" ++ bS ++ ")"
            genm (Multiplication _ a b) = do
                aS <- genm a
                bS <- genm b
                return $ "(" ++ aS ++ ")*(" ++ bS ++ ")"
            -- and the terminals
            genm x = return $ gen x 

            gen :: NaturalExpr -> String
            gen (Constant _ v) = sock_var_to_pl v
            gen (Variable _ v) = sock_var_to_pl v
            gen (Parameter _ v) = sock_var_to_pl v
            gen (Literal _ n) = show n
            gen (Slice _ a bitrange) = "SLICE NYI"
            gen (Concat _ a b) = "CONCAT NYI"


{- Assert wrappers -}
assert_translate ::  String -> String -> SM String
assert_translate src dst = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate "assert_translate" [sIn, src, dst, sOut]

assert_accept ::  String -> SM String
assert_accept reg = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate "assert_accept" [sIn, reg, sOut]

assert_overlay ::  String -> String -> SM String
assert_overlay src dst = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate "assert_overlay" [sIn, src, dst, sOut]

assert_configurable ::   String -> String -> String -> SM String
assert_configurable src bits dst = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate "assert_configurable" [sIn, src, bits, dst, sOut]

add_mod ::  String -> String -> [String] -> SM String
add_mod idname modname args = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate ("add_" ++ modname) ([sIn, idname] ++ args ++ [sOut])


{- Formatting functions -}
pred_join :: Int -> [String] -> String 
pred_join indent preds = 
    let
        nspace = (indent+1) * 4
        spacestr = (intercalate "" $ take nspace (repeat " "))
        jstr = ",\n" ++ spacestr
     in
        spacestr ++ (intercalate jstr preds)

atom :: String -> String
atom "" = ""
atom name@(c:cs)
    | isLower c && allAlphaNum cs = name
    | otherwise = quotes name
    where
        allAlphaNum cs = foldl (\acc c -> isAlphaNum c && acc) Prelude.True cs

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

blank :: String -> Bool
blank = all isSpace

-- 
-- 
-- gen_node_param_list :: [AST.NodeDeclaration] -> [String]
-- gen_node_param_list ndl = map AST.nodeName ndl
-- 
-- gen_nat_param_list :: [AST.ModuleParameter] -> [String]
-- gen_nat_param_list = map local_param_name . map AST.paramName
-- 
-- {- 
--     To support array indices, we wrap those 
--     The wrapping process needs the Monad, we generate temporary variables
-- -}
-- 
-- data QuantSpec = NoQuant PlQualifiedRef                      -- No forall needed, 
--                | OneQuant PlQualifiedRef String AST.ArrayIndex   -- We need a forall quantization
-- 
-- wrap :: AST.Definition -> ModLevelStateS PlDefinition
-- wrap (AST.Forall _ it range defs) = do
--     defs_wrapped <- mapM wrap defs
--     return $ PlForall it [range] defs_wrapped
-- wrap def = 
--     let 
--         ext_ws :: AST.WildcardSet -> AST.NaturalSet
--         ext_ws (AST.ExplicitSet _ ns) = ns
--         ext_ws _ = throw $ NYIException "ext_ws with wildcard"
--         ext_ai :: AST.ArrayIndex -> [AST.NaturalSet]
--         ext_ai (AST.ArrayIndex _ ws) = map ext_ws ws
-- 
--         self_qual :: AST.UnqualifiedRef -> ModLevelStateS QuantSpec
--         self_qual (AST.UnqualifiedRef m i Nothing) = return (NoQuant $ PlQualifiedRef {
--                 propName = m,
--                 propIndex = Nothing,
--                 instName = Nothing,
--                 instIndex = Nothing })
--         self_qual (AST.UnqualifiedRef m i (Just ai)) = do
--             s <- get_next_tmp_var
--             let ref = PlQualifiedRef {
--                 propName = m,
--                 propIndex = PlInternalVar s,
--                 instName = Nothing,
--                 instIndex = Nothing }
--             return $ OneQuant ref s ai
--                 
--         -- substitute the reference in definition with first arg
--         subst_ref :: PlQualifiedRef -> AST.Definition -> PlDefinition
--         subst_ref _  (AST.Forall _ _ _ _) =
--             throw $ AssertException $ "can't get UQR from forall "
--         subst_ref qr (AST.Accepts m _ accepts) = PlAccepts m qr accepts
--         subst_ref qr (AST.Maps m _ b) = PlMaps m qr b
--         subst_ref _  x = throw $ NYIException "implement subst_ref" ++ (show x)
-- 
--         get_ref :: AST.Definition -> AST.UnqualifiedRef
--         get_ref (AST.Forall _ _ _ _) =
--             throw $ AssertException $ "can't get UQR from forall "
--         get_ref (AST.Binds _ i _) = i
--         get_ref (AST.Instantiates _ i _ _) = i
--         get_ref (AST.BlockOverlays _ i _ _) = i
--         get_ref (AST.Overlays _ i _) = i
--         get_ref (AST.Converts _ i _) = i
--         get_ref (AST.Maps _ i _) = i
--         get_ref (AST.Accepts _ i _) = i
--         
--     in do
--         quant <- self_qual (get_ref def)
--         case quant of 
--             NoQuant qr -> return $ subst_ref qr def
--             OneQuant qr it ai -> return $ PlForall it (ext_ai ai) (subst_ref qr def)
-- 
-- class PrologGenerator a where
--     generate :: a -> ModLevelStateS String
-- 
-- instance PrologGenerator AST.Module where
--   generate m = do
--     let name = "add_" ++ AST.moduleName m
--     let mi = gen_module_info m
--     let p1 = gen_nat_param_list (AST.parameters m)
--     let bodyChecks = [predicate "is_list" ["Id"]] ++ map (\x -> predicate "nonvar" [x]) p1
-- 
--     consts <- mapM generate (AST.constants m)
--     tags <- mapM generate (AST.moduleTags m)
--     nodeDecls <- mapM generate (AST.nodeDecls m)
--     let instDecls = []
--     bodyDefs <- mapM generate (AST.definitions m)
--     -- Collect the accumulated extra pred. Must be inserted before the body defs
--     extPred <- get_extra_pred
--     let body = pred_join $ bodyChecks ++ consts ++ tags ++ nodeDecls ++ instDecls ++ extPred ++ bodyDefs
--     cS <- get_state_var
--     return $ name ++ stringify ([statevar 0, "Id"] ++ p1 ++ [cS]) ++ " :- \n    " ++ body ++ ".\n\n"
--     where
--       stringify pp = parens $ intercalate "," pp
-- 
-- instance PrologGenerator AST.NamedConstant where
--     generate nc = let
--             l = local_param_name $ AST.constName nc
--             r = show $ AST.namedConst nc
--         in
--             return $ l ++ " = " ++ r
-- 
-- instance PrologGenerator AST.ModuleTag where
--     generate (AST.ModuleTag _ name exp) = do
--             expS <- generate exp
--             return $ predicate "assert_module_tag" ["Id", doublequotes name, expS]
-- 
-- instance PrologGenerator AST.PropertyExpr where
--   generate (AST.And _ a b) = do
--             l <- generate a
--             r <- generate b
--             return $ predicate "and" [l, r]
--   generate (AST.Or _ a b) = do
--             l <- generate a
--             r <- generate b
--             return $ predicate "or" [l, r]
--   generate (AST.Not _ a) = do
--             l <- generate a
--             return $ predicate "not" [l]
--   generate (AST.Property _ s) = return $ atom s
--   generate (AST.True) = return $ atom "true"
--   generate (AST.False) = return $ atom "false"
-- 
-- 
-- 
-- 
-- 
-- 
-- -- Inside each function we add variable that contains
-- --  * nodeId
-- --  * params
-- --  * constants
-- -- This will return the name of these variables
-- 
-- -- local_inst_name :: String -> String
-- -- local_inst_name x = "ID_" ++ x
-- 
-- local_param_name :: String -> String
-- local_param_name x = "P_" ++ x
-- 
-- -- local_const_name :: String -> String
-- -- local_const_name x = "CONST_" ++ x
-- 
-- -- Generates something a la:
-- -- (ID_RAM) = (['ram', Id])
-- instance PrologGenerator AST.InstanceDeclaration where
--     generate x = return ""
-- --      do
-- --        let var = local_nodeid_name $ AST.instName x
-- --        let decl = list_prepend (doublequotes $ AST.instName x) ("Id")
-- --        return $ var ++ " = " ++ decl
-- 
-- -- Generates something a la:
-- -- (ID_RAM, INKIND_RAM, OUTKIND_RAM) = (['ram' | Id], memory, memory)
-- instance PrologGenerator AST.NodeDeclaration where
--     generate x = do
--         let nn = AST.nodeName x
--         let nn_ref = PlQualifiedRef 
--                 { propName  = nn
--                 , propIndex = Nothing
--                 , instName  = Nothing
--                 , instIndex = Nothing
--                 }
--         nn_ref_s <- generate nn_ref
--         return $ predicate "node_enum" [nn_ref_s, "_"] --eager node enum 
--         
-- -- instance PrologGenerator AST.NodeDeclaration where
-- --     generate x = do
-- --         let var = local_nodeid_name $ AST.nodeName x
-- --         decl_kind_in <- generate (AST.originDomain (AST.nodeType x))
-- --         decl_kind_out <- generate (AST.targetDomain (AST.nodeType x))
-- --         let decl_id = list_prepend (doublequotes $ AST.nodeName x) ("Id")
-- --         let decl_tup = tuple [decl_id, decl_kind_in, decl_kind_out]
-- -- 
-- --         -- Build the variable list
-- --         let pf = AST.nodeName x
-- --         let var_tup = tuple [local_nodeid_name pf, "INKIND_" ++ pf, "OUTKIND_" ++ pf]
-- --         return $ pred_join [
-- --             var_tup ++ " = " ++ decl_tup,
-- --             predicate "node_enum" [local_nodeid_name pf, "_"], --eager node enum 
-- --             predicate "nonvar" ["INKIND_" ++ pf],  -- remove singleton var warning
-- --             predicate "nonvar" ["OUTKIND_" ++ pf]]
-- 
-- 
-- {- Intermediate Prolog Representation -}
-- data PlAddressBlock = PlAddressBlock AST.Address
-- 
-- data PlVariable = PlSockeyeVar String | PlInternalVar String
-- 
-- data PlArrayIndex = PlArrayIndex [PlVariable]
-- 
-- 
-- qualify_ref :: AST.UnqualifiedRef -> AST.UnqualifiedRef -> PlQualifiedRef
-- qualify_ref inst prop = PlQualifiedRef
--     { propName  = AST.refName prop
--     , propIndex = AST.refIndex prop
--     , instName  = Just $ AST.refName inst
--     , instIndex = AST.refIndex inst
--     }
-- 
-- qualify_self :: AST.UnqualifiedRef -> PlQualifiedRef
-- qualify_self prop = PlQualifiedRef
--     { propName  = AST.refName prop
--     , propIndex = AST.refIndex prop
--     , instName  = Nothing
--     , instIndex = Nothing
--     }
-- 
-- qualify_nr :: AST.NodeReference -> PlQualifiedRef
-- qualify_nr (AST.InternalNodeRef _ nr) = qualify_self nr
-- qualify_nr (AST.InputPortRef _ inst node) = qualify_ref inst node
-- 
-- 
-- data ModuleInfo = ModuleInfo
--   {
--     params :: [String],
--     node_type :: Map.Map String ST.NodeType
--   }
-- 
-- 
-- gen_module_info :: AST.Module -> ModuleInfo
-- gen_module_info x =
--   ModuleInfo {
--     params = [],
--     node_type = Map.fromList [(AST.nodeName z, AST.nodeType $ z) | z <- AST.nodeDecls x]
--   }
-- 
-- add_param :: ModuleInfo -> String -> ModuleInfo
-- add_param mi s = ModuleInfo { params = (params mi) ++ [s], node_type = node_type mi}
-- 
-- param_str :: [String] -> String
-- param_str [] = ""
-- param_str li = "," ++ intercalate "," [predicate "param" [p] | p <- li]
-- 
-- 
-- generate_conj :: ModuleInfo -> [AST.Definition] -> String
-- generate_conj mi li = "TODO> NYI"
--    -- intercalate ", sss\n" $ concat [snd(gen_body_defs mi inn 0) | inn <- li]
--    -- TODO: fix the 0 here
-- 
-- 
-- -- generate forall with a explicit variable name
-- {-
-- forall_qual :: ModuleInfo -> String -> AST.NaturalSet -> [AST.Definition] -> String
-- forall_qual mi varName ns body =
--   "(" ++
--    predicate "iblock_values" [generate ns, it_list] ++ "," ++
--    "(" ++
--    predicate "foreach" [it_var, it_list]
--    ++ param_str mi
--    ++ " do \n" ++
--    body_str ++ "\n))"
--    where
--      id_var = "ID_" ++ varName
--      it_var = "IDT_" ++ varName
--      it_list = "IDL_" ++ varName
--      body_str = generate_conj (add_param mi it_var) body
-- -}
-- 
-- {-
-- forall_uqr :: ModuleInfo -> AST.UnqualifiedRef -> String -> String
-- forall_uqr mi ref body_str = case (AST.refIndex ref) of
--   Nothing -> printf "(%s = %s, %s)" it_var id_var body_str
--   Just ai -> "(" ++
--                 predicate "iblock_values" [generate ai, it_list] ++ "," ++
--                 "(" ++
--                 predicate "foreach" [it_var, it_list]
--                 ++ param_str mi
--                 ++ " do " ++
--                 itid_var ++ " = " ++ list_prepend it_var id_var ++ "," ++
--                 body_str ++ "))"
--   where
--     id_var = "ID_" ++ (AST.refName ref)
--     it_var = "IDT_" ++ (AST.refName ref)
--     itid_var = "IDI_" ++ (AST.refName ref)
--     it_list = "IDL_" ++ (AST.refName ref)
-- -}
-- 
-- {-
-- gen_index :: AST.UnqualifiedRef -> String
-- gen_index uqr =
--   case (AST.refIndex uqr) of
--     Nothing -> local_nodeid_name $ AST.refName uqr
--     Just ai -> list_prepend (gen_ai ai) (local_nodeid_name $ AST.refName uqr)
--   where
--     gen_ai (AST.ArrayIndex _ ws) =  list [gen_wildcard_simple w | w <- ws]
--     gen_wildcard_simple (AST.ExplicitSet _ ns) = gen_natural_set ns
--     gen_natural_set (ST.NaturalSet _ nrs) = gen_natural_ranges nrs
--     gen_natural_ranges [nr] = gen_ns_simple nr
--     gen_ns_simple (ST.SingletonRange _ base) = gen_exp_simple base
--     gen_exp_simple (AST.Variable _ vn) = "IDT_" ++ vn
--     gen_exp_simple (AST.Literal _ int) = show int
-- -}
-- 
-- 
-- -- gen_base_address :: AST.Address -> String
-- -- gen_base_address (AST.Address _ ws) = gen_single_ws $ ws !! 0
-- --   where
-- --     gen_single_ws (AST.ExplicitSet _ ns) = gen_single_ns ns
-- --     gen_single_ws (AST.Wildcard _)  = "0 /* WILDCARD_NYI */"
-- --     gen_single_ns (AST.NaturalSet _ nr) = gen_single_nr (nr !! 0)
-- --     gen_single_nr nr = case nr of
-- --         AST.SingletonRange _ b -> generate b
-- --         AST.LimitRange _ b _ -> generate b
-- --         AST.BitsRange _ b bits -> generate b 
-- 
-- 
-- instance PrologGenerator PlAddressBlock where
--     generate (PlAddressBlock ab) = generate ab
--          -- addrStr <- generate ab
--          -- return $ predicate "block" [addrStr]
-- 
-- instance PrologGenerator PlRegionSpec where
--     generate as = do
--         nodeStr <- generate (regNode as)
--         blockStr <- generate (regBlock as)
--         propStr <- generate (regProp as)
--         return $ predicate "region" [nodeStr, blockStr, propStr]
-- 
-- instance PrologGenerator PlNameSpec where
--     generate ns =
--         let
--             gen_addr (Just x) = generate x
--             gen_addr Nothing = return $ "0 /* NYI */"
--         in
--             do
--                 nodeStr <- generate (node ns)
--                 addrStr <- gen_addr (addr ns)
--                 propStr <- generate (prop ns)
--                 return $ predicate "name" [nodeStr, addrStr, propStr]
-- 
-- instance PrologGenerator PlTranslateSpec where
--     generate ts = do
--         srcStr <- generate (src ts)
--         dstStr <- generate (dst ts)
--         ass <- assert_translate srcStr dstStr
--         return ass
-- 
-- instance PrologGenerator PlAcceptSpec where
--     generate (PlAcceptSpec reg) = do
--         regStr <- generate reg
--         ass <- assert_accept regStr
--         return ass
-- 
-- instance PrologGenerator PlPortBindSpec where
--     generate pb =
--         do
--             portS <- generate $ port pb
--             ndS <- generate $ boundNode pb
--             olS <- assert_overlay portS ndS
--             return olS
-- 
-- instance PrologGenerator AST.Definition where
--     generate (AST.Accepts _ n accepts) = do
--         let pl_accepts = map (mk_acc n) accepts
--         accS <- mapM generate pl_accepts
--         return $ pred_join accS
--         where
--             mk_acc :: AST.UnqualifiedRef -> AST.AddressBlock -> PlAcceptSpec
--             mk_acc n ab = 
--                 let
--                     block = PlAddressBlock $ SAST.addresses ab
--                     reg = PlRegionSpec { regNode=n
--                                        , regBlock=block
--                                        , regProp=SAST.properties ab }
--                 in
--                     PlAcceptSpec reg
-- 
--     generate (AST.Maps _ nd maps) = do
--         let pl_trans = concat $ map (mk_trans nd) maps
--         pl_trans_s <- mapM generate pl_trans
--         return $ pred_join pl_trans_s
--         where 
--             mk_trans :: AST.UnqualifiedRef -> AST.MapSpec -> [PlTranslateSpec]
--             mk_trans n mapspec = 
--                 let
--                     ab = AST.mapAddr mapspec
--                     block = PlAddressBlock $ SAST.addresses ab
--                     src = PlRegionSpec { regNode=n
--                                        , regBlock=block
--                                        , regProp=SAST.properties ab }
--                     targets = AST.mapTargets mapspec
--                 in 
--                     map (\x -> PlTranslateSpec {src=src, dst=mk_name x}) targets
-- 
--             mk_name :: AST.MapTarget -> PlNameSpec
--             mk_name t = 
--                 let
--                     ab = AST.targetAddr t
--                     get_ws (SAST.Address _ [ws]) = Just ws
--                     get_ws (SAST.Address _ _) = Nothing
--                     get_ns (SAST.ExplicitSet _ ns) = Just ns
--                     get_ns (SAST.Wildcard _) = Nothing
--                     get_fst (SAST.NaturalSet _ [nr]) = Just nr
--                     get_fst (SAST.NaturalSet _ _) = Nothing
--                     get_base (AST.SingletonRange _ b) = Just b
--                     get_base (AST.LimitRange _ b _) = Just b
--                     get_base (_) = Nothing
--                     add = (get_base <=< get_fst <=< get_ns <=< get_ws) (SAST.addresses ab)
--                 in
--                     PlNameSpec { node=AST.targetNode t
--                                , addr=add
--                                , prop=SAST.properties ab }
-- 
--     generate (AST.BlockOverlays _ src dst bits) = do
--         srcS <- generate src
--         dstS <- generate dst
--         let bitsS = show (bits !! 0)
--         res <- assert_configurable srcS bitsS dstS 
--         return res
-- 
--     generate (AST.Instantiates _ idname imodname args) = do
--         argsS <- mapM generate args
--         idnameS <- generate idname
--         res <- add_mod idnameS imodname argsS
--         return res
-- 
--     generate (AST.Binds _ inst binds) = do
--         let pl_binds = map mk_bind binds 
--         bindsS <- mapM generate pl_binds
--         return $ pred_join bindsS
--         where 
--             mk_bind bind = PlPortBindSpec
--                 { port = qualify_ref inst (AST.boundPort bind)
--                 , boundNode = AST.boundNode bind }
-- 
--     generate (AST.Overlays _ src dst) = do
--         srcS <- generate $ qualify_self src
--         dstS <- generate $ qualify_nr dst
--         olS <- assert_overlay srcS dstS
--         return olS
-- 
--     generate (AST.Forall _ varName varRange body) = do
--         varRangeLi <- get_next_tmp_var
--         varRangeS <- generate varRange
--         push_extra_pred $ predicate "iblock_values" [varRangeS, varRangeLi]
--         scope_vars <- get_scope_vars
-- 
--         bodyDefs <- mapM generate body
--         extPred <- get_extra_pred
-- 
--         return $ 
--             "(" ++
--             predicate "foreach" [local_param_name varName, varRangeLi]
--             ++ param_str scope_vars
--             ++ " do \n    " ++
--             (pred_join (extPred ++ bodyDefs)) ++
--             "\n))"
--             
-- 
--     generate x = throw $ NYIException $ "generate for NYI for " ++ (show x)
-- -- gen_body_defs mi x i = case x of
-- --   (AST.Accepts _ n accepts) -> do
-- -- 
-- --   (AST.Maps _ _ _) -> gen_translate (map_spec_flatten mi x) (i, [])
-- --    --(1, [(assert 0 $ predicate "translate"
-- --     --[generate $ srcNode om, generate $ srcAddr om, generate $ targetNode om, generate $ targetAddr om])
-- --     -- | om <- map_spec_flatten mi x])
-- --   (AST.Overlays _ src dest) -> (i+1, [state_add_overlay i (generate src) (generate dest)])
-- --   (AST.BlockOverlays _ src dst bits) -> gen_blockoverlay (generate src) (generate dst) bits (i, [])
-- --   -- (AST.Instantiates _ i im args) -> [forall_uqr mi i (predicate ("add_" ++ im) ["IDT_" ++ (AST.refName i)])]
-- --   (AST.Instantiates _ ii im args) -> (i+1, [ predicate ("add_" ++ im)
-- --         ([statevar i] ++ [gen_index ii] ++ (map generate args) ++ [statevar (i+1)]) ])
-- --   -- (AST.Binds _ i binds) -> [forall_uqr mi i $ gen_bind_defs ("IDT_" ++ (AST.refName i)) binds]
-- --   (AST.Binds _ ii binds) -> gen_bind_defs (gen_index ii) binds (i, [])
-- --   --(AST.Forall _ varName varRange body) -> (0, [forall_qual mi varName varRange body])
-- --   (AST.Forall _ varName varRange body) -> throw $ NYIException "forall"
-- --   (AST.Converts _ _ _ ) -> throw $ NYIException "Converts"
-- --   where
-- --     new_ab ab = pack_address_block ab
-- 
-- 
-- 
-- -- count_num_facts :: ModuleInfo -> AST.Definition -> Integer
-- -- count_num_facts mi x = case x of
-- --     (AST.Accepts _ n accepts) -> sum([1 | acc <- accepts])
-- --     (AST.Maps _ _ _) -> sum([1 | om <- map_spec_flatten mi x])
-- --     (AST.Overlays _ src dest) -> 1
-- --     -- (AST.Instantiates _ i im args) -> [forall_uqr mi i (predicate ("add_" ++ im) ["IDT_" ++ (AST.refName i)])]
-- --     (AST.Instantiates _ _ _ _) -> 1
-- --     (AST.BlockOverlays _ _ _ bits) -> (toInteger (length bits))
-- --     -- (AST.Binds _ i binds) -> [forall_uqr mi i $ gen_bind_defs ("IDT_" ++ (AST.refName i)) binds]
-- --     (AST.Binds _ i binds) -> sum([1 | b <- binds])
-- --     (AST.Forall _ varName varRange body) -> 0
-- --     (AST.Converts _ _ _ ) -> 0
-- 
-- 
-- 
-- instance PrologGenerator AST.UnqualifiedRef where
--   generate uqr = generate $ qualify_self uqr
-- 
-- instance PrologGenerator PlQualifiedRef where
--     generate qr =
--         let
--             gen_idx Nothing = return Nothing
--             gen_idx (Just x) = do
--                 xS <- generate x
--                 return $ Just xS
--         in do
--             propIndexMS <- gen_idx $ propIndex qr
--             let propNameMS = Just $ propName qr
--             let instNameMS = instName qr
--             instIndexMS <- gen_idx $ instIndex qr
--             let els = catMaybes [
--                             propIndexMS,
--                             propNameMS >>= (Just . doublequotes),
--                             instIndexMS,
--                             instNameMS >>= (Just . doublequotes)]
--             return $ many_list_prepend els "Id"
-- 
-- instance PrologGenerator AST.WildcardSet where
--     generate (AST.ExplicitSet _ ns) = generate ns
--     generate (AST.Wildcard _) = return "block(0,0) /* WILDCARD NYI */"
-- 
-- instance PrologGenerator AST.ArrayIndex where
--     generate (AST.ArrayIndex _ wcs) = do
--         wcsS <- mapM generate wcs
--         return $ brackets $ intercalate "," wcsS
-- 
-- instance PrologGenerator AST.NodeReference where
--     generate (AST.InternalNodeRef _ nn) = generate $ qualify_self nn
--     generate (AST.InputPortRef _ inst node) = generate $ qualify_ref inst node 
-- 
-- instance PrologGenerator AST.Domain where
--   generate (AST.Memory) = return $ atom "memory"
--   generate (AST.Interrupt) = return $ atom "interrupt"
--   generate (AST.Power) = return $ atom "power"
--   generate (AST.Clock) = return $ atom "clock"
-- 
-- instance PrologGenerator AST.AddressBlock where
--   generate ab = generate $ SAST.addresses ab
-- 
-- 
-- instance PrologGenerator AST.Address where
--     generate (AST.Address _ ws) = do
--         wsStr <- mapM generate ws
--         return $ wsStr !! 0
-- 
-- instance PrologGenerator AST.NaturalSet where
--   generate a = case a of
--      AST.NaturalSet _ [nrs] -> generate nrs
--      AST.NaturalSet _ _ -> throw $ NYIException $ "MULTIDIM"
-- 
-- instance PrologGenerator AST.NaturalRange where
--   generate nr = case nr of
--     AST.SingletonRange _ b -> do
--         bS <- generate b
--         return $ predicate "block" [bS,bS]
--     AST.LimitRange _ b l -> do
--         bS <- generate b
--         lS <- generate l
--         return $ predicate "block" [bS,lS]
--     AST.BitsRange _ b bits -> do
--         bS <- generate b
--         bitS <- generate bits
--         limitVar <- get_next_tmp_var
--         push_extra_pred $ limitVar ++ " is " ++ bS ++ " + 2^" ++ bitS ++ " - 1"
--         return $ predicate "block" [bS,limitVar]
-- 
-- instance PrologGenerator AST.NaturalExpr where
--     -- TODO insert monad magic here
--     generate exp = do
--         tmpV <- get_next_tmp_var 
--         expS <- genm exp
--         push_extra_pred $ tmpV ++ " is " ++ expS
--         return tmpV
--         where
--             genm :: AST.NaturalExpr -> ModLevelStateS String
--             genm (SAST.Addition _ a b) = do
--                 aS <- genm a
--                 bS <- genm b
--                 return $ "(" ++ aS ++ ")+(" ++ bS ++ ")"
--             genm (SAST.Subtraction _ a b) = do
--                 aS <- genm a
--                 bS <- genm b
--                 return $ "(" ++ aS ++ ")-(" ++ bS ++ ")"
--             genm (SAST.Multiplication _ a b) = do
--                 aS <- genm a
--                 bS <- genm b
--                 return $ "(" ++ aS ++ ")*(" ++ bS ++ ")"
--             -- and the terminals
--             genm x = return $ gen x 
-- 
--             gen :: AST.NaturalExpr -> String
--             gen (SAST.Constant _ v) = local_const_name v
--             gen (SAST.Variable _ v) = local_param_name v
--             gen (SAST.Parameter _ v) = local_param_name v
--             gen (SAST.Literal _ n) = show n
--             gen (SAST.Slice _ a bitrange) = "SLICE NYI"
--             gen (SAST.Concat _ a b) = "CONCAT NYI"
-- 
-- 
-- 
-- 
-- 
-- -- nat_range_from :: AST.NaturalRange -> String
-- -- nat_range_from nr = case nr of
-- --   AST.SingletonRange _ b -> generate b
-- --   AST.LimitRange _ b _ -> generate b
-- --   AST.BitsRange _ _ _ -> "BitsRange NOT IMPLEMENTED"
-- -- 
-- -- nat_range_to :: AST.NaturalRange -> String
-- -- nat_range_to nr = case nr of
-- --   AST.SingletonRange _ b -> generate b
-- --   AST.LimitRange _ _ l -> generate l
-- --   AST.BitsRange _ _ _ -> "BitsRange NOT IMPLEMENTED"
-- -- 
-- -- -- Params are variables passed into the for body
-- -- for_body_inner :: [String] -> String -> String -> (Int, AST.NaturalRange)  -> String
-- -- for_body_inner params itvar body itrange  =
-- --   let
-- --     itvar_local = itvar ++ (show $ fst itrange)
-- --     from = nat_range_from $ (snd itrange)
-- --     to = nat_range_to $ (snd itrange)
-- --     for = printf "for(%s,%s,%s)" itvar_local from to :: String
-- --     paramf x  = printf "param(%s)" x :: String
-- --     header = intercalate "," ([for] ++ map paramf params)
-- --     in printf "(%s \ndo\n %s \n)" header body
-- -- 
-- -- enumerate = zip [0..]
-- -- 
-- -- for_body :: [String] -> String -> AST.NaturalSet -> String -> String
-- -- for_body params itvar (AST.NaturalSet _ ranges) body =
-- --   foldl fbi body (enumerate ranges)
-- --   where
-- --     fbi = for_body_inner params itvar
-- --
-- pred_join :: [String] -> String 
-- pred_join = intercalate ",\n    "
-- 
-- 
-- add_mod ::  String -> String -> [String] -> ModLevelStateS String
-- add_mod idname modname args = do
--     sIn <- get_state_var
--     sOut <- get_next_state_var
--     return $ predicate ("add_" ++ modname) ([sIn, idname] ++ args ++ [sOut])
-- 
-- assert_translate ::  String -> String -> ModLevelStateS String
-- assert_translate src dst = do
--     sIn <- get_state_var
--     sOut <- get_next_state_var
--     return $ predicate "assert_translate" [sIn, src, dst, sOut]
-- 
-- assert_accept ::  String -> ModLevelStateS String
-- assert_accept reg = do
--     sIn <- get_state_var
--     sOut <- get_next_state_var
--     return $ predicate "assert_accept" [sIn, reg, sOut]
-- 
-- assert_overlay ::  String -> String -> ModLevelStateS String
-- assert_overlay src dst = do
--     sIn <- get_state_var
--     sOut <- get_next_state_var
--     return $ predicate "assert_overlay" [sIn, src, dst, sOut]
-- 
-- assert_configurable ::   String -> String -> String -> ModLevelStateS String
-- assert_configurable src bits dst = do
--     sIn <- get_state_var
--     sOut <- get_next_state_var
--     return $ predicate "assert_configurable" [sIn, src, bits, dst, sOut]
