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

module SockeyeBackendProlog
( compile, compileDirect ) where

import qualified Data.Map as Map
import Data.Char
import Data.List
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
    st <- get
    modify (\c -> c { stateCount = (stateCount c) + 1})
    return $ statevar (stateCount st)

{- add an extra predicate to be inserted at the module level -}
push_extra_pred :: String -> ModLevelStateS ()
push_extra_pred pred = do
    st <- get
    modify (\c -> c { extraPred = (pred : (extraPred c))})
    return ()


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


gen_body_def_maps :: ModuleInfo -> [AST.Definition] -> Integer -> [String]
gen_body_def_maps m [] _ = []
gen_body_def_maps m (xs:x) i = elms  ++ (gen_body_def_maps m x (ii))
    where
        (ii, elms) = (gen_body_defs m xs i)

instance PrologGenerator AST.Module where
  generate m = do
    let name = "add_" ++ AST.moduleName m
    let mi = gen_module_info m
    let p1 = gen_nat_param_list (AST.parameters m)
    let bodyChecks = [predicate "is_list" ["Id"]] ++ map (\x -> predicate "nonvar" [x]) p1

    nodeDecls <- mapM generate (AST.nodeDecls m)
    instDecls <- mapM gen_inst_decls (AST.instDecls m)

    -- bodyDefs = concat $ map (gen_body_defs mi) (AST.definitions m)
    -- let bodyDefs = gen_body_def_maps mi  (AST.definitions m) 0
    let bodyDefs =  []
    -- n =  sum $ map (count_num_facts mi) (AST.definitions m)

    let body = intercalate ",\n    " $ bodyChecks ++ nodeDecls ++ instDecls ++ bodyDefs
    cS <- get_state_var
    return $ name ++ stringify ([statevar 0, "Id"] ++ p1 ++ [cS]) ++ " :- \n    " ++ body ++ ".\n\n"
    where
      stringify [] = ""
      stringify pp = parens $ intercalate "," pp


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
local_nodeid_name :: String -> String
local_nodeid_name x = "ID_" ++ x

local_inst_name :: String -> String
local_inst_name x = "ID_" ++ x

local_param_name :: String -> String
local_param_name x = "P_" ++ x

local_const_name :: String -> String
local_const_name x = "CONST_" ++ x

-- Generates something a la:
-- (ID_RAM) = (['ram', Id])
instance PrologGenerator AST.InstanceDeclaration where
    generate x =
      do
        let var = local_nodeid_name $ AST.instName x
        let decl = list_prepend (doublequotes $ AST.instName x) ("Id")
        return $ var ++ " = " ++ decl

-- Generates something a la:
-- (ID_RAM, INKIND_RAM, OUTKIND_RAM) = (['ram' | Id], memory, memory)
instance PrologGenerator AST.NodeDeclaration where
    generate x = do
        let var = local_nodeid_name $ AST.nodeName x
        decl_kind_in <- generate (AST.originDomain (AST.nodeType x))
        decl_kind_out <- generate (AST.targetDomain (AST.nodeType x))
        let decl_id = list_prepend (doublequotes $ AST.nodeName x) ("Id")
        let decl_tup = tuple [decl_id, decl_kind_in, decl_kind_out]

        -- Build the variable list
        let pf = AST.nodeName x
        let var_tup = tuple [local_nodeid_name pf, "INKIND_" ++ pf, "OUTKIND_" ++ pf]
        return $ intercalate ",\n" [
            var_tup ++ " = " ++ decl_tup,
            predicate "node_enum" [local_nodeid_name pf, "_"], --eager node enum 
            predicate "nonvar" ["INKIND_" ++ pf],  -- remove singleton var warning
            predicate "nonvar" ["OUTKIND_" ++ pf]]


-- This transformation is probably better to be in an AST transform
data MyAddressBlock = MyAddressBlock {
  domain :: !AST.Domain,
  addresses     :: AST.Address,
  properties    :: AST.PropertyExpr
  }

pack_address_block :: AST.AddressBlock -> MyAddressBlock
pack_address_block ab = MyAddressBlock {
  domain = AST.Memory,
  addresses = SAST.addresses ab,
  properties = SAST.properties ab
}

data OneMapSpec = OneMapSpec
  {
  srcNode :: AST.UnqualifiedRef,
  srcAddr :: MyAddressBlock,
  targetNode :: AST.NodeReference,
  targetAddr :: MyAddressBlock
  }


map_spec_flatten :: ModuleInfo -> AST.Definition -> [OneMapSpec]
map_spec_flatten mi def = case def of
  (AST.Maps _ n maps) ->
    [OneMapSpec n (src_ab n $ AST.mapAddr map_spec) (AST.targetNode map_target) (dest_ab n $ AST.targetAddr map_target)
      | map_spec <- maps, map_target <- (AST.mapTargets map_spec)]
  _ -> []
  where
    nt uqr = (node_type mi) Map.! (AST.refName uqr)
    src_ab uqr ab = MyAddressBlock {
      domain = ST.originDomain $ nt uqr,
      properties = SAST.properties ab,
      addresses = SAST.addresses ab
    }
    dest_ab uqr ab = MyAddressBlock {
      domain = ST.targetDomain $ nt uqr,
      addresses = SAST.addresses ab,
      properties = SAST.properties ab
    }

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
    insts = [local_inst_name $ AST.instName d | d <- AST.instDecls x]
    mparams = (gen_nat_param_list $ AST.parameters x)
    nodes = [local_nodeid_name $ AST.nodeName d | d <- AST.nodeDecls x]

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



gen_bind_defs :: String -> [AST.PortBinding] -> (Integer, [String]) -> (Integer, [String])
gen_bind_defs uql_var [] s = s
gen_bind_defs uql_var (x:xs) (i, s) = gen_bind_defs uql_var xs (i + 1, s ++ [ovl])
    where
        ovl = state_add_overlay i src dest
        dest =  generate $ AST.boundNode x
        src = list_prepend (doublequotes $ AST.refName $ AST.boundPort $ x) uql_var

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


gen_accept :: AST.UnqualifiedRef -> [AST.AddressBlock] -> (Integer, [String]) -> (Integer, [String])
gen_accept _ [] s = s
gen_accept n (xs:x) (i, s) = gen_accept n x (i + 1, s ++ [acc])
    where
        acc = state_add_accept i (predicate "region" [generate n, generate pab, generate $ properties pab ])
        pab = pack_address_block xs

gen_base_address :: AST.Address -> String
gen_base_address (AST.Address _ ws) = gen_single_ws $ ws !! 0
  where
    gen_single_ws (AST.ExplicitSet _ ns) = gen_single_ns ns
    gen_single_ws (AST.Wildcard _)  = "0 /* WILDCARD_NYI */"
    gen_single_ns (AST.NaturalSet _ nr) = gen_single_nr (nr !! 0)
    gen_single_nr nr = case nr of
        AST.SingletonRange _ b -> generate b
        AST.LimitRange _ b _ -> generate b
        AST.BitsRange _ b bits -> generate b 

gen_translate :: [OneMapSpec] -> (Integer, [String]) -> (Integer, [String])
gen_translate [] s = s
gen_translate (om:x) (i, s) = gen_translate x (i + 1, s ++ [trs])
    where
        trs = state_add_mapping i (gen_region (srcNode om) (srcAddr om)) (gen_name (targetNode om) (targetAddr om))
        gen_region nodeid x = predicate "region" [generate nodeid, generate x, generate $ properties x]
        gen_name nodeid x = predicate "name" [generate nodeid, gen_base_address $ addresses x, generate $ properties x]

gen_blockoverlay :: String -> String -> [Integer] -> (Integer, [String]) -> (Integer, [String])
gen_blockoverlay src dst [] s = s
gen_blockoverlay src dst (om:x) (i, s) = gen_blockoverlay src dst  x (i + 1, s ++ [trs])
    where
        trs = state_add_block_meta i src (show om) dst

gen_body_defs :: ModuleInfo -> AST.Definition -> Integer -> (Integer, [String])
instance PrologGenerator AST.Definition where
    generate x = do

gen_body_defs mi x i = case x of
  (AST.Accepts _ n accepts) -> do
                        nStr <- generate n
  (AST.Maps _ _ _) -> gen_translate (map_spec_flatten mi x) (i, [])
   --(1, [(assert 0 $ predicate "translate"
    --[generate $ srcNode om, generate $ srcAddr om, generate $ targetNode om, generate $ targetAddr om])
    -- | om <- map_spec_flatten mi x])
  (AST.Overlays _ src dest) -> (i+1, [state_add_overlay i (generate src) (generate dest)])
  (AST.BlockOverlays _ src dst bits) -> gen_blockoverlay (generate src) (generate dst) bits (i, [])
  -- (AST.Instantiates _ i im args) -> [forall_uqr mi i (predicate ("add_" ++ im) ["IDT_" ++ (AST.refName i)])]
  (AST.Instantiates _ ii im args) -> (i+1, [ predicate ("add_" ++ im)
        ([statevar i] ++ [gen_index ii] ++ (map generate args) ++ [statevar (i+1)]) ])
  -- (AST.Binds _ i binds) -> [forall_uqr mi i $ gen_bind_defs ("IDT_" ++ (AST.refName i)) binds]
  (AST.Binds _ ii binds) -> gen_bind_defs (gen_index ii) binds (i, [])
  --(AST.Forall _ varName varRange body) -> (0, [forall_qual mi varName varRange body])
  (AST.Forall _ varName varRange body) -> throw $ NYIException "forall"
  (AST.Converts _ _ _ ) -> throw $ NYIException "Converts"
  where
    new_ab ab = pack_address_block ab

count_num_facts :: ModuleInfo -> AST.Definition -> Integer
count_num_facts mi x = case x of
    (AST.Accepts _ n accepts) -> sum([1 | acc <- accepts])
    (AST.Maps _ _ _) -> sum([1 | om <- map_spec_flatten mi x])
    (AST.Overlays _ src dest) -> 1
    -- (AST.Instantiates _ i im args) -> [forall_uqr mi i (predicate ("add_" ++ im) ["IDT_" ++ (AST.refName i)])]
    (AST.Instantiates _ _ _ _) -> 1
    (AST.BlockOverlays _ _ _ bits) -> (toInteger (length bits))
    -- (AST.Binds _ i binds) -> [forall_uqr mi i $ gen_bind_defs ("IDT_" ++ (AST.refName i)) binds]
    (AST.Binds _ i binds) -> sum([1 | b <- binds])
    (AST.Forall _ varName varRange body) -> 0
    (AST.Converts _ _ _ ) -> 0



instance PrologGenerator AST.UnqualifiedRef where
  generate uq = case (AST.refIndex uq) of
    Nothing -> local_nodeid_name $ AST.refName uq
    Just ai -> list_prepend (generate ai) (local_nodeid_name $ AST.refName uq)

instance PrologGenerator AST.WildcardSet where
  generate a = case a of
    AST.ExplicitSet _ ns -> generate ns
    AST.Wildcard _ -> "NYI!?"

instance PrologGenerator AST.ArrayIndex where
  generate (AST.ArrayIndex _ wcs) = brackets $ intercalate "," [generate x | x <- wcs]

instance PrologGenerator AST.MapSpec where
  generate ms = struct "map" [("src_block", generate (AST.mapAddr ms)),
    ("dests", list $ map generate (AST.mapTargets ms))]

instance PrologGenerator AST.MapTarget where
  generate mt = struct "dest" [("id", generate (AST.targetNode mt)),
    ("base", generate (AST.targetAddr mt))]

instance PrologGenerator AST.NodeReference where
  generate nr = case nr of
    AST.InternalNodeRef _ nn -> gen_index nn
    AST.InputPortRef _ inst node -> list_prepend (doublequotes $ AST.refName node) (gen_index inst)

instance PrologGenerator MyAddressBlock where
  -- We have to generate something like this, probably involves an extra step in the AST.
  -- pred_99(propspec) :- member(prop1, propspec), member(prop2, propspec
  -- node_accept( ..., block{propspec: pred_99}).
  -- to check: B = block{propspec: PS}, call(PS, current_properties)
  generate ab = blocks !! 0
    where
      blocks = gen_a $ addresses ab
      gen_a (AST.Address _ ws) = map gen_ws ws
      gen_ws (AST.ExplicitSet _ ns) = generate ns
      gen_ws (AST.Wildcard _ ) = "block(0,0) /* WILDCARD NYI */"

instance PrologGenerator AST.Domain where
  generate d = case d of
    AST.Memory -> atom "memory"
    AST.Interrupt -> atom "interrupt"
    AST.Power -> atom "power"
    AST.Clock -> atom "clock"

instance PrologGenerator AST.AddressBlock where
  -- TODO: add properties
  -- We have to generate something like this, probably involves an extra step in the AST.
  -- pred_99(propspec) :- member(prop1, propspec), member(prop2, propspec
  -- node_accept( ..., block{propspec: pred_99}).
  -- to check: B = block{propspec: PS}, call(PS, current_properties)
  generate ab = generate $ SAST.addresses ab


instance PrologGenerator AST.Address where
  generate a = case a of
    AST.Address _ ws -> tuple $ map generate ws

instance PrologGenerator AST.NaturalSet where
  generate a = case a of
     AST.NaturalSet _ [nrs] -> generate nrs
     AST.NaturalSet _ _ -> "NO MULTIDIM"

instance PrologGenerator AST.NaturalRange where
  generate nr = case nr of
    AST.SingletonRange _ b -> predicate "block"
      [generate b, generate b]
    AST.LimitRange _ b l -> predicate "block"
      [generate b, generate l]
    AST.BitsRange _ b bits -> "BITSRANGE NYI"
    -- AST.BitsRange _ b bits -> 
    --        -- TODO: This will totally fail if a module uses two of those
    --        -- we need a way to get temporary variable names
    --        "TMP_BASE is " ++ (generate b) ++ "," ++
    --        "TMP_LIMIT is TMP_BASE + 2^(" ++ (generate bits) ++ ")-1," ++
    --        (predicate "block" ["TMP_BASE", "TMP_LIMIT"])

instance PrologGenerator AST.NaturalExpr where
  generate nr = case nr of
    SAST.Constant _ v -> local_const_name v
    SAST.Variable _ v -> "IDT_" ++ v
    SAST.Parameter _ v -> local_param_name v
    SAST.Literal _ n -> show n
    SAST.Addition _ a b -> "(" ++ generate a ++ ")+(" ++ generate b ++ ")"
    SAST.Subtraction _ a b -> "(" ++ generate a ++ ")-(" ++ generate b ++ ")"
    SAST.Multiplication _ a b -> "(" ++ generate a ++ ")*(" ++ generate b ++ ")"
    SAST.Slice _ a bitrange -> "SLICE NYI"
    SAST.Concat _ a b -> "CONCAT NYI"


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


nat_range_from :: AST.NaturalRange -> String
nat_range_from nr = case nr of
  AST.SingletonRange _ b -> generate b
  AST.LimitRange _ b _ -> generate b
  AST.BitsRange _ _ _ -> "BitsRange NOT IMPLEMENTED"

nat_range_to :: AST.NaturalRange -> String
nat_range_to nr = case nr of
  AST.SingletonRange _ b -> generate b
  AST.LimitRange _ _ l -> generate l
  AST.BitsRange _ _ _ -> "BitsRange NOT IMPLEMENTED"

-- Params are variables passed into the for body
for_body_inner :: [String] -> String -> String -> (Int, AST.NaturalRange)  -> String
for_body_inner params itvar body itrange  =
  let
    itvar_local = itvar ++ (show $ fst itrange)
    from = nat_range_from $ (snd itrange)
    to = nat_range_to $ (snd itrange)
    for = printf "for(%s,%s,%s)" itvar_local from to :: String
    paramf x  = printf "param(%s)" x :: String
    header = intercalate "," ([for] ++ map paramf params)
    in printf "(%s \ndo\n %s \n)" header body

enumerate = zip [0..]

for_body :: [String] -> String -> AST.NaturalSet -> String -> String
for_body params itvar (AST.NaturalSet _ ranges) body =
  foldl fbi body (enumerate ranges)
  where
    fbi = for_body_inner params itvar

statevar :: Int -> String
statevar i = printf "S%i" i

assert_translate ::  String -> String -> ModLevelStateS String
assert_translate src dst = "assert_translate" ++ parens ((statevar i) ++ ", " ++  src ++ ", " ++ dst ++ ", " ++ (statevar (i + 1)))

assert_accept ::  String -> ModLevelStateS String
assert_accept reg = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ "assert_accept" ++ parens (sIn ++ ", " ++  reg ++ ", " ++ sOut)

state_add_overlay ::  Integer -> String -> String -> String
state_add_overlay i src dst = "assert_overlay" ++ parens ((statevar i) ++ ", " ++  src ++ ", " ++ dst ++ ", " ++ (statevar (i + 1)))

state_add_block_meta ::  Integer -> String -> String -> String -> String
state_add_block_meta i src bits dst = "assert_configurable" ++ parens ((statevar i) ++ ", " ++  src ++ ", " ++ bits ++ ", " ++ dst ++ ", " ++ (statevar (i + 1)))
