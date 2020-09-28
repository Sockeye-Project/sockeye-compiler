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
  TODO:
  * wildcards
  * slice
-}

{-# LANGUAGE ScopedTypeVariables #-}

module SockeyeBackendProlog
( compile ) where

import Data.Char ( isAlphaNum, isLower, isLetter )
import Data.List ( intercalate )
import Data.Maybe ( catMaybes )
import Text.Printf ( printf )
import Control.Exception ( Exception )
import Control.Monad.State.Strict
    ( MonadState(get), State, modify, runState )

import PrologAST
    ( ModuleParameter(paramName),
      PropertyExpr(..),
      NamedConstant(..),
      PlNaturalExpr(Literal, SockeyeVariable, BitLimit, Concat, Slice,
                    Multiplication, Subtraction, Addition),
      PlRegionSpec(regProp, regBlock, regNode),
      PlNameSpec(PlNameSpec),
      PlQualifiedRef(PlQualifiedRef),
      PlImmediate(..),
      PlDefinition(PlForall, PlInstantiates, PlConfOverlays, PlOverlays,
                   PlTranslate, PlAccepts, PlModuleTag),
      PlBody(definitions, extraPred),
      PlExtraPred(PlIsPred, PlMultiDValues, PlValues),
      PlMultiDSet(..),
      PlNaturalSet(..),
      PlNaturalRange(PlNaturalRange),
      PlVar(..),
      PlModule(body, parameters, constants, moduleName),
      PlFile(..) ) 

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
        join_bd pts = intercalate ",\n" $ filter (any isLetter) pts
        name = "add_" ++ (moduleName mod)
        -- constants
        bodyConsts = map generate_const (constants mod)
        -- parameters from sockeye
        pSock = map (sock_var_to_pl . paramName) (parameters mod)
        bodyChecks = [predicate "is_list" ["Id"]] ++ map (\x -> predicate "nonvar" [x]) pSock
        bodyPreamble = bodyConsts ++ bodyChecks
        (bodyDefStr,ctx) = generate_body (init_toplevel_ctx mod) (body mod)
        bodyStr = join_bd [(pred_join 0 $ bodyPreamble), bodyDefStr]
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
    generate (PlModuleTag _ name exp) = do
        expS <- generate exp
        return $ predicate "assert_module_tag" ["Id", doublequotes name, expS]
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
    generate (PlConfOverlays _ node overlays) = do
        nodeStr <- generate node
        overlaysStr <- generate overlays
        overlaysStr <- generate overlays
        ass <- assert_configurable nodeStr overlaysStr
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
    generate (PlValues varName valNatSet) = do
            varNameS <- generate varName
            valSetS <- generate valNatSet
            return $ predicate "nat_values" [valSetS, varNameS]
    generate (PlMultiDValues varName valMultiDSet) = do
            varNameS <- generate varName
            valSetS <- generate valMultiDSet
            return $ predicate "multid_values" [valSetS, varNameS]
    generate (PlIsPred varName expr) = do
            varNameS <- generate varName
            exprS <- generate expr
            return $ varNameS ++ " is " ++ exprS

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

instance PrologGenerator PlNaturalExpr where
    generate = genm
        where
            genm :: PlNaturalExpr -> SM String
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
            genm (Slice _ a bitStart bitEnd) = do
                aS <- genm a
                bitStartS <- genm bitStart
                bitEndS <- genm bitEnd
                return $ predicate "bitslice" [aS,bitStartS,bitEndS]

            genm (Concat _ a b bBits) = do
                aS <- genm a
                bS <- genm b
                bBitsS <- genm bBits
                return $ predicate "bitconcat" [aS,bS,bBitsS]

            genm (BitLimit _ base bits) = do
                baseS <- genm base
                bitsS <- genm bits
                return $ "(" ++ baseS ++ " + 2^" ++ bitsS ++ " - 1" ++ ")"

            genm x = return $ gen x
            gen :: PlNaturalExpr -> String
            gen (SockeyeVariable _ v) = sock_var_to_pl v
            gen (Literal _ n) = show n


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

assert_configurable ::   String -> String -> SM String
assert_configurable src dst = do
    sIn <- get_state_var
    sOut <- get_next_state_var
    return $ predicate "assert_configurable" [sIn, src, dst, sOut]

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

list :: [String] -> String
list elems = brackets $ intercalate "," elems

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

quotes :: String -> String
quotes = enclose "'" "'"

doublequotes :: String -> String
doublequotes = enclose "\"" "\""
