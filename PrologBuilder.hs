{-
    PrologBuilder.hs: Generate Prolog AST

    Part of Sockeye

    Copyright (c) 2019, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module PrologBuilder ( build ) where

import qualified Data.Map as Map
import Data.Char
import Data.List
import Control.Exception (throw, Exception)
import Control.Monad.State.Strict

import qualified SockeyeAST as SAST
import qualified SockeyeParserAST as AST
import PrologAST 

{- Exceptions -}

data PrologBuilderException
  =  NYIException String
  |  AssertException String 
  deriving(Show)

instance Exception PrologBuilderException

{- Entry point and super-module-level generators -}

concatf :: [PlFile] -> PlFile
concatf fs = 
    let 
        cat (PlFile a) (PlFile b) = PlFile (a++b)
    in
        foldl cat (PlFile []) fs

build :: AST.Sockeye -> PlFile
build s = let
    files = map snd (Map.toList (AST.files s))
    in concatf (map generate_file files)

generate_file :: AST.SockeyeFile -> PlFile
generate_file f = PlFile (map gen_module (AST.modules f))

{- Generator state -} 
data CtxState = CtxState { tmpCount :: Int
                         , ctxPred :: [PlExtraPred]
                         }

type SM = State CtxState 

init_ctx_state :: CtxState 
init_ctx_state = CtxState { tmpCount = 0
                          , ctxPred = []
                          }

get_next_tmp_var :: SM PlVar
get_next_tmp_var = do
     st <- get
     modify (\c -> c { tmpCount = (tmpCount c) + 1})
     return $ PlIntVar (tmpCount st)

gen_exp :: NaturalExpr -> SM PlVar
gen_exp exp = do
    var <- get_next_tmp_var 
    modify (\c -> c { ctxPred = (ctxPred c) ++ [PlIsPred var exp]})
    return var

materialize_multid_set :: PlMultiDSet -> SM PlVar
materialize_multid_set s = do
    var <- get_next_tmp_var 
    modify (\c -> c { ctxPred = (ctxPred c) ++ [PlValues var s]})
    return var

gen_limit_exp :: PlVar -> PlVar -> SM PlVar
gen_limit_exp base bits = do
    var <- get_next_tmp_var 
    modify (\c -> c { ctxPred = (ctxPred c) ++ [PlBitsLimit var base bits]})
    return var

{- Wrapping of indices  -}

-- data QuantSpec = NoQuant PlQualifiedRef                      -- No forall needed, 
--                | OneQuant PlQualifiedRef String AST.ArrayIndex   -- We need a forall quantization
-- 
-- wrap :: AST.Definition -> SM PlDefinition
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
--         self_qual :: AST.UnqualifiedRef -> SM QuantSpec
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

{-
 - This will turn an unqualified reference into a PlQualifiedReference
 - and call the generator with it.
 - If the reference contains block statements and it is therefore 
 - impossible to directly express it in a predicate, we wrap it in a forall.
 - Then the qualifiedRef will contain a variable to the iterator -}

wrap_uqr :: AST.UnqualifiedRef -> (PlQualifiedRef -> SM [PlDefinition]) -> SM [PlDefinition]
wrap_uqr (AST.UnqualifiedRef _ name Nothing) gen =
    gen (PlQualifiedRef name Nothing Nothing Nothing)
wrap_uqr (AST.UnqualifiedRef _ name (Just ai)) gen =
    do
        matV <- gen_array_idx ai
        itV <- get_next_tmp_var
        let it_ref = PlQualifiedRef { propName = show itV
                                    , propIndex = Nothing
                                    , instName = Nothing
                                    , instIndex = Nothing
                                    }
        ctx <- get
        let body = gen_body init_ctx_state (gen it_ref)
        return $ [PlForall itV matV body]

wrap_nr :: AST.NodeReference -> (PlQualifiedRef -> SM [PlDefinition]) -> SM [PlDefinition]
wrap_nr (AST.InternalNodeRef _ uqr) gen = wrap_uqr uqr gen
wrap_nr (AST.InputPortRef _ inst node) gen_outer =
    let
        gen_inner2 :: PlQualifiedRef -> PlQualifiedRef -> SM [PlDefinition]
        gen_inner2 instR nodeR =
            -- Merge the instR and nodeR into a new one
            gen_outer $ PlQualifiedRef { propName = propName nodeR
                                       , propIndex = propIndex nodeR
                                       , instName = Just (propName instR)
                                       , instIndex = propIndex instR
                                       }
        gen_inner1 :: PlQualifiedRef -> SM [PlDefinition]
        gen_inner1 instR = wrap_uqr node (gen_inner2 instR)
    in
        wrap_uqr inst gen_inner1

--wrap_nrs :: [AST.NodeReference] -> ([PlQualifiedRef] -> SM [PlDefinition]) -> SM [PlDefinition]
--wrap_nrs nrs gen = rec nrs [] 
--    where
--        rec :: [AST.NodeReference] -> [PlQualifiedRef] -> SM [PlDefinition]
--        rec [] acc = gen acc
--        rec (x:xs) acc = wrap_nr x (\qr -> rec xs (qr:acc))

gen_nr :: AST.NaturalRange -> SM PlNaturalRange
gen_nr (AST.SingletonRange _ base) = do
    bV <- gen_exp base      
    return $ PlNaturalRange bV bV
gen_nr (AST.LimitRange _ base limit) = do
    bV <- gen_exp base      
    lV <- gen_exp limit      
    return $ PlNaturalRange bV lV
gen_nr (AST.BitsRange _ base bits) = do
    baseV <- gen_exp base      
    bitsV <- gen_exp bits      
    limitV <- gen_limit_exp baseV bitsV
    return $ PlNaturalRange baseV limitV

gen_ns :: AST.NaturalSet -> SM PlNaturalSet
gen_ns (AST.NaturalSet _ nr) = do
    rs <- mapM gen_nr nr
    return $ PlNaturalSet rs

gen_ws :: AST.WildcardSet -> SM PlNaturalSet
gen_ws (AST.ExplicitSet _ ns) = gen_ns ns
gen_ws (AST.Wildcard _) = throw $ NYIException "Wildcard"

gen_ai :: AST.ArrayIndex -> SM PlMultiDSet
gen_ai (AST.ArrayIndex m ws) = do
    natsets <- mapM gen_ws ws
    return $ PlMultiDSet natsets

gen_addr :: AST.Address -> SM PlMultiDSet
gen_addr (AST.Address _ ws) = do
    natsets <- mapM gen_ws ws
    return $ PlMultiDSet natsets

gen_array_idx :: AST.ArrayIndex -> SM PlVar
gen_array_idx (AST.ArrayIndex _ ws) = do
    natsets <- mapM gen_ws ws
    var <- materialize_multid_set (PlMultiDSet natsets)
    return var

gen_module :: AST.Module -> PlModule
gen_module m = PlModule
                    { moduleMeta = (AST.moduleMeta m)
                    , moduleName = (AST.moduleName m)
                    , moduleTags = (AST.moduleTags m)
                    , parameters = (AST.parameters m)
                    , nodeDecls  = (AST.nodeDecls  m)
                    , constants  = (AST.constants  m)
                    , body       = gen_body init_ctx_state (gen_defs $ AST.definitions m)
                    }

gen_body :: CtxState -> SM [PlDefinition] -> PlBody
gen_body state gen = 
    let
        res = runState gen state
        plDefs = fst res    
        plExt = ctxPred $ snd res
    in
        PlBody plExt plDefs
          
gen_defs :: [AST.Definition] -> SM [PlDefinition]
gen_defs defs = do
        mdefs <- mapM gen_def defs
        return $ concat mdefs

gen_region :: PlQualifiedRef -> AST.AddressBlock -> SM PlRegionSpec
gen_region qr ab = do
    block <- gen_addr (SAST.addresses ab)
    return PlRegionSpec { regNode=qr
                        , regBlock=block
                        , regProp=SAST.properties ab }

gen_def :: AST.Definition -> SM [PlDefinition]
gen_def (AST.Accepts m n accepts) =
    let
        mk_acc :: PlQualifiedRef -> AST.AddressBlock -> SM PlDefinition
        mk_acc nq ab = do
            reg <- gen_region nq ab
            return $ PlAccepts m reg

        n_qual :: PlQualifiedRef -> SM [PlDefinition]
        n_qual nq = do
            def <- mapM (mk_acc nq) accepts
            return def
    in wrap_uqr n n_qual

gen_def (AST.Maps meta n maps) =
    let
        mk_target_i :: PlRegionSpec -> AST.AddressBlock -> PlQualifiedRef -> SM [PlDefinition]
        mk_target_i srcReg destAb destN = do
            destReg <- gen_region destN destAb
            return $ [PlTranslate meta srcReg destReg]

        mk_target :: PlRegionSpec -> AST.MapTarget -> SM [PlDefinition]
        mk_target srcReg mt = do
            let wrapi = mk_target_i srcReg (AST.targetAddr mt)
            defs <- wrap_nr (AST.targetNode mt) wrapi
            return $ defs
            
        mk_map :: PlQualifiedRef -> AST.MapSpec -> SM [PlDefinition]
        mk_map srcNode ms = do
            srcReg <- gen_region srcNode (AST.mapAddr ms)
            res <- mapM (mk_target srcReg) (AST.mapTargets ms)
            return $ concat res

        mk_maps :: PlQualifiedRef -> SM [PlDefinition]
        mk_maps nq = do
            def <- mapM (mk_map nq) maps
            return $ concat def

    in wrap_uqr n mk_maps
        
gen_def (AST.Converts m node converts) = return []
gen_def (AST.Overlays m node overlays) = return []
gen_def (AST.BlockOverlays m node overlays sizes) = return []
gen_def (AST.Instantiates m inst instModule arguments) = return []
gen_def (AST.Binds m inst bindings) = return []
gen_def (AST.Forall m varName varRange body) = return []

