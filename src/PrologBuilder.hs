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
import Control.Exception ( throw, Exception )
import Control.Monad.State.Strict
    ( MonadState(get), State, modify, runState )

import qualified SockeyeAST as SAST
import qualified SockeyeParserAST as AST
import PrologAST
    ( PlNaturalExpr(BitLimit, Literal, SockeyeVariable, Multiplication,
                    Addition, Slice, Subtraction, Concat),
      PlRegionSpec(..),
      PlQualifiedRef(..),
      PlImmediate(PlImmediateVar, PlImmediateStr),
      PlDefinition(PlConfOverlays, PlOverlays, PlInstantiates,
                   PlTranslate, PlAccepts, PlModuleTag, PlForall),
      PlBody(PlBody),
      PlExtraPred(PlValues, PlMultiDValues, PlIsPred),
      PlMultiDSet(..),
      PlNaturalSet(..),
      PlNaturalRange(PlNaturalRange),
      PlVar(..),
      PlModule(..),
      PlFile(..) ) 

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

-- {- We build this data structures to lookup declarations etc in the current module -}

{- Generator state -} 
data CtxState = CtxState { tmpCount :: Int
                         , ctxPred :: [PlExtraPred]
                         }


mod_ctx_state :: AST.Module -> CtxState
mod_ctx_state mod = CtxState { tmpCount = 0
                             , ctxPred = []
                             }

child_ctx_state :: CtxState -> CtxState
child_ctx_state paren = paren { ctxPred = [] }


{- Natural Expression mapping -}
map_exp :: SAST.NaturalExpr -> PlNaturalExpr 
map_exp (SAST.Concat m a (SAST.Slice m1 b1 brange)) =
    let 
        b = AST.Slice m1 b1 brange
        (bs,be) = map_nr brange
    in
        Concat m (map_exp a) (map_exp b) (Subtraction m be bs)

map_exp (SAST.Concat m a b) =
    throw $ NYIException "Concat must be followed by explicit range"

map_exp (SAST.Slice m a range) = 
    let
        (start, end) = map_nr range
    in
        Slice m (map_exp a) start end

-- the easy cases    
map_exp (SAST.Addition m a b) = Addition m (map_exp a) (map_exp b)
map_exp (SAST.Subtraction m a b) = Subtraction m (map_exp a) (map_exp b)
map_exp (SAST.Multiplication m a b) = Multiplication m (map_exp a) (map_exp b)
map_exp (SAST.Constant m a) = SockeyeVariable m a
map_exp (SAST.Variable m a) = SockeyeVariable m a
map_exp (SAST.Parameter m a) = SockeyeVariable m a
map_exp (SAST.Literal m a) = Literal m a

{- Generator Monad and helpers -}
type SM = State CtxState 

get_next_tmp_var :: SM PlVar
get_next_tmp_var = do
     st <- get
     modify (\c -> c { tmpCount = (tmpCount c) + 1})
     return $ PlIntVar (tmpCount st)


gen_pl_exp :: PlNaturalExpr -> SM PlVar
gen_pl_exp exp = do
    var <- get_next_tmp_var 
    modify (\c -> c { ctxPred = (ctxPred c) ++ [PlIsPred var exp]})
    return var

gen_exp :: AST.NaturalExpr -> SM PlVar
gen_exp exp = gen_pl_exp (map_exp exp)

materialize_multid_set :: PlMultiDSet -> SM PlVar
materialize_multid_set s = do
    var <- get_next_tmp_var 
    modify (\c -> c { ctxPred = (ctxPred c) ++ [PlMultiDValues var s]})
    return var

materialize_nat_set :: PlNaturalSet -> SM PlVar
materialize_nat_set s = do
    var <- get_next_tmp_var 
    modify (\c -> c { ctxPred = (ctxPred c) ++ [PlValues var s]})
    return var


{-
 - Wrapping of indices  
 - This will turn an unqualified reference into a PlQualifiedReference
 - and call the generator with it.
 - If the reference contains block statements and it is therefore 
 - impossible to directly express it in a predicate, we wrap it in a forall.
 - Then the qualifiedRef will contain a variable to the iterator -}

wrap_uqr :: AST.UnqualifiedRef -> (PlQualifiedRef -> SM [PlDefinition]) -> SM [PlDefinition]
wrap_uqr (AST.UnqualifiedRef _ name Nothing) gen =
    gen (PlQualifiedRef (PlImmediateStr name) Nothing Nothing Nothing)
wrap_uqr (AST.UnqualifiedRef _ name (Just ai)) gen =
    do
        matV <- gen_array_idx ai
        itV <- get_next_tmp_var
        let it_ref = PlQualifiedRef { propName = PlImmediateVar itV
                                    , propIndex = Nothing
                                    , instName = Nothing
                                    , instIndex = Nothing
                                    }
        ctx <- get
        let body = gen_body (child_ctx_state ctx) (gen it_ref)
        return $ [PlForall itV matV body]

wrap_nr :: AST.NodeReference -> (PlQualifiedRef -> SM [PlDefinition]) -> SM [PlDefinition]
wrap_nr (AST.InternalNodeRef _ uqr) gen = wrap_uqr uqr gen
wrap_nr (AST.InputPortRef _ inst node) gen_outer =
    let
        gen_inner2 :: PlQualifiedRef -> PlQualifiedRef -> SM [PlDefinition]
        gen_inner2 instR nodeR =
            -- Merge the instR and nodeR into a new one
            gen_outer $ PlQualifiedRef { propName =  propName nodeR
                                       , propIndex = propIndex nodeR
                                       , instName = Just (propName instR)
                                       , instIndex = propIndex instR
                                       }
        gen_inner1 :: PlQualifiedRef -> SM [PlDefinition]
        gen_inner1 instR = wrap_uqr node (gen_inner2 instR)
    in
        wrap_uqr inst gen_inner1


map_nr :: AST.NaturalRange -> (PlNaturalExpr, PlNaturalExpr)
map_nr (AST.SingletonRange _ base) = (map_exp base, map_exp base)
map_nr (AST.LimitRange _ base limit) = (map_exp base, map_exp limit)
map_nr (AST.BitsRange m base bits) =
    (map_exp base, BitLimit m (map_exp base) (map_exp bits))

gen_nr :: AST.NaturalRange -> SM PlNaturalRange
gen_nr (AST.SingletonRange _ base) = do
    bV <- gen_exp base      
    return $ PlNaturalRange bV bV
gen_nr (AST.LimitRange _ base limit) = do
    bV <- gen_exp base      
    lV <- gen_exp limit      
    return $ PlNaturalRange bV lV
gen_nr (AST.BitsRange m base bits) = do
    bV <- gen_exp base      
    lV <- gen_pl_exp $ BitLimit m (map_exp base) (map_exp bits)
    return $ PlNaturalRange bV lV

gen_ns :: AST.NaturalSet -> SM PlNaturalSet
gen_ns (AST.NaturalSet _ nr) = do
    rs <- mapM gen_nr nr
    return $ PlNaturalSet rs

gen_ws :: AST.WildcardSet -> SM PlNaturalSet
gen_ws (AST.ExplicitSet _ ns) = gen_ns ns
gen_ws (AST.Wildcard _) = -- throw $ NYIException "Wildcard"
    return $ PlNaturalSet []

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
gen_module m = 
    let
        body = gen_body (mod_ctx_state m) (gen_tags_defs (AST.moduleTags m) (AST.definitions m))
    in
        PlModule
                    { moduleMeta = (AST.moduleMeta m)
                    , moduleName = (AST.moduleName m)
                    , parameters = (AST.parameters m)
                    , nodeDecls  = (AST.nodeDecls  m)
                    , constants  = (AST.constants  m)
                    , body       = body
                    }

gen_body :: CtxState -> SM [PlDefinition] -> PlBody
gen_body state gen = 
    let
        res = runState gen state
        plDefs = fst res    
        plExt = ctxPred $ snd res
    in
        PlBody plExt plDefs
          
gen_tags_defs :: [SAST.ModuleTag] -> [AST.Definition] -> SM [PlDefinition]
gen_tags_defs tags defs = do
    mtags <- mapM gen_tag tags
    mdefs <- gen_defs defs
    return $ mtags ++ mdefs

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


gen_tag :: SAST.ModuleTag -> SM PlDefinition
gen_tag (SAST.ModuleTag m name exp) = do
    expV <- gen_exp exp
    return $ PlModuleTag m name expV

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
        mk_target_i :: PlQualifiedRef -> AST.AddressBlock -> AST.AddressBlock -> PlQualifiedRef -> SM [PlDefinition]
        mk_target_i srcNode srcAb destAb destN = do
            srcReg <- gen_region srcNode srcAb
            destReg <- gen_region destN destAb
            return $ [PlTranslate meta srcReg destReg]

        mk_target :: PlQualifiedRef -> AST.AddressBlock -> AST.MapTarget -> SM [PlDefinition]
        mk_target srcNode srcAb mt = do
            let wrapi = mk_target_i srcNode srcAb (AST.targetAddr mt)
            defs <- wrap_nr (AST.targetNode mt) wrapi
            return defs
            
        mk_map :: PlQualifiedRef -> AST.MapSpec -> SM [PlDefinition]
        mk_map srcNode ms = do
            let srcAb = AST.mapAddr ms
            res <- mapM (mk_target srcNode srcAb) (AST.mapTargets ms)
            return $ concat res

        mk_maps :: PlQualifiedRef -> SM [PlDefinition]
        mk_maps nq = do
            def <- mapM (mk_map nq) maps
            return $ concat def

    in wrap_uqr n mk_maps
        
gen_def (AST.Instantiates m inst instModule arguments) =
    let
        mk_inst :: PlQualifiedRef -> SM [PlDefinition]
        mk_inst instqr = do
            args <- mapM gen_exp arguments
            return [PlInstantiates m instqr instModule args]
    in 
        wrap_uqr inst mk_inst 

gen_def (AST.Converts m node converts) = gen_def (AST.Maps m node converts)

gen_def (AST.Forall m varName varRange body) =
    do
        natset <- gen_ns varRange
        matV <- materialize_nat_set natset
        let itV = PlSockVar varName
        ctx <- get
        let pl_body = gen_body (child_ctx_state ctx) (gen_defs body)
        return $ [PlForall itV matV pl_body]

gen_def (AST.Overlays m n o) = 
    let 
        gen1 nq oq = return [PlOverlays m nq oq]
        gen nq = wrap_nr o (gen1 nq)
    in
        wrap_uqr n gen

gen_def (AST.ConfOverlays m n o) = 
    let 
        gen1 nq oq = return [PlConfOverlays m nq oq]
        gen nq = wrap_nr o (gen1 nq)
    in
        wrap_uqr n gen

gen_def (AST.Binds m i bindings) = 
    let 
        gend iq bpq bnq = 
            let
                from = bpq { instName = Just $ propName iq
                           , instIndex = propIndex iq }
            in
                return [PlOverlays m from bnq]
            
        genc iq bn bpq = wrap_nr bn (gend iq bpq)
        genb iq (AST.PortBinding m bp bn) = wrap_uqr bp (genc iq bn) 
        gena iq = do
            defs <- mapM (genb iq) bindings 
            return $ concat defs
    in
        wrap_uqr i gena
