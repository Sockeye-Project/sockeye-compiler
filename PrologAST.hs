{-
    PrologAST.hs: AST for Prolog

    Part of Sockeye

    Copyright (c) 2019, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module PrologAST
    ( module PrologAST
    , module SockeyeASTMeta
    , module SockeyeAST
    , module SockeyeParserAST
    )  where

import SockeyeASTMeta

import SockeyeAST
    ( NaturalExpr(Addition, Subtraction, Multiplication, Slice, Concat, Variable, Literal)
    , PropertyExpr(And, Or, Not, Property, True, False)
    , ModuleTag(ModuleTag)
    , ModuleParameter(ModuleParameter)
    , paramName, paramRange, paramMeta
    )

import SockeyeParserAST
    ( NodeDeclaration(NodeDeclaration)
    , NamedConstant(NamedConstant)
    , constName
    )

data PlFile = PlFile [PlModule]
    deriving (Show)

data PlModule = PlModule
    { moduleMeta  :: ASTMeta
    , moduleName  :: !String
    , moduleTags  :: [ModuleTag]
    , parameters  :: [ModuleParameter]
    , nodeDecls   :: [NodeDeclaration]
    , constants   :: [NamedConstant]
    , body        :: PlBody
    }
    deriving (Show)

type PlVar = String

data PlNaturalRange = PlNaturalRange
    { base :: PlVar
    , limit :: PlVar
    }
    deriving(Show)
data PlNaturalSet = PlNaturalSet [PlNaturalRange]
    deriving(Show)
data PlMultiDSet = PlMultiDSet [PlNaturalSet]
    deriving(Show)
data PlMultiDRange = PlMultiDRange [PlNaturalRange]
    deriving(Show)

data PlExtraPred
    -- will set varName to the evaluation of expr
    = PlIsPred { varName :: PlVar
               , expr :: NaturalExpr }
    -- yields varName = varBase + 2^varBits - 1
    | PlBitsLimit { varName :: PlVar
                  , varBase :: PlVar 
                  , varBits :: PlVar }
    -- will set varName to contain a list of possible values in expr
    -- (ie it will materialize the array index)
    | PlValues { varName :: PlVar
               , valSet :: PlMultiDSet }
    deriving(Show)

data PlBody = PlBody
    { extraPred   :: [PlExtraPred]
    , definitions :: [PlDefinition]
    }
    deriving (Show)

data PlDefinition
    = PlAccepts
        { defMeta :: ASTMeta
        , region :: PlRegionSpec
        }
    | PlTranslate
        { defMeta :: ASTMeta
        , src     :: PlRegionSpec
        , dst     :: PlNameSpec
        }
    | PlOverlays
        { defMeta  :: ASTMeta
        , node     :: PlQualifiedRef
        , overlays :: PlQualifiedRef
        }
    | PlBlockOverlays
        { defMeta  :: ASTMeta
        , node     :: PlQualifiedRef
        , overlays :: PlQualifiedRef
        , blocksizes :: [Integer]
        }
    | PlInstantiates
        { defMeta    :: ASTMeta
        , inst       :: PlQualifiedRef
        , instModule :: !String
        , arguments  :: [PlVar] -- matching PlIsPred must exist
        }
    | PlForall
        { boundVarName   :: !PlVar
        , varRange       :: PlVar -- matching PlValues must exist
        , quantifierBody :: [PlDefinition]
        }
    deriving (Show)

data PlQualifiedRef = PlQualifiedRef
    { propName  :: String
    , propIndex :: Maybe PlVar -- matching PlIsPred must exist
    , instName  :: Maybe String
    , instIndex :: Maybe PlVar -- matching PlIsPref must exist
    }
    deriving (Show)

data PlNameSpec = PlNameSpec
    { nameNode :: PlQualifiedRef
    , nameAddr :: PlVar -- matching PlIsPred must exist
    , nameProp :: PropertyExpr
    }
    deriving (Show)

data PlRegionSpec = PlRegionSpec
    { regNode :: PlQualifiedRef
    , regBlock :: PlMultiDSet 
    , regProp :: PropertyExpr
    }
    deriving (Show)
