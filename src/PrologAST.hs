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
    ( PropertyExpr(And, Or, Not, Property, True, False)
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
    , parameters  :: [ModuleParameter]
    , nodeDecls   :: [NodeDeclaration]
    , constants   :: [NamedConstant]
    , body        :: PlBody
    }
    deriving (Show)

data PlVar = PlSockVar String -- Variable appearing in sockeye source
           | PlIntVar Int     -- Internal variable
           deriving (Show)

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
               , expr :: PlNaturalExpr }
    -- will set varName to be a list of possible values in valSet
    | PlValues { varName :: PlVar
               , valNatSet :: PlNaturalSet }
    -- like plvalues, but with multi dimension.
    -- will set varName to be a list of possible values in expr. Each
    -- of these values is a list of dimension indices
    | PlMultiDValues { varName :: PlVar
                     , valMultiDSet :: PlMultiDSet }
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
        , dst     :: PlRegionSpec
        }
    | PlOverlays
        { defMeta  :: ASTMeta
        , node     :: PlQualifiedRef
        , overlays :: PlQualifiedRef
        }
    | PlConfOverlays
        { defMeta  :: ASTMeta
        , node     :: PlQualifiedRef
        , overlays :: PlQualifiedRef
        }
    | PlInstantiates
        { defMeta    :: ASTMeta
        , inst       :: PlQualifiedRef
        , instModule :: !String
        , arguments  :: [PlVar] -- matching PlIsPred must exist
        }
    | PlForall
        { boundVarName   :: !PlVar
        , varRange       :: PlVar -- matching PlMultiDValues must exist
        , quantifierBody :: PlBody
        }
    -- ModuleTags should only appear in the top level module body, but 
    -- since they need the same context as the other definitions, it's
    -- much easier to keep them here.
    | PlModuleTag 
        { moduleTagMeta  :: ASTMeta
        , tagName        :: !String
        , tagExpr        :: PlVar -- matching PlIsPred must exist
        }
    deriving (Show)

data PlImmediate = PlImmediateStr String
                 | PlImmediateVar PlVar
    deriving (Show)

data PlQualifiedRef = PlQualifiedRef
    { propName  :: PlImmediate
    , propIndex :: Maybe PlVar -- matching PlIsPred must exist
    , instName  :: Maybe PlImmediate
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

data PlNaturalExpr
    = Addition
        { natExprMeta :: ASTMeta
        , natExprOp1  :: PlNaturalExpr
        , natExprOp2  :: PlNaturalExpr
        }
    | Subtraction
        { natExprMeta :: ASTMeta
        , natExprOp1  :: PlNaturalExpr
        , natExprOp2  :: PlNaturalExpr
        }
    | Multiplication
        { natExprMeta :: ASTMeta
        , natExprOp1  :: PlNaturalExpr
        , natExprOp2  :: PlNaturalExpr
        }
    | Slice
        { natExprMeta :: ASTMeta
        , natExprOp1  :: PlNaturalExpr
        , bitStart    :: PlNaturalExpr
        , bitEnd      :: PlNaturalExpr
        }
    | Concat
        { natExprMeta     :: ASTMeta
        , natExprOp1      :: PlNaturalExpr
        , natExprOp2      :: PlNaturalExpr
        , natExprOp2Bits  :: PlNaturalExpr 
        }
    | BitLimit 
        { natExprMeta   :: ASTMeta
        , natExprOp1    :: PlNaturalExpr --base
        , natExprOp2    :: PlNaturalExpr -- bits
        }
    -- A variable with a name in the sockeye source (constant/variable/parameter)
    | SockeyeVariable 
        { natExprMeta        :: ASTMeta
        , natExprVarName     :: !String
        }
    | Literal
        { natExprMeta :: ASTMeta
        , natural     :: !Integer
        }
    deriving (Show)
