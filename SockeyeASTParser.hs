{-
    SockeyeASTParser.hs: AST for the Sockeye parser

    Part of Sockeye

    Copyright (c) 2017, ETH Zurich.

    All rights reserved.

    This file is distributed under the terms in the attached LICENSE file.
    If you do not find this file, copies can be found by writing to:
    ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
    Attn: Systems Group.
-}

module SockeyeASTParser 
( module SockeyeASTParser
, module SockeyeAST
) where

import SockeyeAST
    ( Identifier(SimpleIdent, TemplateIdent)
    , prefix, varName, suffix
    , ModuleParamType(NumberParam, AddressParam)
    , ModuleArg(AddressArg, NumberArg, ParamArg)
    , NodeSpec(NodeSpec)
    , nodeType, accept, translate, overlay
    , NodeType(Memory, Device)
    , BlockSpec(SingletonBlock, RangeBlock, LengthBlock)
    , address, base, limit, bits
    , MapSpec(MapSpec)
    , block, destNode, destBase
    , Address(NumberAddress, ParamAddress)
    , ForLimit(NumberLimit, ParamLimit)
    )

data SockeyeSpec = SockeyeSpec
    { modules :: [Module]
    , net     :: [NetSpec]
    } deriving (Show)

data Module = Module
    { name       :: String
    , parameters :: [ModuleParam]
    , moduleBody :: ModuleBody
    } deriving (Show)

data ModuleParam = ModuleParam
    { paramName :: !String
    , paramType :: ModuleParamType
    } deriving (Show)

data ModuleBody = ModuleBody
    { ports     :: [PortDef]
    , moduleNet :: [NetSpec]
    } deriving (Show)

data PortDef
    = InputPortDef
        { portId :: Identifier }
    | OutputPortDef
        { portId :: Identifier }
    | MultiPortDef (For PortDef)
    deriving (Show)

data NetSpec
    = NodeDeclSpec NodeDecl
    | ModuleInstSpec ModuleInst
    deriving (Show)

data ModuleInst
    = ModuleInst
        { moduleName   :: String
        , nameSpace    :: Identifier
        , arguments    :: [ModuleArg]
        , portMappings :: [PortMap]
        }
    | MultiModuleInst (For ModuleInst)
    deriving (Show)

data PortMap
    = InputPortMap
        { mappedId   :: Identifier
        , mappedPort :: Identifier
        }
    | OutputPortMap
        { mappedId   :: Identifier
        , mappedPort :: Identifier
        }
    | MultiPortMap (For PortMap)
    deriving (Show)

data NodeDecl
    = NodeDecl
        { nodeId   :: Identifier
        , nodeSpec :: NodeSpec
        }
    | MultiNodeDecl (For NodeDecl)
    deriving (Show)

data For a 
    = For
        { varRanges :: [ForVarRange]
        , body      :: a
        } deriving (Show)

data ForVarRange
    = ForVarRange
    { var   :: !String
    , start :: ForLimit
    , end   :: ForLimit
    } deriving (Show)