{-
  SockeyeBackendProlog.hs: Backend for generating ECLiPSe-Prolog for Sockeye

  Part of Sockeye

  Copyright (c) 2018, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module SockeyeBackendProlog
( compile, compileDirect ) where

import qualified Data.Map as Map
import Data.Char
import Data.List

import qualified SockeyeSymbolTable as ST
import qualified SockeyeAST as SAST
import qualified SockeyeParserAST as AST

{- The structure of the code generator should be very similar to the old Prolog Backend -}
compile :: ST.Sockeye -> SAST.Sockeye -> String
compile symTable ast = "Prolog backend not yet implemented"

compileDirect :: AST.Sockeye -> String
compileDirect = generate

{- Code Generator -}
class PrologGenerator a where
    generate :: a -> String

instance PrologGenerator AST.Sockeye where
  generate s = let
    files = map snd (Map.toList (AST.files s))
    in concat (map generate files)

instance PrologGenerator AST.SockeyeFile where
  generate f = concat (map generate (AST.modules f))

gen_node_param_list :: [AST.NodeDeclaration] -> [String]
gen_node_param_list ndl = map AST.nodeName ndl

gen_nat_param_list :: [AST.ModuleParameter] -> [String]
gen_nat_param_list pp = map AST.paramName pp

instance PrologGenerator AST.Module where
  generate m = let
    name = AST.moduleName m
    p1 = gen_nat_param_list (AST.parameters m)
    p2 = gen_node_param_list inp_node_decls
    p3 = gen_node_param_list out_node_decls
    bodyDecls = map gen_body_decls (AST.nodeDecls m)
    bodyDefs = map gen_body_defs (AST.definitions m)
    body = intercalate ",\n    " $ bodyDecls ++ bodyDefs
    in name ++ stringify (p1 ++ p2 ++ p3) ++ " :- " ++ body ++ "."
    where
      stringify [] = ""
      stringify pp = braces $ intercalate "," pp
      inp_node_decls = filter (\x -> (AST.nodeKind x) == AST.InputPort) (AST.nodeDecls m)
      out_node_decls = filter (\x -> (AST.nodeKind x) == AST.OutputPort) (AST.nodeDecls m)

-- Inside each function we add variable that contains
--  * nodeId
--  * params
--  * constants
-- This will return the name of these variables
local_nodeid_name :: String -> String
local_nodeid_name x = "LOCAL_" ++ x

-- Prefix tat as well?
local_param_name :: String -> String
local_param_name x = x

local_const_name :: String -> String
local_const_name x = "CONST_" ++ x

gen_dom_atom :: AST.Domain -> String
gen_dom_atom d = case d of
  AST.Memory -> atom "memory"
  AST.Interrupt -> atom "interrupt"
  AST.Power -> atom "power"
  AST.Clock -> atom "clock"

gen_body_decls :: AST.NodeDeclaration -> String
gen_body_decls x =
  let
    var = local_nodeid_name $ AST.nodeName x
    kind = gen_dom_atom ( AST.originDomain (AST.nodeType x))
    decl = struct "node_id" [("name", quotes $ AST.nodeName x), ("namespace","[]"),
      ("kind", kind)]
    in var ++ " = " ++ decl

gen_body_defs :: AST.Definition -> String
gen_body_defs x = case x of
  (AST.Accepts _ n accepts) -> struct "node" [the_id n,
    ("spec", struct "node_spec" [("accept", list $ map generate accepts), ("translate", list [])])]
  (AST.Maps _ n maps)    -> struct "node" [the_id n,
    ("spec", struct "node_spec" [("accept", list []), ("translate", list $ map generate maps )])]
  _                   -> "NYI"
  where
    the_id n = ("id", local_nodeid_name (AST.refName (n)))


instance PrologGenerator AST.MapSpec where
  generate ms = struct "map" [("src_block", generate (AST.mapAddr ms)),
    ("dests", list $ map generate (AST.mapTargets ms))]

instance PrologGenerator AST.MapTarget where
  generate mt = struct "dest" [("id", generate (AST.targetNode mt)),
    ("base", generate (AST.targetAddr mt))]

instance PrologGenerator AST.NodeReference where
  generate nr = case nr of
    AST.InternalNodeRef _ nn -> local_nodeid_name (AST.refName nn)
    AST.InputPortRef _ _ _ -> "NYI"

instance PrologGenerator AST.AddressBlock where
  -- TODO: add properties
  generate ab = generate $ SAST.addresses ab

instance PrologGenerator AST.Address where
  generate a = case a of
    AST.Address _ ws -> list $ map generate ws

instance PrologGenerator AST.WildcardSet where
  generate a = case a of
    AST.ExplicitSet _ ns -> generate ns
    AST.Wildcard _ -> "_"

instance PrologGenerator AST.NaturalSet where
  generate a = case a of
    AST.NaturalSet _ nrs -> intercalate "::" $ map generate nrs {- horribly wrong -}

instance PrologGenerator AST.NaturalRange where
  generate nr = case nr of
    AST.SingletonRange _ b -> struct "block"
      [("base", generate b), ("limit", generate b)]
    AST.LimitRange _ b l -> struct "block"
      [("base", generate b), ("limit", generate l)]
    AST.BitsRange _ b bits -> "NYI"
      -- struct "block" [("base", generate b), ("limit", show 666)]

instance PrologGenerator AST.NaturalExpr where
  generate nr = case nr of
    SAST.Constant _ v -> local_const_name v
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

list :: [String] -> String
list elems = brackets $ intercalate "," elems

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
