{-
  SockeyeMain.hs: Sockeye

  Copyright (c) 2017, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module Main where

import Control.Monad

import Data.List (intercalate)
import qualified Data.Map as Map

import System.Console.GetOpt
import System.Exit
import System.Environment
import System.FilePath
import System.IO

import qualified SockeyeASTParser as ParseAST
import qualified SockeyeAST as AST
import qualified SockeyeASTDecodingNet as NetAST

import SockeyeParser
import SockeyeChecker
import SockeyeNetBuilder

import qualified SockeyeBackendProlog as Prolog

{- Exit codes -}
usageError :: ExitCode
usageError = ExitFailure 1

parseError :: ExitCode
parseError = ExitFailure 2

checkError :: ExitCode
checkError = ExitFailure 3

buildError :: ExitCode
buildError = ExitFailure 4

{- Compilation targets -}
data Target = Prolog | Make

{- Possible options for the Sockeye Compiler -}
data Options = Options { optInputFile  :: FilePath
                       , optTarget     :: Target
                       , optOutputFile :: FilePath
                       }

{- Default options -}
defaultOptions :: Options
defaultOptions = Options { optInputFile  = ""
                         , optTarget     = Prolog
                         , optOutputFile = ""
                         }

{- Set the input file name -}
optSetInputFileName :: FilePath -> Options -> Options
optSetInputFileName f o = o { optInputFile = f }

{- Set the target -}
optSetTarget :: Target -> Options -> Options
optSetTarget t o = o { optTarget = t }

{- Set the outpue file name -}
optSetOutputFile :: FilePath -> Options -> Options
optSetOutputFile f o = o { optOutputFile = f }

{- Prints usage information possibly with usage errors -}
usage :: [String] -> IO ()
usage errors = do
    prg <- getProgName
    let usageString = "Usage: " ++ prg ++ " [options] file\nOptions:"
    case errors of
        [] -> return ()
        _  -> hPutStrLn stderr $ concat errors
    hPutStrLn stderr $ usageInfo usageString options
    hPutStrLn stderr "The backend (capital letter options) specified last takes precedence."


{- Setup option parser -}
options :: [OptDescr (Options -> IO Options)]
options = 
    [ Option "P" ["Prolog"]
        (NoArg (\opts -> return $ optSetTarget Prolog opts))
        "Generate a prolog file that can be loaded into the SKB (default)."
    , Option "D" ["DepFile"]
        (NoArg (\opts -> return $ optSetTarget Make opts))
        "Generate a dependency file for GNU make"
    , Option "o" ["output-file"]
        (ReqArg (\f opts -> return $ optSetOutputFile f opts) "FILE")
        "Output file in which to store the compilation result (required)."
    , Option "h" ["help"]
        (NoArg (\_ -> do
                    usage []
                    exitWith ExitSuccess))
        "Show help."
    ]

{- evaluates the compiler options -}
compilerOpts :: [String] -> IO (Options)
compilerOpts argv = do
    opts <- case getOpt Permute options argv of
        (actions, fs, []) -> do
            opts <- foldl (>>=) (return defaultOptions) actions
            case fs of
                []  -> do
                    usage ["No input file\n"]
                    exitWith usageError
                [f] -> return $ optSetInputFileName f opts
                _   -> do
                    usage ["Multiple input files not supported\n"]
                    exitWith usageError

        (_, _, errors) -> do
            usage errors
            exitWith $ usageError
    case optOutputFile opts of
        "" -> do
            usage ["No output file\n"]
            exitWith $ usageError
        _  -> return opts

{- Parse Sockeye and resolve imports -}
parseSpec :: FilePath -> IO (ParseAST.SockeyeSpec, [FilePath])
parseSpec file = do
    let
        rootImport = ParseAST.Import file
    specMap <- parseWithImports "" Map.empty rootImport
    let
        specs = Map.elems specMap
        deps = Map.keys specMap
        topLevelSpec = specMap Map.! file
        modules = concat $ map ParseAST.modules specs
        spec = topLevelSpec
            { ParseAST.imports = []
            , ParseAST.modules = modules
            }
    return (spec, deps)
        
    where
        parseWithImports pwd importMap (ParseAST.Import filePath) = do
            let
                dir = case pwd of
                    "" -> takeDirectory filePath
                    _  -> pwd </> takeDirectory filePath
                fileName = takeFileName filePath
                file = if '.' `elem` fileName
                    then dir </> fileName
                    else dir </> fileName <.> "soc"
            if file `Map.member` importMap
                then return importMap
                else do
                    ast <- parseFile file
                    let
                        specMap = Map.insert file ast importMap
                        imports = ParseAST.imports ast
                    foldM (parseWithImports dir) specMap imports

{- Runs the parser on a single file -}
parseFile :: FilePath -> IO (ParseAST.SockeyeSpec)
parseFile file = do
    src <- readFile file
    case parseSockeye file src of
        Left err -> do
            hPutStrLn stderr $ "Parse error at " ++ show err
            exitWith parseError
        Right ast -> return ast

{- Runs the checker -}
checkAST :: ParseAST.SockeyeSpec -> IO AST.SockeyeSpec
checkAST parsedAst = do
    case checkSockeye parsedAst of 
        Left fail -> do
            hPutStr stderr $ show fail
            exitWith checkError
        Right intermAst -> return intermAst

{- Builds the decoding net from the Sockeye AST -}
buildNet :: AST.SockeyeSpec -> IO NetAST.NetSpec
buildNet ast = do
    case sockeyeBuildNet ast of 
        Left fail -> do
            hPutStr stderr $ show fail
            exitWith buildError
        Right netAst -> return netAst

{- Compiles the AST with the appropriate backend -}
compile :: Target -> NetAST.NetSpec -> IO String
compile Prolog ast = return $ Prolog.compile ast

{- Writes a dependency file for GNU make -}
depFile :: FilePath -> [FilePath] -> IO String
depFile outFile deps = do
    let
        targets = outFile ++ " " ++ outFile <.> "depend" ++ ":"
        lines = targets:deps
    return $ intercalate " \\\n " lines

{- Outputs the compilation result -}
output :: FilePath -> String -> IO ()
output outFile out = writeFile outFile out

main = do
    args <- getArgs
    opts <- compilerOpts args
    let
        inFile = optInputFile opts
        outFile = optOutputFile opts
        target = optTarget opts
    (parsedAst, deps) <- parseSpec inFile
    case target of
        Make -> do
            out <- depFile outFile deps
            output (outFile <.> "depend") out
        _ -> do
            ast <- checkAST parsedAst
            netAst <- buildNet ast
            out <- compile (optTarget opts) netAst
            output outFile out
    