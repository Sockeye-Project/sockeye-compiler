{-
  SockeyeMain.hs: Sockeye

  Copyright (c) 2018, ETH Zurich.

  All rights reserved.

  This file is distributed under the terms in the attached LICENSE file.
  If you do not find this file, copies can be found by writing to:
  ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
  Attn: Systems Group.
-}

module Main where

import Control.Monad ( foldM )

import Data.List ( intercalate )
import qualified Data.Map as Map

import System.Console.GetOpt
    ( ArgOrder(Permute),
      OptDescr(..),
      getOpt,
      usageInfo,
      ArgDescr(ReqArg, NoArg) )
import System.Directory ( doesFileExist, findFile )
import System.Exit ( ExitCode(..), exitWith )
import System.Environment ( getArgs, getProgName )
import System.FilePath
    ( (</>), normalise, takeDirectory, takeFileName )
import System.IO ( stderr, hPutStr, hPutStrLn )
--import Debug.Trace

--import Text.Pretty.Simple (pPrint, pShowNoColor)
import Data.Text.Lazy ( unpack )
import Data.Aeson ( eitherDecodeStrict, FromJSON )
import qualified Data.ByteString as B
import Data.String.Utils ( replace )

import qualified SockeyeParserAST as ParseAST
import qualified SockeyeSymbolTable as SymTable
import qualified SockeyeAST as AST

import SockeyeParser ( parseSockeye )
import SockeyeSymbolTableBuilder ( buildSymbolTable )
import SockeyeChecker ( checkSockeye )

import qualified SockeyeBackendProlog as PrologBackend
import qualified PrologBuilder as PrologBuilder
import qualified SockeyeBackendPrologMultiDim as PrologMultiDim
import qualified SockeyeBackendIsabelle as Isabelle
import qualified SockeyeBackendLISA as LISA

{- Exit codes -}
usageError :: ExitCode
usageError = ExitFailure 1

fileError :: ExitCode
fileError = ExitFailure 2

parseError :: ExitCode
parseError = ExitFailure 3

checkError :: ExitCode
checkError = ExitFailure 4

compileError :: ExitCode
compileError = ExitFailure 5

auxParseError :: ExitCode
auxParseError = ExitFailure 6

{- Compilation targets -}
data Target
    = Prolog
    | PrologMultiDim
    | Isabelle
    | LISA



{- Possible options for the Sockeye Compiler -}
data Options = Options
    { optInputFile    :: FilePath
    , optInclDirs     :: [FilePath]
    , optTarget       :: Target
    , optOutputFile   :: FilePath
    , optDepFile      :: Maybe FilePath
    , optParseAstDump :: String
    , optSymTableDump :: String
    , optAstDump      :: String
    }

{- Default options -}
defaultOptions :: Options
defaultOptions = Options
    { optInputFile    = ""
    , optInclDirs     = [""]
    , optTarget       = Prolog
    , optOutputFile   = ""
    , optDepFile      = Nothing
    , optParseAstDump = ""
    , optSymTableDump = ""
    , optAstDump      = ""
    }

{- Set the input file name -}
optSetInputFileName :: FilePath -> Options -> Options
optSetInputFileName f o = o { optInputFile = f }

optAddInclDir :: FilePath -> Options -> Options
optAddInclDir f o = o { optInclDirs = optInclDirs o ++ [f] }

{- Set the target -}
optSetTarget :: Target -> Options -> Options
optSetTarget t o = o { optTarget = t }

{- Set the output file name -}
optSetOutputFile :: FilePath -> Options -> Options
optSetOutputFile f o = o { optOutputFile = f }

{- Set the dependency file name -}
optSetDepFile :: FilePath -> Options -> Options
optSetDepFile f o = o { optDepFile = Just f }

{- Set dump target for AST -}
optSetParseAstDump :: String -> Options -> Options
optSetParseAstDump t o = o { optParseAstDump = t }

{- Set dump target for symbol table -}
optSetSymTableDump :: String -> Options -> Options
optSetSymTableDump t o = o { optSymTableDump = t }

{- Set dump target for AST -}
optSetAstDump :: String -> Options -> Options
optSetAstDump t o = o { optAstDump = t }

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
    , Option "M" ["PrologMultiDim"]
        (NoArg (\opts -> return $ optSetTarget PrologMultiDim opts))
        "Same as Prolog, but with multidimensional addresses"
    , Option "I" ["Isabelle"]
        (NoArg (\opts -> return $ optSetTarget Isabelle opts))
        "Generate Isabelle/HOL code."
    , Option "L" ["LISA"]
        (NoArg (\opts -> return $ optSetTarget
        LISA opts))
        "Generate LISA code."
    , Option "i" ["include"]
        (ReqArg (\f opts -> return $ optAddInclDir f opts) "DIR")
        "Add a directory to the search path where Sockeye looks for imports."
    , Option "o" ["output-file"]
        (ReqArg (\f opts -> return $ optSetOutputFile f opts) "FILE")
        "Output file in which to store the compilation result (required)."
    , Option "d" ["dep-file"]
        (ReqArg (\f opts -> return $ optSetDepFile f opts) "FILE")
        "Generate a dependency file for GNU make"
    , Option "" ["parse-dump"]
        (ReqArg (\f opts -> return $ optSetParseAstDump f opts) "'c'|'f'")
        "Dump parser AST to console ('c') or file ('f')."
    , Option "" ["st-dump"]
        (ReqArg (\f opts -> return $ optSetSymTableDump f opts) "'c'|'f'")
        "Dump symbol table to console ('c') or file ('f')."
    , Option "" ["ast-dump"]
        (ReqArg (\f opts -> return $ optSetAstDump f opts) "'c'|'f'")
        "Dump AST to console ('c') or file ('f')."
    , Option "h" ["help"]
        (NoArg (\_ -> usage [] >> exitWith ExitSuccess))
        "Show help."
    ]

{- Evaluates the compiler options -}
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
parse :: [FilePath] -> FilePath -> IO ParseAST.Sockeye
parse inclDirs fileName = do
    entryPoint <- resolveFile fileName
    files <- resolveImports Map.empty entryPoint
    return ParseAST.Sockeye
        { ParseAST.entryPoint = entryPoint
        , ParseAST.files      = files
        }
    where
        resolveImports fileMap file = do
            if file `Map.member` fileMap
                then return fileMap
                else do
                    ast <- parseFile file
                    resolvedImports <- mapM rewriteFilePath $ ParseAST.imports ast
                    let
                        importPaths = map ParseAST.importFile resolvedImports
                        ast' = ast { ParseAST.imports = resolvedImports }
                        fileMap' = Map.insert file ast' fileMap
                    foldM resolveImports fileMap' importPaths
        rewriteFilePath i = do
            let fileName = ParseAST.importFile i
            file <- resolveFile fileName
            return i { ParseAST.importFile = file }
        resolveFile path = do
            let
                subDir = takeDirectory path
                name = takeFileName path
                dirs = map (</> subDir) inclDirs
            file <- findFile dirs name
            case file of
                Just f -> return $ normalise f
                _ -> do
                    hPutStrLn stderr $ "'" ++ path ++ "' not on import path"
                    exitWith fileError

{- Runs the parser on a single file -}
parseFile :: FilePath -> IO ParseAST.SockeyeFile
parseFile file = do
    src <- readFile file
    case parseSockeye file src of
        Left err -> do
            hPutStrLn stderr $ "Parse error at " ++ show err
            exitWith parseError
        Right ast -> return ast

{- Builds the symbol table from the parsed AST -}
buildSymTable :: ParseAST.Sockeye -> IO SymTable.Sockeye
buildSymTable ast =
    case buildSymbolTable ast of
        Left fail -> do
            hPutStr stderr $ show fail
            exitWith checkError
        Right symTable -> return symTable

{- Checks the AST -}
check :: SymTable.Sockeye -> ParseAST.Sockeye -> IO AST.Sockeye
check symTable pAst =
    case checkSockeye symTable pAst of
        Left fail -> do
            hPutStr stderr $ show fail
            exitWith checkError
        Right ast -> return ast

{- Compiles the AST with the selected backend -}
compile :: Target -> ParseAST.Sockeye -> SymTable.Sockeye -> AST.Sockeye -> IO String
compile Prolog pAst _ _ = return $ (PrologBackend.compile . PrologBuilder.build) pAst
compile PrologMultiDim _ symTable ast = return $ PrologMultiDim.compile symTable ast
compile Isabelle _ symTable ast = return $ Isabelle.compile symTable ast
compile LISA pAst _ _ = compileDirectLISA pAst

{- Note: this function actually comes from the updated 1.4 AESON package,
         which is not available in apt-get's 1.2 version -}
eitherDecodeFileStrict :: (FromJSON a) => FilePath -> IO (Either String a)
eitherDecodeFileStrict = fmap eitherDecodeStrict . B.readFile

{- Generate LISA -}
compileDirectLISA :: ParseAST.Sockeye -> IO String
compileDirectLISA ast = do
    let auxFn = replace  ".soc" ".aux" (ParseAST.entryPoint ast)
    auxEx <- doesFileExist auxFn
    if auxEx then
        do
            aux <- eitherDecodeFileStrict $ replace  ".soc" ".aux" (ParseAST.entryPoint ast)
            case aux of
                Right aux -> return $ LISA.compileDirect ast (Just aux)
                Left err -> do
                    putStrLn $ "Error while parsing " ++ auxFn
                    putStrLn err 
                    exitWith auxParseError
                    
    else
        return $ LISA.compileDirect ast Nothing
        
 

{- Outputs the compilation result -}
output :: FilePath -> String -> IO ()
output outFile out = writeFile outFile out

{- Generates a dependency file for GNU make -}
dependencyFile :: FilePath -> FilePath -> [FilePath] -> IO String
dependencyFile outFile depFile deps = do
    let
        targets = outFile ++ " " ++ depFile ++ ":"
        lines = targets:deps
    return $ intercalate " \\\n " lines

{- Produces debug output -}
{-
debugOutput :: Options -> ParseAST.Sockeye -> SymTable.Sockeye -> AST.Sockeye -> IO ()
debugOutput opts pAst symTable ast = do
    let
        inFile = optInputFile opts
        pAstDump = optParseAstDump opts
        stDump = optSymTableDump opts
        astDump = optAstDump opts
    case pAstDump of
        "c" -> putStrLn "Dumping Parse AST..." >> putStrLn "********************" >> pPrint pAst
        "f" -> writeFile (inFile <.> "past" <.> "txt") (unpack $ pShowNoColor pAst)
        _ -> return ()
    case stDump of
        "c" -> putStrLn "Dumping Symbol Table..." >> putStrLn "***********************" >> pPrint symTable
        "f" -> writeFile (inFile <.> "st" <.> "txt") (unpack $ pShowNoColor symTable)
        _ -> return ()
    case astDump of
        "c" -> putStrLn "Dumping AST..." >> putStrLn "**************" >> pPrint ast
        "f" -> writeFile (inFile <.> "ast" <.> "txt") (unpack $ pShowNoColor ast)
        _ -> return ()
-}
main :: IO ()
main = do
    args <- getArgs
    opts <- compilerOpts args
    let
        inFile = optInputFile opts
        inclDirs = optInclDirs opts
        outFile = optOutputFile opts
        depFile = optDepFile opts
        target = optTarget opts
    parsedAst <- parse inclDirs inFile
    symTable <- buildSymTable parsedAst
    ast <- check symTable parsedAst
    {- debugOutput opts parsedAst symTable ast -}
    out <- compile target parsedAst symTable ast
    output outFile out
    case depFile of
        Nothing -> return ()
        Just f  -> do
            out <- dependencyFile outFile f (Map.keys $ ParseAST.files parsedAst)
            output f out
