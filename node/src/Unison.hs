{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.List
import System.Environment (getArgs)
import System.IO
import Unison.Codebase (Codebase)
import Unison.Codebase.Store (Store)
import Unison.Hash.Extra ()
import Unison.Reference (Reference)
import Unison.Runtime.Address
import Unison.Symbol.Extra ()
import Unison.Term (Term)
import Unison.Term.Extra ()
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified System.IO.Temp as Temp
import qualified Unison.ABT as ABT
import qualified Unison.BlockStore.FileBlockStore as FBS
import qualified Unison.Builtin as Builtin
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.FileStore as FileStore
import qualified Unison.Cryptography as C
import qualified Unison.Note as Note
import qualified Unison.Parser as Parser
import qualified Unison.Reference as Reference
import qualified Unison.Runtime.ExtraBuiltins as EB
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.TermParser as TermParser
import qualified Unison.TypeParser as TypeParser
import qualified Unison.Util.Logger as L
import qualified Unison.View as View

{-
uc new
uc add [<name>]
uc edit name
uc view name
uc rename src target
uc statistics [name]
uc help
uc eval [name]
-}

process :: (Show v, Var v) => IO (Codebase IO v Reference (Type v) (Term v)) -> [String] -> IO ()
process _ [] = putStrLn $ intercalate "\n"
  [ "usage: uc <subcommand> [<args>]"
  , ""
  , "subcommands: "
  , "  uc new"
  , "  uc add [<name>]"
  , "  uc edit <name>"
  , "  uc view <name>"
  , "  uc rename <name-src> <name-target>"
  , "  uc statistics [<name>]"
  , "  uc help [{new, add, edit, view, rename, statistics}]" ]
process codebase ["help"] = process codebase []
process codebase ("help" : sub) = case sub of
  ["new"] -> putStrLn "Creates a new set of scratch files (for new definitions)"
  ["add"] -> putStrLn "Typechecks definitions in scratch files"
  ["edit"] -> putStrLn "Opens a definition for editing"
  ["view"] -> putStrLn "Views the current source of a definition"
  ["rename"] -> putStrLn "Renames a definition"
  ["statistics"] -> do
    putStrLn "Gets statistics about a definition (with args)"
    putStrLn "or about all open edits (no args)"
  ["help"] -> putStrLn "prints this message"
  _ -> do putStrLn $ intercalate " " sub ++ " is not a subcommand"
          process codebase []
process _ ["new"] = do
  (path, handle) <- Temp.openTempFile "." ".u"
  let mdpath = stripu path ++ ".markdown"
  writeFile mdpath ""
  putStrLn $ "Created " ++ stripu path ++ ".{u, markdown} for code + docs"
  hPutStrLn handle "-- add your definition(s) here"
  hClose handle
process codebase ("add" : []) = do
  files <- Directory.getDirectoryContents "."
  let ufiles = filter (".u" `Text.isSuffixOf`) (map Text.pack files)
  case ufiles of
    [] -> putStrLn "No .u files in current directory"
    [name] -> process codebase ("add" : [Text.unpack name])
    _ -> do
      putStrLn "Multiple .u files in current directory"
      putStr "  "
      putStrLn . Text.unpack . Text.intercalate "\n  " $ ufiles
      putStrLn "Supply one of these files as the argument to `uc add`"
process codebase ("add" : [name]) = do
  codebase <- codebase
  str <- readFile name
  let ok = putStrLn "OK"
  putStr $ "Parsing " ++ name ++ " ... "
  bs <- case Parser.run TermParser.moduleBindings str TypeParser.s0 of
    Parser.Fail err _ -> putStrLn " FAILED" >> mapM_ putStrLn err >> fail "parse failure"
    Parser.Succeed bs _ _ -> pure bs
  ok
  putStr "Checking for name ambiguities ... "
  let hooks = Codebase.Hooks ok before after False
      before (v, _) = putStr $ "Typechecking " ++ show v ++ " ... "
      after (_, _, h) = do ok; putStrLn $ "Added to codebase; definition has hash " ++ show h
      baseName = stripu name -- todo - more robust file extension stripping
  results <- Note.run $ Codebase.declareCheckAmbiguous hooks Term.ref bs codebase
  case results of
    Nothing -> do
      Directory.removeFile (baseName `mappend` ".markdown") <|> pure ()
      Directory.removeFile (baseName `mappend` ".u") <|> pure ()
      hasParent <- (True <$ Directory.removeFile (baseName `mappend` ".parent")) <|> pure False
      let suffix = if hasParent then ".{u, markdown, parent}" else ".{u, markdown}"
      putStrLn $ "Removed files " ++ baseName ++ suffix
      -- todo - if hasParent, ask if want to update usages
    Just ambiguous -> error "todo"
process codebase _ = process codebase []

stripu :: [a] -> [a]
stripu = reverse . drop 2 . reverse

hash :: Var v => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = Reference.Derived (ABT.hash t)

store :: IO (Store IO (Symbol.Symbol View.DFO))
store = FileStore.make "store"

makeRandomAddress :: C.Cryptography k syk sk skp s h c -> IO Address
makeRandomAddress crypt = Address <$> C.randomBytes crypt 64

main :: IO ()
main = getArgs >>= process codebase
  where
  codebase = do
    mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
    store' <- store
    logger <- L.atomic (L.atInfo L.toStandardError)
    let crypto = C.noop "dummypublickey"
    blockStore <- FBS.make' (makeRandomAddress crypto) makeAddress "blockstore"
    builtins0 <- pure $ Builtin.make logger
    builtins1 <- EB.make logger blockStore crypto
    codebase <- pure $ Codebase.make hash store'
    Codebase.addBuiltins (builtins0 ++ builtins1) store' codebase
    pure codebase
