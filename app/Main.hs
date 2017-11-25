{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative      ((<|>))
import           Control.Monad.IO.Class   (liftIO)
import           Data.Char                (isAlpha)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified System.Console.Haskeline as Haskeline
import           System.Exit              (ExitCode)
import           System.IO                (BufferMode (..), Handle, IO, stderr,
                                           stdout)
import qualified System.IO                as IO
import qualified System.Process           as Process
import           Text.Megaparsec          (Dec, ParseError, Token, sepBy)
import qualified Text.Megaparsec          as MP
import           Text.Megaparsec.Text     (Parser)

-- XSH would call this a `program` and that makes more semantic sense than a 'command'
-- especially when the context is that of a script being interpreted.
data Program where
  Command :: Text -> [Text] -> Program
  deriving (Show)

-- TODO This is actually just the parser for `Command'
-- so best if we clarify that later via its name.
program :: Parser Program
program = do
  MP.space
  cmd <- MP.some MP.letterChar
  MP.space
  args <- MP.some MP.asciiChar `sepBy` MP.spaceChar -- <|> MP.space
  return $ Command (T.pack cmd) (fmap T.pack args)

{- parse :: Text -> Either (ParseError (Token Text) Dec) Program -}
parse :: Text -> Either String Program
parse line = case MP.parse program "" line of
               Left _    -> Left "urk"
               Right prg -> Right prg

-- TODO Make me customisable.
prompt :: Text
prompt = "> "

runCommand :: Handle -> Handle -> Handle -> Program -> IO ExitCode
runCommand i o e (Command cmd' args') = do
  (_, _, _, h) <-
    Process.createProcess
      (Process.proc cmd args)
      { Process.std_in    = Process.UseHandle i
      , Process.std_out   = Process.UseHandle o
      , Process.std_err   = Process.UseHandle e
      -- TODO This is lifted from XSH and can probably be reworded without the lab context.
      -- NOTE: this is critical, the forked process will have a reference to the
      --       pipe and it would never otherwise be closed in the child. This means
      --       the final process in the pipeline would hang forever waiting for the
      --       EOF on the pipe. If you choose to re-implement your own fork-exec,
      --       be aware of this.
      , Process.close_fds = True
      }
  Process.waitForProcess h
    where cmd  = T.unpack cmd'
          args = fmap T.unpack args'

main :: IO ()
main = do
  IO.hSetBuffering stdout LineBuffering
  IO.hSetBuffering stderr LineBuffering
  Haskeline.runInputT Haskeline.defaultSettings loop
  where
    loop :: Haskeline.InputT IO ()
    loop = do
      input <- Haskeline.getInputLine $ T.unpack prompt
      case input of
        Nothing -> pure ()
        Just "quit" -> pure ()
        Just line ->
          case parse $ T.pack line of
            Left e -> liftIO $ putStrLn e
            Right cmd -> do
              {- liftIO $ print cmd -}
              liftIO $ runCommand IO.stdin IO.stdout IO.stderr cmd
              loop
