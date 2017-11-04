{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

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
import           Text.Earley              ((<?>))
import qualified Text.Earley              as E

data Command = Command String

grammar :: E.Grammar r (E.Prod r String String Command)
grammar = mdo
  cmd <- E.rule $ Command <$> (E.satisfy ident <?> "program")
  return cmd
    where ident (x:_) = isAlpha x
          ident _     = False

-- TODO `words` should be replaced with an appropriate lexer.
-- TODO Tests should ensure that the parse tree is never greater than size one (meaning it's ambiguous).
parse line =
  case E.fullParses (E.parser grammar) tokens of
    -- No parse results which means nothing fit the grammar.
    ([], report) -> error $ show report
    (p:rst, report) ->
      case rst of
        [] -> p
        -- More than one parse result, which means it's ambiguous.
        _  -> error $ "Error: An ambiguous parse occured.\n" ++ show report
  where
    lex :: String -> [String]
    lex = words
    tokens = lex line

-- TODO Make me customisable.
prompt :: Text
prompt = "> "

runCommand :: Handle -> Handle -> Handle -> Command -> [String] -> IO ExitCode
runCommand i o e (Command cmd) args = do
  (_, _, _, h) <-
    Process.createProcess
      (Process.proc cmd args)
      { Process.std_in = Process.UseHandle i
      , Process.std_out = Process.UseHandle o
      , Process.std_err = Process.UseHandle e
      , Process.close_fds = True
      }
  Process.waitForProcess h

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
        Nothing     -> pure ()
        Just "quit" -> pure ()
        Just line   -> do
          liftIO $ runCommand IO.stdin IO.stdout IO.stdin (parse line) []
          loop
