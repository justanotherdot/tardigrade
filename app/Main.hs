{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified System.Console.Haskeline as Haskeline
import           System.Exit              (ExitCode)
import           System.IO                (BufferMode (..), Handle, IO, stderr,
                                           stdout)
import qualified System.IO                as IO
import qualified System.Process           as Process
import           Tardigrade.Parser        (Program (..), parse)

-- TODO Make me customisable.
prompt :: Text
prompt = "> "

-- TODO This will blow up when the passed command does not run on the path.
-- We'll need write a wrapper anyways to determining if something needs to be
-- run as a builtin or as an actual binary on the PATH.
-- As such, we can capture any thrown exceptions there from createProcess.
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

-- Well typed shells don't go wrong.
tardigrade :: IO ()
tardigrade = do
  IO.hSetBuffering stdout LineBuffering
  IO.hSetBuffering stderr LineBuffering
  Haskeline.runInputT Haskeline.defaultSettings loop
  where
    handleInput Nothing       = pure ()
    handleInput (Just "exit") = pure ()
    handleInput (Just line)   =
          case parse $ T.pack line of
            Left e    -> liftIO $ IO.hPutStrLn IO.stderr e
            Right cmd -> do
              liftIO $ print cmd
              liftIO $ runCommand IO.stdin IO.stdout IO.stderr cmd
              loop

    loop :: Haskeline.InputT IO ()
    loop = do
      input <- Haskeline.getInputLine $ T.unpack prompt
      handleInput input

main :: IO ()
main = tardigrade
