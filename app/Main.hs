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

-- TODO Make me customisable.
prompt :: Text
prompt = "> "

runCommand :: Handle -> Handle -> Handle -> String -> [String] -> IO ExitCode
runCommand i o e cmd args = do
  (_, _, _, h) <- Process.createProcess (Process.proc cmd args) {
          Process.std_in = Process.UseHandle i
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
          liftIO $ runCommand IO.stdin IO.stdout IO.stdin line []
          loop
