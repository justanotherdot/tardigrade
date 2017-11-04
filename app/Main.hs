{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)
import qualified System.Console.Haskeline as Haskeline

-- TODO Make me customisable.
prompt :: String
prompt = "> "

main :: IO ()
main = Haskeline.runInputT Haskeline.defaultSettings loop
  where
    loop :: Haskeline.InputT IO ()
    loop = do
            input <- Haskeline.getInputLine prompt
            case input of
              Nothing     -> pure ()
              Just "quit" -> pure ()
              Just line   -> liftIO (putStrLn line) >> loop
