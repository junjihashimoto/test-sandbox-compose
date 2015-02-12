{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Sandbox.Compose
import Options.Applicative

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Shelly hiding (command,run,FilePath)
import Network.HTTP.Conduit
import Control.Monad
import Control.Concurrent

default (T.Text)

type Port=Int

data Command
  = Up [String]
  | Status [String]
  | Conf
  | Kill [String]
  | Logs [String]
  | Destroy
  | Daemon FilePath
  deriving Show

up :: Parser Command
up = Up <$> many (argument str (metavar "TARGET..."))

status :: Parser Command
status = Status <$> many (argument str (metavar "TARGET..."))

conf :: Parser Command
conf = pure Conf

kill :: Parser Command
kill = Kill <$> many (argument str (metavar "TARGET..."))

logs :: Parser Command
logs = Logs <$> many (argument str (metavar "TARGET..."))

destroy :: Parser Command
destroy = pure Destroy

daemon :: Parser Command
daemon = Daemon <$> strOption (long "conf" <> value "test-sandbox-compose.yml" <> metavar "CONFFILE(Yaml)")

parse :: Parser Command
parse = subparser $ foldr1 (<>) [
        command "up"      (info up (progDesc "up registered service"))
      , command "status"  (info status (progDesc "show sandbox-state"))
      , command "conf"    (info conf (progDesc "show internal-conf-file"))
      , command "kill"    (info kill (progDesc "kill service"))
      , command "logs"    (info logs (progDesc "show logs"))
      , command "destroy" (info destroy (progDesc "destroy deamon"))
      , command "daemon" (info daemon (progDesc "start daemon"))
      ]

setup :: IO Port
setup = shelly $ do
  unlessM (test_e ".sandbox/port") $
    liftIO $ do
    runServer "test-sandbox-compose.yml" False
    threadDelay $ 1*1000*1000
  content <- readfile ".sandbox/port"
  return $ read $ T.unpack content


runCmd' :: [String] -> String -> IO ()
runCmd' xs cmd' = do
  port' <- setup
  case xs of
    [] -> simpleHttp ("http://localhost:" <> show port' <> "/" <> cmd') >>= L.putStr
    xs' -> forM_ xs' $ \x ->
      simpleHttp ("http://localhost:" <> show port' <> "/" <> cmd' <> "/" <> x) >>= L.putStr

runCmd :: Command -> IO ()
runCmd (Up targets) = runCmd' targets "up"
runCmd (Status targets) = runCmd' targets "status"
runCmd (Kill targets) = runCmd' targets "kill"
runCmd (Logs targets) = runCmd' targets "logs"
runCmd Conf = runCmd' [] "conf"
runCmd Destroy = runCmd' [] "destroy"
runCmd (Daemon conf) = runServer conf True
  
opts :: ParserInfo Command
opts = info (parse <**> helper) idm

main :: IO ()
main = execParser opts >>= runCmd
