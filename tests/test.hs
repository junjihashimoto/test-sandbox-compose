{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}

import Test.Hspec
import Text.Shakespeare.Text
import Test.Sandbox
import Test.Hspec.Sandbox

import qualified Data.Text as T

import Test.Cabal.Path
import System.Process
import System.Exit

main :: IO ()
main = withSandbox $ \ref -> do
  bin <-  getExePath "." "test-sandbox-compose"
  hspec $ do
    describe "run daemon" $ with ref $ do
      it "run daemon" $ do
        file <-  setFile "yaml" $ T.unpack
                                [sbt|runhaskell:
                                    |  cmd: 'runhaskell'
                                    |  args:
                                    |    - '{{runhaskell_conf_conf}}'
                                    |  tempfiles: []
                                    |  confs:
                                    |    conf: |
                                    |      import Network
                                    |      import System.IO
                                    |      import Control.Monad
                                    |      import Control.Concurrent
                                    |      
                                    |      main :: IO ()
                                    |      main = withSocketsDo $ do
                                    |        let val = "hoge\n"
                                    |        sock <- listenOn $ PortNumber {{runhaskell_port_2181}}
                                    |        forever $ do
                                    |          (handle, _host, _port) <- accept sock
                                    |          forkFinally (talk handle val) (\_ -> hClose handle)
                                    |        where
                                    |          talk h val = do
                                    |            hPutStr h val
                                    |            v <-  hGetContents h
                                    |            putStr v
                                    |  ports:
                                    |    - '2181'
                                    |]
        let args = ["daemon", "--conf", file]
        register "daemon" bin args def >>= start
    describe "Server Test" $ do
      it "help" $ do
        (c,o,e) <- readProcessWithExitCode bin ["--help"] []
        c `shouldBe` ExitSuccess
        e `shouldBe` ""
        o `shouldBe` T.unpack [sbt|Usage: test-sandbox-compose COMMAND
                                  |
                                  |Available options:
                                  |  -h,--help                Show this help text
                                  |
                                  |Available commands:
                                  |  up                       up registered service
                                  |  status                   show sandbox-state
                                  |  conf                     show internal-conf-file
                                  |  kill                     kill service
                                  |  logs                     show logs
                                  |  destroy                  destroy deamon
                                  |  daemon                   start daemon
                                  |]
      it "up" $ do
        (c,o,e) <- readProcessWithExitCode bin ["up"] []
        e `shouldBe` ""
        o `shouldBe` T.unpack [sbt|OK
                                  |]
        c `shouldBe` ExitSuccess
      it "kill" $ do
        (c,o,e) <- readProcessWithExitCode bin ["kill"] []
        e `shouldBe` ""
        o `shouldBe` T.unpack [sbt|OK
                                  |]
        c `shouldBe` ExitSuccess
      it "up" $ do
        (c,o,e) <- readProcessWithExitCode bin ["up"] []
        e `shouldBe` ""
        o `shouldBe` T.unpack [sbt|OK
                                  |]
        c `shouldBe` ExitSuccess
      it "destroy" $ do
        (c,o,e) <- readProcessWithExitCode bin ["destroy"] []
        e `shouldBe` ""
        o `shouldBe` T.unpack [sbt|OK
                                  |]
        c `shouldBe` ExitSuccess
