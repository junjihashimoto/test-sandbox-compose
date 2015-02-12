{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}

import Test.Hspec
import Text.Shakespeare.Text

import Network.Wai
import Network.Wai.Test

import Yesod
import Data.IORef.Lifted
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8,decodeUtf8)

import Test.Cabal.Path
import System.Process
import System.Exit

main = do
  bin <-  getExePath "." "test-sandbox-compose"
  hspec $ do
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
        o `shouldBe` T.unpack [sbt|OK
                                  |]
        c `shouldBe` ExitSuccess
      it "kill" $ do
        (c,o,e) <- readProcessWithExitCode bin ["kill"] []
        o `shouldBe` T.unpack [sbt|OK
                                  |]
        c `shouldBe` ExitSuccess
      it "up" $ do
        (c,o,e) <- readProcessWithExitCode bin ["up"] []
        o `shouldBe` T.unpack [sbt|OK
                                  |]
        c `shouldBe` ExitSuccess
      it "destroy" $ do
        (c,o,e) <- readProcessWithExitCode bin ["destroy"] []
        o `shouldBe` T.unpack [sbt|OK
                                  |]
        c `shouldBe` ExitSuccess
