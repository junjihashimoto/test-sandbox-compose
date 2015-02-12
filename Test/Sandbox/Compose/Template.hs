
module Test.Sandbox.Compose.Template (
  applyTemplate
) where

import qualified Text.Hastache as H
import qualified Text.Hastache.Context as H
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Map as M

applyTemplate :: H.MuVar a
         => [(String, a)]
         -> String
         -> IO String
applyTemplate keyValues template  = do
  str' <- H.hastacheStr
          H.defaultConfig
          (H.encodeStr template)
          (H.mkStrContext context')
  return $ T.unpack $ TL.toStrict str'
  where
    context' :: Monad m => String -> H.MuType m
    context' str' = case M.lookup str' (M.fromList (map (\(k,v) -> (k,H.MuVariable v)) keyValues)) of
      Just val -> val
      Nothing -> H.MuVariable ("{{"++str'++"}}")

