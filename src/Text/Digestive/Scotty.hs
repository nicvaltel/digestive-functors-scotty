-- | Module providing a scotty backend for the digestive-functors library
module Text.Digestive.Scotty
    ( runForm
    ) where

import Control.Monad ( liftM )
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Lazy as TL
import qualified Web.Scotty.Trans as Scotty
import Network.Wai ( requestMethod )
import Network.Wai.Parse ( fileName )
import Network.HTTP.Types ( methodGet )

import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View

scottyEnv :: Monad m => Env (Scotty.ActionT m)
scottyEnv path = do
    inputs <- liftM (map $ TextInput . TL.toStrict) $ Scotty.param name
    files  <- liftM (map (FileInput . B.unpack . fileName . snd) . filter ((== name) . fst)) Scotty.files
    return $ inputs ++ files
  where name = TL.fromStrict . fromPath $ path

-- | Runs a form with the HTTP input provided by Scotty.
runForm :: Monad m
        => T.Text                               -- ^ Name of the form
        -> Form v (Scotty.ActionT m) a          -- ^ Form to run
        -> (Scotty.ActionT m) (View v, Maybe a) -- ^ Result
runForm name form = Scotty.request >>= \rq ->
    if requestMethod rq == methodGet
        then getForm name form >>= \v -> return (v, Nothing)
        else postForm name form scottyEnv

