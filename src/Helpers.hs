{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Snap
import qualified Data.ByteString.Char8 as B8
import Data.Maybe

readSafe :: Read a => String -> Maybe a
readSafe = fmap fst . listToMaybe . reads

getId :: MonadSnap m => m Int
getId = do mi <- getParam "id"
           case readSafe . B8.unpack =<<  mi  of
             Nothing -> pass
             Just i -> return i
