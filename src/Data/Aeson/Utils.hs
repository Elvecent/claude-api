module Data.Aeson.Utils where

import           Data.List (isSuffixOf)

dropSuffix :: String -> String -> String
dropSuffix suffix s =
  if suffix `isSuffixOf` s
  then take (length s - length suffix) s
  else s
