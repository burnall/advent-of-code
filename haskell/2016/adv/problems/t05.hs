-- stack script --resolver lts-21.25 --package base16-bytestring  --package containers  --package cryptohash-md5 --package bytestring

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit)
import qualified Data.Map as Map

main :: IO ()
main = do
  let prefix = "ugkcyxxp"
  print $ task2 prefix

task1 :: String -> String
task1 = map (!! 5) . take 8 . generate

md5Hex :: String -> String
md5Hex = BS.unpack . B16.encode . MD5.hash . BS.pack

generate :: String -> [String]
generate prefix = filter hasZeroes $ map encode [1 ..]
  where
    encode = md5Hex . (prefix ++) . show
    hasZeroes = ("00000" ==) . take 5

task2 :: String -> String
task2 prefix = go (generate prefix) Map.empty
  where
    go (hash : rest) m
      | Map.size mNew == 8 = map snd (Map.toAscList mNew)
      | otherwise = go rest mNew
      where
        idxChar = hash !! 5
        idxValid = isDigit idxChar
        idx = read [idxChar] :: Int
        mNew =
          if idxValid
            && idx < 8
            && idx + 5 < length hash
            && not (Map.member idx m)
            then Map.insert idx (hash !! 6) m
            else m
