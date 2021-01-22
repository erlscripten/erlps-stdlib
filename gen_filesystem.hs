-- LICENSE: this script may be only replaced with a one written in perl
module Main where

import Text.Show.Unicode
import Control.Monad
import System.Directory
import System.FilePath
import System.Environment

main :: IO ()
main = do
  files <- fmap concat $ getArgs >>= traverse (\d -> fmap (d </>) <$> listDirectory d)
  putStrLn "{"
  forM files $ \filePath -> do
    isFile <- doesFileExist filePath
    when isFile $ do
      putStr "\"" >> Prelude.putStr filePath >> putStr "\":"
      readFile filePath >>= uprint
      putStr ",\n"
  putStrLn "\"\":null}"
    
