module Main where

import           Lib
import           Model
import           Text.Parsec.Text
import           System.Environment
import           Data.List                      ( elemIndex
                                                , intercalate
                                                )

identifyProblemPolygons :: Mesh -> IO ()
identifyProblemPolygons mesh = do
  putStrLn
    . intercalate ", "
    . map (maybe "Oops" show . flip elemIndex mesh)
    $ anti
  where (_, anti) = meshOreintations mesh

main :: IO ()
main = do
  args        <- getArgs
  parseResult <- parseFromFile off (head args)
  case parseResult of
    Right mesh -> identifyProblemPolygons mesh
    Left  err  -> putStrLn $ "Could not read specified file: " ++ (show err)
