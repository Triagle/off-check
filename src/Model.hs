-- | Read in an OFF file and return a mesh

module Model where

import           Text.Parsec
import           Text.Parsec.Text
import           Data.Text                      ( Text )
import           Control.Monad
import           Linear.V3
import           Lib

type Polygon = (Int, Int, Int)


header :: Parser ()
header = void $ string "OFF"

surroundedBy :: Parser a -> Parser b -> Parser b
surroundedBy p = between p p

decimal :: Parser Double
decimal = read
  <$> mconcat [string "-" <|> return "", many digit, string ".", many digit]

integer :: Parser Int
integer = read <$> many digit

vertex :: Parser (V3 Double)
vertex =
  V3
    <$> surroundedBy spaces decimal
    <*> surroundedBy spaces decimal
    <*> surroundedBy spaces decimal

polygon :: Parser Polygon
polygon =
  (\_ v1 v2 v3 -> (v1, v2, v3))
    <$> surroundedBy spaces integer
    <*> surroundedBy spaces integer
    <*> surroundedBy spaces integer
    <*> surroundedBy spaces integer

off :: Parser Mesh
off = do
  header
  newline
  nvert <- integer
  spaces
  npoly <- integer
  spaces
  nedges   <- integer
  vertices <- count nvert vertex
  polygons <- count npoly polygon
  return $ map
    (\(v1, v2, v3) -> (vertices !! v1, vertices !! v2, vertices !! v3))
    polygons
