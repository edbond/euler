module Main
where

import qualified Test.HUnit as T
import Data.List

data Node = Node {x::Int, y::Int} deriving (Show, Eq)
data Grid = Grid {gnodes :: [Node], r :: Int} deriving (Show)

buildGrid :: Int -> Grid
buildGrid n = Grid [Node a b | b <- [1..n], a <- [1..n]] n

nexty :: Grid -> Node -> [Node]
nexty g n =
  let
    nx = x n
    ny = y n
    rank = (r g)
  in
   case (ny + 1) <= rank of
     True -> [(gnodes g) !! ((ny) * rank + (nx - 1))]
     False -> []

nextx :: Grid -> Node -> [Node]
nextx g n =
  let
    nx = x n
    ny = y n
    rank = (r g)
  in
   case (nx + 1) <= rank of
     True -> [(gnodes g) !! (((ny-1) * rank) + (nx))]
     False -> []

nextNodes :: Grid -> Node -> [Node]
nextNodes g n =
  concat [(nextx g n), (nexty g n)]

lastNode :: Grid -> Node -> Bool
lastNode g n = and [(r g) == (x n), (r g) == (y n)]

walkPaths :: Grid -> Int -> [Node] -> Int
walkPaths g acc [] = acc
walkPaths g acc nodes = let
  end = filter (lastNode g) nodes
  newacc = acc + (length end)
  newnodes = nodes \\ end
  -- advance nodes
  advnodes = concat $ map (nextNodes g) newnodes
  in
   walkPaths g newacc advnodes

pathCount :: Int -> Int
pathCount r = let
  g = buildGrid r
  in
   walkPaths g 0 [Node 1 1]

-- Tests
test3 = T.TestCase $ T.assertEqual
  "3x3 grid has 6 paths"
  6
  (pathCount 3)

testNextNodes = T.TestCase $ T.assertEqual
  "next nodes from 1,2 is 2,2 and 1,3"
  [(Node 2 2), (Node 1 3)]
  (nextNodes (buildGrid 4) (Node 1 2))

runTests =
  T.runTestTT (T.TestList [test3, testNextNodes])

main = do
  -- runTests
  let
    a = pathCount 20
  print a
