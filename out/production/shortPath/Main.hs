module Main where
import Dijkstra
import System.IO


main = do
    graphString <- readFile "./graph/railway.txt"
    let graph = string2Graph graphString
        path = package graph "beijing" "shenyang"
    print(path)