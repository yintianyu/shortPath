--Name : Tianyu Yin
--Student ID: 18212020046
module Dijkstra(
    string2Graph,
    package
) where


import Data.List
import System.IO

-- Define the data types
data Edge = Edge {node::Node, weight::Float} deriving (Show)
type Node = String
type Graph = [(Node, [Edge])]
type Dnode = (Node, (Float, Node))  -- Dnode类型是说两点之间的距离


-- ********************************************
-- Section A
------ Deal with input file
------ Convert string to graph
-- ********************************************

-- get the edges of a node
edgesOf :: Graph -> Node -> [Edge]
edgesOf graph aNode = snd . head . filter(\(nd, _) -> nd == aNode) $ graph  -- 找到graph中Node为aNode的第一个元素，取它的那些Edges

-- convert text elements in the input file to Graph
elements2Graph :: [((String , String), Float)] -> Graph
elements2Graph elements =
    let nodes = nub . map (fst . fst) $ elements            -- 对于每一项取前面的那个节点并去重
        edgesOf elements node =                             -- 覆盖了外面的edgesOf函数，把elements中的头节点为node的项的edge取出来
            let connected = filter (\((n, _), _) -> node == n) $ elements   -- connected中包含了输入文件中第一个节点为node的项
            in map (\((_, n), weight) -> Edge n weight) connected           -- 对于connected中的每一项构建一个边（第二个节点和权重）
    in map (\n -> (n, edgesOf elements n)) nodes            -- 对于每个不同的头节点（所有的节点）构建它的边


-- add reversed edges for non-directed graph
addReversedEdges :: [((String, String), Float)] -> [((String , String), Float)]
addReversedEdges elements = elements ++ map (\((n1, n2), w) -> ((n2, n1), w)) elements

-- convert text string in the input file to Graph
string2Graph :: String -> Graph
string2Graph string =
    let readElement[n1, n2, w] = ((n1, n2), read w :: Float)
        elements = map (readElement . words) $ lines string  -- 读每一行，每个单词，把他们(n1, n2, w)归到elements这个列表中
    in elements2Graph $ addReversedEdges elements            -- 转化为Graph



-- ********************************************
-- Section B
------ Deal with input file
------ Convert string to graph
-- ********************************************




-- get the weight of an node from some edges
weightOf :: Node -> [Edge] -> Float
weightOf n = weight . head . filter (\e -> n == node e)     -- 返回了一个[Edge]->Float的函数(运用函数的柯里化），在一堆边中找到以n开头的边，取它的权重


nodesOfEdges :: [Edge] -> [Node]
nodesOfEdges = map node                                     -- 取edges中每个的node



-- build the Dnode list of a node from a graph initially
dnodeInit :: Graph -> Node -> [Dnode]
dnodeInit graph startNode =
    let distanceInit (n, edges) =                           -- 这个函数计算在这些边中点n到startNode的距离
            if n == startNode                               -- 要是n就是startNode的话就是0
            then 0
            else if startNode `elem` nodesOfEdges edges     -- 如果目标节点是在这些节点中的话
                then weightOf startNode edges               -- 就取startNode的权重
                else 1e10                                   -- 要是目标节点不在这些边中，那就是无穷大
    in map (\graphElement@(n,_) -> (n, ((distanceInit graphElement), startNode))) graph -- 对图里每一项，找到和startNode直接相连的项，没有就是无穷大，有就是这个值

-- 更新Dnode
dnodeUpdate :: Dnode -> Dnode -> [Node] -> [Edge] -> Dnode
dnodeUpdate dn@(n, (nd, p)) (c, (cd, _)) cnodes edges =     -- 输入原Dnode，当前要连接的Dnode，目标节点连出去的节点和边
    let w = weightOf n edges                                -- 得到n的权重
    in  if n `notElem` cnodes then dn                       -- 如果n不在交集之中，则还用原来的那个dnode
        else if cd+w < nd then (n, (cd+w, c)) else dn       -- 否则的话判断新的路径是不是比原来的段，短就用新的，要不还用原来的



-- 递归来算迪杰斯特拉算法，输入是图、当前构建的Dnode，剩余没判断的节点，输出是判断完的Dnode
dijkstraRecursive :: Graph -> [Dnode] -> [Node] -> [Dnode]
dijkstraRecursive graph dnodes [] = dnodes                  -- 递归基，当没判断的节点为空时，输出当前的dnode
dijkstraRecursive graph dnodes nodesRemaining =
    let dnodesRemaining = filter (\dn -> (fst dn) `elem` nodesRemaining) dnodes -- 在dnodes中找到目标节点是属于剩余节点的项过滤出来
        shortestDnode = head . sortBy (\(_,(distance1, _)) (_,(distance2,_)) -> compare distance1 distance2) $ dnodesRemaining -- 在剩余的dnodes中把距离按小到大排序，距离最小的一项
        shortestTarget = fst shortestDnode                              -- 取最短路径的目标节点
        newNodesRemaining = delete shortestTarget nodesRemaining        -- 在剩余nodes中把最短路径的目标节点删除掉
        edges = edgesOf graph shortestTarget                -- 找到图中最短路径的目标节点连接的边
        cnodes = intersect (nodesOfEdges edges) newNodesRemaining       -- 找出目标节点连接的边的节点和剩余的节点中的交集
        newDnodes = map (\dn -> dnodeUpdate dn shortestDnode cnodes edges) dnodes  -- dnodes中每一个都update一下
    in dijkstraRecursive graph newDnodes newNodesRemaining  -- 开始递归，再次重复上述行为，直到remaining不剩下的时候

-- 主函数，算出startNode到每一个节点的最短路径
dijkstra :: Graph -> Node -> [Dnode]
dijkstra graph startNode =
    let dnodes = dnodeInit graph startNode                  -- dnodes初始化
        nodesRemaining = map fst dnodes                     -- 剩下的节点（在这里所有节点都是剩下的）
        in dijkstraRecursive graph dnodes nodesRemaining    -- 调用递归函数

nodeMatchDnode :: [Dnode] -> Node -> Dnode
nodeMatchDnode dnodes n = head . filter (\(x, _) -> x == n) $ dnodes -- 在dnode列表中找到头节点为n的dnode输出出来

-- 输出结果，输入dijkstra算出来的dnodes和目标节点，得到路径
result :: [Dnode] -> Node -> [Node]
result dnodes destinationNode =
    let dn@(n, (d, p )) = nodeMatchDnode dnodes destinationNode -- 找到dnodes中目标节点为destinationNode的dnode
    in if n == p then [n] else result dnodes p ++ [n]       -- 递归基是这个dnode是自己指向自己的，输出当前目标节点的序列；否则头插法建表，在目标节点序列前加上一个节点


-- 打包，输入图，起始节点和目标节点，输出路径序列
package :: Graph -> Node -> Node -> [Node]
package graph startNode destinationNode =
    let so = dijkstra graph startNode
    in result so destinationNode




