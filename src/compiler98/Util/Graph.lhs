A module for manipulating graphs

> module Util.Graph(NodeId, Graph, GraphMonad, 
>                   emptyGraph, getAllNodes, getNodes,
>                   hasNode, getNodeData, getOutEdges, getInEdges, addNode, updateNode, removeNode,
>                   hasEdge, getEdgeData, addEdge, updateEdge, removeEdge, getGraph)
>                   where

> import qualified Data.Map as Map
> import StateMonad
> import Control.Monad.State

NodeIds are designed to be opaque the outside world

> data NodeId = NId Int deriving (Eq,Ord)

> instance Show NodeId where
>     show (NId n) = "#"++show n

A graph consists of the nodes in the graph as well as a counter giving the next
available node id. The ndata and edata are the data to be carried on the nodes and
edges respectively.

> data Graph edata ndata = Graph { nodes :: Map.Map NodeId (Node edata ndata),
>                                  nextId :: Int }

A node contains the node's id and a list of edges to other nodes as well as a list
of which nodes have edges to it. Nodes also have a custom data item

> data Node edata ndata = Node { nid :: NodeId, 
>                                outEdges :: [Edge edata],
>                                inEdges :: [NodeId],
>                                ndata :: ndata }

Edges record which nodes the edge points to as well as the data carried on that edge.

> data Edge edata = Edge { edge  :: NodeId,
>                          edata :: edata }

A graph monad is used to manipulate a graph.

> type GraphMonad g a = State g a

The empty graph

> emptyGraph :: Graph e n
> emptyGraph = Graph Map.empty 0

Get the nodes in the graph

> getNodes :: Graph e n -> [n]
> getNodes graph = map ndata $ Map.elems $ nodes graph


get a list of all the node identifiers in the graph

> getAllNodes :: GraphMonad (Graph e n) [NodeId]
> getAllNodes = readState (map fst . Map.toList . nodes)

hasNode returns whether the given node is in the graph

> hasNode :: NodeId -> GraphMonad (Graph e n) Bool
> hasNode i = getNode i >>= return . maybe False (const True)

getNodeData gets the data associated with a particular node (which must exist)

> getNodeData :: NodeId -> GraphMonad (Graph e n) n
> getNodeData i = getNode_ "getNodeData" i >>= return . ndata

getOutEdges gets the list of nodes this node (which must exist) connects to

> getOutEdges :: NodeId -> GraphMonad (Graph e n) [NodeId]
> getOutEdges i = getNode_ "getOutEdges" i >>= return . map edge . outEdges

getInEdges gets the list of nodes which connect to this node (which must exist) 

> getInEdges :: NodeId -> GraphMonad (Graph e n) [NodeId]
> getInEdges i = getNode_ "getInEdges" i >>= return . inEdges 

addNode function adds a node (with the given node data) to
the graph and returns the id of the node added.

> addNode :: n -> GraphMonad (Graph e n) NodeId
> addNode nd = writeState $ \ g -> let i      = nextId g
>                                      nid    = NId i
>                                      node   = Node nid [] [] nd
>                                      nodes' = Map.insert nid node (nodes g)
>                                      g'     = g { nextId = i+1, nodes = nodes' }
>                                  in (g', nid)

updateNode takes a function to transform the node data and updates the node
specified by the node id.

> updateNode :: NodeId -> (n -> n) -> GraphMonad (Graph e n) ()
> updateNode i upf = updNode (\ n -> n { ndata = upf (ndata n) }) i

removeNode removes a given node, and all the edges associated with the node

> removeNode :: NodeId -> GraphMonad (Graph e n) ()
> removeNode i = 
>     do outs <- getOutEdges i
>        ins  <- getInEdges i 
>        mapM_ (\ n -> removeInEdge n i) outs
>        mapM_ (\ n -> removeOutEdge n i) ins
>        writeState_ $ \ g -> g { nodes = Map.delete i (nodes g) }

hasEdge returns whether two nodes (which must exist) have an edge between them

> hasEdge :: NodeId -> NodeId -> GraphMonad (Graph e n) Bool
> hasEdge fid tid = getEdge fid tid >>= return . maybe False (const True)

getEdgeData gets the data associated with the edge between two nodes (which must
exist) or Nothing if there is no such edge.

> getEdgeData :: NodeId -> NodeId -> GraphMonad (Graph e n) e
> getEdgeData fid tid = getEdge_ fid tid >>= return . edata

addEdge adds an edge between two nodes, with the given edge data. It fails if
either node does not exist, but does allow multiple edges between nodes.

> addEdge :: NodeId -> NodeId -> e -> GraphMonad (Graph e n) ()
> addEdge fid tid ed = 
>     do checkNode tid
>        updNode (\ fnode -> fnode { outEdges = Edge tid ed : (outEdges fnode) }) fid
>        updNode (\ tnode -> tnode { inEdges = fid : (inEdges tnode) }) tid

updateEdge changes the data on a particular edge (which must exist)

> updateEdge :: NodeId -> NodeId -> (e -> e) -> GraphMonad (Graph e n) ()
> updateEdge fid tid upf = updNode (\ fnode -> fnode { outEdges = updEdge (outEdges fnode) }) fid
>     where
>     updEdge []     = error $ "updateEdge: there is not edge between "++show fid ++" and "++show tid
>     updEdge (e:es) 
>         | edge e == tid = e { edata = upf (edata e) } : es
>         | otherwise     = e : updEdge es

removeEdge removes an edge between two nodes. Both nodes must exist and there
must be an edge between them
                                            
> removeEdge :: NodeId -> NodeId -> GraphMonad (Graph e n) ()
> removeEdge fid tid = 
>     do removeOutEdge fid tid
>        removeInEdge tid fid

getGraph gets the complete current graph from the monad

> getGraph :: GraphMonad (Graph e n) (Graph e n)
> getGraph = readState id

code to show a graph, assuming we know how to show the node and edge data

> instance (Show e, Show n) => Show (Graph e n) where
>     show (Graph ns _) = "{\n"++concatMap (showNode . snd) (Map.toList ns)++"}\n"
>         where
>         showNode (Node i os is nd) = "  " ++ show i ++ "  " ++ show nd ++ {- " <- " ++ show is  ++ -} "\n" ++
>                                     concatMap showEdge os 
>         showEdge (Edge e ed)      = "    -> " ++ show e ++ " " ++ show ed ++ "\n"

getNode is an internal helper. It gets a node for a node id, or Nothing if there is no such node.
                                              
> getNode :: NodeId -> GraphMonad (Graph e n) (Maybe (Node e n))
> getNode i = readState $ \ g -> Map.lookup i (nodes g)

getNode_ is the same as getNode but will give an error if the node does not exist

> getNode_ :: String -> NodeId -> GraphMonad (Graph e n) (Node e n)
> getNode_ from i = do node <- getNode i
>                      case node of
>                          Nothing -> error $ from++": there is no node "++show i++" in the graph"
>                          Just n  -> return n

checkNode asserts that the graph does have a node of the given id

> checkNode :: NodeId -> GraphMonad (Graph e n) ()
> checkNode i = do getNode_ "checkNode" i
>                  return ()

updNode is a helper to update the entire contents of a node in the graph

> updNode :: (Node e n -> Node e n) -> NodeId -> GraphMonad (Graph e n) ()
> updNode upf i = writeState_ $ \ g -> g { nodes = Map.update (Just . upf) i (nodes g) }

removeOutEdge removes only a out edge between the two nodes in the graph 
(both nodes and the edge must exist)

> removeOutEdge :: NodeId -> NodeId -> GraphMonad (Graph e n) ()
> removeOutEdge fid tid = 
>     updNode (\ fnode -> let outEdges' = deleteBy (\ i e -> edge e == i) tid (outEdges fnode)
>                         in fnode { outEdges = outEdges' }) fid

removeInEdge does the same as removeOutEdge but for in edges

> removeInEdge :: NodeId -> NodeId -> GraphMonad (Graph e n) ()
> removeInEdge tid fid = 
>     updNode (\ tnode -> let inEdges' = deleteBy (==) fid (inEdges tnode)
>                         in tnode { inEdges = inEdges' }) tid

getEdge returns the edge data of the edge between two nodes (which must exist),
or Nothing if there is no such edge

> getEdge :: NodeId -> NodeId -> GraphMonad (Graph e n) (Maybe (Edge e))
> getEdge fid tid = 
>     do mn <- getNode fid 
>        return $ mn >>= \ n -> findEdge tid (outEdges n)
>     where
>     findEdge tid []     = Nothing
>     findEdge tid (e:es) = if edge e == tid then Just e else findEdge tid es

getEdge_ is like getEdge but ensures that the edge must exist

> getEdge_ :: NodeId -> NodeId -> GraphMonad (Graph e n) (Edge e)
> getEdge_ fid tid = getEdge fid tid >>= return . maybe err id 
>     where
>     err = error $ "getEdge_: this is no edge between "++show fid++" and "++show tid

deleteBy is a helper function to delete something from a list, deciding whether it
is in a list by the given function. Note this has a slightly different type signature to List.deleteBy 

> deleteBy :: (a -> b -> Bool) -> a -> [b] -> [b]
> deleteBy f x []     = []
> deleteBy f x (y:ys) = if f x y then ys else y : deleteBy f x ys