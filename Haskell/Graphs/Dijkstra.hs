import qualified Data.Map as Map

type PointId = String
data Edge a = Edge PointId a
data Point a = Point PointId [Edge a]
data Graph a = Map PointId (Point a)
data GraphWeight a b = Map PointId (b Point a)

getPointId (Point id  _) = id
getPointEdges (Point _ edges) = edges


getBestPath :: Graph a -> PointId -> PointId -> [PointId]
getBestPath graph a b = reverse graph a b (getBestPath' [a])

getBestPath' graph b resolved =
  if (resolved `containsKey` b)
  then resolved
  else getBestPath' graph b (resolve graph resolved)


resolve graph resolved front = update_state graph resolved front (get_best front)

update_state graph resolved front best_vertex =  where 
    edges = get_edges graph
    graph' = update_graph graph edges (get_price best_vertex)

update_graph graph edges price = edges.map( \(id p) -> update_vertex( get_price (Map.get graph e)) )

update_vertex point@(Point id p edges) price = if p > price then (Point id price edges) else point11

resolve graph front resolved = 
    (graph', activePoint : resolved, changed ++ (front `except` activePoint))
    where (graph', changed) = updateGraph graph (map (\ id w_edge -> (id, w_edge + w_point)) $ getEdges activePoint)
          activePoint = getMinPoint front
          w_point = ???
          

test3 =
    do 
        (from, edges) <- test2
        (to, price)   <- edges
        [(from, to, price), (to, from, price)]

foldl ( \(from, to, price) -> Map.insert from (Edge (to) ) Map.fromList [] 
    

test2 = 
     [(1, [(2,7),  (3,9),  (6,14)])
     ,(2, [(3,10), (4,15)])
     ,(3, [(4,11), (6,2)])
     ,(4, [(5,6)])
     ,(5, [(6,9)])
     ]

do 
    (from, edges) <- test2
    (to, price)   <- edges
    (from, to, price)


test = Map.fromList .
       map (\ point@(Point id edges) -> (id, point)) $ 
       [ Point "A" [Edge "B" 1, Edge "C" 2]
       , Point "B" [Edge "A" 1, Edge "D" 2]
       , Point "C" [Edge "A" 2, Edge "D" 3]
       , Point "D" [Edge "B" 2, Edge "C" 3]
       ] 

main = print "sdgf"
