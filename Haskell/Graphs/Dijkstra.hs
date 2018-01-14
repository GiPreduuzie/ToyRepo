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

getBestPath' graph a b resolved =
  if (resolved `containsKey` b)
  then resolved
  else getBestPath' graph a b (resolve graph resolved)

resolve graph front resolved = 
    (graph', activePoint : resolved, changed ++ (front `except` activePoint))
    where (graph', changed) = updateGraph graph (map (\ id w_edge -> (id, w_edge + w_point)) $ getEdges activePoint)
          activePoint = getMinPoint front
          w_point = ???
          




test = Map.fromList .
       map (\ point@(Point id edges) -> (id, point)) $ 
       [ Point "A" [Edge "B" 1, Edge "C" 2]
       , Point "B" [Edge "A" 1, Edge "D" 2]
       , Point "C" [Edge "A" 2, Edge "D" 3]
       , Point "D" [Edge "B" 2, Edge "C" 3]
       ] 

main = print "sdgf"
