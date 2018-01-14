import qualified Data.Map as Map
import Control.Monad

type PointId = String
data Edge a = Edge PointId a
data PointInfo a b = PointInfo a [Edge b]
data Point a b = Point PointId (PointInfo a b)
type Graph a b = Map.Map PointId (PointInfo a b)

type WorkingPoints n = Map.Map PointId (n, [String])

min_by :: Ord b => (a -> b) -> [a] -> Either String a
min_by f []     = Left "Empty list, could not take min"
min_by f (x:xs) = Right $ foldl (\ a b -> if f a < f b then a else b) x xs

make_string separator []      = []
make_string separator (x:[])  = show x
make_string separator (x:xs)  = show x ++ separator ++ (make_string separator xs)

instance (Show a, Show b) => Show (Point a b) where
    show (Point id edges) = show id ++ " ->" ++ (show edges)

instance (Show a, Show b) => Show (PointInfo a b) where
    show (PointInfo info edges) = show info ++ show edges 

instance (Show a) => Show (Edge a) where
    show (Edge id edge_info) = show id ++ ":" ++ show edge_info 

to_either message Nothing  = Left message
to_either message (Just x) = Right x 

get_info :: Point a b -> a
get_info (Point _ (PointInfo info _)) = info

get_price (_, (price, _)) = price
 
get_point id graph = 
    to_either message $ 
    do 
        info <- Map.lookup id graph
        return (Point id info) 
    where message = "Fatal::no point " ++ show id ++ " in the graph"

get_edges id graph = fmap (\(Point _ (PointInfo _ edges)) -> edges) $ get_point id graph

--get_best_path :: (Ord b, Num b) => Graph () b ->  PointId -> PointId -> Either String (b, [String])
get_best_path graph a b =
    get_best_path' graph (Map.fromList []) (Map.fromList [(a, (0, [a]))]) b
    where get_best_path' graph resolved front target = 
              if target `Map.member` resolved
              then to_either ("we've made it sure that " ++ show target ++ " exists in 'resolved'") $ Map.lookup target resolved
              else
                  do
                      (graph', resolved', front') <- resolve graph resolved front
                      get_best_path' graph' resolved' front' target

--resolve :: (Ord b, Num b) => Graph a b -> WorkingPoints b -> WorkingPoints b -> Either String (Graph a b, WorkingPoints b, WorkingPoints b)
resolve graph resolved front = 
    do 
        min@(min_id, min_value) <- min_by get_price (Map.toList front)
        edges <- get_edges min_id graph
        let front' = foldl (resolve_edge resolved min_value) front edges
        return (graph, Map.insert min_id min_value resolved, Map.delete min_id front')
    

resolve_edge resolved (price, path) front (Edge id p) = 
    if id `Map.member` resolved
    then front
    else case Map.lookup id front of
             Nothing  -> Map.insert id new_value front
             (Just (price', path')) -> if new_price < price'
                                       then Map.insert id new_value front
                                       else front
    where new_value = (new_price, new_path)
          new_price = price + p
          new_path  = id : path


map_simple_format test =
    Map.map (PointInfo ()) . foldl (\ accum (from, to, price) -> Map.insertWith (++) (show from) [Edge (show to) price] accum) (Map.fromList [])
    $ do
        (from, edges) <- test
        (to, price)   <- edges
        [(from, to, price), (to, from, price)]


test2 = 
    ("1", [("1", 0), ("2", 7), ("3", 9), ("4", 20), ("5", 20), ("6", 11)],
     map_simple_format $
      [(1, [(2,7),  (3,9),  (6,14)])
      ,(2, [(3,10), (4,15)])
      ,(3, [(4,11), (6,2)])
      ,(4, [(5,6)])
      ,(5, [(6,9)])
      ])

test1 = Map.fromList .
       map (\ (Point id info) -> (id, info)) $ 
       [ Point "A" (PointInfo () [Edge "B" 1, Edge "C" 2])
       , Point "B" (PointInfo () [Edge "A" 1, Edge "D" 2])
       , Point "C" (PointInfo () [Edge "A" 2, Edge "D" 3])
       , Point "D" (PointInfo () [Edge "B" 2, Edge "C" 3])
       ] 

test (from, expected, graph) =
    do 
        tests <- sequence . map (\(to, price) -> fmap (\x -> (from, to, price,x)) (get_best_path graph from to) ) $ expected
        return . filter (\(from, to, price, (actual_price, actual_path)) -> price /= actual_price) $ tests

main = print "sdgf"
