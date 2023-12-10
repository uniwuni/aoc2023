-- |

module Day10 where
import Data.Text (Text)
import Data.Graph
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Bifunctor
import qualified Data.Set as S
parse :: Text -> (Vertex, Graph, Vertex -> (Char, (Int, Int), [(Int, Int)]), (Int, Int) -> Maybe Vertex)
parse t = (fromJust $ vertexFromKey s_pos, graph, nodeFromVertex, vertexFromKey)
  where chs :: [((Int, Int), Char)]
        chs = concatMap (\(n,l) -> map (\(m,y) -> ((n,m),y)) l) $ zip [0..] $ zip [0..] . T.unpack <$> T.lines t
        s_pos = fromJust $ fst <$> find ((== 'S') . snd) chs

        indexThing :: ((Int, Int), Char) -> [(Int, Int)]
        indexThing ((n,m), '|') = [(n-1,m),(n+1,m)]
        indexThing ((n,m), '-') = [(n,m-1),(n,m+1)]
        indexThing ((n,m), 'L') = [(n-1,m),(n,m+1)]
        indexThing ((n,m), 'J') = [(n-1,m),(n,m-1)]
        indexThing ((n,m), '7') = [(n+1,m),(n,m-1)]
        indexThing ((n,m), 'F') = [(n+1,m),(n,m+1)]
        indexThing _ = []

        graphMaker :: ((Int, Int), Char) -> (Char, (Int, Int), [(Int, Int)])
        graphMaker p@((n,m), c) = (c, (n,m), indexThing p)

        thingsAtS = map (\(_,y,_) -> y) $ filter (\(_,_,z) -> s_pos `elem` z) (graphMaker <$> chs)

        indexThing' (_, 'S') = thingsAtS
        indexThing' a = indexThing a

        graphMaker' :: ((Int, Int), Char) -> (Char, (Int, Int), [(Int, Int)])
        graphMaker' p@((n,m), c) = (c, (n,m), indexThing' p)
        (graph, nodeFromVertex, vertexFromKey) = graphFromEdges $ graphMaker' <$> chs

reducePoints :: (Num a, Eq a) => [(a,a)] -> [(a,a)]
reducePoints ((x1,y1):(x2,y2):(x3,y3):xs)
          | x'1 * y'2 == y'1 * x'2 = reducePoints ((x1,y1):(x3,y3):xs)
          | otherwise = (x1,y1):reducePoints ((x2,y2):(x3,y3):xs)
          where x'1 = x2 - x1
                x'2 = x3 - x1
                y'1 = y2 - y1
                y'2 = y3 - y1

reducePoints a = a

day10part1 :: Text -> String
day10part1 t = show $ (`div` 2) $ length $ head $ filter (s `elem`) $ foldr (:) [] <$> scc g
  where (s,g,_,_) = parse t

day10part2 :: Text -> String
day10part2 t = show $ S.size $ S.filter isIn xys
  where (s,g,f,_) = parse t
        loop :: [(Int, Int)]
        loop = map ((\(_,y,_) -> y) . f) $ head $ filter (s `elem`) $ foldr (:) [] <$> scc g

        groupedLoop :: [((Float, Float),(Float, Float))]
        groupedLoop = sortOn (\((a,b),(_,d)) -> (- min d b, a)) $ map reorder $ zip l (tail l) ++ [(last l, head l)]
          where l = map (bimap fromIntegral fromIntegral) $ reducePoints loop
                reorder p@((sx,sy),(ex,ey)) | sx <= ex = p
                                            | otherwise = ((ex,ey),(sx,sy))

        loopSet = S.fromList loop

        shootRay (x,y) = filter p $ takeWhile q groupedLoop
          where p ((sx,_),(ex,_)) = sx <= x && x <= ex
                q ((_,sy),(_,ey)) = y < min sy ey

        isIn (x,y) = odd $ length $ shootRay (fromIntegral x + 0.5, fromIntegral y + 0.5)
        xys :: S.Set (Int, Int)
        xys = S.fromDistinctAscList [(x, y)
                                    | x <- [ax..bx],
                                      y <- [ay..by]] S.\\ loopSet
        (ay,by) = (minimum (map fst loop), maximum (map fst loop))
        (ax,bx) = (minimum (map snd loop), maximum (map snd loop))
