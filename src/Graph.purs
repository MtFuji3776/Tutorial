module Graph where

import Prelude
import Effect (Effect)
import Effect.Console (log,logShow)
import Math(sqrt)
import Data.Semiring
import Data.Set as Set
import Data.Tuple
import Data.Array
--import Data.List as List

data Graph = Empty | Vertex Int | Overlay Graph Graph | Connect Graph Graph

instance showGraph :: Show Graph where
    show Empty           = "Empty"
    show (Vertex n)      = "Vertex " <> show n
    show (Overlay g1 g2) = "(" <> show g1 <> "+" <> show g2 <> ")"
    show (Connect g1 g2) = "(" <> show g1 <> "*" <> show g2 <> ")"

instance semiringGraph :: Semiring Graph where
    add g1 g2 = Overlay g1 g2
    zero      = Empty
    mul g1 g2 = Connect g1 g2
    one       = Empty

-- グラフから頂点集合を構成
vertices :: Graph -> Set.Set Int
vertices Empty           = Set.empty
vertices (Vertex n)      = Set.singleton n
vertices (Overlay g1 g2) = vertices g1 <> vertices g2
vertices (Connect g1 g2) = vertices g1 <> vertices g2

-- グラフから辺集合を構成
edges :: Graph -> Set.Set (Tuple Int Int)
edges Empty           = Set.empty
edges (Vertex n)      = Set.empty
edges (Overlay g1 g2) = edges g1 <> edges g2
edges (Connect g1 g2) = edges g1 <> edges g2 <> (Set.fromFoldable $ (\x y -> Tuple x y) <$> (Set.toUnfoldable $ vertices g1 :: Array Int) <*> (Set.toUnfoldable $ vertices g2 :: Array Int))

-- グラフを頂点集合と辺集合の組に変換
destruct_ :: Graph -> Tuple (Set.Set Int) (Set.Set (Tuple Int Int))
destruct_ g = Tuple (vertices g) (edges g)

-- 実用上のデストラクタ。グラフを頂点と頂点タプルのArrayに変換
destruct   :: Graph -> Tuple (Array Int) (Array (Tuple Int Int))
destruct g = let cross f g (Tuple x y) = Tuple (f x) (g y)
             in cross (Set.toUnfoldable :: Set.Set Int -> Array Int) (Set.toUnfoldable :: Set.Set (Tuple Int Int) -> Array (Tuple Int Int)) $ destruct_ g

-- 頂点Arrayから頂点のみのグラフを生成
genVertices :: Array Int -> Graph
genVertices ns = foldr Overlay Empty $ map Vertex ns

-- 頂点タプルから辺だけ貼り合わせたグラフを生成
genEdges :: Array (Tuple Int Int) -> Graph
genEdges ts = let toedge (Tuple x y) = Connect (Vertex x) (Vertex y) 
              in foldr Overlay Empty $ map toedge ts

-- 頂点と頂点タプルのArrayからグラフを生成。
    -- destruct . graph . destruct = destruct
    -- destruct . graph = id が成立。
graph :: Tuple (Array Int) (Array (Tuple Int Int)) -> Graph
graph ts = let cross f g (Tuple x y) = Tuple (f x) (g y)
           in uncurry Overlay $ cross genVertices genEdges ts