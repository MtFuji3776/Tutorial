module Graph where

import Prelude
import Effect (Effect)
import Effect.Console (log,logShow)
import Math(sqrt)
import Data.Semiring
import Data.Set as Set
import Data.Tuple
import Data.Array
import Data.Functor
--import Data.List as List

data Graph a = Empty | Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)

instance showGraph :: Show a => Show (Graph a) where
    show Empty           = "Empty"
    show (Vertex n)      = "Vertex " <> show n
    show (Overlay g1 g2) = "(" <> show g1 <> "+" <> show g2 <> ")"
    show (Connect g1 g2) = "(" <> show g1 <> "*" <> show g2 <> ")"

instance semiringGraph :: Semiring (Graph a) where
    add g1 g2 = Overlay g1 g2
    zero      = Empty
    mul g1 g2 = Connect g1 g2
    one       = Empty

instance functorGraph :: Functor Graph where
    map f g = case g of Empty         -> Empty -- pursではcaseに;がつかないことに注意
                        Vertex x      -> Vertex (f x)
                        Overlay g1 g2 -> Overlay (map f g1) (map f g2)
                        Connect g1 g2 -> Connect (map f g1) (map f g2)

-- グラフから頂点集合を構成
vertices :: forall a . Ord a => Graph a -> Set.Set a
vertices Empty           = Set.empty
vertices (Vertex n)      = Set.singleton n
vertices (Overlay g1 g2) = vertices g1 <> vertices g2
vertices (Connect g1 g2) = vertices g1 <> vertices g2

-- グラフから辺集合を構成
edges :: forall a . Ord a => Graph a -> Set.Set (Tuple a a)
edges Empty           = Set.empty
edges (Vertex n)      = Set.empty
edges (Overlay g1 g2) = edges g1 <> edges g2
edges (Connect g1 g2) = edges g1 <> edges g2 <> (Set.fromFoldable $ (\x y -> Tuple x y) <$> (Set.toUnfoldable $ vertices g1 :: Array a) <*> (Set.toUnfoldable $ vertices g2 :: Array a))

-- グラフを頂点集合と辺集合の組に変換
destruct_ :: forall a . Ord a => Graph a -> Tuple (Set.Set a) (Set.Set (Tuple a a))
destruct_ g = Tuple (vertices g) (edges g)

-- 実用上のデストラクタ。グラフを頂点と頂点タプルのArrayに変換
destruct   :: forall a . Ord a => Graph a -> Tuple (Array a) (Array (Tuple a a))
destruct g = let cross f g (Tuple x y) = Tuple (f x) (g y)
             in cross (Set.toUnfoldable :: Set.Set a -> Array a) (Set.toUnfoldable :: Set.Set (Tuple a a) -> Array (Tuple a a)) $ destruct_ g

-- 頂点Arrayから頂点のみのグラフを生成
genVertices :: forall a . Ord a => Array a -> Graph a
genVertices ns = foldr Overlay Empty $ map Vertex ns

-- 頂点タプルから辺だけ貼り合わせたグラフを生成
genEdges :: forall a . Ord a => Array (Tuple a a) -> Graph a
genEdges ts = let toedge (Tuple x y) = Connect (Vertex x) (Vertex y) 
              in foldr Overlay Empty $ map toedge ts

-- 頂点と頂点タプルのArrayからグラフを生成。
    -- destruct . graph . destruct = destruct
    -- destruct . graph = id が成立。
graph :: forall a . Ord a => Tuple (Array a) (Array (Tuple a a)) -> Graph a
graph ts = let cross f g (Tuple x y) = Tuple (f x) (g y)
           in uncurry Overlay $ cross genVertices genEdges ts


-- 多分木
data Rose a = Node a (Array (Rose a))

instance showRose :: Show a => Show (Rose a) where
    show (Node x ts) = "Node " <> show x <>  " (" <> (foldr (\x ys -> show x <> ys) "" ts) <> ")"

instance functorRose :: Functor Rose where
  map f (Node x ts) = Node (f x) $ map (map f) ts

genRose_ :: forall a . Eq a => a -> Array (Tuple a a) -> Rose a
genRose_ n [] = Node n []
genRose_ n es = let es' = filter (\t -> fst t == n) es
                    es1 = map snd es'
                    es_ = filter (\t -> snd t /= n) es'
               in Node n $ map (flip genRose_ es_) es1


genRose :: forall a . Eq a => Ord a => a -> Graph a -> Rose a
genRose n g = let es = snd $ destruct g
              in genRose_ n es