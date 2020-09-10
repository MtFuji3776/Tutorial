module Graph where

import Prelude
import Effect (Effect)
import Effect.Console (log,logShow)
import Math(sqrt)
import Data.Semiring
import Data.Set as Set
import Data.Tuple
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

vertices :: Graph -> Set.Set Int
vertices Empty           = Set.empty
vertices (Vertex n)      = Set.singleton n
vertices (Overlay g1 g2) = vertices g1 <> vertices g2
vertices (Connect g1 g2) = vertices g1 <> vertices g2

edges :: Graph -> Set.Set (Tuple Int Int)
edges Empty           = Set.empty
edges (Vertex n)      = Set.empty
edges (Overlay g1 g2) = edges g1 <> edges g2
edges (Connect g1 g2) = (edges g1) <> (edges g2) <> ( Set.fromFoldable $ (\x y -> (Tuple x y)) <$> (Set.toUnfoldable  (vertices g1) :: Array Int) <*> (Set.toUnfoldable (vertices g2) :: Array Int))
