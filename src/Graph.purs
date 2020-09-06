module Graph where

import Prelude
import Effect (Effect)
import Effect.Console (log,logShow)
import Math(sqrt)
import Data.Semiring

data Graph = Empty | Vertex Int | Overlay Graph Graph | Connect Graph Graph

instance showGraph :: Show Graph where
    show Empty = "Empty"
    show (Vertex n) = "Vertex " <> show n
    show (Overlay g1 g2) = "(" <> show g1 <> "+" <> show g2 <> ")"
    show (Connect g1 g2) = "(" <> show g1 <> "*" <> show g2 <> ")"

instance semiringGraph :: Semiring Graph where
    add g1 g2 = Overlay g1 g2
    zero      = Empty
    mul g1 g2 = Connect g1 g2
    one       = Empty

vertices Empty = []
vertices (Vertex n) = [n]
vertices (Overlay g1 g2) = vertices g1 <> vertices g2
vertices (Connect g1 g2) = vertices g1 <> vertices g2