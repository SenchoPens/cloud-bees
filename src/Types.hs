module Types where

import CRDT.Cv.GSet (GSet)

type Flower = (Int, Int)  -- тип цветок - координаты цветка в декартовой системе координат

type Flowers = GSet Flower  -- Grow-Only Set цветков

type FlowerList = [Flower]  -- список цветков из GSetа цветков (для REPL)
