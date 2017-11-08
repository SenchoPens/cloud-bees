module Types where

import CRDT.Cv.GSet (GSet)

type Flower = (Int, Int)

type Flowers = GSet Flower

type FlowerList = [Flower]
