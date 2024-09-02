{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-incomplete-patterns #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Jet.Demo.Drawer.Logic where

import Coalmine.Prelude

data Cmd
  = ResizeCmd Int Int
  | DrawMenuCmd [Text]
  | DrawProgressCmd Rational

data Event
  = CellChangedEvent Int Int Char

data Model = Model
  { width :: Int,
    height :: Int,
    filledContent :: Map (Int, Int) Char
  }

transition :: Cmd -> Model -> ([Event], Model)
transition cmd _state = case cmd of
  ResizeCmd _widthDelta _heightDelta ->
    error "TODO"
