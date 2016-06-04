module Tile exposing (..)

--      A   B
--   H         C
--
--   G         D
--      F   E
type Coordinate = A | B | C | D | E | F | G | H

type Path = (Coordinate, Coordinate)

type alias Tile = List Path

