module FaceModel.Cube where

import Prelude hiding (Right, Left)

data Face = Front | Right | Back | Left | Up | Down deriving (Eq, Ord, Enum, Show)
data Colour = Green | Red | Orange | Blue | Yellow | White deriving (Eq, Enum, Ord, Show)

type Facelet = (Face, Color)

type Cublet = [Facelet]

type Cube = [Cublet]

data Rotation = Clockwise | Anticlockwise deriving (Eq, Enum, Ord, Show)
