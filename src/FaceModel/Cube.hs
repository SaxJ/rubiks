module FaceModel.Cube where

import Prelude hiding (Right, Left)

data Face = Front | Right | Back | Left | Up | Down deriving (Eq, Ord, Enum, Show)
data Colour = Green | Red | Orange | Blue | Yellow | White deriving (Eq, Enum, Ord, Show)

instance Show Colour where
    show c = "\x1b[" ++ (colourCode c) ++ "m\x1b[0m"
    where
        colourCode Green = "32"
        colourCode Red = "31"
        colourCode Orange = "35"
        colourCode Blue = "34"
        colourCode Yellow = "33"
        colourCode White = "37"

type Facelet = (Face, Colour)

type Cublet = [Facelet]

type Cube = [Cublet]

data Rotation = Clockwise | Anticlockwise deriving (Eq, Enum, Ord, Show)

faceletFace :: Facelet -> Face
faceletFace = fst

faceletColour :: Facelet -> Colour
faceletColour = snd

correctFacelet :: Facelet -> Face
correctFacelet = toEnum . fromEnum . faceletColour

isFaceletCorrect :: Facelet -> Bool
isFaceletCorrect f = faceletFace f == correctFacelet f

isCubeletCorrect :: Cublet -> Bool
isCubeletCorrect = all isFaceletCorrect
