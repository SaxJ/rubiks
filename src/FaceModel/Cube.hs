module FaceModel.Cube where

import Prelude hiding (Right, Left)

data Face = Front | Right | Back | Left | Up | Down deriving (Eq, Ord, Enum, Show)
data Colour = Green | Red | Orange | Blue | Yellow | White deriving (Eq, Enum, Ord)

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

type Transform = [Face]

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

isCorner :: Cublet -> Bool
isCorner c = length c == 3

isEdge :: Cublet -> Bool
isEdge c = length c == 2

cubletFaces :: Cublet -> [Face]
cubletFaces = map fst

cubletHasFace :: Cublet -> Face -> Bool
cubletHasFace cublet face = elem face $ cubletFaces cublet

{-|
Turning this face clockwise will move the facelets of this face around this pattern.
For example, moving the front face clockwise will transform facelets like so: up -> right, right -> down, down -> left, left -> up
-}
getClockwiseTransform :: Face -> Transform
getClockwiseTransform face = case face of
    Front -> frontBackAxis
    Back -> reverse frontBackAxis
    Right -> rightLeftAxis
    Left -> reverse rightLeftAxis
    Up -> upDownAxis
    Down -> reverse upDownAxis
    where
        frontBackAxis = [Up, Right, Down, Left]
        rightLeftAxis = [Front, Up, Back, Down]
        upDownAxis = [Front, Right, Back, Left]