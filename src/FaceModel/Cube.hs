module FaceModel.Cube where

import Prelude hiding (Right, Left)
import Data.List

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

-- PRINT HELPERS ------------------------------
viewFace :: Face -> Cube -> [Colour]
viewFace face cube = map snd $ filter (\(f,_) -> f == face) $ concat cube
-- --------------------------------------------

-- ACCESSORS
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

cubletHasFace :: Face -> Cublet -> Bool
cubletHasFace face cublet = elem face $ cubletFaces cublet

-- CONVERSION
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n ls = take (length ls) $ drop n $ cycle ls

rotateCublet :: Cublet -> Cublet
rotateCublet c = zip fs cs where
    fs = rotate 1 $ map fst c
    cs = map snd c

cubletToOrientation :: Cublet -> Int
cubletToOrientation c
    | isCorner c = cornerOrientation c
    | isEdge c = edgeOrientation c
    | otherwise = 0
    where
        cornerOrientation c
            | isCubeletCorrect c = 0
            | otherwise = if isCubeletCorrect $ rotateCublet c then 1 else -1
        edgeOrientation c = if isCubeletCorrect c then 0 else 1

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

nextInTransform :: Face -> Transform -> Face
nextInTransform f fs = let currentIndex = elemIndex f fs
    in case currentIndex of
        Just idx -> fs !! (idx + 1)
        Nothing -> f

transformCublet :: Transform -> Cublet -> Cublet
transformCublet t = map (\(f,c) -> (nextInTransform f t, c))

rotateFace :: Rotation -> Face -> Cube -> Cube
rotateFace rot face cube = moved ++ notOnFace
    where
        (onFace, notOnFace) = partition (cubletHasFace face) cube
        transform = if rot == Clockwise then getClockwiseTransform face else reverse $ getClockwiseTransform face
        moved = map (transformCublet transform) onFace


