{-# LANGUAGE DeriveFunctor, FlexibleInstances, UndecidableInstances, TupleSections #-}
import Control.Comonad
import qualified Data.Array as A
import qualified  Data.FixedList as FL
import Data.Ix

instance Ix a => Ix (FL.Nil a) where
    range (FL.Nil, FL.Nil) = [FL.Nil]
    index (FL.Nil, FL.Nil) FL.Nil = 0
    inRange _ _ = True
    rangeSize _ = 1

instance (Ix a, Ix (f a), FL.FixedList f) => Ix (FL.Cons f a) where
    range (fllb, flhb) = let lb = FL.head fllb;
                             hb = FL.head flhb;
                             rn = range (lb, hb);
                             fllb' = FL.tail fllb;
                             flhb' = FL.tail flhb
                             rn' = range (fllb', flhb')
                         in concatMap (\e -> map (\es -> e FL.:. es) rn') rn
    index (fllb, flhb) el = let lb = FL.head fllb;
                                hb = FL.head flhb;
                                ci  = index (lb, hb) $ FL.head el;
                                fllb' = FL.tail fllb;
                                flhb' = FL.tail flhb;
                                el' = FL.tail el;
                                rs = rangeSize (fllb', flhb');
                                i = index (fllb', flhb') el'
                            in ci*rs + i
    inRange (fllb, flhb) el = let lb = FL.head fllb;
                                  hb = FL.head flhb;
                                  e  = FL.head el;
                                  fllb' = FL.tail fllb;
                                  flhb' = FL.tail flhb
                                  el' = FL.tail el
                              in (inRange (lb, hb) e) &&
                                     (inRange (fllb', flhb') el')
                                     
data U i a = U i (A.Array i a)
             deriving (Functor, Show)
data Dir = Neg | Neut | Pos
           deriving (Show, Eq)

instance Enum Dir where
    toEnum (-1) = Neg
    toEnum 0 = Neut
    toEnum 1 = Pos
    fromEnum Neg = -1
    fromEnum Neut = 0
    fromEnum Pos = 1

-- Not very efficient...
toIntegral :: Integral a => Int -> a
toIntegral n = case signum n of
                 1 -> sum $ replicate n 1
                 0 -> 0
                 -1 -> sum $ replicate (-n) (-1) 
                    

class Move nc where
    neighbourIx :: (Integral i, Ix i, Ord i) => nc i -> (nc i, nc i) -> nc Dir -> Int -> nc i

instance Move (FL.Nil) where
    neighbourIx FL.Nil (FL.Nil, FL.Nil) FL.Nil _ = FL.Nil

instance (FL.FixedList fl, Move fl) => Move (FL.Cons fl) where
    neighbourIx (i FL.:. ifl') (lb FL.:. lbfl', ub FL.:. ubfl') (d FL.:. dfl') n =
        let i' = i + (toIntegral . (*n) . fromEnum $ d);
            si = case (compare i' lb, compare i' ub) of
                   (LT, LT) -> i' + ub - lb + 1
                   (GT, GT) -> i' - ub + lb - 1
                   (_, _) -> i'
        in si FL.:. (neighbourIx ifl' (lbfl', ubfl') dfl' n)

instance Enum (FL.Nil Dir) where
    toEnum 0 = FL.Nil
    fromEnum FL.Nil = 0

instance (FL.FixedList fl, Enum (fl Dir)) => Enum (FL.Cons fl Dir) where
    toEnum i = (toEnum ((i `mod` 3) - 1)) FL.:. (toEnum (i `div` 3))
    fromEnum (d FL.:. ds) = (1 + (fromEnum d)) + (3 * ((fromEnum ds)))

class Neutral n where
    neut :: n

instance Neutral (FL.Nil Dir) where
    neut = FL.Nil

instance (FL.FixedList fl, Neutral (fl Dir)) => Neutral (FL.Cons fl Dir) where
    neut = Neut FL.:. neut

instance Bounded (FL.Nil Dir) where
    maxBound = FL.Nil
    minBound = FL.Nil

instance (FL.FixedList fl, Bounded (fl Dir)) => Bounded (FL.Cons fl Dir) where
    maxBound = Pos FL.:. maxBound
    minBound = Neg FL.:. minBound

data Neighbourhood = Moore | VonNeumann | VonNeumannExt deriving (Bounded, Enum, Eq, Show)

class VonNeumannMoves fl where
    vonNeumannMove :: Int -> [fl Dir]

instance VonNeumannMoves FL.Nil where
    vonNeumannMove _ = undefined

instance (FL.FixedList fl, VonNeumannMoves fl, Neutral (fl Dir)) => VonNeumannMoves (FL.Cons fl) where
    vonNeumannMove 0 = [Neg FL.:. neut, Pos FL.:. neut]
    vonNeumannMove n = map (Neut FL.:.) (vonNeumannMove (n-1))
                   
class EuclidianNeighbours fl where
    neighboursMoore :: (Integral i, Ix i) => U (fl i) a -> [U (fl i) a]
    neighboursVonNeumann :: (Integral i, Ix i) => U (fl i) a -> [U (fl i) a]
    neighboursVonNeumannExt :: (Integral i, Ix i) => U (fl i) a -> [U (fl i) a] 
    neighbours :: (Integral i, Ix i) => Neighbourhood -> U (fl i) a -> [U (fl i) a]
    neighbours Moore = neighboursMoore
    neighbours VonNeumann = neighboursVonNeumann
    neighbours VonNeumannExt = neighboursVonNeumannExt

instance EuclidianNeighbours FL.Nil where
    neighboursMoore (U fli arr) = []
    neighboursVonNeumann (U fli arr) = []
    neighboursVonNeumannExt (U fli arr) = []

instance (FL.FixedList fl, EuclidianNeighbours fl, Move fl, Eq (fl Dir),
            Neutral (fl Dir), Enum (fl Dir), Bounded (fl Dir), VonNeumannMoves fl) =>
    EuclidianNeighbours (FL.Cons fl) where
    neighboursMoore (U fli arr) = let dirs = filter (/=neut) [minBound..maxBound]
                                  in map (\dir -> U (neighbourIx fli (A.bounds arr) dir 1) arr) dirs
    neighboursVonNeumann (U fli arr) = let dirs = concatMap (vonNeumannMove) [0..((FL.length fli) - 1)]
                                       in map (\dir -> U (neighbourIx fli (A.bounds arr) dir 1) arr) dirs
    neighboursVonNeumannExt (U fli arr) = let dirs = concatMap (vonNeumannMove) [0..((FL.length fli) - 1)]
                                          in map (\dir -> U (neighbourIx fli (A.bounds arr) dir 2) arr) dirs

instance Ix i => Comonad (U i) where
    extract   (U i a) = a A.! i
    duplicate (U i a) = U i $ A.listArray (A.bounds a) (flip U a <$> (A.range $ A.bounds a))
    extend f u        = fmap f $ duplicate u
                                             
numNeighbours :: (Eq a, EuclidianNeighbours fl, Ix i, Integral i, Ix (fl i)) => Neighbourhood -> U (fl i) a -> a -> Int
numNeighbours nb u e = length . filter (==e) . fmap extract . neighbours nb $ u 

                                             
type P1D = FL.Cons FL.Nil Int
type P2D = FL.Cons (FL.Cons FL.Nil) Int
type P3D = FL.Cons (FL.Cons (FL.Cons FL.Nil)) Int
                                     
neighboursMoore1D :: U P1D a -> [U P1D a]
neighboursMoore1D = neighboursMoore

neighboursMoore2D :: U P2D a -> [U P2D a]
neighboursMoore2D = neighboursMoore

neighboursMoore3D :: U P3D a -> [U P3D a]
neighboursMoore3D = neighboursMoore

toP1D :: Int -> P1D
toP1D = flip (FL.:.) FL.Nil

toP2D :: (Int, Int) -> P2D
toP2D (x, y) = x FL.:. y FL.:. FL.Nil

toP3D :: (Int, Int, Int) -> P3D
toP3D (x, y, z) = x FL.:. y FL.:. z FL.:. FL.Nil
               
arr1D = A.array (toP1D 0, toP1D 2) [(toP1D 0, True), (toP1D 1, False), (toP1D 2, True)]
test1D = U (toP1D 1) arr1D

arr2D = A.array (toP2D (0,0), toP2D (2,2)) (map ((,True) . toP2D) [(i,j)| i<-[0..2], j <- [0..2]])
test2D = U (toP2D (1,1)) arr2D

arr3D = A.array (toP3D (0,0,0), toP3D (2,2,2)) (map ((,True) . toP3D) [(i,j,k)| i<-[0..2], j <- [0..2], k <- [0..2]])
test3D = U (toP3D (1,1,1)) arr3D
