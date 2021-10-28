module Monomio
(
    Monomio,
    crearM,
    obtenerExp,
    obtenerCoef,
    evalM
) where

data Monomio a b = M a b

crearM = M
obtenerExp (M _ b) = b
obtenerCoef (M a _) = a

evalM (M a b) x = a*(x^b)

_shownum x | x<0 = "("++show x++")"
           | otherwise = show x

_showCoef x | x==0 || x==1 = ""
            | otherwise = _shownum x

_showExp x | x==0 = ""
           | x==1 = "x"
           | otherwise = "x^" ++ _shownum x

instance (Eq a,Eq b) => Eq (Monomio a b) where
    (M _ b1) == (M _ b2) = b1 == b2
    (M _ b1) /= (M _ b2) = b1 /= b2

instance (Ord a,Ord b) => Ord (Monomio a b) where
    compare (M _ b1) (M _ b2) | b1 < b2 = LT
                              | b1 > b2 = GT
                              | otherwise = EQ

instance (Show a,Show b,Ord a,Ord b,Num a,Num b) => Show (Monomio a b) where
    show (M a b) | a==0 = ""
                 | a==1 && b==0 = "1"
                 | a==1 = _showExp b
                 | b==0 = _showCoef a
                 | otherwise = _showCoef a ++ "*" ++ _showExp b