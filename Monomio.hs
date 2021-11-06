{-# LANGUAGE FlexibleContexts #-}

module Monomio where

data Monomio = M Float Int

crearM :: Float -> Int -> Monomio
crearM = M

obtenerExp :: Monomio -> Int
obtenerExp (M _ b) = b

obtenerCoef :: Monomio -> Float
obtenerCoef (M a _) = a

evalM :: Float -> Monomio -> Float
evalM x (M a b) = a * (x^^b)

instance Ord Monomio where
    compare (M a1 b1) (M a2 b2) | b1==b2 = compare a1 a2
                                | otherwise = compare b1 b2
instance Eq Monomio where
    m1 == m2 = compare m1 m2 == EQ
    m1 /= m2 = compare m1 m2 /= EQ 

instance Show Monomio where
    show (M a b) | a==0 = ""
                 | a==1 && b==0 = "1"
                 | a==1 = _showExp b
                 | b==0 = _showCoef a
                 | otherwise = _showCoef a ++ "*" ++ _showExp b
                 
                 where
                    _shownum x  | x<0 = "("++show x++")"
                                | otherwise = show x

                    _showCoef x | x==0 || x==1 = ""
                                | otherwise = _shownum x

                    _showExp x  | x==0 = ""
                                | x==1 = "x"
                                | otherwise = "x^" ++ _shownum x

instance Read Monomio where
    readsPrec _ xs = let
                            coeficiente = read(takeWhile (/= '*') xs) :: Float
                            exponente = read(tail $ dropWhile (/= '^') xs) :: Int
                     in
                            [(crearM coeficiente exponente,"")]