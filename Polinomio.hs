module Polinomio where

import Data.List
import Monomio

type Polinomio = [Monomio]

nulo :: Polinomio
nulo = []

grado::Polinomio->Int
grado p | p == nulo = -1
        | otherwise = (obtenerExp . maximum) p

coeficientePrincipal::Polinomio->Float
coeficientePrincipal p | p == nulo = 0.0
                       | otherwise = (obtenerCoef . maximum) p

evalP :: Float -> Polinomio -> Float
evalP x = sum . map (evalM x)

sumarMonomio :: Monomio -> Polinomio -> Polinomio
sumarMonomio m xs | obtenerCoef m == 0 = xs
                  | otherwise = sumarMonomio' m xs
                  where
                      sumarMonomio' m [] = [m]
                      sumarMonomio' m (x:xs) | obtenerExp m == obtenerExp x = let 
                                                                                nuevoCoeficiente = obtenerCoef m + obtenerCoef x   
                                                                              in
                                                                                if nuevoCoeficiente == 0.0 then xs else crearM nuevoCoeficiente (obtenerExp x) : xs
                                             | otherwise = x : sumarMonomio' m xs

toStr :: Polinomio -> String 
toStr p | p == nulo = "0"
        | otherwise = concat (mergeList (map show orderedPolinomio) listaDeMases)
                   where
                       orderedPolinomio = (reverse . sort) p
                       listaDeMases = replicate (length orderedPolinomio-1) "+"
                       mergeList::[a]->[a]->[a]
                       mergeList [] ys = ys
                       mergeList (x:xs) ys = x:mergeList ys xs

fromPairList :: [(Float, Int)] -> Polinomio
fromPairList = map (uncurry crearM)

printPolinomio :: Polinomio -> IO ()
printPolinomio p | p == nulo = putStr "Polinomio nulo"
                 | otherwise = (putStr . toStr) p

split :: Char -> String -> [String]
split p s =  case dropWhile (==p) s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break (==p) s'

readPolinomio :: String -> Polinomio
readPolinomio = map read . split '+'