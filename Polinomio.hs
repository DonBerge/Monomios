module Polinomio where

import Data.List
import Monomio

type Polinomio = [Monomio]

nulo :: Polinomio
nulo = []

grado::Polinomio->Int
grado = obtenerExp . maximum

coeficientePrincipal::Polinomio->Float
coeficientePrincipal [] = 0.0
coeficientePrincipal xs = (obtenerCoef . maximum) xs

evalP :: Float -> Polinomio -> Float
evalP x = sum . map (evalM x)

sumarMonomio :: Monomio -> Polinomio -> Polinomio
sumarMonomio m xs | obtenerCoef m == 0 = xs
                  | otherwise = sumarMonomio' m xs
                  where
                      sumarMonomio' m [] = [m]
                      sumarMonomio' m (x:xs) | obtenerCoef m == obtenerCoef x = crearM (obtenerCoef m + obtenerCoef x) (obtenerExp x) : xs
                                             | otherwise = x : sumarMonomio' m xs

toStr :: Polinomio -> String 
toStr xs | null xs = "0"
         | otherwise = concat (mergeList (map show orderedPolinomio) listaDeMases)
                   where
                       orderedPolinomio = (reverse . sort) xs
                       listaDeMases = replicate (length orderedPolinomio-1) "+"
                       mergeList [] ys = ys
                       mergeList (x:xs) ys = x:mergeList ys xs

fromPairList :: [(Float, Int)] -> Polinomio
fromPairList = map (uncurry crearM)

printPolinomio :: Polinomio -> IO ()
printPolinomio = print . toStr

split :: Char -> String -> [String]
split p s =  case dropWhile (==p) s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break (==p) s'

readPolinomio :: String -> Polinomio
readPolinomio = map read . split '+'