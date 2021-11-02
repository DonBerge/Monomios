{-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.List
import MonomioPro
import RBT

class Polinomio t a b where
    nulo:: t (Monomio a b)
    grado::t (Monomio a b) -> b
    coeficientePrincipal::t (Monomio a b)->a
    evalP:: a -> t (Monomio a b) -> a
    sumarMonomio:: Monomio a b -> t (Monomio a b) -> t (Monomio a b)
    toStr::t (Monomio a b)->String
    _maximoMonomio::(Ord a, Ord b)=>t (Monomio a b) -> Monomio a b

newtype Lista x = L [x]

mergeList [] ys = ys
mergeList (x:xs) ys = x:mergeList ys xs

instance (Show a,Show b,Floating a,Integral b,Ord a,Ord b)=>Polinomio Lista a b where
    nulo = L []
    _maximoMonomio (L xs) = maximum xs
    grado = obtenerExp . _maximoMonomio
    coeficientePrincipal = obtenerCoef . _maximoMonomio
    evalP x (L xs) = sum (map (evalM x) xs)
    sumarMonomio x (L xs) | obtenerCoef x == 0 = L xs
                          | otherwise = L (x:xs)
    toStr (L xs) = concat (mergeList (map show orderedPolinomio) listaDeMases)
                   where
                       orderedPolinomio = (reverse . sort) xs
                       listaDeMases = replicate (length orderedPolinomio-1) "+"


a = crearM 2.0 3

p :: Lista (Monomio Double Integer)
p = nulo

main = do print (grado p)