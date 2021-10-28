import System.IO
import Data.List
import Monomio

data Polinomio a b = N | P (Monomio a b) (Polinomio a b)

nulo = N

_maximoMonomio N = crearM 0 0
_maximoMonomio (P x xs) | obtenerExp x < obtenerExp m = x
                      | otherwise = m 
                          where m = _maximoMonomio xs

grado = obtenerExp . _maximoMonomio

coeficientePrincipal = obtenerCoef . _maximoMonomio

evalP N _ = 0
evalP (P x xs) val = evalM x val + evalP xs val



sumarMonomio m p | obtenerCoef m == 0 = p
                 | otherwise = sumarMonomio' m p
                 where 
                    sumarMonomio' m N = P m N
                    sumarMonomio' m (P x xs) | obtenerExp x == obtenerExp m = P (crearM (obtenerCoef x + obtenerCoef m) (obtenerExp x)) xs
                                            | otherwise = P x (sumarMonomio' m xs)

fromTupleList [] = N
fromTupleList ((a,b):xs) = sumarMonomio (crearM a b) (fromTupleList xs)

p = fromTupleList [(1,2),(2,1),(1,0)]

instance (Show a,Show b,Ord a,Ord b,Num a,Num b) => Show (Polinomio a b) where
    show N = ""
    show (P x N) = show x
    show (P x xs) = show x ++ "+" ++ show xs 

main = do print p