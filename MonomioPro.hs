{-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

class Monomio a b where
    crearM::a->b->(a,b)
    crearM a b = (a,b)
    obtenerCoef::(a,b)->a
    obtenerCoef (a,b) = a
    obtenerExp::(a,b)->b
    obtenerExp (a,b) = b
    evalM::(Num a,Integral b)=>(a,b)->a->a
    evalM (a,b) x = a*(x^b)

x = crearM 2.0 3
main = do print(1)