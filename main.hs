import Polinomio
import System.IO

printMenu = do
                putStrLn "Menu"
                putStrLn "a) Ingresar Polinomio"
                putStrLn "b) Grado del Polinomio"
                putStrLn "c) Sumar monomio al polinomio dado"
                putStrLn "d) Obtener coeficiente principal"
                putStrLn "e) Evaluar Polinomio en x"
                putStrLn "f) Salir"
                putStrLn "Opcion a seleccionar: "

-- Ingresar Polinomio 
-- Grado del Polinomio
-- Sumar monomio al polinomio dado
-- Obtener coeficiente principal
-- Evaluar Polinomio en x

clear = putStr "\ESC[2J"

wait = do
            putStr "Pulsa enter para continuar..."
            hFlush stdout
            getChar 

preguntar p = do
                clear
                putStr "Polinomio actual: "
                printPolinomio  p
                putChar '\n'
                printMenu
                opcion <- getChar
                getChar
                if opcion == 'a' then
                    do
                        putStr "Ingrese un polinomio de la forma \"2*x^3+4*x^5+(-2)*x^2\": "
                        hFlush stdout
                        str <- getLine 
                        putChar '\n'
                        preguntar (readPolinomio str)

                else if opcion == 'b' then
                    do
                        putStr "El grado del polinomio es: "
                        print(grado p)
                        wait
                        preguntar p

                else if opcion == 'c' then
                    do
                        putStr "Ingrese un monomio de la forma \"a*x^b\": "
                        hFlush stdout
                        str <- getLine
                        putChar '\n'
                        preguntar (sumarMonomio (read str) p)

                else if opcion == 'd' then
                    do
                        putStr "El coeficiente principal del polinomio es: "
                        print(coeficientePrincipal p)
                        wait
                        preguntar p

                else if opcion == 'e' then
                    do
                        putStr "Ingrese un numero para evaluar al polinomio: "
                        hFlush stdout
                        num <- getLine
                        putChar '\n'
                        putStr ("Evaluar " ++ num ++ " en " ++ toStr p ++ " da como resultado ") 
                        print (evalP (read num) p)
                        wait
                        preguntar p

                else if opcion == 'f' then
                    return ()

                else
                    do
                        putStrLn "Opcion invalida"
                        preguntar p
                
                return ()


main = do
        hSetBuffering stdin NoBuffering
        preguntar nulo
