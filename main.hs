import Data.Char

printMenu = do
				putStrLn("Menu")
				putStrLn("a) Ingresar Polinomio")
				putStrLn("b) Grado del Polinomio")
				putStrLn("c) Sumar monomio al polinomio dado")
				putStrLn("d) Obtener coeficiente principal")
				putStrLn("e) Evaluar Polinomio en x")
				putStrLn("f) Salir")
				putStrLn("Opcion a seleccionar: ")

-- Ingresar Polinomio 
-- Grado del Polinomio
-- Sumar monomio al polinomio dado
-- Obtener coeficiente principal
-- Evaluar Polinomio en x


main = do 
			printMenu
			x <- getChar
			getChar
			if x == 'a' then
				print("a")
			else
				print("c")
			if x/='f' then
				main
			else
				return ()
			
