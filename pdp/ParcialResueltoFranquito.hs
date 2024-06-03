data Fremen = UnFremen {
    nombre :: String,
    toleranciaEspecia :: Float,
    titulos :: [String],
    cantReconocientos :: Int
} deriving (Show)


nuevoReconocimiento :: Fremen -> Fremen
nuevoReconocimiento fremen = fremen {cantReconocientos = cantReconocientos fremen + 1}

algunCandidato :: Fremen -> Bool
algunCandidato fremen = elem "Domador" (titulos fremen) && toleranciaEspecia fremen > 100

algunCandidatoLista :: [Fremen] -> Bool
algunCandidatoLista lista = any algunCandidato lista

candidatosAElegidos :: [Fremen] -> [Fremen]
candidatosAElegidos fremen = filter algunCandidato fremen


obtenerMayorDeLista :: [Fremen] -> Fremen
obtenerMayorDeLista [a] = a
obtenerMayorDeLista (x:y:xs)
    | cantReconocientos x > cantReconocientos y = obtenerMayorDeLista (x:xs)
    | otherwise = obtenerMayorDeLista (y:xs)


hallarElegido :: [Fremen] -> Fremen
hallarElegido tribu = obtenerMayorDeLista (candidatosAElegidos tribu)  


---------------------------------------------------------------------------------------

data GusanoDeArena = UnGusano {
    longitud :: Float,
    hidratacion :: Int,
    descripcion :: String
} deriving (Show)


procrear :: GusanoDeArena -> GusanoDeArena -> GusanoDeArena
procrear gusano1 gusano2 = UnGusano ((max (longitud gusano1) (longitud gusano2)) * 0.1) 0 (descripcion gusano1 ++ " y " ++ descripcion gusano2)   


procrearLista :: [GusanoDeArena] -> [GusanoDeArena] -> [GusanoDeArena]
procrearLista [a] [] = []
procrearLista [] [b] = []
procrearLista (x:xs) (y:ys) = procrear x y : procrearLista xs ys

---------------------------------------------------------------------------------------

type Mision = Fremen -> GusanoDeArena -> Fremen

domarGusano :: Mision
domarGusano fremen gusano 
    |toleranciaEspecia fremen >= (longitud gusano / 2) = UnFremen (nombre fremen) (toleranciaEspecia fremen + 100) (titulos fremen ++ ["Domador"]) (cantReconocientos fremen+2) 
    |otherwise = fremen{toleranciaEspecia = toleranciaEspecia fremen * 0.9}


destruirGusano :: Mision
destruirGusano fremen gus
    |(elem "Domador" (titulos fremen)) && (toleranciaEspecia fremen < (longitud gus /2)) = UnFremen (nombre fremen) (toleranciaEspecia fremen + 100) (titulos fremen) (cantReconocientos fremen + 1)
    |otherwise = fremen{toleranciaEspecia = toleranciaEspecia fremen * 0.8}

--Mision inventada
--La longitud del gusano jefe tiene que ser mayor a 500
--El fremen tiene que tener los titulos de Domador y Guerrero y al derrotar el gusano jefe
--este se vuelve "guerrero distinguido"

sacarDeLista :: Eq a => a -> [a] -> [a]
sacarDeLista a [] = []
sacarDeLista a (x:xs)
    |a == x = xs
    |otherwise = x : sacarDeLista a xs 

derrotarGusanoJefe :: Mision
derrotarGusanoJefe fremen gusanoJefe
    |(elem "Domador" (titulos fremen)) && (elem "Guerrero" (titulos fremen)) &&  (longitud gusanoJefe > 500) && toleranciaEspecia fremen > (longitud gusanoJefe) = UnFremen (nombre fremen) (toleranciaEspecia fremen + 170) ((sacarDeLista "Guerrero" (titulos fremen)) ++ ["Guerrero distinguido"]) (cantReconocientos fremen + 1)
    |otherwise = fremen{toleranciaEspecia = toleranciaEspecia fremen * 0.7}


type Tribu = [Fremen]

realizacionColectiva :: Tribu -> Mision -> GusanoDeArena -> Tribu
realizacionColectiva tribu mision gusano = map (\fremen -> mision fremen gusano) tribu

--tambien se puede realizar con recursion
-- realizacionColectiva [] _ _ = []
-- realizacionColectiva (x:xs) mision gusano = mision x gusano : realizacionColectiva xs mision gusano 



mismoElegido :: Tribu -> Mision -> GusanoDeArena -> Bool  
mismoElegido tribu mision gusano = cantReconocientos (hallarElegido tribu) == cantReconocientos (hallarElegido (realizacionColectiva tribu mision gusano))

--como criterio de comparacion se usa "cantReconocimientos" porque no se 
--pueden comparar Fremens directamente

---------------------------------------------------------------------------------------

--Punto 4
--a)Qué pasaría con una tribu de infinitos Fremen al realizar una mision colectiva?
--Al realizar una mision con una lista infinita, Haskell no podrá terminar de realizar la operacion
--ya que como la lista es infinita, debe reccorrer todos los elementos para tener un resultado final

--ejemplo: ghci> realizacionColectiva (cycle tribu1) domarGusano gusano1
--[UnFremen {nombre = "Stilgar", toleranciaEspecia = 250.0, titulos = ["Guia","Domador","Domador"], cantReconocientos = 3},
--UnFremen {nombre = "Pedro", toleranciaEspecia = 1100.0, titulos = ["Domador","Guia","Guerrero","Domador"], cantReconocientos = 2},
--UnFremen {nombre = "Stilgar", toleranciaEspecia = 250.0, titulos = ["Guia","Domador","Domador"], cantReconocientos = 3},
--UnFremen {nombre = "Pedro", toleranciaEspecia = 1100.0, titulos = ["Domador","Guia","Guerrero","Domador"], cantReconocientos = 2}, ... y sigue infinitamente


--b)Qué pasaría con una tribu de infinitos Fremen al querer saber si hay algún candidato a ser elegido?
--Debido a que haskell es peresozo, al realizar la operacion any algunCandidato lista, Haskell no necesita recorrer toda la lista
--porque al encontrar un candidato que ya es true, este devuelve True y no sigue recorriendo la lista

--ejemplo: ghci> algunCandidatoLista (cycle tribu1)
--               True

--c)Qué pasaría con una tribu de infinitos Fremen al encontrar al elegido?
--Al igual que en el punto a, Haskell no podrá terminar de realizar la operacion ya que debería recorrer todos los elementos
--de la lista e ir comparando la cantidad de reconocimientos de cada uno para saber cual es el elegido.

--ejemplo: ghci> hallarElegido (cycle tribu1)
--               ... (no devuelve nada porque no termina de realizar la operacion)



stilgar = UnFremen "Stilgar" 150 ["Guia","Domador"] 3
pedro = UnFremen "Pedro" 1000 ["Domador","Guia","Guerrero"] 2   


gusano1 = UnGusano 10 5 "rojo con lunares"
gusano2 = UnGusano 8 1000 "dientes puntiagudos"
gusano3 = UnGusano 15 10 "verde con manchas"
gusano4 = UnGusano 20 15 "azul con rayas"
gusano5 = UnGusano 5 2 "amarillo con puntos"

listaGusano1 = [gusano1,gusano2]
listaGusano2 = [gusano3,gusano4,gusano5]

tribu1 = [stilgar,pedro]