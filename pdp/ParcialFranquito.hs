data Fremen = UnFremen{
    nombre:: String,
    tolerancia:: Float,
    titulos:: [String],
    reconocimientos:: Int} deriving (Show,Ord,Eq)

stilgar :: Fremen
stilgar = UnFremen "stilgar" 150 ["Guia"] 101

otroFremen :: Fremen
otroFremen= UnFremen "otroFremen" 150 ["Guia","Domador"] 5 

tribu :: [Fremen]
tribu=[stilgar,otroFremen]

nuevoReconocimiento :: Fremen -> Fremen
nuevoReconocimiento fremen = fremen { reconocimientos=reconocimientos fremen+1 } 

unCandidato :: Fremen -> Bool
unCandidato fremen = elem "Domador" (titulos fremen) && tolerancia fremen > 100

hayElegido :: [Fremen] -> Bool
hayElegido = any unCandidato

posiblesElegidos :: [Fremen] -> [Fremen]
posiblesElegidos = filter unCandidato

obtenerMayorDeLista :: [Fremen] -> Fremen
obtenerMayorDeLista [a] = a
obtenerMayorDeLista (x:y:xs)
    | reconocimientos x > reconocimientos y = obtenerMayorDeLista (x:xs)
    | otherwise = obtenerMayorDeLista (y:xs)

hallarElegido :: [Fremen] -> Fremen
hallarElegido tribu = obtenerMayorDeLista (posiblesElegidos tribu)  
data Gusano = UnGusano{
    longitud::Float,
    hidratacion::Int,
    descripcion::String} deriving Show

gusano1 :: Gusano
gusano1 = UnGusano 10 5 "rojo con lunares"

gusano2 :: Gusano
gusano2 = UnGusano 8 1 "dientes puntiagudos"

grupoGusanos1 :: [Gusano]
grupoGusanos1 = [gusano1,gusano2]

grupoGusanos2 :: [Gusano]
grupoGusanos2 = [gusano2,gusano1]

longitudMaxima :: Gusano -> Gusano -> Float
longitudMaxima (UnGusano longitud1 _ _) (UnGusano longitud2 _ _) = 0.10 * max longitud1 longitud2

descripcionConcatenada :: Gusano -> Gusano -> String
descripcionConcatenada (UnGusano _ _ descripcion1) (UnGusano _ _ descripcion2) = descripcion1 ++ ", " ++ descripcion2

longitudCria :: Gusano -> Gusano -> Gusano
longitudCria gusano1 gusano2 = UnGusano (longitudMaxima gusano1 gusano2) 0 (descripcionConcatenada gusano1 gusano2)

crias :: [Gusano] -> [Gusano] -> [Gusano]
crias [] _ = []
crias _ [] = []
crias (gusano1:gusanos1) (gusano2:gusanos2) = longitudCria gusano1 gusano2 : crias gusanos1 gusanos2

domarGusano :: Fremen -> Gusano -> Fremen
domarGusano fremen gusano
    | tolerancia fremen >= longitud gusano * 0.5 = fremen {titulos = "Domador" : titulos fremen, tolerancia = tolerancia fremen + 100}
    | otherwise = fremen {tolerancia = tolerancia fremen - tolerancia fremen * 0.1}

destruirGusano :: Fremen -> Gusano -> Fremen
destruirGusano fremen gusano
    | "Domador" `elem` titulos fremen && tolerancia fremen < longitud gusano * 0.5 = fremen {reconocimientos = reconocimientos fremen + 1, tolerancia = tolerancia fremen + 100}
    | otherwise = fremen {tolerancia= tolerancia fremen - tolerancia fremen * 0.2}

misionInventada :: Fremen -> Gusano -> Fremen
misionInventada fremen gusano
    | reconocimientos fremen >= 100 = nuevoReconocimiento fremen 
    | reconocimientos fremen < 100 && length (descripcion gusano) < length (nombre fremen) = fremen {tolerancia = tolerancia fremen + 50}
    | otherwise = fremen {tolerancia= tolerancia fremen - tolerancia fremen * 0.8}


type Mision = Fremen -> Gusano -> Fremen
type Tribu = [Fremen]

realizarMisionColectiva ::  Tribu -> Mision -> Gusano -> Tribu
realizarMisionColectiva tribu mision gusano = map (\fremen -> mision fremen gusano) tribu

mismoElegido :: Tribu -> Mision -> Gusano -> Bool  
mismoElegido tribu mision gusano = reconocimientos (hallarElegido tribu) == reconocimientos (hallarElegido (realizarMisionColectiva tribu mision gusano))

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

