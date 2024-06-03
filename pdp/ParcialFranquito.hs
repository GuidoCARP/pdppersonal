data Fremen = UnFremen{
    nombre:: String,
    tolerancia:: Float,
    titulos:: [String],
    reconocimientos:: Int} deriving (Show,Ord,Eq)

stilgar :: Fremen
stilgar = UnFremen "stilgar" 150 ["Guia"] 101

otroFremen :: Fremen
otroFremen= UnFremen "otroFremen" 2 ["Guia","Domador"] 5 

tribu :: [Fremen]
tribu=[stilgar,otroFremen]

nuevoReconocimiento :: Fremen -> Fremen
nuevoReconocimiento fremen = fremen { reconocimientos=reconocimientos fremen+1 } 

candidato :: [Fremen] -> Bool
candidato  = any (\fremen -> "Domador" `elem` titulos fremen && tolerancia fremen > 100)

{-Hallar al Elegido: Es el Fremen de la tribu que más reconocimientos 
tenga entre los candidatos a serlo. -}

elgido:: [Fremen] -> Fremen
elgido fremen = maximum reconocimientos 

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

{-Domar gusano de arena: Un Fremen puede domar a un gusano de arena si su
 nivel de tolerancia a la Especia es al menos la mitad de la longitud del gusano. 
 Al hacerlo, obtiene el título de "Domador" y su tolerancia a la especia aumenta en 100 unidades. 
Si no lo puede hacer su tolerancia a la Especia baja un 10%.-}


domarGusano :: Fremen -> Gusano -> Fremen
domarGusano fremen gusano
    | tolerancia fremen >= longitud gusano * 0.5 = fremen {titulos = "Domador" : titulos fremen, tolerancia = tolerancia fremen + 100}
    | otherwise = fremen {tolerancia = tolerancia fremen - tolerancia fremen * 0.1}

{-Destruir gusano de arena: Un Fremen puede destruir a un gusano de arena si tiene el
 título de "Domador" y si su nivel de tolerancia a la Especia es menor que
  la mitad de la longitud del gusano. Al hacerlo, recibe un 
reconocimiento y su tolerancia a la especia aumenta en 100 unidades. 
Si no lo logra, su especia baja un 20%. -}

destruirGusano :: Fremen -> Gusano -> Fremen
destruirGusano fremen gusano
    | "Domador" `elem` titulos fremen && tolerancia fremen < longitud gusano * 0.5 = fremen {reconocimientos = reconocimientos fremen + 1, tolerancia = tolerancia fremen + 100}
    | otherwise = fremen {tolerancia= tolerancia fremen - tolerancia fremen * 0.2}

{-Inventada: Inventar otra misión que un Fremen pueda hacer con un gusano,
 que también se pueda realizar dependiendo de cómo sea el 
gusano en relación al Fremen y que provoque consecuencias diferentes sobre el 
Fremen si lo logra o no.-}

misionInventada :: Fremen -> Gusano -> Fremen
misionInventada fremen gusano
    | reconocimientos fremen >= 100 = nuevoReconocimiento fremen 
    | reconocimientos fremen < 100 && length (descripcion gusano) < length (nombre fremen) = fremen {tolerancia = tolerancia fremen + 50}
    | otherwise = fremen {tolerancia= tolerancia fremen - tolerancia fremen * 0.8}




{-Simular la realización colectiva de una misión: Dada una tribu, una misión cualquiera 
y un gusano de arena, hacer que cada uno de los Fremen de la tribu intenten llevar a cabo
 la misión con dicho gusano, obteniendo cómo queda la tribu en consecuencia.
 
Averiguar, para una tribu, una misión y un gusano, si el hecho de realizarla colectivamente
 haría que el elegido de la tribu fuera un Fremen diferente al que hubieran elegido 
 previamente.
-}

type Mision = Fremen -> Gusano -> Fremen
type Tribu = [Fremen]
realizarMisionColectiva ::  Tribu -> Mision -> Gusano -> Tribu
realizarMisionColectiva tribu mision gusano = map (\fremen -> mision fremen gusano) tribu


--elegido:: Tribu -> Fremen
--elegido = maximum 

--cambiaElegido :: Tribu -> Mision -> Gusano -> Bool
--cambiaElegido tribu mision gusano = elegido tribu /= elegido (realizarMisionColectiva tribu mision gusano)