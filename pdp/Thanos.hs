data Guante=UnGuante{
    material::String,
    gemas:: Gemas}

type Gemas=[Gema]

guanteleteDeGoma :: Guante
guanteleteDeGoma=UnGuante "uru" [tiempo,alma "usar Mjolnir",gemaLoca (alma "programación en Haskell")]

data Personaje= UnPersonaje{
    edad::Int,
    energia::Int,
    habilidades::[String],
    nombre::String,
    planeta::String
} deriving(Show)

guanteApto::Guante->Bool
guanteApto guante = length(gemas guante) == 6 && material guante == "uru"

type Universo=[Personaje]

ironMan::Personaje
ironMan=UnPersonaje 48 100 ["fuerza"] "IronMan" "Tierra"

drStrange::Personaje
drStrange=UnPersonaje 42 100 [] "DrStrange" "Tierra"

groot::Personaje
groot=UnPersonaje 5 100 ["fuerza","regeneracion","crecimiento"] "Groot" "Arbol"

wolverine::Personaje
wolverine=UnPersonaje 197 100 ["garras","regeneracion","fuerza"] "Wolverine" "Tierra"

miUniverso :: Universo
miUniverso=[ironMan,drStrange,groot,wolverine]

reducirUniverso::Universo->Universo
reducirUniverso universo=take (length universo `div` 2) universo

chasquidoUniverso::Guante->Universo->Universo
chasquidoUniverso guante universo | guanteApto guante = reducirUniverso universo
                                | otherwise = universo

menorDe45::Personaje->Bool
menorDe45 personaje= edad personaje < 45

personajeJoven::Universo->Bool
personajeJoven = any menorDe45 

masDeUnaHabilidad::Personaje->Bool
masDeUnaHabilidad personaje= length (habilidades personaje) > 1

personajesMasDeUnaHabilidad::Universo->Universo
personajesMasDeUnaHabilidad = filter masDeUnaHabilidad 

energiaTotalUniverso::Universo->Int
energiaTotalUniverso = sum . map energia . personajesMasDeUnaHabilidad


debilitar::Int->Personaje->Personaje
debilitar debilitar personaje= personaje{energia= energia personaje  - debilitar}

mente::Int->Gema
mente = debilitar

alma::String->Gema
alma habilidad = debilitar 10 . eliminiarHabilidad habilidad

eliminiarHabilidad::String->Personaje->Personaje
eliminiarHabilidad habilidad personaje= personaje{habilidades= filter (/= habilidad) (habilidades personaje)}

espacio::Gema
espacio = debilitar 20 . moverDePlaneta "Jujuy"

moverDePlaneta::String->Personaje->Personaje
moverDePlaneta nuevoplaneta personaje= personaje{planeta=nuevoplaneta}

poder::Gema
poder = debilitar 100 . quitarHabilidades

cantidadDeHabilidades::Personaje->Int
cantidadDeHabilidades = length . habilidades

quitarHabilidades :: Personaje -> Personaje
quitarHabilidades personaje | cantidadDeHabilidades personaje < 2 = personaje { habilidades = [] }
                            | otherwise = personaje

tiempo::Gema
tiempo = debilitar 50 . rejuvenecer

rejuvenecer::Personaje->Personaje
rejuvenecer personaje= personaje{edad= max 18 (edad personaje `div` 2)}

type Gema=Personaje->Personaje

gemaLoca::Gema->Gema
gemaLoca gema = gema . gema

misGemas::Gemas
misGemas=[tiempo,alma "fuerza",gemaLoca (alma "crecimiento")]

utilizar::Gemas->Personaje->Personaje
utilizar gemas personaje= foldl (\personaje gema-> gema personaje) personaje gemas