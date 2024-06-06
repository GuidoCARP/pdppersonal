-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


type Palo= Habilidad->Tiro

putter::Palo
putter habilidad = UnTiro {
    velocidad = 10,
    precision = precisionJugador habilidad*2,
    altura = 0
}

madera::Palo
madera habilidad = UnTiro {
    velocidad = 100,
    precision = div (precisionJugador habilidad) 2,
    altura = 5
}

hierro::Int->Palo
hierro n habilidad = UnTiro {
    velocidad = fuerzaJugador habilidad * n,
    precision = div (precisionJugador habilidad) n,
    altura = max 0 (n-3)
}

palos::[Palo]
palos=[putter,madera,hierro 1]

type Golpe=Jugador->Palo->Tiro

golpear::Golpe
golpear jugador palo = palo (habilidad jugador)

componentesTiroEn0::Tiro->Tiro
componentesTiroEn0 tiro = tiro {velocidad = 0,precision = 0,altura = 0}

modificarPrecision::Tiro->Int->Tiro
modificarPrecision tiro nuevaprecision = tiro {precision = nuevaprecision}

modificarVelocidad::Tiro->Int->Tiro
modificarVelocidad tiro nuevavelocidad = tiro {velocidad = nuevavelocidad}

modificarAltura::Tiro->Int->Tiro
modificarAltura tiro nuevavaltura = tiro {altura = nuevavaltura}


tunel::Tiro->Tiro
tunel tiro | condicionTunel tiro = modificaTiroTunel tiro
           | otherwise = componentesTiroEn0 tiro

laguna::Int->Tiro->Tiro
laguna largo tiro | condicionLaguna tiro = modificaTiroLaguna largo tiro
                  | otherwise = componentesTiroEn0 tiro

hoyo::Tiro->Tiro
hoyo tiro |  condicionHoyo tiro = componentesTiroEn0 tiro
          | otherwise = tiro 


condicionTunel::Tiro->Bool
condicionTunel tiro = precision tiro > 90 && altura tiro == 0

condicionLaguna::Tiro->Bool
condicionLaguna tiro = velocidad tiro > 80 && altura tiro >= 1 && altura tiro <= 5

condicionHoyo::Tiro->Bool
condicionHoyo tiro =  velocidad tiro > 5 && velocidad tiro < 20 && altura tiro == 0 && precision tiro > 95

modificaTiroTunel::Tiro->Tiro
modificaTiroTunel tiro = modificarPrecision (modificarVelocidad (modificarAltura tiro 0) (velocidad tiro * 2)) 100

modificaTiroLaguna::Int->Tiro->Tiro
modificaTiroLaguna largo tiro = modificarAltura tiro (altura tiro `div` largo)

type Obstaculo = Tiro->Tiro

