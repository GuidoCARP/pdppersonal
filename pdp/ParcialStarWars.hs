data Nave = UnaNave{
    nombre::String,
    durabilidad::Int,
    escudo::Int,
    ataque::Int,
    poder::String} deriving (Show,Eq,Ord)

nave1 :: Nave
nave1= UnaNave "TIE Fighter" 200 100 50 "Hace un movimiento Turbo, el cual incrementa su ataque en 25"
nave2 :: Nave
nave2= UnaNave "X Wing" 300 150 100 "Hace una reparación de emergencia, lo cual aumenta su durabilidad en 50 pero reduce su ataque en 30."
nave3 :: Nave
nave3= UnaNave "Nave de Darth Vader" 500 300 200 "Hace un movimiento Super Turbo, lo cual significa hacer 3 veces el movimiento Turbo y reducir la durabilidad en 45."
nave4 :: Nave
nave4= UnaNave "Millennium Falcon" 1000 500 50 "Hace una reparación de emergencia y además se incrementan sus escudos en 100."
nave5 :: Nave
nave5= UnaNave "Nave destructora de mundos" 10000 10000 10000 "Destruye todo a su paso"

type Flota = [Nave]

unaFlota :: [Nave]
unaFlota=[nave1,nave2,nave3,nave4,nave5]

durabilidadTotal::Flota->Int
durabilidadTotal flota = sum (map durabilidad flota)

{-Saber cómo queda una nave luego de ser atacada por otra. Cuando ocurre un ataque ambas naves primero activan su poder especial y luego la nave atacada reduce su durabilidad según el daño recibido, que es la diferencia entre el ataque de la atacante y el escudo de la atacada. (si el escudo es superior al ataque, la nave atacada no es afectada). La durabilidad, el escudo y el ataque nunca pueden ser negativos, a lo sumo 0.
-}

atacar::Nave->Nave->Nave
atacar atacante atacada = atacada {durabilidad = max 0 (durabilidad atacada - max 0 (ataque atacante - escudo atacada))}

{-Averiguar si una nave está fuera de combate, lo que se obtiene cuando su durabilidad llegó a 0. -}

fueraDeCombate::Nave->Bool
fueraDeCombate nave = durabilidad nave == 0

type Estrategia = Nave->Flota->Flota

mision::Estrategia
mision nave  = map (atacar nave) 

navesDebiles::Estrategia
navesDebiles nave flota = mision nave (filter (\nave -> escudo nave < 200) flota)

navesConCiertaPeligrosidad::Int->Estrategia
navesConCiertaPeligrosidad valor nave flota = mision nave (filter (\nave -> ataque nave > valor) flota)

navesFueraDeCombate::Estrategia
navesFueraDeCombate nave flota = filter fueraDeCombate (mision nave flota)

{-Considerando una nave y una flota enemiga en particular, dadas dos estrategias, determinar cuál de ellas es la que minimiza la durabilidad total de la flota atacada y llevar adelante una misión con ella.-}

minimizarDurabilidad::Estrategia->Estrategia->Nave->Flota->Flota
minimizarDurabilidad estrategia1 estrategia2 nave flota | durabilidadTotal (estrategia1 nave flota) < durabilidadTotal (estrategia2 nave flota) = estrategia1 nave flota
                                                        | otherwise = estrategia2 nave flota