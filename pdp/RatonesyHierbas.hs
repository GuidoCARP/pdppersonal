data Ratones=UnRaton{
    nombre::String,
    edad::Float,
    peso::Float,
    enfermedeades::[String]}deriving(Show)

type Hierba=Ratones->Ratones

cerebro :: Ratones
cerebro=UnRaton "Cerebro" 9 0.2 ["brucelosis","sarampion","tuberculosis"]
bicenterrata :: Ratones
bicenterrata=UnRaton "Bicenterrata" 256 0.2 []
huesudo :: Ratones
huesudo=UnRaton "Huesudo" 4 10 ["alta obesidad","sinusitis"]

modificaEdad::Ratones->Float
modificaEdad raton=sqrt (edad raton)

hierbaBuena::Hierba
hierbaBuena raton=raton{edad=modificaEdad raton}

hierbaVerde :: String -> Hierba
hierbaVerde terminacion raton = raton {enfermedeades= filter (not . terminaCon terminacion)(enfermedeades raton)}

terminaCon::String->String->Bool
terminaCon terminacion enfermedad=terminacion==reverse(take (length terminacion) (reverse enfermedad))

pierdePeso::Float->Ratones->Ratones
pierdePeso porcentaje raton= raton {peso=peso raton * porcentaje / 100}

alcachofa::Hierba
alcachofa raton | peso raton > 2 = pierdePeso 10 raton
                | otherwise = pierdePeso 5 raton

hierbaZort :: Hierba
hierbaZort  = cambiaNombre "Pinky" . pisaEdad 0 . limpiarEnfermedades 

cambiaNombre::String->Ratones->Ratones
cambiaNombre nuevonombre raton=raton{nombre=nuevonombre}

pisaEdad::Float->Ratones->Ratones
pisaEdad nuevaedad raton=raton{edad=nuevaedad}

limpiarEnfermedades::Ratones->Ratones
limpiarEnfermedades raton=raton{enfermedeades=[]}

type Enfermedades=[String]

enfermedeadesConMenosDe10Letras :: String -> Bool
enfermedeadesConMenosDe10Letras  =  (<10) . length

filtrarEnfermedades :: Enfermedades -> Enfermedades
filtrarEnfermedades = filter enfermedeadesConMenosDe10Letras

hierbaDelDiablo::Hierba
hierbaDelDiablo raton=raton{enfermedeades=filtrarEnfermedades (enfermedeades raton),peso= peso raton - 0.1}

type Medicamento=[Hierba]

pondsAntiAge::Medicamento
pondsAntiAge=[hierbaBuena,hierbaBuena,hierbaBuena,alcachofa]

aplicarMedicamento::Ratones->Medicamento->Ratones
aplicarMedicamento= foldl (\raton hierba->hierba raton)

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina::Medicamento
pdepCilina=map hierbaVerde sufijosInfecciosas

{-Hacer la función que encuentra la cantidadIdeal. Recibe una condición y dice cuál es el primer número natural que la cumple.-}

cantidadIdeal :: (Int -> Bool) -> Int
cantidadIdeal condicion = head (filter condicion [1..])

type Comunidad=[Ratones]
comunidadRatones :: Comunidad
comunidadRatones=[cerebro,bicenterrata,huesudo]

sobrepeso::Ratones->Bool
sobrepeso raton=peso raton > 1

cantEnfermedades::Ratones->Int
cantEnfermedades raton=length (enfermedeades raton)