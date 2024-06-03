type Edad = Int
type Nombre = String
type Altura = Float
type Persona = (Nombre,Edad)
type Empresa = (Nombre,Int,Int)
type MiLista = [Altura]

miPersona::Persona
miPersona = ("Juan",25)

esMayor :: Edad->Bool
esMayor edad = edad >= 18

edad=snd

personaMayor :: Persona->Bool
personaMayor = esMayor.edad

esG::Char->Bool
esG = ('G' ==)

nombreConG::Persona->Bool
nombreConG persona = 'G' == head(fst persona)

personaGrande::Persona->Bool
personaGrande persona = esMayor (snd persona)

trd :: (a, b, Int) -> Int
trd (_,_,x) = x

miEmpresa :: Empresa
miEmpresa = ("GuidoS.A",1988,2024)

otraEmpresa :: Empresa
otraEmpresa =("SocotrocoS.R.L",1950,2020)

avanzaAño :: Empresa->Int
avanzaAño empresa = trd empresa +1

empresaAntigua :: Empresa->Empresa->Int
empresaAntigua empresa1 empresa2 = min(trd empresa1)(trd empresa2)

unaLista :: MiLista
unaLista = [1.23,1.70,1.88,1.55,1.66]

