type Numero = Int
type Palabra = String
type Dia = String
type Feriado = Bool

funcion:: Numero->Bool
funcion numero = False

eldoblemas1::Numero->Numero
eldoblemas1 numero=(numero*2)+1

paridadcadena:: Palabra->Bool
paridadcadena cadena=even(length cadena)

horariocierre :: Dia->Feriado->Numero
horariocierre "sabado" False = 21
horariocierre "domingo" True = 13
horariocierre dia False = length dia+12
horariocierre _ True = 20