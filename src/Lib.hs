module Lib where
import Text.Show.Functions

laVerdad = True

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Int,
  superficie :: Int,
  precio :: Int,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun :: (a->a->Bool)->[a]->[a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta",
  Depto 4 100 10000 "Puerto Madero"]

{- 
===========PUNTO 1===========
Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar esa función 
sobre el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.
-}

mayor :: (Ord b)=>(a->b)->a->a->Bool
mayor funcion valor1 valor2 = funcion valor1 > funcion valor2

menor :: (Ord b)=>(a->b)->a->a->Bool
menor funcion valor1 valor2 = funcion valor2 > funcion valor1

{- 
Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.
-}

-- > ordenarSegun mayor length ["Palermo", "Constitucion", "Barracas", "Lanus", "Villa Urquiza", "Puerto Madero", "Once"]
--["Puerto Madero","Villa Urquiza","Constitucion","Barracas","Palermo","Lanus","Once"]




{- 
===========PUNTO 2===========
Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
  •ubicadoEn que dada una lista de barrios que le interesan al usuario, 
  retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista.
 -}

ubicadoEn :: Depto->[Barrio]->Bool
ubicadoEn depto = elem (barrio depto)


{- 
  •cumpleRango que a partir de una función y dos números, indique si el valor retornado 
  por la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.
-}

cumpleRango :: (Depto->Int)->Depto->Int->Int->Bool
cumpleRango parametroDepto depto valor1 valor2 = between valor1 valor2 $ parametroDepto depto 


{- 
===========PUNTO 3===========
a)Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.
-}

cumpleBusqueda ::  Depto-> Busqueda-> Bool

cumpleBusqueda depto = all $ cumpleRequisito depto

cumpleRequisito :: Depto -> Requisito -> Bool
cumpleRequisito depto requisito = requisito depto 


{- 
b)Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y 
una lista de departamentos retorne todos aquellos que cumplen con la búsqueda ordenados en base al criterio recibido.
-}

buscar :: Busqueda->(Depto->Depto->Bool)->[Depto]->[Depto]
buscar busqueda criterio departamentos = ordenarSegun criterio $ filter (`cumpleBusqueda` busqueda) departamentos

{- 
Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con:
  •Encontrarse en Recoleta o Palermo 
  •Ser de 1 o 2 ambientes 
  •Alquilarse a menos de $6000 por mes -}

requisito1 :: Requisito
requisito1 depto = ambientes depto == 1 || ambientes depto == 2

requisito2 :: Requisito
requisito2 depto = barrio depto == "Palermo" || barrio depto == "Recoleta"

requisito3 :: Requisito
requisito3 depto = precio depto < 6000

requisito4 depto = superficie depto > 90

requisito5 depto = barrio depto == "Puerto Madero" || barrio depto == "Lanus"

busqueda1 :: Busqueda
busqueda1 = [requisito1 , requisito2, requisito3]
busqueda2 = [requisito4, requisito5]

{- 
buscar busqueda1 (mayor superficie) deptosDeEjemplo 
-> [Depto {ambientes = 2, superficie = 50, precio = 5000, barrio = "Palermo"},
   Depto {ambientes = 1, superficie = 45, precio = 5500, barrio = "Recoleta"}]

===========PUNTO 4===========
Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas 
retorne los mails de las personas que tienen alguna búsqueda que se cumpla para el departamento dado.
-}

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto personas = map mail $ filter (personaInteresada depto) personas

personaInteresada ::  Depto -> Persona -> Bool
personaInteresada depto persona = any (cumpleBusqueda depto) $ busquedas persona
