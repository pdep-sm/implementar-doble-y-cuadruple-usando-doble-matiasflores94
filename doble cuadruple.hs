import Text.Show.Functions
doble = (2*)
cuadruple = doble . doble
triple = (3*)
doce = triple . cuadruple


puedeAvanzar color = color == "Verde" || color == "verde"

maximoEntre x y 
  | x>y = x
  |otherwise =y
  
nombrePar :: String -> Bool
nombrePar = even . length

---edad = snd

-- TYPE --
-- type Persona = (String, Integer, String)
-- edad :: Persona -> Integer
-- edad (_, e) = e
--
-- DATA --
-- data Persona  = Persona String Integer
-- nombre (Persona n e) = n
-- edad (Persona n e) = e
--
-- laura :: Persona
-- laura = Persona "Laura" 41

--RECORD SYNTAX --
-- data Persona = Persona {
 -- nombre :: String,
 -- edad :: Integer,
 -- domicilio :: String,
 -- telefono :: String,
 -- fechaDeNacimiento :: (Int, Int, Int),
 -- buenaPersona :: Bool,
 -- plata :: Float
-- } deriving (Show)
 
-- juan = Persona {
    -- nombre = "Juan",
    -- telefono = "45232598",
    -- domicilio = "Ayacucho 554",
    -- fechaNacimiento = (17,7,1988),
    -- buenaPersona = True,
    -- edad = 29,
    -- plata = 30.0
-- }

-- mayorDeEdad :: Integer -> Bool
-- mayorDeEdad edad = edad > 18

-- esMayorEdad :: Persona -> Bool
-- esMayorEdad = mayorDeEdad . edad


pesoPino altura = (min altura 3) * 300 + ((max altura 3) - 3) * 200
esPesoUtil peso = (peso > 400) && (peso < 1000) 
sirvePino = esPesoUtil . pesoPino

prueba  =  (^2)
prueba2  = (2^)

costoEstacionamiento = (* 50) . max 2
costoEstacionamiento2 = (* 50) . max 2 

data Vehiculo = Propio Float | Contratado String Float
type Flota = [Vehiculo]
vehiculos :: Flota
vehiculos = [Propio 9, Propio 15, Contratado "Remixes" 4, Propio 12, Contratado "Remisese" 23]

valorLitroNafta :: Float
valorLitroNafta = 16.5

costoPorKm :: Vehiculo -> Float
costoPorKm (Propio consumo) = valorLitroNafta * (consumo / 10)
costoPorKm (Contratado _ costo) = costo


-----------------
---- ALUMNOS ----
-----------------

data Alumno = Alumno {
  nombre :: String,
  fechaDeNacimiento :: (Int, Int, Int),
  legajo :: Int,
  materiasCursando :: [String],
  criterioEstudio :: CriterioEstudio
} deriving (Show)

data Parcial = Parcial String Int deriving (Show)
materia :: Parcial -> String
materia (Parcial mat _) = mat
cantidadPreguntas :: Parcial -> Int
cantidadPreguntas (Parcial _ cant) = cant


type CriterioEstudio = Parcial -> Bool

estudioso :: CriterioEstudio
estudioso _ = True
hijoDelRigor :: Int  -> CriterioEstudio
hijoDelRigor n (Parcial _ preguntas) =  preguntas > n
cabulero :: CriterioEstudio
cabulero  (Parcial materia _)  = (odd . length) materia 

cambiarCriterioEstudio :: CriterioEstudio -> Alumno -> Alumno
cambiarCriterioEstudio nuevoCriterio alumno = alumno { 
criterioEstudio = nuevoCriterio 
}

estudia :: Parcial -> Alumno -> Bool
estudia parcial alumno = (criterioEstudio alumno) parcial

------------ DATOS ----------

nico = Alumno {
 fechaDeNacimiento = (10, 3, 1993),
 nombre = "Nicolas",
 materiasCursando = ["Sintaxis" , "PdeP"],
 criterioEstudio = estudioso,
 legajo = 123345
 }

parcialPDP = Parcial "PDP" 3

---------------------
---RECURSIVIDAD-----
-------------------

factorial n 
  | n == 0     = 1
  | n > 0      = n * factorial (n - 1)