--Punto 1
data Planta = Planta {
    nombre::String,
    puntosDeVida :: Int,
    generaSoles :: Int,
    poderDeAtaque :: Int
}deriving (Show)

data Zombie = Zombie {
nombreZombie :: String,
nivelDeMuerte :: Int,
articulos :: [String], --creo que habria que listar los accesorios y no solo contarlos, ya que a futuro se pueden agregar a la lista mas accesorios, y con un length alcanza para saber la cantidad
poderDeMordida :: Int
}deriving (Show)

data LineaDeDefensa = LineaDeDefensa { 
plantas ::[Planta], 
zombies :: [Zombie]
} deriving (Show)

peaShooter = Planta {nombre = "PeaShooter" , puntosDeVida = 5, generaSoles =0, poderDeAtaque = 2}
repeater = peaShooter {nombre = "Repeater", poderDeAtaque = 2*(poderDeAtaque peaShooter)} --con copia, si cambia el ataque del peashooter tambien el del repeater.
sunFlower = Planta {nombre = "Sunflower" , puntosDeVida = 7, generaSoles =1, poderDeAtaque = 0}
nut = Planta {nombre = "Nut" , puntosDeVida = 100, generaSoles =0, poderDeAtaque = 0}

zombieBase = Zombie {nombreZombie = "Zombie", nivelDeMuerte = calcularNivelDeMuerte zombieBase, articulos=[], poderDeMordida=1}
balloonZombie = zombieBase {nombreZombie = "Pepe Colgado", nivelDeMuerte = calcularNivelDeMuerte balloonZombie, articulos = ["un globo"]}
paperZombie = Zombie {nombreZombie = "Beto el chismoso", nivelDeMuerte = calcularNivelDeMuerte paperZombie, articulos = ["un diario"], poderDeMordida=2}
gargantuar = Zombie {nombreZombie = "Gargantuar Hulk Smash Puny God", nivelDeMuerte = calcularNivelDeMuerte gargantuar, articulos = ["un poste de tendido de cableado","un zombie enano"], poderDeMordida=30}

calcularNivelDeMuerte  = length . nombreZombie  
cantidadDeArticulos = length . articulos

--Punto 2
especialidad :: Planta -> String
esPeligroso :: Zombie -> Bool

--Item a
especialidad (Planta _ puntosDeVida generaSoles poderDeAtaque ) 
   | (poderDeAtaque*2)>puntosDeVida = "Atacante" 
   | generaSoles>=1 = "Provedora" 
   | otherwise = "Defensiva"

--Item b
esPeligroso zombie = nivelDeMuerte zombie > 10 || cantidadDeArticulos zombie > 1

--Punto 3
linea1 = LineaDeDefensa { 
plantas =[sunFlower, sunFlower, sunFlower], 
zombies =[]
}

linea2 = LineaDeDefensa { 
plantas = [peaShooter, peaShooter, sunFlower, nut], 
zombies = [zombieBase, paperZombie]
}

linea3 = LineaDeDefensa { 
plantas = [sunFlower, peaShooter], 
zombies = [gargantuar, zombieBase, zombieBase]
}

linea4 = LineaDeDefensa { 
plantas = [peaShooter], 
zombies = [zombieBase]
}

lineaA = LineaDeDefensa { 
  plantas = [sunFlower, peaShooter], 
  zombies = repeat zombieBase
}

lineaB = LineaDeDefensa { 
  plantas = repeat peaShooter, 
  zombies = [gargantuar, zombieBase, zombieBase]
}

lineaC = LineaDeDefensa { 
  plantas = repeat sunFlower, 
  zombies = [gargantuar, zombieBase, zombieBase]
}

agregarPlantaA :: Planta -> LineaDeDefensa -> [Planta]
agregarZombieA :: Zombie -> LineaDeDefensa -> [Zombie]

--Item a
agregarPlantaA planta linea = agregarPlantaALista (plantas linea) planta
agregarPlantaALista  [] planta = [planta]
agregarPlantaALista (x:xs) planta = x : agregarPlantaALista xs planta

agregarPlanta2 planta linea  = reverse $ planta : (reverse . plantas $ linea) --otra forma media rara de agregar al final

agregarZombieA zombie linea = (zombies linea) ++ [zombie]

--Item b
ataqueDePlantasEsMenor linea = (totalDeAtaque . plantas $ linea) < (totalDeMordiscos . zombies $ linea)
totalDeMordiscos [] = 0
totalDeMordiscos (x:xs) =  poderDeMordida x + totalDeMordiscos xs
totalDeAtaque [] = 0
totalDeAtaque (x:xs) = poderDeAtaque x + totalDeAtaque xs

todosLosZombiesSonPeligrosos linea = hayPeligro . zombies $ linea
hayPeligro [] = True
hayPeligro (x:xs) = esPeligroso x && hayPeligro xs

hayZombies linea = (length . zombies $ linea) > 0

estaEnPeligro linea = ataqueDePlantasEsMenor linea  || (todosLosZombiesSonPeligrosos linea && hayZombies linea)

--Item c
necesitaSerDefendida linea = esProvedora . plantas $ linea
esProvedora [] = True
esProvedora (x:xs) = (especialidad x == "Provedora") && esProvedora xs

--Item d
{--i. Si se consulta con una linea de infinitos zombies el programa se colgara al no poder terminar de recorrer la lista en la funcion hayPeligro, ya que esta es recursiva y su salida solo se realiza con la lista vacia "[]". Se puede hacer la prueba con la lineaA

--ii. Al consultar si una línea con cantidad infinita de Peashooter necesita ser defendida, esta daría falso y no necesitaría recorrer toda la lista por concepto de evaluación diferida. Se puede hacer la prueba con la lineaB
  Pero si hablamos de una cantidad infinita de Sunflower, está recorrerá la lista infinitamente ya que todas son proveedoras y de la funcion solo se puede salir con al menos un false o llegando a la lista vacia "[]". Se puede hacer la prueba con la lineaC.
--}

--Punto 4
lineaMixta linea = distintaEspecialidad (plantas linea)
 
distintaEspecialidad [] = True
distintaEspecialidad [x] = False
distintaEspecialidad (x:y:ys) = (especialidad x) /= (especialidad y) && distintaEspecialidad ys

--Punto 5
--a.
ataquePlantaA planta zombie = zombie {nombreZombie=quitarleLetrasAlNombre planta zombie ,nivelDeMuerte = length (quitarleLetrasAlNombre planta zombie)}  

quitarleLetrasAlNombre planta zombie = drop (poderDeAtaque planta) (nombreZombie zombie)

---funcion extra, inicial del nombre de zombie, para usar en caso de prueba
inicialDelZombie zombie = head . nombreZombie $ zombie

--b.
ataqueZombieA zombie planta = planta {puntosDeVida = (puntosDeVida planta - poderDeMordida zombie) }



