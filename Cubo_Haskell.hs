--IMPORTS
import Data.List

--Definicion de datos
data Color = Blanco | Amarillo | Azul | Verde | Naranja | Rojo deriving (Eq, Show)

data Pieza = Pieza{colorPieza :: Color} deriving (Show)

data Cara = Cara {supIzq :: Pieza, infIzq :: Pieza, supDer :: Pieza, infDer :: Pieza} deriving(Show)

data Cubo = Cubo {arriba :: Cara, abajo :: Cara, frente :: Cara, atras :: Cara, izquierda :: Cara, derecha :: Cara}  deriving(Show)

type Cubo_Movimiento=(Cubo,Movimientos)
type Arbol=[Cubo_Movimiento]

data Movimientos = ArribaHorario
          | ArribaAntihorario
          | AbajoHorario
          | AbajoAntihorario
          | FrenteHorario
          | FrenteAntihorario
          | AtrasHorario
          | AtrasAntihorario
          | IzquierdaHorario
          | IzquierdaAntihorario
          | DerechaHorario
          | DerechaAntihorario
		  |VueltaArriba 
		  |VueltaIzquierda
		  |	VueltaDerecha 
		  |	VueltaFrente 
		  |	VueltaAtras
		  deriving(Eq,Show)

--Obtener color de una pieza 
getColorPieza :: Pieza -> Color
getColorPieza (Pieza color)=color

--Obtener caras
getCaraArriba :: Cubo -> Cara
getCaraArriba (Cubo arriba _ _ _ _ _)=arriba

getCaraAbajo :: Cubo -> Cara
getCaraAbajo (Cubo _ abajo _ _ _ _)=abajo

getCaraFrente :: Cubo -> Cara
getCaraFrente (Cubo _ _ frente _ _ _)=frente

getCaraAtras :: Cubo -> Cara
getCaraAtras (Cubo _ _ _ atras _ _)=atras

getCaraIzquierda :: Cubo -> Cara
getCaraIzquierda (Cubo _ _ _ _ izquierda _)=izquierda

getCaraDerecha :: Cubo -> Cara
getCaraDerecha (Cubo _ _ _ _ _ derecha)=derecha

--Obtener colores de una cara
getColoresCara :: Cara -> [Color]
getColoresCara (Cara supIzq infIzq supDer infDer) = [(getColorPieza supIzq)]++[(getColorPieza infIzq)]++[(getColorPieza supDer)]++[(getColorPieza infDer)]

--Representar cubo
representarCubo :: Cubo -> IO()
representarCubo cubo = do
	putStrLn("Arriba:"++show (getColoresCara(getCaraArriba cubo)))
	putStrLn("Abajo:"++show (getColoresCara(getCaraAbajo cubo)))
	putStrLn("Frente:"++show (getColoresCara(getCaraFrente cubo)))
	putStrLn("Atras:"++show (getColoresCara(getCaraAtras cubo)))
	putStrLn("Izquierda:"++show (getColoresCara(getCaraIzquierda cubo)))
	putStrLn("Derecha:"++show (getColoresCara(getCaraDerecha cubo)))

--Cubo resuelto

--Cara Blanca(arriba)
caraBlanca :: Cara
caraBlanca = Cara { supIzq = Pieza Blanco, infIzq = Pieza Blanco, supDer = Pieza Blanco, infDer = Pieza Blanco}

--Cara amarilla(abajo)
caraAmarilla :: Cara
caraAmarilla = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Amarillo, supDer = Pieza Amarillo, infDer = Pieza Amarillo}

--Cara roja(frente)
caraRoja :: Cara
caraRoja = Cara { supIzq = Pieza Rojo, infIzq = Pieza Rojo, supDer = Pieza Rojo, infDer = Pieza Rojo}

--Cara naranja(atras)
caraNaranja :: Cara
caraNaranja = Cara { supIzq = Pieza Naranja, infIzq = Pieza Naranja, supDer = Pieza Naranja, infDer = Pieza Naranja}

--Cara verde (izquierda)
caraVerde:: Cara
caraVerde = Cara { supIzq = Pieza Verde, infIzq = Pieza Verde, supDer = Pieza Verde, infDer = Pieza Verde}

--Cara azul(derecha)
caraAzul :: Cara
caraAzul = Cara { supIzq = Pieza Azul, infIzq = Pieza Azul, supDer = Pieza Azul, infDer = Pieza Azul}

cuboResuelto :: Cubo
cuboResuelto = Cubo { arriba = caraBlanca, abajo = caraAmarilla, frente = caraRoja, atras = caraNaranja, izquierda = caraVerde, derecha = caraAzul}
---------------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo
--Cara arriba
caraArriba :: Cara
caraArriba = Cara { supIzq = Pieza Azul, infIzq = Pieza Amarillo, supDer = Pieza Naranja, infDer = Pieza Naranja}

--Cara abajo
caraAbajo :: Cara
caraAbajo = Cara { supIzq = Pieza Rojo, infIzq = Pieza Verde, supDer = Pieza Naranja, infDer = Pieza Azul}

--Cara frente
caraFrente :: Cara
caraFrente = Cara { supIzq = Pieza Rojo, infIzq = Pieza Blanco, supDer = Pieza Azul, infDer = Pieza Blanco}

--Cara atras
caraAtras :: Cara
caraAtras = Cara { supIzq = Pieza Blanco, infIzq = Pieza Verde, supDer = Pieza Naranja, infDer = Pieza Verde}

--Cara izquierda
caraIzquierda :: Cara
caraIzquierda = Cara { supIzq = Pieza Blanco, infIzq = Pieza Amarillo, supDer = Pieza Azul, infDer = Pieza Rojo}

--Cara derecha
caraDerecha :: Cara
caraDerecha = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Rojo, supDer = Pieza Verde, infDer = Pieza Amarillo}

cuboEjemplo :: Cubo
cuboEjemplo = Cubo { arriba = caraArriba, abajo = caraAbajo, frente = caraFrente, atras = caraAtras, izquierda = caraIzquierda, derecha = caraDerecha}

------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo 2
caraArriba2 :: Cara
caraArriba2 = Cara { supIzq = Pieza Verde, infIzq = Pieza Azul, supDer = Pieza Amarillo, infDer = Pieza Rojo}

--Cara abajo
caraAbajo2 :: Cara
caraAbajo2 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Naranja, supDer = Pieza Amarillo, infDer = Pieza Amarillo}

--Cara frente
caraFrente2 :: Cara
caraFrente2 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Verde, supDer = Pieza Blanco, infDer = Pieza Azul}

--Cara atras
caraAtras2 :: Cara
caraAtras2 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Verde, supDer = Pieza Blanco, infDer = Pieza Azul}

--Cara izquierda
caraIzquierda2 :: Cara
caraIzquierda2 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Naranja, supDer = Pieza Blanco, infDer = Pieza Naranja}

--Cara derecha
caraDerecha2 :: Cara
caraDerecha2 = Cara { supIzq = Pieza Verde, infIzq = Pieza Blanco, supDer = Pieza Azul, infDer = Pieza Amarillo}

cuboEjemplo2 :: Cubo
cuboEjemplo2 = Cubo { arriba = caraArriba2, abajo = caraAbajo2, frente = caraFrente2, atras = caraAtras2, izquierda = caraIzquierda2, derecha = caraDerecha2}

------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo 3
caraArriba3 :: Cara
caraArriba3 = Cara { supIzq = Pieza Verde, infIzq = Pieza Verde, supDer = Pieza Blanco, infDer = Pieza Amarillo}

--Cara abajo
caraAbajo3 :: Cara
caraAbajo3 = Cara { supIzq = Pieza Verde, infIzq = Pieza Naranja, supDer = Pieza Rojo, infDer = Pieza Naranja}

--Cara frente
caraFrente3 :: Cara
caraFrente3 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Blanco, supDer = Pieza Azul, infDer = Pieza Amarillo}

--Cara atras
caraAtras3 :: Cara
caraAtras3 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Naranja, supDer = Pieza Blanco, infDer = Pieza Blanco}

--Cara izquierda
caraIzquierda3 :: Cara
caraIzquierda3 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Azul, supDer = Pieza Amarillo, infDer = Pieza Azul}

--Cara derecha
caraDerecha3 :: Cara
caraDerecha3 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Azul, supDer = Pieza Verde, infDer = Pieza Amarillo}

cuboEjemplo3 :: Cubo
cuboEjemplo3 = Cubo { arriba = caraArriba3, abajo = caraAbajo3, frente = caraFrente3, atras = caraAtras3, izquierda = caraIzquierda3, derecha = caraDerecha3}

------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo 4
caraArriba4 :: Cara
caraArriba4 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Blanco, supDer = Pieza Rojo, infDer = Pieza Rojo}

--Cara abajo
caraAbajo4 :: Cara
caraAbajo4 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Azul, supDer = Pieza Blanco, infDer = Pieza Amarillo}

--Cara frente
caraFrente4 :: Cara
caraFrente4 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Verde, supDer = Pieza Amarillo, infDer = Pieza Blanco}

--Cara atras
caraAtras4 :: Cara
caraAtras4 = Cara { supIzq = Pieza Blanco, infIzq = Pieza Azul, supDer = Pieza Verde, infDer = Pieza Verde}

--Cara izquierda
caraIzquierda4 :: Cara
caraIzquierda4 = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Naranja, supDer = Pieza Verde, infDer = Pieza Naranja}

--Cara derecha
caraDerecha4 :: Cara
caraDerecha4 = Cara { supIzq = Pieza Azul, infIzq = Pieza Naranja, supDer = Pieza Azul, infDer = Pieza Amarillo}

cuboEjemplo4 :: Cubo
cuboEjemplo4 = Cubo { arriba = caraArriba4, abajo = caraAbajo4, frente = caraFrente4, atras = caraAtras4, izquierda = caraIzquierda4, derecha = caraDerecha4}

------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo 5
caraArriba5 :: Cara
caraArriba5 = Cara { supIzq = Pieza Blanco, infIzq = Pieza Rojo, supDer = Pieza Naranja, infDer = Pieza Verde}

--Cara abajo
caraAbajo5 :: Cara
caraAbajo5 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Naranja, supDer = Pieza Naranja, infDer = Pieza Azul}

--Cara frente
caraFrente5 :: Cara
caraFrente5 = Cara { supIzq = Pieza Azul, infIzq = Pieza Blanco, supDer = Pieza Blanco, infDer = Pieza Amarillo}

--Cara atras
caraAtras5 :: Cara
caraAtras5 = Cara { supIzq = Pieza Azul, infIzq = Pieza Verde, supDer = Pieza Verde, infDer = Pieza Verde}

--Cara izquierda
caraIzquierda5 :: Cara
caraIzquierda5 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Amarillo, supDer = Pieza Amarillo, infDer = Pieza Rojo}

--Cara derecha
caraDerecha5 :: Cara
caraDerecha5 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Azul, supDer = Pieza Blanco, infDer = Pieza Amarillo}

cuboEjemplo5 :: Cubo
cuboEjemplo5 = Cubo { arriba = caraArriba5, abajo = caraAbajo5, frente = caraFrente5, atras = caraAtras5, izquierda = caraIzquierda5, derecha = caraDerecha5}

------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo 6
caraArriba6 :: Cara
caraArriba6 = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Azul, supDer = Pieza Naranja, infDer = Pieza Blanco}

--Cara abajo
caraAbajo6 :: Cara
caraAbajo6 = Cara { supIzq = Pieza Blanco, infIzq = Pieza Verde, supDer = Pieza Blanco, infDer = Pieza Blanco}

--Cara frente
caraFrente6 :: Cara
caraFrente6 = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Rojo, supDer = Pieza Naranja, infDer = Pieza Amarillo}

--Cara atras
caraAtras6 :: Cara
caraAtras6 = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Verde, supDer = Pieza Rojo, infDer = Pieza Naranja}

--Cara izquierda
caraIzquierda6 :: Cara
caraIzquierda6 = Cara { supIzq = Pieza Verde, infIzq = Pieza Azul, supDer = Pieza Rojo, infDer = Pieza Azul}

--Cara derecha
caraDerecha6 :: Cara
caraDerecha6 = Cara { supIzq = Pieza Verde, infIzq = Pieza Naranja, supDer = Pieza Azul, infDer = Pieza Rojo}

cuboEjemplo6 :: Cubo
cuboEjemplo6 = Cubo { arriba = caraArriba6, abajo = caraAbajo6, frente = caraFrente6, atras = caraAtras6, izquierda = caraIzquierda6, derecha = caraDerecha6}

------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo 7
caraArriba7 :: Cara
caraArriba7 = Cara { supIzq = Pieza Verde, infIzq = Pieza Blanco, supDer = Pieza Azul, infDer = Pieza Verde}

--Cara abajo
caraAbajo7 :: Cara
caraAbajo7 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Azul, supDer = Pieza Verde, infDer = Pieza Azul}

--Cara frente
caraFrente7 :: Cara
caraFrente7 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Naranja, supDer = Pieza Naranja, infDer = Pieza Rojo}

--Cara atras
caraAtras7 :: Cara
caraAtras7 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Amarillo, supDer = Pieza Blanco, infDer = Pieza Rojo}

--Cara izquierda
caraIzquierda7 :: Cara
caraIzquierda7 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Amarillo, supDer = Pieza Verde, infDer = Pieza Blanco}

--Cara derecha
caraDerecha7 :: Cara
caraDerecha7 = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Blanco, supDer = Pieza Amarillo, infDer = Pieza Azul}

cuboEjemplo7 :: Cubo
cuboEjemplo7 = Cubo { arriba = caraArriba7, abajo = caraAbajo7, frente = caraFrente7, atras = caraAtras7, izquierda = caraIzquierda7, derecha = caraDerecha7}
------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo 8
caraArriba8 :: Cara
caraArriba8 = Cara { supIzq = Pieza Azul, infIzq = Pieza Amarillo, supDer = Pieza Naranja, infDer = Pieza Rojo}

--Cara abajo
caraAbajo8 :: Cara
caraAbajo8 = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Amarillo, supDer = Pieza Blanco, infDer = Pieza Naranja}

--Cara frente
caraFrente8 :: Cara
caraFrente8 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Verde, supDer = Pieza Azul, infDer = Pieza Verde}

--Cara atras
caraAtras8 :: Cara
caraAtras8 = Cara { supIzq = Pieza Azul, infIzq = Pieza Verde, supDer = Pieza Amarillo, infDer = Pieza Rojo}

--Cara izquierda
caraIzquierda8 :: Cara
caraIzquierda8 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Verde, supDer = Pieza Azul, infDer = Pieza Blanco}

--Cara derecha
caraDerecha8 :: Cara
caraDerecha8 = Cara { supIzq = Pieza Blanco, infIzq = Pieza Rojo, supDer = Pieza Blanco, infDer = Pieza Naranja}

cuboEjemplo8 :: Cubo
cuboEjemplo8 = Cubo { arriba = caraArriba8, abajo = caraAbajo8, frente = caraFrente8, atras = caraAtras8, izquierda = caraIzquierda8, derecha = caraDerecha8}
------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo 9
caraArriba9 :: Cara
caraArriba9 = Cara { supIzq = Pieza Azul, infIzq = Pieza Rojo, supDer = Pieza Amarillo, infDer = Pieza Verde}

--Cara abajo
caraAbajo9 :: Cara
caraAbajo9 = Cara { supIzq = Pieza Blanco, infIzq = Pieza Verde, supDer = Pieza Rojo, infDer = Pieza Amarillo}

--Cara frente
caraFrente9 :: Cara
caraFrente9 = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Verde, supDer = Pieza Rojo, infDer = Pieza Naranja}

--Cara atras
caraAtras9 :: Cara
caraAtras9 = Cara { supIzq = Pieza Azul, infIzq = Pieza Azul, supDer = Pieza Rojo, infDer = Pieza Blanco}

--Cara izquierda
caraIzquierda9 :: Cara
caraIzquierda9 = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Azul, supDer = Pieza Verde, infDer = Pieza Naranja}

--Cara derecha
caraDerecha9 :: Cara
caraDerecha9 = Cara { supIzq = Pieza Blanco, infIzq = Pieza Blanco, supDer = Pieza Naranja, infDer = Pieza Naranja}

cuboEjemplo9 :: Cubo
cuboEjemplo9 = Cubo { arriba = caraArriba9, abajo = caraAbajo9, frente = caraFrente9, atras = caraAtras9, izquierda = caraIzquierda9, derecha = caraDerecha9}
------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo 10
caraArriba10 :: Cara
caraArriba10 = Cara { supIzq = Pieza Amarillo, infIzq = Pieza Rojo, supDer = Pieza Blanco, infDer = Pieza Amarillo}

--Cara abajo
caraAbajo10 :: Cara
caraAbajo10 = Cara { supIzq = Pieza Azul, infIzq = Pieza Verde, supDer = Pieza Naranja, infDer = Pieza Azul}

--Cara frente
caraFrente10 :: Cara
caraFrente10 = Cara { supIzq = Pieza Verde, infIzq = Pieza Blanco, supDer = Pieza Naranja, infDer = Pieza Naranja}

--Cara atras
caraAtras10 :: Cara
caraAtras10 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Rojo, supDer = Pieza Rojo, infDer = Pieza Verde}

--Cara izquierda
caraIzquierda10 :: Cara
caraIzquierda10 = Cara { supIzq = Pieza Verde, infIzq = Pieza Amarillo, supDer = Pieza Blanco, infDer = Pieza Rojo}

--Cara derecha
caraDerecha10 :: Cara
caraDerecha10 = Cara { supIzq = Pieza Azul, infIzq = Pieza Blanco, supDer = Pieza Azul, infDer = Pieza Amarillo}

cuboEjemplo10 :: Cubo
cuboEjemplo10 = Cubo { arriba = caraArriba10, abajo = caraAbajo10, frente = caraFrente10, atras = caraAtras10, izquierda = caraIzquierda10, derecha = caraDerecha10}
------------------------------------------------------------------------------------------------------------------------------------------------
--Cubo ejemplo 11
caraArriba11 :: Cara
caraArriba11 = Cara { supIzq = Pieza Verde, infIzq = Pieza Azul, supDer = Pieza Amarillo, infDer = Pieza Rojo}

--Cara abajo
caraAbajo11 :: Cara
caraAbajo11 = Cara { supIzq = Pieza Rojo, infIzq = Pieza Naranja, supDer = Pieza Amarillo, infDer = Pieza Blanco}

--Cara frente
caraFrente11 :: Cara
caraFrente11 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Verde, supDer = Pieza Azul, infDer = Pieza Azul}

--Cara atras
caraAtras11 :: Cara
caraAtras11 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Amarillo, supDer = Pieza Blanco, infDer = Pieza Verde}

--Cara izquierda
caraIzquierda11 :: Cara
caraIzquierda11 = Cara { supIzq = Pieza Naranja, infIzq = Pieza Rojo, supDer = Pieza Amarillo, infDer = Pieza Rojo}

--Cara derecha
caraDerecha11 :: Cara
caraDerecha11 = Cara { supIzq = Pieza Blanco, infIzq = Pieza Blanco, supDer = Pieza Verde, infDer = Pieza Azul}

cuboEjemplo11 :: Cubo
cuboEjemplo11 = Cubo { arriba = caraArriba11, abajo = caraAbajo11, frente = caraFrente11, atras = caraAtras11, izquierda = caraIzquierda11, derecha = caraDerecha11}


--Movimientos
rotarArribaHorario :: Cubo -> Cubo
rotarArribaHorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajo frenteAux atrasAux izquierdaAux derechaAux)
	where
		frenteAux = Cara { supIzq = supIzq(derecha), infIzq = infIzq(frente), supDer = supDer(derecha), infDer = infDer(frente)}
		atrasAux = Cara { supIzq = supIzq(izquierda), infIzq = infIzq(atras), supDer = supDer(izquierda), infDer = infDer(atras)}
		izquierdaAux = Cara { supIzq = supIzq(frente), infIzq = infIzq(izquierda), supDer = supDer(frente), infDer = infDer(izquierda)}
		derechaAux = Cara { supIzq = supIzq(atras), infIzq = infIzq(derecha), supDer = supDer(atras), infDer = infDer(derecha)}
		arribaAux = Cara { supIzq = infIzq(arriba), infIzq = infDer(arriba), supDer = supIzq(arriba), infDer = supDer(arriba)}
		
rotarArribaAntihorario :: Cubo -> Cubo
rotarArribaAntihorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajo frenteAux atrasAux izquierdaAux derechaAux)
	where
		frenteAux = Cara { supIzq = supIzq(izquierda), infIzq = infIzq(frente), supDer = supDer(izquierda), infDer = infDer(frente)}
		atrasAux = Cara { supIzq = supIzq(derecha), infIzq = infIzq(atras), supDer = supDer(derecha), infDer = infDer(atras)}
		izquierdaAux = Cara { supIzq = supIzq(atras), infIzq = infIzq(izquierda), supDer = supDer(atras), infDer = infDer(izquierda)}
		derechaAux = Cara { supIzq = supIzq(frente), infIzq = infIzq(derecha), supDer = supDer(frente), infDer = infDer(derecha)}
		arribaAux = Cara { supIzq = supDer(arriba), infIzq = supIzq(arriba), supDer = infDer(arriba), infDer = infIzq(arriba)}
		
rotarAbajoHorario :: Cubo -> Cubo
rotarAbajoHorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arriba abajoAux frenteAux atrasAux izquierdaAux derechaAux)
	where
		frenteAux = Cara { supIzq = supIzq(frente), infIzq = infIzq(izquierda), supDer = supDer(frente), infDer = infDer(izquierda)}
		atrasAux = Cara { supIzq = supIzq(atras), infIzq = infIzq(derecha), supDer = supDer(atras), infDer = infDer(derecha)}
		izquierdaAux = Cara { supIzq = supIzq(izquierda), infIzq = infIzq(atras), supDer = supDer(izquierda), infDer = infDer(atras)}
		derechaAux = Cara { supIzq = supIzq(derecha), infIzq = infIzq(frente), supDer = supDer(derecha), infDer = infDer(frente)}
		abajoAux = Cara { supIzq = infIzq(abajo), infIzq = infDer(abajo), supDer = supIzq(abajo), infDer = supDer(abajo)}
		
rotarAbajoAntihorario :: Cubo -> Cubo
rotarAbajoAntihorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arriba abajoAux frenteAux atrasAux izquierdaAux derechaAux)
	where
		frenteAux = Cara { supIzq = supIzq(frente), infIzq = infIzq(derecha), supDer = supDer(frente), infDer = infDer(derecha)}
		atrasAux = Cara { supIzq = supIzq(atras), infIzq = infIzq(izquierda), supDer = supDer(atras), infDer = infDer(izquierda)}
		izquierdaAux = Cara { supIzq = supIzq(izquierda), infIzq = infIzq(frente), supDer = supDer(izquierda), infDer = infDer(frente)}
		derechaAux = Cara { supIzq = supIzq(derecha), infIzq = infIzq(atras), supDer = supDer(derecha), infDer = infDer(atras)}
		abajoAux = Cara { supIzq = supDer(abajo), infIzq = supIzq(abajo), supDer = infDer(abajo), infDer = infIzq(abajo)}

rotarFrenteHorario :: Cubo -> Cubo
rotarFrenteHorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atras izquierdaAux derechaAux)
	where
		arribaAux = Cara { supIzq = supIzq(arriba), infIzq = infDer(izquierda), supDer = supDer(arriba), infDer = supDer(izquierda)}
		abajoAux = Cara { supIzq = supIzq(abajo), infIzq = supIzq(derecha), supDer = supDer(abajo), infDer = infIzq(derecha)}
		izquierdaAux = Cara { supIzq = supIzq(izquierda), infIzq = infIzq(izquierda), supDer = infDer(abajo), infDer = infIzq(abajo)}
		derechaAux = Cara { supIzq = infIzq(arriba), infIzq = infDer(arriba), supDer = supDer(derecha), infDer = infDer(derecha)}
		frenteAux = Cara { supIzq = infIzq(frente), infIzq = infDer(frente), supDer = supIzq(frente), infDer = supDer(frente)}
		
rotarFrenteAntihorario :: Cubo -> Cubo
rotarFrenteAntihorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atras izquierdaAux derechaAux)
	where
		arribaAux = Cara { supIzq = supIzq(arriba), infIzq = supIzq(derecha), supDer = supDer(arriba), infDer = infIzq(derecha)}
		abajoAux = Cara { supIzq = supIzq(abajo), infIzq = infDer(izquierda), supDer = supDer(abajo), infDer = supDer(izquierda)}
		izquierdaAux = Cara { supIzq = supIzq(izquierda), infIzq = infIzq(izquierda), supDer = infDer(arriba), infDer = infIzq(arriba)}
		derechaAux = Cara { supIzq = infIzq(abajo), infIzq = infDer(abajo), supDer = supDer(derecha), infDer = infDer(derecha)}
		frenteAux = Cara { supIzq = supDer(frente), infIzq = supIzq(frente), supDer = infDer(frente), infDer = infIzq(frente)}
		
rotarAtrasHorario :: Cubo -> Cubo
rotarAtrasHorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frente atrasAux izquierdaAux derechaAux)
	where
		arribaAux = Cara { supIzq = supDer(derecha), infIzq = infIzq(arriba), supDer = infDer(derecha), infDer = infDer(arriba)}
		abajoAux = Cara { supIzq = infIzq(izquierda), infIzq = infIzq(abajo), supDer = supIzq(izquierda), infDer = infDer(abajo)}
		izquierdaAux = Cara { supIzq = supDer(arriba), infIzq = supIzq(arriba), supDer = supDer(izquierda), infDer = infDer(izquierda)}
		derechaAux = Cara { supIzq = supIzq(derecha), infIzq = infIzq(derecha), supDer = supIzq(abajo), infDer = supDer(abajo)}
		atrasAux = Cara { supIzq = infIzq(atras), infIzq = infDer(atras), supDer = supIzq(atras), infDer = supDer(atras)}
		
rotarAtrasAntihorario :: Cubo -> Cubo
rotarAtrasAntihorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frente atrasAux izquierdaAux derechaAux)
	where
		arribaAux = Cara { supIzq = infIzq(izquierda), infIzq = infIzq(arriba), supDer = supIzq(izquierda), infDer = infDer(arriba)}
		abajoAux = Cara { supIzq = supDer(derecha), infIzq = infIzq(abajo), supDer = infDer(derecha), infDer = infDer(abajo)}
		izquierdaAux = Cara { supIzq = supDer(abajo), infIzq = supIzq(abajo), supDer = supDer(izquierda), infDer = infDer(izquierda)}
		derechaAux = Cara { supIzq = supIzq(derecha), infIzq = infIzq(derecha), supDer = supIzq(arriba), infDer = supDer(arriba)}
		atrasAux = Cara { supIzq = supDer(atras), infIzq = supIzq(atras), supDer = infDer(atras), infDer = infIzq(atras)}
		
rotarIzquierdaHorario :: Cubo -> Cubo
rotarIzquierdaHorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atrasAux izquierdaAux derecha)
	where
		arribaAux = Cara { supIzq = infDer(atras), infIzq = supDer(atras), supDer = supDer(arriba), infDer = infDer(arriba)}
		abajoAux = Cara { supIzq = supIzq(abajo), infIzq = infIzq(abajo), supDer = infIzq(frente), infDer = supIzq(frente)}
		frenteAux = Cara { supIzq = supIzq(arriba), infIzq = infIzq(arriba), supDer = supDer(frente), infDer = infDer(frente)}
		atrasAux = Cara { supIzq = supIzq(atras), infIzq = infIzq(atras), supDer = supDer(abajo), infDer = infDer(abajo)}
		izquierdaAux = Cara { supIzq = infIzq(izquierda), infIzq = infDer(izquierda), supDer = supIzq(izquierda), infDer = supDer(izquierda)}
		
rotarIzquierdaAntihorario :: Cubo -> Cubo
rotarIzquierdaAntihorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atrasAux izquierdaAux derecha)
	where
		arribaAux = Cara { supIzq = supIzq(frente), infIzq = infIzq(frente), supDer = supDer(arriba), infDer = infDer(arriba)}
		abajoAux = Cara { supIzq = supIzq(abajo), infIzq = infIzq(abajo), supDer = supDer(atras), infDer = infDer(atras)}
		frenteAux = Cara { supIzq = infDer(abajo), infIzq = supDer(abajo), supDer = supDer(frente), infDer = infDer(frente)}
		atrasAux = Cara { supIzq = supIzq(atras), infIzq = infIzq(atras), supDer = infIzq(arriba), infDer = supIzq(arriba)}
		izquierdaAux = Cara {supIzq = supDer(izquierda), infIzq = supIzq(izquierda), supDer = infDer(izquierda), infDer = infIzq(izquierda)}
		
rotarDerechaHorario :: Cubo -> Cubo
rotarDerechaHorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atrasAux izquierda derechaAux)
	where
		arribaAux = Cara { supIzq = supIzq(arriba), infIzq = infIzq(arriba), supDer = supDer(frente), infDer = infDer(frente)}
		abajoAux = Cara { supIzq = supIzq(atras), infIzq = infIzq(atras), supDer = supDer(abajo), infDer = infDer(abajo)}
		frenteAux = Cara { supIzq = supIzq(frente), infIzq = infIzq(frente), supDer = infIzq(abajo), infDer = supIzq(abajo)}
		atrasAux = Cara { supIzq = infDer(arriba), infIzq = supDer(arriba), supDer = supDer(atras), infDer = infDer(atras)}
		derechaAux = Cara { supIzq = infIzq(derecha), infIzq = infDer(derecha), supDer = supIzq(derecha), infDer = supDer(derecha)}
		
rotarDerechaAntihorario :: Cubo -> Cubo
rotarDerechaAntihorario (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atrasAux izquierda derechaAux)
	where
		arribaAux = Cara { supIzq = supIzq(arriba), infIzq = infIzq(arriba), supDer = infIzq(atras), infDer = supIzq(atras)}
		abajoAux = Cara { supIzq = infDer(frente), infIzq = supDer(frente), supDer = supDer(abajo), infDer = infDer(abajo)}
		frenteAux = Cara { supIzq = supIzq(frente), infIzq = infIzq(frente), supDer = supDer(arriba), infDer = infDer(arriba)}
		atrasAux = Cara { supIzq = supIzq(abajo), infIzq = infIzq(abajo), supDer = supDer(atras), infDer = infDer(atras)}
		derechaAux = Cara { supIzq = supDer(derecha), infIzq = supIzq(derecha), supDer = infDer(derecha), infDer = infIzq(derecha)}

darVueltaCuboArriba :: Cubo -> Cubo
darVueltaCuboArriba (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atrasAux izquierdaAux derechaAux)
	where
		arribaAux = Cara { supIzq = supIzq(abajo), infIzq = infIzq(abajo), supDer = supDer(abajo), infDer = infDer(abajo)}
		abajoAux = Cara { supIzq = supIzq(arriba), infIzq = infIzq(arriba), supDer = supDer(arriba), infDer = infDer(arriba)}
		frenteAux = Cara { supIzq = infDer(frente), infIzq = supDer(frente), supDer = infIzq(frente), infDer = supIzq(frente)}
		atrasAux = Cara { supIzq = infDer(atras), infIzq = supDer(atras), supDer = infIzq(atras), infDer = supIzq(atras)}
		izquierdaAux = Cara { supIzq = infDer(derecha), infIzq = supDer(derecha), supDer = infIzq(derecha), infDer = supIzq(derecha)}
		derechaAux = Cara { supIzq = infDer(izquierda), infIzq = supDer(izquierda), supDer = infIzq(izquierda), infDer = supIzq(izquierda)}
		
darVueltaCuboIzquierda :: Cubo -> Cubo
darVueltaCuboIzquierda (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atrasAux izquierdaAux derechaAux)
	where
		arribaAux = Cara { supIzq = infIzq(izquierda), infIzq = infDer(izquierda), supDer = supIzq(izquierda), infDer = supDer(izquierda)}
		abajoAux = Cara { supIzq = supIzq(derecha), infIzq = infIzq(derecha), supDer = supDer(derecha), infDer = infDer(derecha)}
		frenteAux = Cara { supIzq = infIzq(frente), infIzq = infDer(frente), supDer = supIzq(frente), infDer = supDer(frente)}
		atrasAux = Cara { supIzq = supDer(atras), infIzq = supIzq(atras), supDer = infDer(atras), infDer = infIzq(atras)}
		izquierdaAux = Cara { supIzq = supDer(abajo), infIzq = supIzq(abajo), supDer = infDer(abajo), infDer = infIzq(abajo)}
		derechaAux = Cara { supIzq = infIzq(arriba), infIzq = infDer(arriba), supDer = supIzq(arriba), infDer = supDer(arriba)}

darVueltaCuboDerecha :: Cubo -> Cubo
darVueltaCuboDerecha (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atrasAux izquierdaAux derechaAux)
	where
		arribaAux = Cara { supIzq = supDer(derecha), infIzq = supIzq(derecha), supDer = infDer(derecha), infDer = infIzq(derecha)}
		abajoAux = Cara { supIzq = infIzq(izquierda), infIzq = infDer(izquierda), supDer = supIzq(izquierda), infDer = supDer(izquierda)}
		frenteAux = Cara { supIzq = supDer(frente), infIzq = supIzq(frente), supDer = infDer(frente), infDer = infIzq(frente)}
		atrasAux = Cara{ supIzq = supDer(atras), infIzq = supIzq(atras), supDer = infDer(atras), infDer = infIzq(atras)}
		izquierdaAux = Cara { supIzq = supDer(arriba), infIzq = supIzq(arriba), supDer = infDer(arriba), infDer = infIzq(arriba)}
		derechaAux = Cara { supIzq = infIzq(abajo), infIzq = infDer(abajo), supDer = supIzq(abajo), infDer = supDer(abajo)}

darVueltaCuboFrente :: Cubo -> Cubo
darVueltaCuboFrente (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atrasAux izquierdaAux derechaAux)
	where
		arribaAux = Cara { supIzq = supIzq(frente), infIzq = infIzq(frente), supDer = supDer(frente), infDer = infDer(frente)}
		abajoAux = Cara { supIzq = supIzq(atras), infIzq = infIzq(atras), supDer = supDer(atras), infDer = infDer(atras)}
		frenteAux = Cara { supIzq = infDer(abajo), infIzq = supDer(abajo), supDer = infIzq(abajo), infDer = supIzq(abajo)}
		atrasAux = Cara{ supIzq = infDer(arriba), infIzq = supDer(arriba), supDer = infIzq(arriba), infDer = supIzq(arriba)}
		izquierdaAux = Cara { supIzq = supDer(izquierda), infIzq = supIzq(izquierda), supDer = infDer(izquierda), infDer = infIzq(izquierda)}
		derechaAux = Cara { supIzq = infIzq(derecha), infIzq = infDer(derecha), supDer = supIzq(derecha), infDer = supDer(derecha)}

darVueltaCuboAtras :: Cubo -> Cubo
darVueltaCuboAtras (Cubo arriba abajo frente atras izquierda derecha) = (Cubo arribaAux abajoAux frenteAux atrasAux izquierdaAux derechaAux)
	where
		arribaAux = Cara { supIzq = infDer(atras), infIzq = supDer(atras), supDer = infIzq(atras), infDer = supIzq(atras)}
		abajoAux = Cara { supIzq = infDer(frente), infIzq = supDer(frente), supDer = infIzq(frente), infDer = supIzq(frente)}
		frenteAux = Cara { supIzq = supIzq(arriba), infIzq = infIzq(arriba), supDer = supDer(arriba), infDer = infDer(arriba)}
		atrasAux = Cara{ supIzq = supIzq(abajo), infIzq = infIzq(abajo), supDer = supDer(abajo), infDer = infDer(abajo)}
		izquierdaAux = Cara { supIzq = infIzq(izquierda), infIzq = infDer(izquierda), supDer = supIzq(izquierda), infDer = supDer(izquierda)}
		derechaAux = Cara { supIzq = supDer(derecha), infIzq = supIzq(derecha), supDer = infDer(derecha), infDer = infIzq(derecha)}
		
aplicaMovimientos :: Cubo -> Movimientos -> Cubo
aplicaMovimientos cubo movimiento =
	case movimiento of
			ArribaHorario -> rotarArribaHorario cubo
			ArribaAntihorario -> rotarArribaAntihorario cubo
			AbajoHorario -> rotarAbajoHorario cubo
			AbajoAntihorario -> rotarAbajoAntihorario cubo
			FrenteHorario -> rotarFrenteHorario cubo
			FrenteAntihorario -> rotarFrenteAntihorario cubo
			AtrasHorario -> rotarAtrasHorario cubo
			AtrasAntihorario -> rotarAtrasAntihorario cubo
			IzquierdaHorario -> rotarIzquierdaHorario cubo
			IzquierdaAntihorario -> rotarIzquierdaAntihorario cubo
			DerechaHorario -> rotarDerechaHorario cubo
			DerechaAntihorario -> rotarDerechaAntihorario cubo
			VueltaArriba -> darVueltaCuboArriba cubo
			VueltaIzquierda -> darVueltaCuboIzquierda cubo
			VueltaDerecha -> darVueltaCuboDerecha cubo
			VueltaFrente -> darVueltaCuboFrente cubo
			VueltaAtras -> darVueltaCuboAtras cubo

aplicarListaMovimientos:: Cubo -> [Movimientos] -> Cubo
aplicarListaMovimientos cubo [] = cubo
aplicarListaMovimientos cubo [x] = aplicaMovimientos cubo x
aplicarListaMovimientos cubo (x:xs) = aplicarListaMovimientos (aplicaMovimientos cubo x)  xs

--Comprobar si esta resuelto
--Comprobar que dos piezas son iguales
instance Eq Pieza where
	(Pieza colorPieza) == (Pieza colorPiezaAux) = colorPieza == colorPiezaAux
	
--Comprobar que las caras son iguales
instance Eq Cara where
	(Cara supIzq infIzq supDer infDer) == (Cara supIzqAux infIzqAux supDerAux infDerAux) = supIzq == supIzqAux && infIzq == infIzqAux && supDer == supDerAux && infDer == infDerAux  

--Comprobar que un cubo es igual a otro
instance Eq Cubo where
	(Cubo arriba abajo frente atras izquierda derecha) == (Cubo arriba2 abajo2 frente2 atras2 izquierda2 derecha2) = arriba == arriba2 && abajo == abajo2 && frente == frente2 && atras == atras2 && izquierda == izquierda2 && derecha == derecha2
 
listaMovimientosCaraBlanca::[Movimientos]
listaMovimientosCaraBlanca = [ArribaHorario,DerechaHorario,IzquierdaAntihorario,AbajoAntihorario]

--Resolver la cara Blanca
siguientesCaraBlanca :: Cubo -> Arbol
siguientesCaraBlanca cubo = [(aplicaMovimientos cubo mov, mov) | mov <-listaMovimientosCaraBlanca]

resolverCaraBlanca :: Cubo -> [Movimientos] -> [[Movimientos]]
resolverCaraBlanca cuboInicio movimiento = anchura [(movimiento, cuboInicio)]
  where
    anchura [] = [] 
    anchura ((movimiento, cubo):xs)
      | all(==Blanco) (getColoresCara(getCaraArriba(cubo))) =  [movimiento] 
	  | all(==Blanco) (getColoresCara(getCaraAbajo(cubo))) = [movimiento++[VueltaArriba]]  
	  | all(==Blanco) (getColoresCara(getCaraIzquierda(cubo))) = [movimiento++[VueltaIzquierda]]  
	  | all(==Blanco) (getColoresCara(getCaraDerecha(cubo))) =  [movimiento++[VueltaDerecha]]  
	  | all(==Blanco) (getColoresCara(getCaraAtras(cubo))) =  [movimiento++[VueltaAtras]]  
	  | all(==Blanco) (getColoresCara(getCaraFrente(cubo))) =  [movimiento++[VueltaFrente]]  
      | otherwise =  anchura (xs++[(movimiento ++ [movimiento2], cuboSiguiente) | (cuboSiguiente, movimiento2) <- siguientesCaraBlanca cubo] )


--Resolver la cara amarilla
--Caso 1 Distintas rotaciones
caso1_1::[Movimientos]
caso1_1=[DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaHorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaAntihorario]

caso1_2::[Movimientos]
caso1_2=[ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaHorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaAntihorario]

caso1_3::[Movimientos]
caso1_3=[ArribaHorario,ArribaHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaHorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaAntihorario]

caso1_4::[Movimientos]
caso1_4=[ArribaHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaHorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaAntihorario]

--Caso 2 Distintas rotaciones
caso2_1::[Movimientos]
caso2_1=[DerechaHorario,ArribaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario]

caso2_2::[Movimientos]
caso2_2=[ArribaHorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario]

caso2_3::[Movimientos]
caso2_3=[ArribaHorario,ArribaHorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario]

caso2_4::[Movimientos]
caso2_4=[ArribaAntihorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario]


--Caso 3 Distintas rotaciones
caso3_1::[Movimientos]
caso3_1=[DerechaHorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario]

caso3_2::[Movimientos]
caso3_2=[ArribaHorario,DerechaHorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario]

--Caso 4 Distintas rotaciones
caso4_1::[Movimientos]
caso4_1=[FrenteHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,FrenteAntihorario]

caso4_2::[Movimientos]
caso4_2=[ArribaAntihorario,FrenteHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,FrenteAntihorario]

caso4_3::[Movimientos]
caso4_3=[ArribaHorario,FrenteHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,FrenteAntihorario]

caso4_4::[Movimientos]
caso4_4=[ArribaHorario,ArribaHorario,FrenteHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,FrenteAntihorario]

--Caso 5 Distintas rotaciones
caso5_1::[Movimientos]
caso5_1=[FrenteHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,FrenteAntihorario]

caso5_2::[Movimientos]
caso5_2=[ArribaHorario,FrenteHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,FrenteAntihorario]

caso5_3::[Movimientos]
caso5_3=[ArribaHorario,ArribaHorario,FrenteHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,FrenteAntihorario]

caso5_4::[Movimientos]
caso5_4=[ArribaAntihorario,FrenteHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,FrenteAntihorario]

--Caso 6 Distintas rotaciones
caso6_1::[Movimientos]
caso6_1=[DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,FrenteAntihorario]

caso6_2::[Movimientos]
caso6_2=[ArribaHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,FrenteAntihorario]

caso6_3::[Movimientos]
caso6_3=[ArribaHorario,ArribaHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,FrenteAntihorario]

caso6_4::[Movimientos]
caso6_4=[ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,FrenteAntihorario]

--Caso 7 Distintas rotaciones
caso7_1::[Movimientos]
caso7_1=[FrenteHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]

caso7_2::[Movimientos]
caso7_2=[ArribaAntihorario,FrenteHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]

caso7_3::[Movimientos]
caso7_3=[ArribaHorario,ArribaHorario,FrenteHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]

caso7_4::[Movimientos]
caso7_4=[ArribaHorario,FrenteHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]

girarHastaAlinear::Cubo->[Movimientos]
girarHastaAlinear cubo =
	if getColorPieza(infIzq(getCaraFrente(cubo)))==getColorPieza(supIzq(getCaraFrente(cubo)))  then []
	else [ArribaHorario]++(girarHastaAlinear(aplicaMovimientos cubo ArribaHorario))
	
resolverCaraAmarilla::Cubo->[Movimientos]
resolverCaraAmarilla (Cubo arriba abajo frente atras izquierda derecha)
	| getColorPieza(infIzq(arriba))==Amarillo && getColorPieza(supDer(derecha))==Amarillo && getColorPieza(supDer(atras))==Amarillo && getColorPieza(supDer(frente))==Amarillo = caso1_1
	| getColorPieza(supIzq(arriba))==Amarillo && getColorPieza(supDer(derecha))==Amarillo && getColorPieza(supDer(izquierda))==Amarillo && getColorPieza(supDer(frente))==Amarillo = caso1_2
	| getColorPieza(supDer(arriba))==Amarillo && getColorPieza(supDer(atras))==Amarillo && getColorPieza(supDer(izquierda))==Amarillo && getColorPieza(supDer(frente))==Amarillo = caso1_3
	| getColorPieza(infDer(arriba))==Amarillo && getColorPieza(supDer(atras))==Amarillo && getColorPieza(supDer(izquierda))==Amarillo && getColorPieza(supDer(derecha))==Amarillo = caso1_4
	| getColorPieza(supDer(arriba))==Amarillo && getColorPieza(supIzq(derecha))==Amarillo && getColorPieza(supIzq(izquierda))==Amarillo && getColorPieza(supIzq(frente))==Amarillo = caso2_1
	| getColorPieza(supIzq(arriba))==Amarillo && getColorPieza(supIzq(derecha))==Amarillo && getColorPieza(supIzq(atras))==Amarillo && getColorPieza(supIzq(frente))==Amarillo = caso2_2
	| getColorPieza(infIzq(arriba))==Amarillo && getColorPieza(supIzq(derecha))==Amarillo && getColorPieza(supIzq(izquierda))==Amarillo && getColorPieza(supIzq(atras))==Amarillo = caso2_3
	| getColorPieza(infDer(arriba))==Amarillo && getColorPieza(supIzq(frente))==Amarillo && getColorPieza(supIzq(izquierda))==Amarillo && getColorPieza(supIzq(atras))==Amarillo = caso2_4
	| getColorPieza(supDer(atras))==Amarillo && getColorPieza(supIzq(atras))==Amarillo && getColorPieza(supIzq(frente))==Amarillo && getColorPieza(supDer(frente))==Amarillo = caso3_1
	| getColorPieza(supDer(izquierda))==Amarillo && getColorPieza(supIzq(izquierda))==Amarillo && getColorPieza(supIzq(derecha))==Amarillo && getColorPieza(supDer(derecha))==Amarillo = caso3_2
	| getColorPieza(supDer(atras))==Amarillo && getColorPieza(supIzq(atras))==Amarillo && getColorPieza(supIzq(frente))==Amarillo && getColorPieza(supDer(frente))==Amarillo = caso3_1
	| getColorPieza(supDer(izquierda))==Amarillo && getColorPieza(supIzq(izquierda))==Amarillo && getColorPieza(supIzq(atras))==Amarillo && getColorPieza(supDer(frente))==Amarillo = caso4_1
	| getColorPieza(supDer(izquierda))==Amarillo && getColorPieza(supIzq(derecha))==Amarillo && getColorPieza(supIzq(atras))==Amarillo && getColorPieza(supDer(atras))==Amarillo = caso4_2
	| getColorPieza(supDer(derecha))==Amarillo && getColorPieza(supIzq(izquierda))==Amarillo && getColorPieza(supIzq(frente))==Amarillo && getColorPieza(supDer(frente))==Amarillo = caso4_3
	| getColorPieza(supDer(derecha))==Amarillo && getColorPieza(supIzq(derecha))==Amarillo && getColorPieza(supDer(atras))==Amarillo && getColorPieza(supIzq(frente))==Amarillo = caso4_4
	| getColorPieza(supDer(izquierda))==Amarillo && getColorPieza(supIzq(izquierda))==Amarillo && getColorPieza(supDer(arriba))==Amarillo && getColorPieza(infDer(arriba))==Amarillo = caso5_1
	| getColorPieza(supDer(frente))==Amarillo && getColorPieza(supIzq(frente))==Amarillo && getColorPieza(supIzq(arriba))==Amarillo && getColorPieza(supDer(arriba))==Amarillo = caso5_2
	| getColorPieza(supDer(derecha))==Amarillo && getColorPieza(supIzq(derecha))==Amarillo && getColorPieza(supIzq(arriba))==Amarillo && getColorPieza(infIzq(arriba))==Amarillo = caso5_3
	| getColorPieza(supDer(atras))==Amarillo && getColorPieza(supIzq(atras))==Amarillo && getColorPieza(infDer(arriba))==Amarillo && getColorPieza(infIzq(arriba))==Amarillo = caso5_4
	| getColorPieza(supDer(arriba))==Amarillo && getColorPieza(infDer(arriba))==Amarillo && getColorPieza(supDer(atras))==Amarillo && getColorPieza(supIzq(frente))==Amarillo = caso6_1
	| getColorPieza(supDer(arriba))==Amarillo && getColorPieza(supIzq(arriba))==Amarillo && getColorPieza(supDer(izquierda))==Amarillo && getColorPieza(supIzq(derecha))==Amarillo = caso6_2
	| getColorPieza(supIzq(arriba))==Amarillo && getColorPieza(infIzq(arriba))==Amarillo && getColorPieza(supIzq(atras))==Amarillo && getColorPieza(supDer(frente))==Amarillo = caso6_3
	| getColorPieza(infDer(arriba))==Amarillo && getColorPieza(infIzq(arriba))==Amarillo && getColorPieza(supDer(izquierda))==Amarillo && getColorPieza(supIzq(derecha))==Amarillo = caso6_4
	| getColorPieza(supIzq(arriba))==Amarillo && getColorPieza(infDer(arriba))==Amarillo && getColorPieza(supDer(derecha))==Amarillo && getColorPieza(supIzq(frente))==Amarillo = caso7_1
	| getColorPieza(supDer(arriba))==Amarillo && getColorPieza(infIzq(arriba))==Amarillo && getColorPieza(supDer(frente))==Amarillo && getColorPieza(supIzq(izquierda))==Amarillo = caso7_2
	| getColorPieza(supIzq(arriba))==Amarillo && getColorPieza(infDer(arriba))==Amarillo && getColorPieza(supDer(izquierda))==Amarillo && getColorPieza(supIzq(atras))==Amarillo = caso7_3
	| getColorPieza(supDer(arriba))==Amarillo && getColorPieza(infIzq(arriba))==Amarillo && getColorPieza(supDer(atras))==Amarillo && getColorPieza(supIzq(derecha))==Amarillo = caso7_4
	| otherwise = []

resolverCapasMedio::Cubo->[Movimientos]
resolverCapasMedio (Cubo arriba abajo frente atras izquierda derecha) =
	
	if getColorPieza(supIzq(frente))==getColorPieza(supDer(frente)) && getColorPieza(supIzq(derecha))==getColorPieza(supDer(derecha)) && getColorPieza(supIzq(izquierda))==getColorPieza(supDer(izquierda)) && getColorPieza(supIzq(atras))==getColorPieza(supDer(atras)) then 
		--Caso Resuelto+No resuelto
		if getColorPieza(infIzq(frente))/=getColorPieza(infDer(frente)) && getColorPieza(infIzq(derecha))/=getColorPieza(infDer(derecha)) && getColorPieza(infIzq(izquierda))/=getColorPieza(infDer(izquierda)) && getColorPieza(infIzq(atras))/=getColorPieza(infDer(atras)) then
			[VueltaArriba,FrenteHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,FrenteAntihorario]
		--Caso Resuelto+Bloque de dos
		else if getColorPieza(infIzq(derecha))==getColorPieza(infDer(derecha)) then [VueltaArriba,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]
		else if getColorPieza(infIzq(frente))==getColorPieza(infDer(frente)) then
			 [VueltaArriba,ArribaHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]
		else if getColorPieza(infIzq(izquierda))==getColorPieza(infDer(izquierda)) then
			 [VueltaArriba,ArribaHorario,ArribaHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]
		else if getColorPieza(infIzq(atras))==getColorPieza(infDer(atras)) then [VueltaArriba,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]
		else []
	else if getColorPieza(infIzq(frente))==getColorPieza(infDer(frente)) && getColorPieza(infIzq(derecha))==getColorPieza(infDer(derecha)) && getColorPieza(infIzq(izquierda))==getColorPieza(infDer(izquierda)) && getColorPieza(infIzq(atras))==getColorPieza(infDer(atras)) then 
		--Caso Resuelto+No resuelto
		if getColorPieza(supIzq(frente))/=getColorPieza(supDer(frente)) && getColorPieza(supIzq(derecha))/=getColorPieza(supDer(derecha)) && getColorPieza(supIzq(izquierda))/=getColorPieza(supDer(izquierda)) && getColorPieza(supIzq(atras))/=getColorPieza(supDer(atras)) then 
			[FrenteHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,FrenteAntihorario]
		--Caso Resuelto+Bloque de dos
		else if getColorPieza(supIzq(derecha))==getColorPieza(supDer(derecha)) then [ArribaHorario,ArribaHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]
		else if getColorPieza(supIzq(frente))==getColorPieza(supDer(frente)) then [ArribaHorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]
		else if getColorPieza(supIzq(izquierda))==getColorPieza(supDer(izquierda)) then [DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]
		else if getColorPieza(supIzq(atras))==getColorPieza(supDer(atras)) then [ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,ArribaAntihorario,DerechaAntihorario,FrenteHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaAntihorario,ArribaAntihorario,DerechaHorario,ArribaHorario,DerechaAntihorario,FrenteAntihorario]
		else []
	--Caso Bloque de dos + Bloque de dos
	else if getColorPieza(supIzq(frente))==getColorPieza(supDer(frente)) && getColorPieza(infIzq(frente))==getColorPieza(infDer(frente)) then [DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(supIzq(frente))==getColorPieza(supDer(frente)) && getColorPieza(infIzq(derecha))==getColorPieza(infDer(derecha)) then [AbajoAntihorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(supIzq(frente))==getColorPieza(supDer(frente)) && getColorPieza(infIzq(izquierda))==getColorPieza(infDer(izquierda)) then [AbajoHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(supIzq(frente))==getColorPieza(supDer(frente)) && getColorPieza(infIzq(atras))==getColorPieza(infDer(atras)) then [AbajoHorario,AbajoHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(infIzq(frente))==getColorPieza(infDer(frente)) && getColorPieza(supIzq(derecha))==getColorPieza(supDer(derecha)) then [ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(infIzq(frente))==getColorPieza(infDer(frente)) && getColorPieza(supIzq(izquierda))==getColorPieza(supDer(izquierda)) then [ArribaAntihorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(infIzq(frente))==getColorPieza(infDer(frente)) && getColorPieza(supIzq(atras))==getColorPieza(supDer(atras)) then [ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(supIzq(derecha))==getColorPieza(supDer(derecha)) && getColorPieza(infIzq(derecha))==getColorPieza(infDer(derecha)) then [ArribaHorario,AbajoHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(supIzq(derecha))==getColorPieza(supDer(derecha)) && getColorPieza(infIzq(izquierda))==getColorPieza(infDer(izquierda)) then [ArribaHorario,AbajoAntihorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(supIzq(derecha))==getColorPieza(supDer(derecha)) && getColorPieza(infIzq(atras))==getColorPieza(infDer(atras)) then [ArribaHorario,AbajoHorario,AbajoHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(infIzq(derecha))==getColorPieza(infDer(derecha)) && getColorPieza(supIzq(izquierda))==getColorPieza(supDer(izquierda)) then [ArribaAntihorario,AbajoHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(infIzq(derecha))==getColorPieza(infDer(derecha)) && getColorPieza(supIzq(atras))==getColorPieza(supDer(atras)) then 
		[ArribaAntihorario,ArribaAntihorario,AbajoHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(supIzq(izquierda))==getColorPieza(supDer(izquierda)) && getColorPieza(infIzq(izquierda))==getColorPieza(infDer(izquierda)) then [ArribaAntihorario,AbajoAntihorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(supIzq(izquierda))==getColorPieza(supDer(izquierda)) && getColorPieza(infIzq(atras))==getColorPieza(infDer(atras)) then [ArribaAntihorario,AbajoHorario,AbajoHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(infIzq(izquierda))==getColorPieza(infDer(izquierda)) && getColorPieza(supIzq(atras))==getColorPieza(supDer(atras)) then
		 [AbajoAntihorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	else if getColorPieza(supIzq(atras))==getColorPieza(supDer(atras)) && getColorPieza(infIzq(atras))==getColorPieza(infDer(atras)) then 
		[ArribaAntihorario,ArribaAntihorario,AbajoHorario,AbajoHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,AtrasHorario,AtrasHorario,ArribaHorario,ArribaHorario,DerechaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,DerechaHorario]
	--Caso Nada + Dos
	else if getColorPieza(supIzq(frente))/=getColorPieza(supDer(frente)) && getColorPieza(supIzq(derecha))/=getColorPieza(supDer(derecha)) && getColorPieza(supIzq(izquierda))/=getColorPieza(supDer(izquierda)) && getColorPieza(supIzq(atras))/=getColorPieza(supDer(atras)) then
		if getColorPieza(infIzq(derecha))==getColorPieza(infDer(derecha)) then [AbajoAntihorario,VueltaArriba,DerechaHorario,ArribaAntihorario,DerechaHorario,FrenteHorario,FrenteHorario,DerechaAntihorario,ArribaHorario,DerechaAntihorario]
		else if getColorPieza(infIzq(frente))==getColorPieza(infDer(frente)) then [VueltaArriba,DerechaHorario,ArribaAntihorario,DerechaHorario,FrenteHorario,FrenteHorario,DerechaAntihorario,ArribaHorario,DerechaAntihorario]
		else if getColorPieza(infIzq(izquierda))==getColorPieza(infDer(izquierda)) then [AbajoHorario,VueltaArriba,DerechaHorario,ArribaAntihorario,DerechaHorario,FrenteHorario,FrenteHorario,DerechaAntihorario,ArribaHorario,DerechaAntihorario]
		else if getColorPieza(infIzq(atras))==getColorPieza(infDer(atras)) then [AbajoAntihorario,AbajoAntihorario,VueltaArriba,DerechaHorario,ArribaAntihorario,DerechaHorario,FrenteHorario,FrenteHorario,DerechaAntihorario,ArribaHorario,DerechaAntihorario]
		else []
	else if getColorPieza(infIzq(frente))/=getColorPieza(infDer(frente)) && getColorPieza(infIzq(derecha))/=getColorPieza(infDer(derecha)) && getColorPieza(infIzq(izquierda))/=getColorPieza(infDer(izquierda)) && getColorPieza(infIzq(atras))/=getColorPieza(infDer(atras)) then 
		if getColorPieza(supIzq(derecha))==getColorPieza(supDer(derecha)) then [ArribaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,FrenteHorario,FrenteHorario,DerechaAntihorario,ArribaHorario,DerechaAntihorario]
		else if getColorPieza(supIzq(frente))==getColorPieza(supDer(frente)) then [DerechaHorario,ArribaAntihorario,DerechaHorario,FrenteHorario,FrenteHorario,DerechaAntihorario,ArribaHorario,DerechaAntihorario]
		else if getColorPieza(supIzq(izquierda))==getColorPieza(supDer(izquierda)) then [ArribaAntihorario,DerechaHorario,ArribaAntihorario,DerechaHorario,FrenteHorario,FrenteHorario,DerechaAntihorario,ArribaHorario,DerechaAntihorario]
		else if getColorPieza(supIzq(atras))==getColorPieza(supDer(atras)) then [ArribaHorario,ArribaHorario,DerechaHorario,ArribaAntihorario,DerechaHorario,FrenteHorario,FrenteHorario,DerechaAntihorario,ArribaHorario,DerechaAntihorario]
		else []
	else girarHastaAlinear((Cubo arriba abajo frente atras izquierda derecha))++[DerechaHorario,DerechaHorario,FrenteHorario,FrenteHorario,DerechaHorario,DerechaHorario]


proyecto :: Cubo -> IO()
proyecto cubo = do
	let movimientosResolverCaraBlanca = head(resolverCaraBlanca cubo [])++[VueltaArriba]
	let cuboCaraBlanca = aplicarListaMovimientos cubo movimientosResolverCaraBlanca
	putStrLn("Movimientos para resolver la cara blanca: ")
	putStrLn(show(movimientosResolverCaraBlanca))
	putStrLn(" ")
	putStrLn("Cubo despues de resolver la cara blanca y situarla abajo: ")
	representarCubo cuboCaraBlanca
	putStrLn(" ")
	putStrLn("Movimientos para resolver la cara amarilla(asegurarse que esta en la cara superior):")
	let movimientosResolverCaraAmarilla = (resolverCaraAmarilla cuboCaraBlanca)++[VueltaArriba]
	putStrLn(show(movimientosResolverCaraAmarilla))
	putStrLn(" ")
	let cuboCaraBlancaAmarilla = aplicarListaMovimientos cuboCaraBlanca movimientosResolverCaraAmarilla
	putStrLn("Cubo despues de resolver la cara amarilla y situarla debajo: ")
	representarCubo cuboCaraBlancaAmarilla
	putStrLn(" ")
	putStrLn("Movimientos para resolver las capas del medio:")
	let movimientosResolverCapasMedias = resolverCapasMedio cuboCaraBlancaAmarilla
	putStrLn(show(movimientosResolverCapasMedias))
	putStrLn(" ")
	let cuboRes = aplicarListaMovimientos cuboCaraBlancaAmarilla movimientosResolverCapasMedias
	putStrLn("Cubo despues de resolver la capas del medio ")
	representarCubo cuboRes
	let movimientosFinales = girarHastaAlinear(cuboRes)
	let cuboResFinal=aplicarListaMovimientos cuboRes movimientosFinales
	putStrLn(" ")
	putStrLn("Movimientos para alinear las caras: ")
	putStrLn(show(movimientosFinales))
	putStrLn(" ")
	putStrLn("Cubo resuelto final: ")
	representarCubo cuboResFinal 
	

	

	






 



