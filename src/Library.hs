module Library where
import PdePreludat


doble :: Number -> Number
doble numero = numero + numero


data Chofer = Chofer {
    nombreChofer :: String,
    kilometraje  :: Number,
    viajes       :: [Viaje],
    condicion    :: Condicion
} deriving (Show)

data Viaje = Viaje {
    cliente :: Cliente,
    fecha   :: (Number, Number, Number),
    costo   :: Number
} deriving (Show)

data Cliente = Cliente {
    nombreCliente :: NombreCliente,
    dondeVive     :: DondeViveCliete
} deriving (Show)

type Condicion = Viaje -> Bool
type NombreCliente = String
type DondeViveCliete = String

-- choferes toman cualquier viaje
cualquierViaje :: Condicion
cualquierViaje _ = True

-- solo toman los viajes que salgan más de $ 200
viajesDeMasDe200pe :: Condicion
viajesDeMasDe200pe = (> 200) . costo

-- viajes donde nombre del cliente tenga más de n letras
viajesClienteNletras :: Number -> Condicion
viajesClienteNletras cantLetrasCliente = (> cantLetrasCliente) . length . nombreCliente . cliente
                       --(tiene que tener mas de nLetras).(longitud).(primer paramatro).(del cliente)

-- viajes donde el cliente no viva en una zona determinada
viajesFueraDeZona :: String -> Condicion
viajesFueraDeZona zonaX = (/= zonaX) . dondeVive . cliente
            -- (dist a zonaX).(segundo paramatro).(del cliente)


-- Definir las siguientes expresiones: 
-- a. el cliente “Lucas” que vive en Victoria
lucas :: Cliente
lucas = Cliente {
    nombreCliente = "Lucas",
    dondeVive = "Victoria"
} -- lucas = Cliente "Lucas" "Victoria"


-- b. el chofer “Daniel”, su auto tiene 23.500 kms., hizo un viaje con el cliente 
-- Lucas el 20/04/2017 cuyo costo fue $ 150, y toma los viajes donde el 
-- cliente no viva en “Olivos”.
daniel :: Chofer
daniel = Chofer {
    nombreChofer = "Daniel",
    kilometraje = 23500,
    viajes = [Viaje {
        cliente = lucas,
        fecha = (20, 4, 2017),
        costo = 150
    }],
    condicion = viajesFueraDeZona "Olivos"
} -- dani = Chofer "Daniel" 23500 [Viaje (20, 4, 2017) lucas 150] (clienteNoViveEn "Olivos")

-- c. la chofer “Alejandra”, su auto tiene 180.000 kms, no hizo viajes y toma 
-- cualquier viaje
alejandra :: Chofer
alejandra = Chofer {
    nombreChofer = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condicion = cualquierViaje
} -- ale = Chofer "Alejandra" 180000 [] cualquierViaje



-- Saber si un chofer puede tomar un viaje.
choferPuedeTomarViaje :: Viaje -> Chofer -> Bool
choferPuedeTomarViaje viaje chofer = condicion chofer viaje

-- Saber la liquidación de un chofer (sumar los costos de cada uno de los viajes)
liquidacionChofer :: Chofer -> Number
liquidacionChofer = sum . map costo . viajes


-- Realizar un viaje: dado un viaje y una lista de choferes, se pide que
type RealizarViaje = Viaje -> [Chofer] -> [Chofer]

-- a. filtre los choferes que toman ese viaje. Si ningún chofer está interesado, no se preocupen: el viaje no se puede realizar.
realizarViaje :: RealizarViaje
realizarViaje viaje = filter $ choferPuedeTomarViaje viaje

-- b. considerar el chofer que menos viaje tenga. Si hay más de un chofer elegir cualquiera.
choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes (elQueMenosViajesHizo chofer1 chofer2:choferes)

elQueMenosViajesHizo :: Chofer -> Chofer -> Chofer
elQueMenosViajesHizo chofer1 chofer2
   | cuantosViajes chofer1 > cuantosViajes chofer2 = chofer2
   | otherwise                                     = chofer1

cuantosViajes = length . viajes

-- c. efectuar el viaje: esto debe incorporar el viaje a la lista de viajes del chofer. ¿Cómo logra representar este cambio de estado?
efectuarViaje :: RealizarViaje
efectuarViaje viaje = map (agregarViaje viaje)

agregarViaje :: Viaje -> Chofer -> Chofer
agregarViaje viaje chofer = chofer { viajes = viaje : viajes chofer }


--Al infinito y más allá
-- a. Modelar al chofer “Nito Infy”, su auto tiene 70.000 kms., que el 
-- 11/03/2017 hizo infinitos viajes de $ 50 con Lucas y toma cualquier 
-- viaje donde el cliente tenga al menos 3 letras. 
repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

nitoInfy :: Chofer
nitoInfy = Chofer {
    nombreChofer = "Nito Infy",
    kilometraje = 70000,
    viajes = repetirViaje Viaje {
        cliente = lucas,
        fecha = (11, 3, 2017),
        costo = 50
    },
    condicion = viajesClienteNletras 3
}
-- b. ¿Puede calcular la liquidación de Nito? Justifique.
-- No, Haskell no puede calcular la liquidacion de Nito porque liquidacionChofer sumar los costos de cada uno de los viajes,
-- si vamos a la definicion de 'sum' Recibe una lista de números y retorna la sumatoria de los mismos. 
-- si esta lista son los infinitos viajes de Nito, Haskell no podra converger a un resultado.

-- c. ¿Y saber si Nito puede tomar un viaje de Lucas de $ 500 el 2/5/2017? 
-- Nito no puede tomar el viaje de Lucas de $500 el 2/5/2017 porque la condicion de Nito es que el cliente tenga al menos 3 letras

-- Inferir el tipo de la función gōngnéng
gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
gongNeng arg1 arg2 arg3 =
     max arg1 . head . filter arg2 . map arg3
