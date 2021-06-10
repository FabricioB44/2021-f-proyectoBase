module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

--between n m x = elem x [n .. m]

--maximoSegun :: a -> a -> a -> a
--maximoSegun f = foldl1 (mayorSegun f)
--mayorSegun f a b
--  | f a > f b = a
--  | otherwise = b

--1)a)

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {velocidad = 10, precision = precisionJugador habilidad * 2, altura = 0}

madera :: Palo
madera habilidad = UnTiro {velocidad = 100, precision = precisionJugador habilidad / 2, altura = 5}

hierro :: Number -> Palo
hierro num habilidad = UnTiro {velocidad = fuerzaJugador habilidad * num, precision = precisionJugador habilidad / num, altura = (num - 3) `max` 0}

--1)b)

palos :: [Palo]
palos = [putter, madera] ++ map hierro[1..10]

--2)

type Golpe = Jugador -> Palo -> Tiro

golpe :: Golpe
golpe unJugador palo = UnTiro {velocidad = velocidad (palo (habilidad unJugador)), precision = precision (palo (habilidad unJugador)), altura = altura (palo (habilidad unJugador))}

--3)a)

type Obstaculo = Tiro -> Tiro

tiroDetenido :: Tiro
tiroDetenido = UnTiro {velocidad = 0, precision = 0, altura = 0}

tunel :: Obstaculo
tunel tiro
    |superaTunel tiro = tiroTunel tiro
    |otherwise = tiroDetenido

tiroTunel :: Tiro -> Tiro
tiroTunel tiro = UnTiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}

superaTunel :: Tiro -> Bool
superaTunel tiro = (precision tiro > 90) && (altura tiro == 0)

--3)b)

laguna :: Number -> Obstaculo
laguna num tiro
    |superaLaguna tiro = tiroLaguna num tiro
    |otherwise = tiroDetenido


tiroLaguna :: Number -> Tiro -> Tiro
tiroLaguna num tiro = UnTiro {velocidad = velocidad tiro, precision = precision tiro, altura = altura tiro / num}

superaLaguna :: Tiro -> Bool
superaLaguna tiro = (velocidad tiro > 80) && (altura tiro <= 5 && altura tiro >= 1)

--3)c)

hoyo :: Obstaculo
hoyo tiro
    |superaHoyo tiro = tiroHoyo
    |otherwise = tiroDetenido

tiroHoyo :: Tiro
tiroHoyo = tiroDetenido

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (velocidad tiro <= 20 && velocidad tiro >= 5) && (altura tiro == 0) && (precision tiro > 95)

--4)a)
type Palos = [Palo]

palosUtiles :: Jugador -> Obstaculo -> [Palo] -> Palos
palosUtiles unJugador obstaculo  = filter (paloSirveParaObstaculo unJugador obstaculo)

paloSirveParaObstaculo :: Jugador -> Obstaculo -> Palo -> Bool
paloSirveParaObstaculo unJugador obstaculo palo = not(obstaculo (golpe unJugador palo) == tiroDetenido)

--4)b)

type Obstaculos = [Obstaculo]

cuantosObstaculosSupera :: Obstaculos -> Tiro -> Number
cuantosObstaculosSupera obstaculos tiro = length (takeWhile (tiroSuperaObstaculo tiro) obstaculos)

tiroSuperaObstaculo :: Tiro -> Obstaculo -> Bool
tiroSuperaObstaculo tiro obstaculo = not(obstaculo tiro == tiroDetenido)

tiroEjemplo :: Tiro
tiroEjemplo = UnTiro {velocidad = 10, precision = 95, altura = 0}

--4)c)

--paloMasUtil :: Jugador -> Obstaculos -> [Palo] -> Palo
--paloMasUtil unJugador obstaculos = maximoSegun (cuantosObstaculosSupera obstaculos golpe unJugador)

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (t -> x) -> (t -> t -> t)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--5)

jugadorDeTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo
  = (map (padre.jugadorDeTorneo) . filter (not . gano puntosDeTorneo)) puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador
  = (all ((< puntosGanados puntosDeUnJugador).puntosGanados)
      . filter (/= puntosDeUnJugador)) puntosDeTorneo