module Library where

import PdePreludat

type Durabilidad = Number

type Escudo = Number

type Ataque = Number

type Poder = Nave -> Nave

-- PUNTO 1: MODELADO DE NAVES

data Nave = UnaNave
  { nombre :: String,
    durabilidad :: Durabilidad,
    escudo :: Escudo,
    ataque :: Ataque,
    poder :: Poder
  }
  deriving (Show, Eq)

tieFighter :: Nave
tieFighter = UnaNave "TIE Fighter" 200 100 50 turbo

xWing :: Nave
xWing = UnaNave "X Wing" 300 150 100 reparacionEmergencia

naveDarthVader :: Nave
naveDarthVader = UnaNave "Nave Darth Vader" 500 300 200 superTurbo

millenniumFalcon :: Nave
millenniumFalcon = UnaNave "Millennium Falcon" 1000 500 50 (reparacionEmergencia . incrementarEscudo 100)

turbo :: Poder
turbo = aumentarAtaque (25)

aumentarAtaque :: Number -> Nave -> Nave
aumentarAtaque n nave = nave {ataque = ataque nave + n}

reparacionEmergencia :: Poder
reparacionEmergencia = incrementarEscudo 50 . aumentarAtaque (-30)

incrementarEscudo :: Number -> Nave -> Nave
incrementarEscudo n nave = nave {escudo = escudo nave + n}

superTurbo :: Poder
superTurbo = daño (-45) . turbo . turbo . turbo

daño :: Number -> Nave -> Nave
daño n nave = nave {durabilidad = durabilidad nave + n}

-- PUNTO 2:
type Flota = [Nave]

durabilidadTotal :: Flota -> Durabilidad
durabilidadTotal naves = foldr ((+) . durabilidad) 0 naves

-- PUNTO 3:

estadoNaveAtacada :: Nave -> Nave -> Nave
estadoNaveAtacada atacante atacada = atacar (potenciar atacante) (potenciar atacada)

atacar :: Nave -> Nave -> Nave
atacar atacante atacada = daño (dañar atacante atacada) atacada

dañar :: Nave -> Nave -> Number
dañar atacante atacada = restaNoNegativa (ataque atacante) (ataque atacada)

restaNoNegativa :: Number -> Number -> Number
restaNoNegativa pri seg = max 0 (pri - seg)

potenciar :: Poder
potenciar nave = (poder nave) nave

-- PUNTO 4:

fueraDeCombate :: Nave -> Bool
fueraDeCombate nave = durabilidad nave == 0

-- PUNTO 5:

type Estrategia = Nave -> Bool

debil :: Estrategia
debil nave = escudo nave < 200

peligrosidad :: Number -> Estrategia
peligrosidad n nave = ataque nave > n

naveFueraDeCombate :: Nave -> Estrategia
naveFueraDeCombate atacante = fueraDeCombate . estadoNaveAtacada atacante

misionSorpresa :: Estrategia -> Nave -> Flota -> Flota
misionSorpresa estrategia nave = seleccionNave (atacar nave) estrategia

seleccionNave :: (a -> a) -> (a -> Bool) -> [a] -> [a]
seleccionNave cambio condicion lista = map cambio (filter condicion lista) ++ filter (not . condicion) lista

-- PUNTO 6:

mejorEstrategia :: Estrategia -> Estrategia -> Nave -> Flota -> Estrategia
mejorEstrategia estrategia1 estrategia2 nave flota
  | durabilidadTotal (misionSorpresa estrategia1 nave flota) < durabilidadTotal (misionSorpresa estrategia2 nave flota) = estrategia1
  | otherwise = estrategia2

-- PUNTO 7:

infinitasNaves :: Flota
infinitasNaves = xWing : infinitasNaves