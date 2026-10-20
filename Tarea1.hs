-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Carlos Bisogno
--  Carnet: 22-10041
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Gemini, mayormente al buscar información en google sobre el uso de funciones,
--  la sintaxis correcta y como manejar tipos de datos. Una vez completaba el
--  problema y verificaba que cubriera lo requerido lo confirmaba con la IA.
--  Ninguna otra IA usada, 
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)
import Control.Monad.Accum (MonadAccum(accum))
import Text.XHtml (h2)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo x = 
    let limpio = minus (miFilt x)
    in limpio == invertir limpio

invertir :: String -> String
invertir [] = ""
invertir (x:xs) = invertir xs ++ [x]

minus :: String -> String
minus [] = ""
minus (x:xs) = toLower x : minus xs

miFilt :: String -> String
miFilt [] = ""
miFilt (x:xs)
  | isAlphaNum x = x : miFilt xs
  | otherwise    = miFilt xs

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- El primer elemento se guarda como mayor y menor, y se llama a la función recursiva para los elementos
-- restantes, si es el nuevo máximo se cambia y volvemos a buscar en los elementos restantes, de forma
-- análoga para el mínimo. Si no hay cambios se sigue buscando en los elementos restantes.
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) =
  let t = (x,x)
  in Just (reMayorYMenor xs t)

reMayorYMenor :: Ord a => [a] -> (a,a) -> (a,a)
reMayorYMenor [] (x,y) = (x,y)
reMayorYMenor (x:xs) (r,t)
  | x > r     = reMayorYMenor xs (x,t)
  | x < t     = reMayorYMenor xs (r,x)
  | otherwise = reMayorYMenor xs (r,t)

-- A3

leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
  | hora > 23    = Left "hora fuera de rango"
  | minutos > 59 = Left "minutos fuera de rango"
  | otherwise    = Right (hora, minutos)
  where
    hora = read [a,b] :: Int
    minutos = read [c,d] :: Int
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion th1 th2 =
  case leerHora th1 of
    Left err1 -> Left err1
    Right (h1,m1) ->
      case leerHora th2 of
        Left err2 -> Left err2
        Right (h2,m2) ->
          let t1 = toMin (h1,m1)
              t2 = toMin (h2,m2)
          in if t2 > t1
             then Right (t2-t1)
             else Right (t2 + 1440 - t1)
          

toMin :: (Int,Int) -> Int
toMin (h,m) = 60 * h + m

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f = foldr (\x acc -> f x : acc) []

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter p = foldr (\x acc -> if p x then x : acc else acc) []

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl f z xs = foldr (\x g acc -> g (f acc x)) id xs z
