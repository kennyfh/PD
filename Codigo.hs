module Codigo (letraNumero, numeroLetra) where

import Data.Char

letraNumero :: Char -> Int
letraNumero 'ñ' = 14
letraNumero l | l < 'a' || 'z' < l = error "letraNumero: sÃ³lo disponible para el alfabeto espaÃ±ol (minÃºsculas)."
              | l < 'o' = (ord l) - 97
              | otherwise = (ord l) - 96

numeroLetra :: Int -> Char
numeroLetra 14 = 'ñ'
numeroLetra n | n < 0 || 26 < n = error "numeroLetra: sÃ³lo hay 27 letras disponibles (numeradas de 0 a 26)."
              | n < 14 = chr (n + 97)
              | otherwise = chr (n + 96)
