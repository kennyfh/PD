notasParciales = [3, 1, 5]
notasAcumuladas = scanl1 (\x y -> max y (x*0.4+y*0.6)) notasParciales
notaFinal = last (notasAcumuladas)


