import BigNumber
--Uma função recursiva, fibRec
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n-1) + fibRec (n-2)


-- versão otimizada da função anterior, fibLista
fibLista :: Integral a => Int -> [a] -> a
fibLista i lista = head(drop i [fibRec x|x<-lista])


--gerar uma lista infinita com todos os números de Fibonacci e retornar o elemento de ordem n,  fibListaInfinita
fibListaInfinita :: Integral a => Int -> a
fibListaInfinita i = head(drop i [fibRec x|x<-[0..]])


{--

-- EXERCICIO 3
-- Agora com BN
fibRecBN :: 
fibRecBN =


fibListaBN ::
fibListaBN = 


fibListaInfinitaBN ::
fibListaInfinitaBN =

--}