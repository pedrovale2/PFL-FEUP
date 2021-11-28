import BigNumber
--Uma função recursiva, fibRec
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n-1) + fibRec (n-2)


-- versão otimizada da função anterior, fibListafib::Int->[Int]
fibListaux :: Integral b => b -> [b]
fibListaux n = map fibRec [0..n]

fibLista :: Int -> Int
fibLista n = fibListaux n !! n


--gerar uma lista infinita com todos os números de Fibonacci e retornar o elemento de ordem n,  fibListaInfinita
fibListaInfinitaaux :: [Integer]
fibListaInfinitaaux = 0 : 1 : zipWith (+) fibListaInfinitaaux (tail fibListaInfinitaaux)

fibListaInfinita :: Int -> Integer
fibListaInfinita a = fibListaInfinitaaux !! a

fibRecBN :: BigNumber -> BigNumber
fibRecBN [1] = [1]
fibRecBN [0] = [0]
fibRecBN a = somaBN (fibRecBN (subBN a [1])) (fibRecBN(subBN a [2]))


createBNList :: BigNumber -> [BigNumber]
createBNList [0] = [[0]]
createBNList a = [a] ++ createBNList (subBN a [1])


-- versão otimizada da função anterior, fibLista
fibListaBNaux :: BigNumber -> [BigNumber]
fibListaBNaux a = map fibRecBN (reverse (createBNList a))


fibListaBN :: BigNumber -> BigNumber
fibListaBN a = last (fibListaBNaux a)



fibListaInfinitaBNaux :: [BigNumber]
fibListaInfinitaBNaux = [0] : [1] : zipWith (somaBN) fibListaInfinitaBNaux (tail fibListaInfinitaBNaux)

fibListaInfinitaBN :: Int -> BigNumber
fibListaInfinitaBN n = fibListaInfinitaBNaux !! n

