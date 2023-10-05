import Mapa

getCidade :: Mapa -> Nome -> Cidade
getCidade [] _ = error "Não existe no mapa"
getCidade ((cidade, localizacao, rotas):xs) alvo
    | cidade == alvo = (cidade, localizacao, rotas)
    | otherwise = getCidade xs alvo

euclidiana :: Localizacao -> Localizacao -> Double
euclidiana (xa, ya) (xb, yb) = sqrt(p1 + p2)
    where p1 = (xa-xb)^2; p2 = (ya-yb)^2

-- Calcular distância Euclidiana
distEuclidiana :: Mapa -> Nome -> Nome -> IO ()
distEuclidiana mapa cidadeA cidadeB = do
    let (_, locA, _) = getCidade mapa cidadeA
    let (_, locB, _) = getCidade mapa cidadeB

    let distancia = euclidiana locA locB

    print distancia

-- Verifica se determinada cidade existe nas rotas de outra
possuiEstrada :: [String] -> Bool
possuiEstrada lista = if length lista > 0 then True else False

hasEstrada :: Mapa -> Nome -> Nome -> IO ()
hasEstrada mapa cidadeA cidadeB = do
    let (_, _, estradasB) = getCidade mapa cidadeB
    print (possuiEstrada (filter (==cidadeA) estradasB))
    
-- Função que retorna os nomes das cidades conectadas a uma cidade por uma estrada
buscarVizinhos :: Mapa -> Nome -> IO ()
buscarVizinhos mapa cidadeAlvo = do
    let (_, _, rotas) = getCidade mapa cidadeAlvo
    print rotas

verdade :: Nome -> Rotas -> Bool
verdade _ [] = False
verdade cidadeA (x:xs)
         | cidadeA == x = True
         | otherwise  = verdade cidadeA xs

testenovo :: Nome -> Rotas -> Bool
testenovo _ [] = False
testenovo cidadeA (z:zs)
  | cidadeA == z = True
  | otherwise  = testenovo cidadeA zs

testeverdade :: Mapa -> Nome -> Nome -> IO ()
testeverdade mapa cidadeA cidadeB = do
    let (_, _, estradasA) = getCidade mapa cidadeA
    let testada = testenovo cidadeB estradasA
    print testada

buscarVizinhos :: Mapa -> Nome -> IO ()
buscarVizinhos mapa cidadeAlvo = do
    let (_, _, rotas) = getCidade mapa cidadeAlvo
    print rotas

verdade :: Nome -> [String] -> Bool
verdade _ [] = False
verdade cidadeA estradas = cidadeA `elem` estradas

testenovo :: Nome -> [String] -> Bool
testenovo cidadeA estradas = cidadeA `elem` estradas

testeverdade :: Mapa -> Nome -> Nome -> IO ()
testeverdade mapa cidadeA cidadeB = do
    let (_, _, estradasA) = getCidade mapa cidadeA
    let testada = all (\estrada -> verdade cidadeB estradasA) estradasA
    print testada
-- Função que mostra as cidades que aparecem em uma rota entre duas cidades, se houver
--rotaComum :: Mapa -> Nome -> Nome -> IO()
--rotaComum mapa cidadeA cidadeB = do
--   let (_, _, estradasB) = getCidade mapa cidadeB
--   let (_, _, estradasA) = getCidade mapa 
