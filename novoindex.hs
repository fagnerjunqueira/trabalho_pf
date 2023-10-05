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

-- Função que mostra as cidades que aparecem em uma rota entre duas cidades, se houver.
mostrarRotaEntreCidades :: Mapa -> Nome -> Nome -> IO ()
mostrarRotaEntreCidades mapa cidadeOrigem cidadeDestino = do
    let (_, _, rotasOrigem) = getCidade mapa cidadeOrigem
    let (_, _, rotasDestino) = getCidade mapa cidadeDestino

    let rotaComum = filter (`elem` rotasDestino) rotasOrigem

    if null rotaComum
        then putStrLn $ "Não há rota entre " ++ cidadeOrigem ++ " e " ++ cidadeDestino
        else putStrLn $ "Cidades na rota entre " ++ cidadeOrigem ++ " e " ++ cidadeDestino
