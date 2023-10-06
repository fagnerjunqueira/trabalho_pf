import Mapa

showMapa :: Mapa
showMapa = []

--Valida se cidade ja existe
cidadeJaCidade :: Mapa -> Nome -> Bool
cidadeJaCidade mapa cidade = any (\(nome, _, _) -> nome == cidade) mapa

--Valida se coordanada ja existe
coordenadaJaExiste :: Mapa -> Localizacao -> Bool
coordenadaJaExiste mapa localizacao = any (\(_, local, _) -> local == localizacao) mapa

--Adicionar cidade'
adcCidade :: Mapa -> Nome -> Localizacao -> IO ()
adcCidade mapa cidadeNV localizacao = do
    let valida :: Bool -> Bool -> String
        valida True _ = error "Cidade já existe"
        valida _ True = error "Coordenada já existe"
        valida _ _ = "Cidade adicionada"
    
    print (valida (cidadeJaCidade mapa cidadeNV) (coordenadaJaExiste mapa localizacao))

    let adcCid = [(cidadeNV, localizacao, [])]
    let novoMapa = mapa ++ adcCid

    salvarMapa novoMapa "saida.mapa"

    print novoMapa

colocarEstrada :: Nome -> Mapa -> Rotas -> Mapa
colocarEstrada _ [] _ = []
colocarEstrada cidadeQueRecebe ((cidade, localizacao, rotas):xs) novasRotas
    | cidade == cidadeQueRecebe = [(cidade, localizacao, rotas++novasRotas)] ++ colocarEstrada cidadeQueRecebe xs novasRotas
    | otherwise = [(cidade, localizacao, rotas)] ++ colocarEstrada cidadeQueRecebe xs novasRotas

adcEstrada :: Mapa -> Nome -> Rotas -> IO ()
adcEstrada mapa cidadeQueRecebe novasRotas = do
    
    -- Adicionar estrada
    let newMapa = colocarEstrada cidadeQueRecebe mapa novasRotas

    salvarMapa newMapa "saida.mapa"

    print newMapa

removido :: Rotas -> Nome -> Rotas
removido [] _ = []
removido (rotas:xs) estradaQuePerde
    | rotas == [] = []
    | rotas == estradaQuePerde = removido xs estradaQuePerde
    | otherwise = removido xs estradaQuePerde ++ [rotas]

tirarEstrada :: Mapa -> Nome -> Nome -> Mapa
tirarEstrada [] _ _ = []
tirarEstrada ((cidade, localizacao, rotas):xs) cidadeAlvo estradaQuePerde
    | rotas == [] = []
    | cidade == cidadeAlvo = [(cidade, localizacao, (removido rotas estradaQuePerde))] ++ tirarEstrada xs cidadeAlvo estradaQuePerde
    | otherwise = [(cidade, localizacao, rotas)] ++ tirarEstrada xs cidadeAlvo estradaQuePerde

rmvEstrada :: Mapa -> Nome -> Nome -> IO ()
rmvEstrada mapa cidadeAlvo estradaQuePerde = do

    let novoMapa = tirarEstrada mapa cidadeAlvo estradaQuePerde

    salvarMapa novoMapa "saida.mapa"

    print novoMapa

rmvCidade :: Mapa -> Nome -> IO ()
rmvCidade mapa cidadeAlvo = do

    let remover :: Mapa -> Mapa
        remover [] = []
        remover ((cidade, localizacao, estradas):xs)
            | [(cidade, localizacao, estradas)] == [] = []
            | cidade == cidadeAlvo = remover xs
            | otherwise = [(cidade, localizacao, estradas)] ++ remover xs
    
    let novoMapa = remover mapa 

    let loopRotas :: Rotas -> Rotas
        loopRotas [] = []
        loopRotas (x:xs)
            | x == [] = []
            | x == cidadeAlvo = loopRotas xs
            | otherwise = [x] ++ loopRotas xs

    let tiraCidade :: Cidade -> Cidade
        tiraCidade (cidade,localizacao,estradas) = (cidade,localizacao,novasEstradas)
            where novasEstradas = loopRotas estradas

    let removerCidadeDasRotas :: Mapa
        removerCidadeDasRotas = [tiraCidade (cidade,localizacao,estradas) | (cidade,localizacao,estradas) <- novoMapa]

    let novoMapa = removerCidadeDasRotas
    
    salvarMapa novoMapa "saida.mapa"
    print novoMapa

mostrarRotaEntreCidades :: Mapa -> Nome -> Nome -> IO ()
mostrarRotaEntreCidades mapa cidadeOrigem cidadeDestino = do
    let (_, _, rotasOrigem) = getCidade mapa cidadeOrigem
    let (_, _, rotasDestino) = getCidade mapa cidadeDestino

    let rotaComum = filter (`elem` rotasDestino) rotasOrigem

    if null rotaComum
        then putStrLn $ "Não há rota entre " ++ cidadeOrigem ++ " e " ++ cidadeDestino
        else putStrLn $ "Cidades na rota entre " ++ cidadeOrigem ++ " e " ++ cidadeDestino ++ ": " ++ show rotaComum

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