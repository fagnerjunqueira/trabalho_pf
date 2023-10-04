import Mapa


showMapa :: Mapa
showMapa = [("",(0,0),[""])]

getCidade :: Mapa -> Nome -> Cidade
getCidade [] _ = error "Mapa vazio"
getCidade ((cidade, localizacao, rotas):xs) alvo
    | cidade == alvo = (cidade, localizacao, rotas)
    | otherwise = getCidade xs alvo

euclidiana :: Localizacao -> Localizacao -> Double
euclidiana (xa, ya) (xb, yb) = sqrt(p1 + p2)
    where p1 = (xa-xb)^2; p2 = (ya-yb)^2

possuiEstrada :: [String] -> Bool
possuiEstrada lista = if length lista > 0 then True else False
-- Verifica se determinada cidade existe nas rotas de outra
hasEstrada :: IO ()
hasEstrada = do
    putStrLn "Informe o nome do arquivo de mapa:"
    -- nomeArquivo <- getLine
    loadMapa <- carregarMapa "teste.mapa"
    putStrLn "Informe a primeira cidade:"
    let cidadeA = "Recife" -- getLine
    putStrLn "Informe a segunda cidade:"
    let cidadeB = "Sao Paulo" -- getLine

    let (_, _, estradasB) = getCidade loadMapa cidadeB

    print (possuiEstrada (filter (==cidadeA) estradasB))

-- Calcular distância Euclidiana
distEuclidiana :: IO ()
distEuclidiana = do
    putStrLn "Informe o nome do arquivo de mapa:"
    -- nomeArquivo <- getLine
    loadMapa <- carregarMapa "teste.mapa"
    putStrLn "Informe a primeira cidade:"
    let cidadeA = "Recife" -- getLine
    putStrLn "Informe a segunda cidade:"
    let cidadeB = "Sao Paulo" -- getLine

    let (_, locA, _) = getCidade loadMapa cidadeA
    let (_, locB, _) = getCidade loadMapa cidadeB

    let distancia = euclidiana locA locB

    print distancia

--Adicionar cidade
adcCidade :: IO ()
adcCidade = do
    putStrLn "Informe o nome do arquivo de mapa:"
    nomeArquivo <- getLine
    loadMapa <- carregarMapa nomeArquivo
    putStrLn "Informe a cidade que será adicionada:"
    cidadeNV <- getLine
    putStrLn "Informe as coordenadas da cidade no padrao '(Double,Double)':"
    localizacao <- getLine
    let nvlocalizacao = read localizacao :: (Double,Double) -- Convertendo String para (Double,Double)

    let adcCid :: [Cidade]
        adcCidade [] = []
        adcCid = [(cidadeNV, nvlocalizacao, [])]
    let novoMapa = loadMapa ++ adcCid
    salvarMapa novoMapa "saida.mapa"
    
    print novoMapa

adcEstrada :: IO ()
adcEstrada = do
    putStrLn "Informe o nome do arquivo de mapa:"
    nomeArquivo <- getLine
    loadMapa <- carregarMapa nomeArquivo
    putStrLn "Informe a cidade que recebera as estradas:"
    cidadeQueRecebe <- getLine
    putStrLn "Digite as novas estradas no padrao '[String]':"
    listaRotas <- getLine
    let listaFinalDeRotas = read listaRotas :: [String]

    -- Adicionar estrada
    let colocarEstrada :: Mapa -> Mapa
        colocarEstrada [] = []
        colocarEstrada ((cidade, localizacao, rotas):xs)
            | cidade == cidadeQueRecebe = [(cidade, localizacao, rotas++listaFinalDeRotas)] ++ colocarEstrada xs
            | otherwise = [(cidade, localizacao, rotas)] ++ colocarEstrada xs

    let novoMapa = colocarEstrada loadMapa

    salvarMapa novoMapa "saida.mapa"

    print novoMapa

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

rmvEstrada :: IO ()
rmvEstrada = do
    putStrLn "Informe o nome do arquivo de mapa:"
    nomeArquivo <- getLine
    loadMapa <- carregarMapa nomeArquivo
    putStrLn "Informe a cidade que perdera a estrada:"
    cidadeAlvo <- getLine
    putStrLn "Digite a estrada que sera removida:"
    estradaQuePerde <- getLine

    let novoMapa = tirarEstrada loadMapa cidadeAlvo estradaQuePerde

    salvarMapa novoMapa "saida.mapa"

    print novoMapa

rmvCidade :: IO ()
rmvCidade = do
    putStrLn "Informe o nome do arquivo de mapa:"
    nomeArquivo <- getLine
    loadMapa <- carregarMapa nomeArquivo
    putStrLn "Informe a cidade que será removida:"
    cidadeAlvo <- getLine

    let remover :: Mapa -> Mapa
        remover [] = []
        remover ((cidade, localizacao, estradas):xs)
            | [(cidade, localizacao, estradas)] == [] = []
            | cidade == cidadeAlvo = remover xs
            | otherwise = [(cidade, localizacao, estradas)] ++ remover xs
    
    let novoMapa = remover loadMapa 

    let tirarEstrada :: Rotas -> Rotas
        tirarEstrada [] = []
        tirarEstrada (rotas:xs)
            | rotas == [] = []
            | rotas == cidadeAlvo = tirarEstrada xs
            | otherwise = [rotas] ++ tirarEstrada xs

    let mapaFinal :: Mapa -> Mapa
        mapaFinal [] = []
        mapaFinal ((cidade , coordenadas, rotas):xs)
            | rotas == [] = []
            | otherwise = [(cidade, coordenadas, (tirarEstrada rotas))] ++ mapaFinal xs

    salvarMapa (mapaFinal novoMapa) "saida.mapa"

    print (mapaFinal novoMapa)

buscarVizinhos :: IO ()
buscarVizinhos = do
    putStrLn "Informe o nome do arquivo de mapa:"
    nomeArquivo <- getLine
    loadMapa <- carregarMapa nomeArquivo
    putStrLn "Cidade a ser pesquisada:"
    cidadeAlvo <- getLine
    
    let (_, _, rotas) = getCidade loadMapa cidadeAlvo

    print rotas


mostrarRotaEntreCidades :: Mapa -> Nome -> Nome -> IO ()
mostrarRotaEntreCidades mapa cidadeOrigem cidadeDestino = do
    let (_, _, rotasOrigem) = getCidade mapa cidadeOrigem
    let (_, _, rotasDestino) = getCidade mapa cidadeDestino

    let rotaComum = filter (`elem` rotasDestino) rotasOrigem

    if null rotaComum
        then putStrLn $ "Não há rota entre " ++ cidadeOrigem ++ " e " ++ cidadeDestino
        else putStrLn $ "Cidades na rota entre " ++ cidadeOrigem ++ " e " ++ cidadeDestino ++ ": " ++ show rotaComum