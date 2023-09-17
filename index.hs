import Mapa

adcEstrada :: IO ()
adcEstrada = do
    putStrLn "Informe o nome do arquivo de mapa:"
    loadMapa <- carregarMapa "teste.mapa"
    putStrLn "Informe a cidade que recebera as estradas:"
    cidadeQueRecebe <- getLine
    putStrLn "Digite as novas estradas separadas por espaco:"
    listaRotas <- getLine
    let listaFinalDeRotas = words listaRotas

    -- Adicionar estrada
    let colocarEstrada :: Mapa -> Mapa
        colocarEstrada [] = []
        colocarEstrada ((cidade, localizacao, rotas):xs)
            | rotas == [] = []
            | cidade == cidadeQueRecebe = [(cidade, localizacao, rotas++listaFinalDeRotas)] ++ colocarEstrada xs
            | otherwise = [(cidade, localizacao, rotas)] ++ colocarEstrada xs

    let novoMapa = colocarEstrada loadMapa

    salvarMapa novoMapa "saida.mapa"

    print novoMapa

rmvEstrada :: IO ()
rmvEstrada = do
    putStrLn "Informe o nome do arquivo de mapa:"
    loadMapa <- carregarMapa "teste.mapa"
    putStrLn "Informe a cidade que perdera a estrada:"
    cidadeAlvo <- getLine
    putStrLn "Digite a estrada que sera removida:"
    estradaQuePerde <- getLine

    let removido :: Rotas -> Rotas
        removido [] = []
        removido (rotas:xs)
            | rotas == [] = []
            | rotas == estradaQuePerde = removido xs
            | otherwise = removido xs ++ [rotas]

    let tirarEstrada :: Mapa -> Mapa
        tirarEstrada [] = []
        tirarEstrada ((cidade, localizacao, rotas):xs)
            | rotas == [] = []
            | cidade == cidadeAlvo = [(cidade, localizacao, (removido rotas))] ++ tirarEstrada xs
            | otherwise = [(cidade, localizacao, rotas)] ++ tirarEstrada xs

    let novoMapa = tirarEstrada loadMapa 

    salvarMapa novoMapa "saida.mapa"

    print novoMapa