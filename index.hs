import Mapa

showMapa :: Mapa
showMapa = [("",(0,0),[""])]

--Função mara mostrar opções e seus passos (Opção 1)
help :: IO ()
help = do
    putStrLn "\nA seguir, mostraremos como criar e manipular um mapa com as funções:"
    putStrLn "A princípio, precisaremos de um mapa inicial, vazio e chamado teste.mapa, para criar um outro mapa, chamado saida.mapa."
    putStrLn "Depois, para manipular o mapa criado, sempre que solicitado o nome do arquivo do mapa, insira o mapa chamado saida.mapa.\n"
    putStrLn "1.showMapa. Função que mostra um mapa vazio.\n2.adcCidade. Função que adiciona uma cidade ao mapa."
    putStrLn "3.adcEstrada. Função que adiciona estradas às cidades existentes no mapa."
    putStrLn "4.rmvEstrada. Função que remove estrada de uma cidade existente no mapa."
    putStrLn "5.rmvCidade. Função que remove cidade existente no mapa.\n"
    putStrLn "Portanto, digite a função desejada, respeitando que o mapa está vazio e pretende criá-lo, para depois manipulá-lo:"
--Adicionar cidade
adcCidade :: IO ()
adcCidade = do
    putStrLn "Informe o nome do arquivo de mapa:"
    nomeArquivo <- getLine
    loadMapa <- carregarMapa nomeArquivo
    putStrLn "Informe a cidade que será adicionada:"
    cidadeNV <- getLine
    putStrLn "Informe as coordenadas da cidade no padrao '(x.x,y.y)':"
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
    putStrLn "Digite as novas estradas separadas por espaco:"
    listaRotas <- getLine
    let listaFinalDeRotas = words listaRotas

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
    putStrLn "Cidade A ser Pesquisada:"
    cidadeAlvo <- getLine
    
    let vizinhos :: Nome -> Mapa -> [Nome]
        vizinhos cidadeAlvo mapa = concatMap rotasCidades mapa
            where
                rotasCidades :: Cidade -> [Nome]
                rotasCidades (_, _, rotas) = filter (/= cidadeAlvo) rotas
                   
    let vizinhosDaCidade = vizinhos cidadeAlvo loadMapa
    putStrLn "Cidades vizinhas:"
    mapM_ putStrLn vizinhosDaCidade
    