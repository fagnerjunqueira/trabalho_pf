import Mapa
import Data.Typeable
import Control.Monad

main = do
    mapaCompleto <- carregarMapa "teste.mapa"
    print (mapaCompleto)

    -- Adicionar estrada
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