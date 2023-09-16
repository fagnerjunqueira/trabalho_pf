import Mapa

loadMapa :: Mapa
loadMapa = [("Aracaju", (10.0,11.0), ["Recife", "Maceio"]), ("Maceio", (10.0,11.0), ["Recife", "Aracaju"]), ("Recife", (10.0,11.0), ["Aracaju", "Maceio"])]

-- Função mapa vazio
exibirMapa :: Mapa
exibirMapa = []

adcCidade :: Cidade -> Cidade
adcCidade cidade = cidade

rmvCidade :: Cidade -> Cidade
rmvCidade cidade = cidade

-- Adicionar estrada
salvarEstrada :: Cidade -> Rotas -> Cidade
salvarEstrada (cidade, localizacao, rotas) novaEstrada
    | ps == [] = ("",(0,0),[""])
    |otherwise = head ps
    where ps = [(cidade2, localizacao2, rotas2++novaEstrada) | (cidade2, localizacao2, rotas2)<-loadMapa, (cidade2 == cidade && localizacao2 == localizacao && rotas2 == rotas)]

adcEstrada :: Nome -> Rotas -> Cidade
adcEstrada cidade rotas = salvarEstrada (snd (rodarMapa cidade mapa)) rotas
    where mapa = loadMapa

rodarMapa :: Nome -> Mapa -> (Bool, Cidade)
rodarMapa cidadeAlvo (x:xs)
    | fst (buscarCidade cidadeAlvo x) = (True, x)
    | otherwise = rodarMapa cidadeAlvo xs

buscarCidade :: Nome -> Cidade -> (Bool, Cidade)
buscarCidade cidadeAlvo (cidade, localizacao, rotas)
    | cidadeAlvo == cidade = (True, (cidade, localizacao, rotas))
    | otherwise = (False, (cidade, localizacao, rotas))
