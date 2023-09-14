type NomeCidade = String
type Coordenadas = (Double, Double)
type Estradas = [NomeCidade]
type Cidade = (NomeCidade, Coordenadas, Estradas) --(String, (Int, Int),[String])
type Mapa = [Cidade]

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
-- adcEstrada :: NomeCidade -> Estradas -> NomeCidade
-- adcEstrada cidade estradas
--     | buscarCidade dados = 
--     | otherwise = adcEstrada cidade estradas
--     where mapa = loadMapa; dados = mapa!!0

-- buscarCidade :: Cidade -> Cidade -> (Cidade -> Bool)
-- buscarCidade cidadeAlvo (nome, coordenadas, estradas)
--     | cidadeAlvo == nome = (nome, True)
--     | otherwise = (nome, False)

encontraPosCidade :: Cidade -> Int
encontraPosCidade (cidade, coordenadas, estradas)
    | ps == [] = 0
    |otherwise = head ps
    where ps = [(cidade2, coordenadas2, estradas2) | (cidade2, coordenadas2, estradas2)<-loadMapa, (cidade2 == cidade && coordenadas2 == coordenadas && estradas2 == estradas)]

adcEstrada :: NomeCidade -> Estradas -> Int
adcEstrada cidade estradas = encontraPosCidade (snd (rodarMapa cidade mapa))
    where mapa = loadMapa

rodarMapa :: NomeCidade -> Mapa -> (Bool, Cidade)
rodarMapa cidadeAlvo (x:xs)
    | fst (buscarCidade cidadeAlvo x) = (True, x)
    | otherwise = rodarMapa cidadeAlvo xs

buscarCidade :: NomeCidade -> Cidade -> (Bool, Cidade)
buscarCidade cidadeAlvo (cidade, coordenadas, estradas)
    | cidadeAlvo == cidade = (True, (cidade, coordenadas, estradas))
    | otherwise = (False, (cidade, coordenadas, estradas))