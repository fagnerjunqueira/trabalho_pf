type NomeCidade = String
type Coordenadas = (Int, Int)
type Estradas = [NomeCidade]
type Cidade = (NomeCidade, Coordenadas, Estradas) --(String, (Int, Int),[String])
type Mapa = [Cidade]

--Função sem entrada
mapa :: Mapa
mapa = []

--Adição de cidade ao mapa
adcCidade :: Cidade -> Mapa -> Mapa
adcCidade cidade0 mapa0 = cidade0:mapa0
