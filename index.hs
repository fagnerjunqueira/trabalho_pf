type NomeCidade = String
type Coordenadas = (Int, Int)
type Estradas = [NomeCidade]
type Cidade = (NomeCidade, Coordenadas, Estradas) --(String, (Int, Int),[String])
type Mapa = [Cidade]