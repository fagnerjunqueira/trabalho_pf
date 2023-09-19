# Relatório: Manipulação de Mapa em Haskell

Este relatório descreve um conjunto de funções em Haskell que permitem a manipulação de um mapa contendo informações sobre cidades e estradas. O código baseia-se em operações simples de adição e remoção de cidades e estradas no mapa.

### Equipe
- ALEFE FREIRE SANTOS
- HERNANDISON DA SILVA BISPO
- JOSE FAGNER SILVA JUNQUEIRA
- MATHEUS EVANGELISTA OLIVEIRA DA SILVA

### 1. Estrutura Básica do Mapa
A estrutura básica do mapa é uma lista de tuplas, onde cada tupla representa uma cidade. Cada cidade é composta por um nome, coordenadas (expressas como um par de valores Double) e uma lista de rotas.

```
type Mapa = [(Nome, Coordenadas, Rotas)]
type Coordenadas = (Double, Double)
type Rotas = [String]
```

Aqui, Nome representa o nome de uma cidade, Coordenadas são as coordenadas geográficas e Rotas são as estradas associadas à cidade.

### 2. Adição de Cidades (adcCidade :: IO ())
A função adcCidade permite a adição de uma cidade ao mapa. O usuário é solicitado a fornecer o nome da cidade, suas coordenadas e um arquivo de mapa. A cidade é então adicionada ao mapa existente.

- A função putStrLn é usada para exibir mensagens no console, solicitando informações ao usuário.
- getLine é usado para obter entrada do usuário.
- A função carregarMapa é chamada para carregar o mapa existente a partir de um arquivo.
- A nova cidade é criada e adicionada ao mapa existente.
- O novo mapa é salvo em um arquivo chamado "saida.mapa" e exibido no console.

### 3. Adição de Estradas a uma Cidade (adcEstrada :: IO ())
A função adcEstrada permite adicionar novas estradas a uma cidade existente no mapa. O usuário fornece o nome da cidade e uma lista de novas estradas.

- Assim como em adcCidade, o usuário é solicitado a fornecer informações.
- A função colocarEstrada é usada para adicionar as novas estradas à cidade especificada.
- A cidade é então atualizada no mapa existente, e o novo mapa é salvo e exibido no console.

### 4. Remoção de Estradas de uma Cidade (tirarEstrada :: Mapa -> Nome -> Nome -> Mapa)
A função tirarEstrada remove uma estrada especificada de uma cidade no mapa.

- A função percorre o mapa, encontra a cidade alvo e remove a estrada indicada.
- O novo mapa com a estrada removida é retornado.

### 5. Remoção de Cidades (rmvCidade :: IO ())
A função rmvCidade permite ao usuário remover uma cidade do mapa.

- Similar às funções anteriores, as informações são solicitadas ao usuário.
- A função remover é usada para remover a cidade alvo do mapa.
- A função tirarEstrada é chamada para remover a estrada associada à cidade que está sendo removida.
- O novo mapa é então salvo e exibido no console.

### Conclusão
Essas funções em Haskell permitem ao usuário interagir com um mapa, adicionando e removendo cidades e estradas de maneira simples e interativa. Elas utilizam conceitos básicos da linguagem, como entrada/saída, manipulação de listas e definição de tipos, proporcionando uma introdução inicial ao desenvolvimento de algoritmos em Haskell.

### Observação
- Sempre que um mapa for manipulado a saida dele será salva no arquivo ``saida.mapa``

## Requisitos

- Criação dos tipos de dados que representam os mapas, cidades, estradas, e o que mais julgarem
necessário
- Função sem entrada e que retorna um mapa vazio
- Função que, dado o mapa, o nome de uma cidade e as coordenadas da cidade, adiciona a cidade ao mapa
- Função que, dado o nome de uma cidade e um mapa, retorna o mapa sem a cidade (e, consequentemente,
remove as rotas que a envolvam)
- Função que adiciona uma estrada a um mapa
- Função que remove uma estrada de um mapa
- Função que carrega um mapa de um arquivo (vide Subseção 1.1)
- Função que salva um mapa em um arquivo (vide Subseção 1.1)
- Função que salva um mapa como uma imagem (será fornecido código de apoio)
