\name{mapMunicipality}
\alias{mapMunicipality}
\title{Colore o mapa brasileiro dividido em municípios.}
\usage{
mapMunicipality(matrizInput,titulo="",regiao="Brasil", paleta="YlOrRd", borda="black", hachura="blue", densidade_hachura=30, angulo_hachura=45)
}


\arguments{
  \item{matrizInput}{A matriz de input com os dados. Na primeira coluna, ela deverá conter os códigos de cada município. A segunda coluna deverá conter os valores referentes a cada município.}

  \item{titulo}{Opcional. O titulo a ser mostrado no gráfico. O padrão é ficar sem título.}
  
  \item{regiao}{Opcional. Representa a região a ser visualizada. O padrão, "Brasil", mostra o mapa do país inteiro, dividido em municípios. Caso se queira algo mais específico, pode ser escolhido um Estado, utilizando sua sigla, ou uma região, utilizando o próprio nome.}
  
  \item{paleta}{Opcional. A paleta de cores a ser utilizada para construir o degradê do mapa. A paleta deve constar na lista de paletas do pacote RColorBrewer. Para visualizar quais estão disponiveis, rode a função display.brewer.all().}
  
  \item{borda}{Opcional. Define a cor das bordas do mapa. O padrão é preto. Use FALSE para um mapa sem bordas.}
  
  \item{hachura}{Opcional. Define a cor da hachura que será utilizada no mapa para municípios sem dados disponíveis na matriz de input.}
  
  \item{densidade_hachura}{Opcional. Define a densidade das hachuras.}
  
  \item{angulo_hachura}{Opcional. Define o ângulo das hachuras.}
  
  
  
}

\value{
Esta função recebe como input uma matriz com um valor para cada município, e gera um mapa colorindo cada município de acordo com o valor correspondente, utilizando um degradê.
Caso o usuário queira mapas a nivel de estado ou região, ver as funções coloreMapaEstados e coloreMapaRegioes.
}

\examples{

exemploMunicipios = read.csv("/RProjetos/Mapas/ExemploInputMunicipios.csv", sep=",", stringsAsFactors = FALSE)
coloreMapaMunicipios(exemploMunicipios, regiao="Sul", titulo="Exemplo 1 - Sul (Sem borda)", borda=FALSE)
coloreMapaMunicipios(exemploMunicipios, regiao="MG", paleta="RdBu", titulo="Exemplo 2 - Minas Gerais (Borda cinza)", borda="grey")
coloreMapaMunicipios(exemploMunicipios, titulo="Exemplo 3 - RJ", paleta="Greens", regiao="RJ", borda="white")
coloreMapaMunicipios(exemploMunicipios, titulo="Exemplo 4 - Geral", borda=FALSE)
}
