\name{mapRegion}
\alias{mapRegion}
\title{Colore o mapa brasileiro dividido em regiões.}
\usage{
mapRegion(matrizInput,titulo="",paleta="YlOrRd", borda="black", hachura="blue", densidade_hachura=10, angulo_hachura=45)
}


\arguments{
  \item{matrizInput}{A matriz de input com os dados. Na primeira coluna, ela deverá conter as siglas das regiões. A segunda coluna deverá conter os valores referentes a cada região.}

  \item{titulo}{Opcional. O titulo a ser mostrado no gráfico. O padrão é ficar sem título.}
  
  \item{paleta}{Opcional. A paleta de cores a ser utilizada para construir o degradê do mapa. A paleta deve constar na lista de paletas do pacote RColorBrewer. Para visualizar quais estão disponiveis, rode a função display.brewer.all().}
  
  \item{borda}{Opcional. Define a cor das bordas do mapa. O padrão é preto. Use FALSE para um mapa sem bordas.}
  
  \item{hachura}{Opcional. Define a cor da hachura que será utilizada no mapa para regiões sem dados disponíveis na matriz de input.}
  
  \item{densidade_hachura}{Opcional. Define a densidade das hachuras.}
  
  \item{angulo_hachura}{Opcional. Define o ângulo das hachuras.}
  
}

\value{
Esta função recebe como input uma matriz com um valor para cada região, e gera um mapa colorindo cada estado de acordo com o valor correspondente, utilizando um degradê.
Caso o usuário queira mapas a nivel de municipio ou estado, ver as funções coloreMapaMunicipios e coloreMapaEstados.
}

\examples{

exemploRegioes = read.csv("/RProjetos/Mapas/ExemploInputRegioes.csv", sep=",", stringsAsFactors = FALSE)
mapRegion(exemploRegioes, paleta="Blues", titulo="Exemplo 1 - Regioes Faltantes", angulo_hachura = 90, hachura="brown", densidade_hachura=20)
}

\author{
  Coded by Thiago Milagres, last updated on 01-11-2016.
  }
