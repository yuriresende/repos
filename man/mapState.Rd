\name{mapState}
\alias{mapState}
\title{Colore o mapa brasileiro dividido em estados.}
\usage{
mapState(matrizInput,titulo="",paleta="YlOrRd", borda="black", hachura="blue", densidade_hachura=10, angulo_hachura=45)
}


\arguments{
  \item{matrizInput}{A matriz de input com os dados. Na primeira coluna, ela deverá conter as siglas dos estados. A segunda coluna deverá conter os valores referentes a cada estado.}

  \item{titulo}{Opcional. O titulo a ser mostrado no gráfico. O padrão é ficar sem título.}
  
  \item{paleta}{Opcional. A paleta de cores a ser utilizada para construir o degradê do mapa. A paleta deve constar na lista de paletas do pacote RColorBrewer. Para visualizar quais estão disponiveis, rode a função display.brewer.all().}
  
  \item{borda}{Opcional. Define a cor das bordas do mapa. O padrão é preto. Use FALSE para um mapa sem bordas.}
  
  \item{hachura}{Opcional. Define a cor da hachura que será utilizada no mapa para estados sem dados disponíveis na matriz de input.}
  
  \item{densidade_hachura}{Opcional. Define a densidade das hachuras.}
  
  \item{angulo_hachura}{Opcional. Define o ângulo das hachuras.}
  
}

\value{
Esta função recebe como input uma matriz com um valor para cada estado, e gera um mapa colorindo cada estado de acordo com o valor correspondente, utilizando um degradê.
Caso o usuário queira mapas a nivel de municipio ou região, ver as funções coloreMapaMunicipios e coloreMapaRegioes.
}

\examples{

exemploEstados = read.csv("/RProjetos/Mapas/ExemploInputEstados.csv", sep=",", stringsAsFactors = FALSE)
mapState(exemploEstados, titulo="Exemplo 1 - Estados Faltantes", borda="white")
mapState(exemploEstados, paleta="Reds", titulo="Exemplo 2 - Estados Faltantes")
}
