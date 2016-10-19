\name{forBundleMerge}
\alias{forBundleMerge}
\title{Agrega uma matriz onde cada coluna é uma série temporal e retorna a série agregada e a proporção de cada série no todo em cada período}
\usage{
forBundleMerge(data)
}


\arguments{
  \item{data}{Matriz de séries de tempo T x N.}
}

\value{
Esta função retorna uma lista contendo os seguintes itens:

  - agg.data: Série agregada
  - apportionment: Matrix T x N com a proporção de cada série no todo em cada período. 
}

\examples{

## == gera várias séries cujas variáveis dependentes são as mesmas mas os parâmetros e o erro variam.
set.seed(123)
X=matrix(rnorm(1500),150,10)

dadosy=matrix(NA,100,20)
for(j in 1:20){
  erro=rnorm(150)
  y=rep(20,150)
  for(i in 2:150){
    y[i]=10*j+0.7*y[i-1]+t(X[i,]%*%rnorm(10))+erro[i]
  }
  y=y[-c(1:50)]
  dadosy[,j]=y
}
X=X[-c(1:50),]

## == rateio == ##
rateio=forBundleMerge(dadosy)
rateio$agg.data
rateio$apportionment

}
