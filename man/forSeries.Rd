\name{forSeries}
\alias{forSeries}
\title{Recebe uma série de tempo e uma matriz de variáveis exógenas e computa previsões em rolling windows para vários modelos com a série em nível e log-variação}
\usage{
forSeries(y,X,Xlog=NULL,h=1,npred=8,alpha.sh=0,alpha.ew=0.95)
}


\arguments{
  \item{y}{Vetor contendo a série de tempo a ser prevista.}

  \item{X}{Matriz com variáveis exógenas}
  
  \item{Xlog}{Matriz de Variáveis exógenas que devem ser diferenciadas no modelo de log variação. }
  
  \item{h}{Horizonte de previsão. }
  
  \item{npred}{Número de rolling windows} 
  
  \item{alpha.sh}{Determina o tipo de shrinkage. 0 para Ridge, 1 para LASSO e valores entre 0 e 1 para elastic-net.}
  
  \item{alpha.ew}{Determina o peso dado para as últimas observações no EWMA.}
}

\value{
Esta função retorna uma lista contendo os seguintes itens:

  - logvar: previsão feita em cima da log-variação da série y já reconstruída. Matriz nprev x número de modelos.
  - nivel: Previsão feita em cima da série em nível. Matriz nprev x número de modelos.
  - real: valor realizado da série y nos períodos das janelas de previsão. 
}


\details{

IMPORTANTE: Esta função utiliza o pacote HDeconometrics disponível no github. Para baixa-lo:
library(devtools)
install_github("gabrielrvsc/HDeconometrics")
library(HDeconometrics)

IMPORTANTE: Se y tiver valores negativos a log-variação não será feita. 

Os modelos implementados até o momento são?
- AR
- ARX
- EWMA
- Shrinkage
- randomForest
}

\examples{


## == gerar dados quaisquer == ##

set.seed(123)
X=matrix(rnorm(1500),150,10)

erro=rnorm(150)
y=rep(20,150)
betas=(-1)^(1:10)

for(i in 2:150){
  y[i]=3+0.7*y[i-1]+t(X[i,]%*%betas)+erro[i]
}
y=y[-c(1:50)]; X=X[-c(1:50),]

## == utilizar função == ##
teste=forSeries(y,X,h=1,npred=10)
teste$logvar
teste$nivel
teste$real

}
