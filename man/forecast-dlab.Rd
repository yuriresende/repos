\name{forecast.dlab}
\alias{forecast.dlab}
\title{Recebe uma série de tempo e uma matriz de variáveis exógenas e computa previsões em rolling windows para vários modelos com a série em nível e log-variação}
\usage{
forecast.dlab(y,X,h=1,npred=8,alpha.sh=0,alpha.ew=0.95)
}


\arguments{
  \item{y}{Vetor contendo a série de tempo a ser prevista.}

  \item{X}{Matriz com variáveis exógenas}
  
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



############# type=cia ############
teste1=data.download(variables=variables, type="cia", depart=depart)

############# type=cd #############
teste2=data.download(variables=variables, dates, type="cd", depart=depart)

############# type=skus #############
teste3=data.download(variables=variables, type="skus", SKUS=SKUS)

############# type=opened-stores #############
teste1=data.download(type="opened-stores", depart=depart)



}
