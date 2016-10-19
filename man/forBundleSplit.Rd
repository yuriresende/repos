\name{forBundleSplit}
\alias{forBundleSplit}
\title{Calcula previsões individuais com base em um rateio e uma previsão agregada.}
\usage{
forBundleSplit(real,rate,pred,npred,h)
}


\arguments{
  \item{real}{Matriz nprev x Nseries contendo o valores realizado das séries desagregadas.}

  \item{rate}{Matriz de rateio.}
  
  \item{pred}{Matriz de previsões onde cada coluna representa um modelo.}
  
  \item{npred}{Número de rolling windows.} 
  
  \item{h}{Horizonte de previsão. }

}

\value{
Esta função retorna uma lista contendo os seguintes itens:

  - indpred: lista contendo previsões individuais para cada modelo. Cada elemento da lista é uma matriz com as previsões das séries desagregadas para um modelo.
  - real: valor realizado das séries desagregadas nos períodos das janelas de previsão. 
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
rateio= forBundleMerge(dadosy)
y=rateio$agg.data # = serie agregada
rate=rateio$apportionment


## == fazer previsão == ##
previsao=forSeries(y,X,h=1,npred=10)

plvar=previsao$logvar
pniv=previsao$nivel
real=previsao$real

## == Calcular erros == ##
erros.agg=forErrorMeasures(pniv,real)

## == calcular previsoes das series desagregadas com base em rateio == ##
pred.all=forBundleSplit(tail(dadosy,10),rate,pniv,npred=10,h=1)
pred.all$indpred$AR
real.all=pred.all$real

## == calcular erros para cada modelo nas series desagregadas == ##
error.all.by.model=lapply(pred.all$indpred,error.measures,real=real.all)
error.all.by.model$LASSO


}
\author{
  Coded by Gabriel Vasconcelos, last updated on 01-11-2016.
  }
