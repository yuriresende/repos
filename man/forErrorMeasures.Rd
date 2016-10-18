\name{forErrorMeasures}
\alias{forErrorMeasures}
\title{Recebe uma matriz contendo previsões de diversos modelos e o valor realizado da série e computa as principais medidas de erro.}
\usage{
forErrorMeasures(pred,real)
}


\arguments{
  \item{pred}{Matriz npred x nmodels ou vetor npred contendo as previsões}

  \item{real}{vetor de tamanho npred com os valores realizados da série no período das previsões. Tambem recebe uma matriz de valores realizados da mesma dimensão que a matriz pred. Neste caso, cada coluna será o realizado referente a uma previsão da matriz pred.}
}

\value{
Esta função retorna uma matrix onde cada linha é uma medida de erro e cada coluna uma previsao recebida na matriz pred.

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
y=rateio$agg.data # = serie agregada
rate=rateio$apportionment


## == fazer previsão == ##
previsao=forSeries(y,X,h=1,npred=10)

plvar=previsao$logvar
pniv=previsao$nivel
real=previsao$real

## == Calcular erros == ##
erros.agg=forErrorMeasures(pniv,real)

}
