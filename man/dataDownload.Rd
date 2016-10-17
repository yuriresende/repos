\name{dataDownload}
\alias{dataDownload}
\title{Baixa dados do Oracle para o R gerando querys automaticamente.}
\usage{
data.download(variables, type=c("cia","cd","skus","opened-stores"), SKUS=NULL, depart=NULL)
}


\arguments{
  \item{variables}{vetor de string indicando as variáveis a serem baixadas do Oracle, inclusive a operação. Por exemplo, para baixar a 
  variável VDA_BRT na agregação de compania, deve-se considerar qual a operação agregadora, que pode ser SUM(VDA_BRT) ou AVG(VDA_BRT). Mais detalhes no item Details.}

  \item{type}{string "cia" para agregação de compania, "cd" para agregação de centros de distribuição, "skus" para montar cubos com as SKUS desejadas e "opened-stores" para indicadores de loja aberta e atividade (quantidade vendida).}
  
  \item{SKUS}{Vetor de string com os SKUS desejado, apenas se type="skus".}
  
  \item{depart}{Vetor de string com departamentos desejados. Apenas se type=c("cia","cd","opened-stores")} 
}

\value{
Esta função retorna uma lista contendo cubos ou matrizes da seguinte forma:

  - Se type="skus" e mais de um SKU for selecionado, ou se type="cd", a função retorna uma lista contendo cubos.
  - Se type="cia" ou type="skus" com apenas um SKU, a função retorna uma lista contendo matrizes.
  - Se type="opened-stores" a função retorna uma lista com duas matrizes, uma de loja aberta e outra de atividade. 
  - IMPORTANTE: as listas de matrizes ou cubos para todos os tipos menos type="opened-stores" possuem uma matriz/cubo para cada variável do vetor varables. 
}


\details{

Operações possíveis no banco de dados:
- AVG()
- SUM()

Variáveis dispoíveis no banco:
- EST_VAL
- EST_QTD
- VDA
- VDA_QTD
- MARGEM_LIQ
- PRECO
- EST_TRAN
- VDA_BRT
- CMV

IMPORTANTE: Mesmo no nível desagregado, ou seja, se type="skus" o Oracle requer alguma operação nas variáveis, mesmo que a operação seja redundante. 
}

\examples{
######Definir argumentos######
##############################
variables=c("SUM(VDA_QTD)","SUM(EST_QTD)")
SKUS=c("4040347","4040346")
depart=c("D040")

############# type=cia ############
teste1=data.download(variables=variables, type="cia", depart=depart)

############# type=cd #############
teste2=data.download(variables=variables, dates, type="cd", depart=depart)

############# type=skus #############
teste3=data.download(variables=variables, type="skus", SKUS=SKUS)

############# type=opened-stores #############
teste1=data.download(type="opened-stores", depart=depart)



}
