\name{dataDownloadPrice}
\alias{dataDownloadPrice}
\title{Baixa a base de preços SAP do Oracle por loja para o R gerando querys automaticamente.}
\usage{
dataDownloadPrice(SKUS=NULL)
}

\arguments{
  \item{SKUS}{Vetor de string com os SKUS desejados.}
}

\value{
Esta função retorna uma lista contendo cubos ou matrizes de preço SAP por loja dependendo do número de SKUS selecionados.
 }


\examples{
######Definir argumentos######
##############################
SKUS=c("4040347","4040346")
teste = preco.download(SKUS=SKUS)
}
