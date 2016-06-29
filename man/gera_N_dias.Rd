\name{gera.N.dias}
\alias{gera.N.dias}
\title{Retorna o número de dias entre duas datas}
\usage{
gera.N.dias(inicio=NULL,fim=NULL,N_t=NULL)
}


\arguments{
  \item{inicio}{Argumento opcional com a data de início da análise.}

  \item{fim}{Argumento opcional com a data final da análise.}
  
  \item{N_t}{Caso disponível, é possível introduzir a série contendo o número de lojas ativas no mês N_t}

\value{
Esta função retorna uma matriz de dados com a freqüência de dias dentro de cada mês da análise.
  }

\examples{
N.dias = gera.N.dias(c("2013-01-01"), c("2016-06-01"))
}
