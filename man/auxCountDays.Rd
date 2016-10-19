\name{auxCountDays}
\alias{auxCountDays}
\title{Retorna o número de dias entre duas datas}
\usage{
auxCountDays(inicio=NULL,fim=NULL,N_t=NULL)
}


\arguments{
  \item{inicio}{Argumento opcional com a data de início.}

  \item{fim}{Argumento opcional com a data final.}
  
  \item{N_t}{Caso disponível, é possível introduzir a série contendo o número de lojas ativas no mês N_t}
  
}

\value{
Esta função retorna uma matriz de dados com a freqüência de dias dentro de cada mês da análise.
  }

\examples{
N.dias = auxCountDays(c("2013-01-01"), c("2016-06-01"))
}
