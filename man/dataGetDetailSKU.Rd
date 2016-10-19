\name{dataGetDetailSKU}
\alias{dataGetDetailSKU}
\title{Baixa tabelas com informações detalhadas dos SKUS}
\usage{
dataGetDetailSKU(depart=c("D040"))
}


\arguments{

  \item{depart}{Apenas se type="skus". Determina sobre quais SKUS o usuário deseja as informações}
  
}

\value{
Esta função retorna uma matriz de detalhes de SKUs.

}


\examples{
# um departamento
m=dataGetDetailSKU(depart="D040")

# vários departamentos
m=dataGetDetailSKU(depart=c("D040","D021"))


}

\author{
  Coded by Henrique Helfer Hoeltgebaum and Gabriel Vasconcelos, last updated on 01-11-2016.
  }
