\name{dataGetDetailSKU}
\alias{dataGetDetailSKU}
\title{Baixa tabelas com detalhes dos SKUS ou das lojas.}
\usage{
dataGetDetailSKU(depart=c("D040"))
}


\arguments{

  \item{depart}{Apenas se type="skus". Determina sobre quais SKUS o usuário deseja as informações}
  
}

\value{
Esta função retorna uma matriz de detalhes de SKUS.

}


\examples{
# um departamento
m=dataGetDetailSKU(depart="D040")

# vários departamentos
m=dataGetDetailSKU(depart=c("D040","D021"))


}
}
