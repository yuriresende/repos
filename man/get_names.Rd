\name{get.names}
\alias{get.names}
\title{Baixa tabelas com detalhes dos SKUS ou das lojas.}
\usage{
get.names(type=c("skus","lojas"), depart=c("D040")
}


\arguments{
  \item{type}{Se "skus" baixa uma tabela onde cada linha é um SKU e nas colunas há informações sobre os mesmos. Se "lojas" baxa uma tabela semelhante, mas para as lojas.
  
  \item{depart}{Apenas se type="skus". Determina sobre quais SKUS o usuário deseja as informações}
  
}

\value{
Esta função retorna uma matriz de SKUS ou lojas. 

}


\examples{
# um departamento
m=get.names(type="skus",depart="D040")

# vários departamentos
m=get.names(type="skus",depart=c("D040","D021"))

#Lojas
m=get.names(type="loja")

}
}
