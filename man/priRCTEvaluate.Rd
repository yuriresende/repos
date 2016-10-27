\name{priRCTEvaluate}
\alias{priRCTEvaluate}
\title{Fits regression model with:(i) OLS with average price, (ii) OLS with PSAP and (iii) IV}
\description{This function returns the coefficients and errors of the three proposed regression models:OLS with average price, OLS with PSAP and IV}

}
\usage{
priRCTEvaluate<-function(data.painel, dias.tratamento, familia)
}

\arguments{
  \item{data.painel}{Panel data which will be analysed.}
  
  \item{dias.tratamento}{Number of days in which the treatment was applied.}
  
  \item{familia}{The SKU group number.}
  
}

\examples{
# carregar a Base
  load(file="/RProjetos/Pricing/Dados Atualizados/Painel/Base.RData")

# Datas
  inicio.experimento <- '2016-09-09' # Pré tratamento
  inicio.tratamento <- '2016-09-16'
  final.experimento <- '2016-09-22'
  
# Parâmetros do experimento
  familia <- '11'
  preco <- 1.49
  delta_p <- 0.3
    
# Caso não queira remover nenhuma loja pelo nível de preço pré tratamento, basta colocar critério de corte = 0.
  criterio_corte <- 1.4
  
# Grupos tratamento e controle
  load("/RProjetos/Pricing/Dados Atualizados/Grupos de lojas da aleatorizacao/lojas.aleatorizacao.RData")
  d_lojas = lojas.aleatorizacao

  grupo.tratamento <- c('A1','A2')
  grupo.controle <- c('B1','B2')
  
  base_aux <- priDataPainel(Base,d_lojas,inicio.experimento,inicio.tratamento,final.experimento,
                          familia,preco,delta_p,criterio_corte,grupo.controle,grupo.tratamento)


  regressao_painel <- priRCTEvaluate(base_aux$data.painel,base_aux$dias.tratamento, familia)


  regressao_painel$`OLS com p.médio`
  regressao_painel$`IV primeiro estágio`
  regressao_painel$`OLS com IV`
  regressao_painel$DID
  regressao_painel$coef_agregado
  regressao_painel$`Qtd referência`

}

\author{
  Coded by Henrique Helfer Hoeltgebaum and Yuri Fonseca, last updated on 27-10-2016.
  }
