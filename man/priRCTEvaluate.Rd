\name{priRCTEvaluate}
\alias{priRCTEvaluate}
\title{Fits regression model with:(i) OLS with average price, (ii) OLS with PSAP and (iii) IV: average price instrumented with PSAP}
\description{This function returns the coefficients and standard errors of the three proposed regression models with dummy variables and fixed effects:OLS with average price, OLS with PSAP and average price instrumented with PSAP}
          
}
\usage{
priRCTEvaluate<-function(data.painel, dias.tratamento, SKUGroup)
}

\arguments{
  \item{data.painel}{The panel is the value 'data.painel' produced by the priDataPainel function, which is avaliable in this same package.}
  
  \item{dias.tratamento}{Number of days in which the treatment was applied. It is the output 'dias.tratamento' of the priDataPainel function.}
  
  \item{SKUGroup}{The SKU group number}
  
}
  
  \values{This function returns a list of outputs with the following components:

  \item{OLS com p.médio}{Returns the OLS fitted coefficients with its standard errors when using Quantity as independent variable and PSAP as dependent.}
  \item{IV primeiro estágio}{Returns the fitted coefficients and standard errors of the IV´s first stage regression. In this stage we use PSAP and dummy variables as independent variables and the average price as dependent variable.}
  \item{OLS com IV}{Returns the fitted coefficients and standard errors of the IV´s first stage regression}
  \item{DID}{Returns the fitted coefficients and standard errors of Diff in Diff approach. The variable D2 is a dummy variable which indicates if the observation lies inside the treatment period (D2=1) or not (D2=0). The other variable D3 is another dummy variable which indicates if the observation lies inside the treatment period and belongs to the treatment group (D3=1) otherwise (D3=0).}
  \item{coef_agregado}{This output is the expected quantity of sales under which the analyzed SKU group might have if we increase its price by 1 unit.}
  \item{Qtd referência}{This output is the expected quantity of sales under which the analyzed SKU group would had if there was no treatment.}
}
             
\examples{
# carregar a Base
  load(file="/RProjetos/Pricing/Dados Atualizados/Painel/Base.RData")

# Datas
  inicio.experimento <- '2016-09-09' # Pré tratamento
  inicio.tratamento <- '2016-09-16'
  final.experimento <- '2016-09-22'
  
# Parâmetros do experimento
  SKUGroup <- '11'
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
                          SKUGroup,preco,delta_p,criterio_corte,grupo.controle,grupo.tratamento)

  regressao_painel <- priRCTEvaluate(base_aux$data.painel,base_aux$dias.tratamento, SKUGroup)

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
