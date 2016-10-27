\name{priStone}
\alias{priStone}
\title{Fitting Stone model}
\description{This function fits the Stone model in a specific panel data.}

}
\usage{
priStone = function(painel,graphic,graphic.path,graphic.name)
}

\arguments{
  \item{painel}{Input data should be a panel data.}
  
  \item{graphic}{If graphic=TRUE, a correlation plot will be provided.}
  
  \item{graphic.path}{If graphic.path=TRUE, this argument is the path in which a pdf file containing the aforementioned graphic will be saved.}
  
  \item{graphic.name}{If graphic.name=TRUE, this argument is the name of the pdf file saved in the graphic.path.}
}

\examples{
load("/RProjetos/Pricing/Dados Atualizados/Painel/Base.RData")  
dados = Base
graphic=TRUE
graphic.path = "/RProjetos"
graphic.name = "azulejo"
}

\author{
  Coded by Henrique Helfer Hoeltgebaum, last updated on 27-10-2016.
  }
