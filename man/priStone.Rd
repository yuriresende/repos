\name{priStone}
\alias{priStone}
\title{Fitting Stone model}
\description{This function fits the Stone model in a specific panel data.}

}
\usage{
priStone = function(painel,graphic,graphic.path,graphic.name)
}

\arguments{
  \item{datec}{Date in the format YYY-MM-DD. If the user do not enter a date, it will return the most recent composition of store groups.}
}

\examples{
table1 = priRCTComposition("2016-10-17")
table2 = priRCTComposition()
}

\author{
  Coded by Henrique Helfer Hoeltgebaum, last updated on 27-10-2016.
  }
