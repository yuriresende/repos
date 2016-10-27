\name{priRCTComposition}
\alias{priRCTComposition}
\title{Return the composition of store groups on a specific date}
\description{This function returns which stores belong to control and treatment groups on a specific date.}

}
\usage{
priRCTComposition = function(date=NULL)
}

\arguments{
  \item{date}{Date in the format YYY-MM-DD. If the user do not enter a date, it will return the most recent composition of store groups.}
}

\examples{
table1 = priRCTComposition("2016-10-17")
table2 = priRCTComposition()
}

\author{
  Coded by Henrique Helfer Hoeltgebaum, last updated on 27-10-2016.
  }
