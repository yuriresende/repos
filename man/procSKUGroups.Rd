\name{procSKUGroups}
\alias{procSKUGroups}
\title{Return the composition of SKU groups on a specific date}
\description{This function returns a table with the composition of SKU groups on a specific date}

}
\usage{
procSKUGroups = function(date=NULL)
}

\arguments{
  \item{date}{Date in the format YYY-MM-DD. If the user do not enter a date, it will return the most recent composition of SKU groups.}
}

\examples{
table1 = procSKUGroups("2016-10-17")
table2 = procSKUGroups()
}

\author{
  Coded by Henrique Helfer Hoeltgebaum, last updated on 27-10-2016.
  }
