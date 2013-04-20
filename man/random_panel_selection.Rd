\name{random_panel_selection}
\alias{random_panel_selection}
\title{random_panel_selection}
\usage{
  random_panel_selection(indf, n)
}
\arguments{
  \item{indf}{The initial dataframe from wich a selection
  will be made.}

  \item{n}{The number of random rows}
}
\value{
  a selection of rows as a dataframe
}
\description{
  Select n random rows from a dataframe
}
\details{
  Select n random rows from a dataframe
}
\examples{
library(sms)
n=20
panel=data.frame(pid=6001:6020, he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
some.individuals=random_panel_selection(panel,4)
print(some.individuals) #print the selection of individuals
}
\author{
  Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

