\name{random_panel_selection}
\alias{random_panel_selection}
\title{random_panel_selection}
\usage{
  random_panel_selection(indf, n)
}
\arguments{
  \item{indf}{A dataframe}

  \item{n}{Number of random rows}
}
\value{
  a selection of rows.
}
\description{
  Select random rows
}
\details{
  Select a specific number of random rows from a data.frame
}
\examples{
library(sms)
#========================= Panel Data ============================
n=20
panel=data.frame(pid=6001:6020, he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
some.individuals=random_panel_selection(panel,4)
print(some.individuals) #print the selection of individuals
}
\author{
  Dimitris Kavroudakis
}

