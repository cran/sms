\name{run_parallel_HC}
\alias{run_parallel_HC}
\title{run_parallel_HC}
\usage{
  run_parallel_HC(insms)
}
\arguments{
  \item{insms}{A microsimulation object which holds the
  data and details of the simulation such as iterations,
  lexicon.}
}
\value{
  msm_results An object with the results of the simulation,
  for each area.
}
\description{
  Run a simulation in serial mode with Hill Climbing
}
\details{
  Run a simulation in serial mode with Hill Climbing
}
\examples{
library(sms)
library(sms)
n=20
panel=data.frame(pid=6001:6020, he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
in.lexicon=data.frame(c("he","he" ),c("fem","females" ) )
row.names(in.lexicon)=c("census_row","panel_row")
insms= new("microsimulation",census=census,panel=panel, lexicon=in.lexicon, iterations=10)
re=run_parallel_HC(insms)
print(re)
}
\author{
  Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

