\name{run_parallel_SA}
\alias{run_parallel_SA}
\title{run_parallel_SA}
\usage{
  run_parallel_SA(insms)
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
  Run a simulation in parallel mode with Simulated
  Annealing
}
\examples{
library(sms)
n=20
panel=data.frame(pid=6001:6020, he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
con_01=c("he","he" )
con_02=c("fem","females" )
in.lexicon=data.frame(con_01,con_02 )
row.names(in.lexicon)=c("census_row","panel_row")
insms= new("microsimulation",census=census, panel=panel, lexicon=in.lexicon, iterations=5)
results= run_parallel_SA(insms)
print(results)
}
\author{
  Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

