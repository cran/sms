\name{run_parallel_SA}
\alias{run_parallel_SA}
\title{run_parallel_SA}
\usage{
  run_parallel_SA(census, iterations, bhps, in.lexicon)
}
\arguments{
  \item{census}{A census dataset consisting of various
  areas rows.}

  \item{iterations}{The number of iterations}

  \item{bhps}{the panel dataset}

  \item{in.lexicon}{The data lexicon.}
}
\value{
  msm_results An object with the results of the simulation,
  for each area.
}
\description{
  Run a simulation in parallel mode with Simulated
  annealing
}
\examples{
library(sms)
#========================= Panel Data ============================
n=20
panel=data.frame(pid=6001:6020, he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
#========================= Census Data ============================
census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
#========================= Data linkage ===========================
con_01=c("he","he" )
con_02=c("fem","females" )
in.lexicon=data.frame(con_01,con_02 )
row.names(in.lexicon)=c("census_row","panel_row")

iterations=70
results= run_parallel_SA( census, iterations, panel, in.lexicon)
print(results)
}
\author{
  Dimitris Kavroudakis
}

