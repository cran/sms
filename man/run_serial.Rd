\name{run_serial}
\alias{run_serial}
\title{Run_serial}
\usage{
  run_serial(census, iterations, panel, this.lexicon)
}
\arguments{
  \item{census}{A census dataset consisting of various
  areas (rows)}

  \item{iterations}{The number of iterations}

  \item{panel}{the panel dataset}

  \item{this.lexicon}{The data lexicon.}
}
\value{
  msm_results An object with the results of the simulation,
  for each area.
}
\description{
  Run a simulation in serial mode
}
\details{
  Run a simulation in serial mode.
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
results= run_serial( census, iterations, panel, in.lexicon)
print(results)
}
\author{
  Dimitris Kavroudakis
}

