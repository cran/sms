\name{plotTries}
\alias{plotTries}
\title{Plot selection results}
\usage{
  plotTries(inresult)
}
\arguments{
  \item{inresult}{The input selection process to plot}
}
\description{
  Plot selections
}
\details{
  Plot errors during selection process for an area.
}
\examples{
library(sms)
#========================= Panel Data ============================
n=20
panel=data.frame(pid=6001:(6000+n), he=sample(0:1,n,rep=TRUE),females=sample(0:1,n,rep=TRUE))
#========================= Census Data ============================
census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
#========================= Data linkage ===========================
con_01=c("he","he" )
con_02=c("fem","females" )
in.lexicon=data.frame(con_01,con_02 )
row.names(in.lexicon)=c("census_row","panel_row")

iterations=60
sa=run_parallel_SA(census,iterations,panel,in.lexicon)
plotTries(sa[[2]])
}
\author{
  Dimitris Kavroudakis
}

