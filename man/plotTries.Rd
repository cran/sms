\name{plotTries}
\alias{plotTries}
\title{Plot selection results}
\usage{
  plotTries(insms, number)
}
\arguments{
  \item{insms}{The input results}

  \item{number}{the number of the area to plot}
}
\description{
  Plot the selection process of an area from a
  microsimulation object.
}
\details{
  Plot errors during selection process for an area.
}
\examples{
library(sms)
n=20
panel=data.frame(pid=6001:(6000+n), he=sample(0:1,n,replace=TRUE),females=sample(0:1,n,replace=TRUE))
census=data.frame(areaid=c(101,102) , population=c(54,50), he=c(36,30), fem= c(24,27))
con_01=c("he","he" )
con_02=c("fem","females" )
in.lexicon=data.frame(con_01,con_02 )
row.names(in.lexicon)=c("census_row","panel_row")

ansms= new("microsimulation",census=census,panel=panel, lexicon=in.lexicon, iterations=5)
sa=run_parallel_SA(ansms)
plotTries( sa, 1 )
}
\author{
  Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

