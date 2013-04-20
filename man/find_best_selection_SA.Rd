\name{find_best_selection_SA}
\alias{find_best_selection_SA}
\title{find_best_selection_SA}
\usage{
  find_best_selection_SA(area_census, insms)
}
\arguments{
  \item{area_census}{A census dataset consisting of various
  areas rows.}

  \item{insms}{A microsimulation object which holds the
  data and details of the simulation such as iterations,
  lexicon.}
}
\value{
  msm_results An object with the results of the simulation,
  of this area.
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

this_area=as.data.frame(census[1,]) #Select the first area from the census table
insms= new("microsimulation",census=census, panel=panel, lexicon=in.lexicon, iterations=5)
myselection= find_best_selection_SA( this_area, insms)
print(myselection)
}
\author{
  Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

