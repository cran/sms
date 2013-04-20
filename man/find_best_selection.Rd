\name{find_best_selection}
\alias{find_best_selection}
\title{find_best_selection}
\usage{
  find_best_selection(area, insms)
}
\arguments{
  \item{area}{A census area}

  \item{insms}{A microsimulation object which holds the
  data and details of the simulation such as iterations,
  lexicon.}
}
\value{
  list A list with results (#areaid, #selection, #tae,
  #tries, #error_states).
}
\description{
  Find the best selection of individual records for a
  census area.
}
\details{
  Calculate the best area representation, after a series of
  selection tries.
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

this_area=as.data.frame(census[1,]) #Select the first area from the census table
insms= new("microsimulation",census=census,panel=panel, lexicon=in.lexicon, iterations=10)
best=find_best_selection(this_area, insms)
print(best)
}
\author{
  Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

