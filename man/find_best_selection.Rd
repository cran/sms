\name{find_best_selection}
\alias{find_best_selection}
\title{find_best_selection}
\usage{
  find_best_selection(area, it, bhps, my.lexicon)
}
\arguments{
  \item{area}{A census area}

  \item{it}{Number of iterations}

  \item{bhps}{A panel dataset.}

  \item{my.lexicon}{The data lexicon}
}
\value{
  list A list with results (#areaid, #selection, #tae,
  #tries, #error_states).
}
\description{
  Find the best selection for an area
}
\details{
  Calculate the best area representation, after a series of
  selection tries.
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
this_area=as.data.frame(census[1,]) #Select the first area from the census table
best=find_best_selection(this_area, iterations, panel, in.lexicon)
print(best)
}
\author{
  Dimitris Kavroudakis
}

