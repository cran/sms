\name{selection_for_area}
\alias{selection_for_area}
\title{selection_for_area}
\usage{
  selection_for_area(inpanel, area_census, inlexicon)
}
\arguments{
  \item{inpanel}{The panel dataset}

  \item{area_census}{A census area}

  \item{inlexicon}{A data lexicon showing the variable
  associations.}
}
\value{
  list A list of results (#areaid, #selection, #error)
}
\description{
  Make a selection
}
\details{
  Select a number of population from panel data, to
  represent an area.
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

this_area=as.data.frame(census[1,]) #Select the first area from the census table
sel=selection_for_area(panel,this_area, in.lexicon) #make a representation for this area.
print(sel) #print the representation
}
\author{
  Dimitris Kavroudakis
}

