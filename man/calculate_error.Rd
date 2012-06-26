\name{calculate_error}
\alias{calculate_error}
\title{Calculate error of a selection}
\usage{
  calculate_error(selection, area_census, lexicon)
}
\arguments{
  \item{selection}{A population selection, to evaluate its
  error}

  \item{area_census}{An area from census (a row)}

  \item{lexicon}{A data.frame with details about data
  connections}
}
\value{
  TAE Total Absolute Error of this selection against the
  census description of this area.
}
\description{
  Calculate error of a selection
}
\details{
  Calculates the Total Absolute Error of an area selection.
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

this_area=as.data.frame(census[1,]) #Select the first area from the census table
selection=random_panel_selection(panel,this_area$population) #make a random selection of individuals for this area.
error=calculate_error(selection,this_area,in.lexicon) #evaluate the Total Absolute Error (TAE) for this selection
print(error) #print the error of the selection
}
\author{
  Dimitris Kavroudakis
}

