% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sms.R
\name{calculate_error}
\alias{calculate_error}
\title{Calculate error of a selection}
\usage{
calculate_error(selection, area_census, lexicon)
}
\arguments{
\item{selection}{A population selection, to evaluate its error}

\item{area_census}{An area from census (a row)}

\item{lexicon}{A data.frame with details about data connections}
}
\value{
TAE Total Absolute Error of this selection against the census description of this area.
}
\description{
Calculate the error of a selection.
}
\details{
Calculates the Total Absolute Error (TAE) of a selection for a census area.
}
\examples{
library(sms)
data(survey) #load the data
data(census)
in.lexicon=createLexicon() # Create a data lexicon for holding the associated column names.
in.lexicon=addDataAssociation(in.lexicon, c("he","he"))
in.lexicon=addDataAssociation(in.lexicon, c("females","female"))

#Select the first area from the census table
this_area=as.data.frame(census[1,]) 

#make a random selection of individuals for this area.
selection=random_panel_selection( survey, this_area$population ) 

#evaluate the Total Absolute Error (TAE) for this selection
error=calculate_error( selection, this_area, in.lexicon ) 
print( error )    # print the error of the selection
}
\author{
Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

