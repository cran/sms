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
Make a single selection of individual records for a census
area.
}
\details{
Select a number of individual records from panel dataset,
to represent a census description of an area.
}
\examples{
library(sms)
data(survey) #load the data
data(census)
in.lexicon=createLexicon() # Create a data lexicon for holding the associated column names.
in.lexicon=addDataAssociation(in.lexicon, c("he","he"))
in.lexicon=addDataAssociation(in.lexicon, c("females","female"))

# Select the first area from the census table
this_area=as.data.frame(census[1,])

#make a representation for this area.
sel=selection_for_area(survey, this_area, in.lexicon)

print(sel) #print the representation
}
\author{
Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

