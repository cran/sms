\name{addDataAssociation}
\alias{addDataAssociation}
\title{addDataAssociation}
\usage{
addDataAssociation(indf, data_names)
}
\arguments{
  \item{indf}{A data Lexicon (data.frame) created from the
  function: \code{\link{createLexicon}}}

  \item{data_names}{A vector vith two elements. The first
  element should be the name of the \code{\link{census}}
  data column, and the second element should be the name of
  the \code{\link{survey}} data column}
}
\value{
indf The imported data lexicon with one extra column.
}
\description{
Create a data lexicon for holding the associated column
names
}
\examples{
library(sms)
data(survey)
data(census)
in.lexicon=createLexicon()
in.lexicon=addDataAssociation(in.lexicon, c("he","he"))
in.lexicon=addDataAssociation(in.lexicon, c("females","female"))
print(in.lexicon)
}
\author{
Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

