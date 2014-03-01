\name{createLexicon}
\alias{createLexicon}
\title{createLexicon}
\usage{
createLexicon()
}
\value{
dataLexicon A data.frame holding the associated column
names.
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

