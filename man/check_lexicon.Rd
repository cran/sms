% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sms.R
\name{check_lexicon}
\alias{check_lexicon}
\title{check_lexicon}
\usage{
check_lexicon(inlex)
}
\arguments{
\item{inlex}{A data.frame which will be used a data lexicon 
for listing the associated data columns.}
}
\description{
Check the lexicon data.frame
}
\examples{
library(sms)
df=createLexicon()
df=addDataAssociation(df, c("ena","duo"))
check_lexicon(df) 
}
\author{
Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

