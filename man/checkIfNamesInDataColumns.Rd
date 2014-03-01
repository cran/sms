\name{checkIfNamesInDataColumns}
\alias{checkIfNamesInDataColumns}
\title{checkIfNamesInDataColumns}
\usage{
checkIfNamesInDataColumns(names, incensus, insurvey)
}
\arguments{
  \item{names}{A vector with names to check if they exist
  as column names in the data (census and survey)}

  \item{incensus}{The census data}

  \item{insurvey}{The survey data}
}
\value{
anumber If both names are valid then it return '1' else if
the names are not valid data column names, it returns '0'.
}
\description{
Check the integrisy of the data Lexicon
}
\author{
Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

