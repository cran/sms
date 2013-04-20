\name{getSampleData}
\alias{getSampleData}
\title{getSampleData}
\usage{
  getSampleData(census_n = 3, bhps_n = 20)
}
\arguments{
  \item{census_n}{A The census data in dataframe format}

  \item{bhps_n}{The size of the panel data}
}
\value{
  A list with 2 dataframes. a "census" and "panel"
  dataframe
}
\description{
  Generate sample census and panel (survey) datasets
}
\details{
  Generate random census and panel (survey) data for
  testing purpose
}
\examples{
library(sms)
gen=getSampleData(census_n=10, bhps_n=55) #Generate a sample census dataset of 10 areas and a individual based survey dataset of 55 individual records.
print(gen) #print the generated data
}
\author{
  Dimitris Kavroudakis

  Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

