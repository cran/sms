\name{getSampleMicrosimulation}
\alias{getSampleMicrosimulation}
\title{getSampleMicrosimulation}
\usage{
  getSampleMicrosimulation(census_n = 3, bhps_n = 20,
    iterations = 5)
}
\arguments{
  \item{census_n}{number of areas to generate}

  \item{bhps_n}{number of individual records to generate}

  \item{iterations}{Number of iterations to use when
  simulating}
}
\value{
  a microsimulation object with data
}
\description{
  Generate a sample Microsimulation object
}
\details{
  Generate a sample Microsimulation object with sample data
  for testing purpose.
}
\examples{
library(sms)
insms=getSampleMicrosimulation(census_n=10,bhps_n=50)
print(insms)
}
\author{
  Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

