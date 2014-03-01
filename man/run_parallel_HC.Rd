\name{run_parallel_HC}
\alias{run_parallel_HC}
\title{run_parallel_HC}
\usage{
run_parallel_HC(insms)
}
\arguments{
  \item{insms}{A microsimulation object which holds the
  data and details of the simulation such as iterations,
  lexicon.}
}
\value{
msm_results An object with the results of the simulation,
for each area.
}
\description{
Run a simulation in serial mode with Hill Climbing
}
\details{
Run a simulation in serial mode with Hill Climbing
}
\examples{
library(sms)
data(survey) #load the data
data(census)
in.lexicon=createLexicon() # Create a data lexicon for holding the associated column names.
in.lexicon=addDataAssociation(in.lexicon, c("he","he"))
in.lexicon=addDataAssociation(in.lexicon, c("females","female"))

insms= new("microsimulation",census=census,panel=survey, lexicon=in.lexicon, iterations=10)
re=run_parallel_HC(insms)
print(re)
}
\author{
Dimitris Kavroudakis \email{dimitris123@gmail.com}
}

