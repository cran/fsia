\name{questionnaire}
\alias{questionnaire}
\docType{data}
\title{Questionnaire Responses}
\description{
This data set contains responses to a questionnaire.
}
\usage{data("questionnaire")}
\format{
  A data frame with 5 observations on the following 14 variables.
	\tabular{ll}{
		\code{File.name} \tab file name. \cr
		\code{Q1.gender} \tab gender. \cr
		\code{Q2.age} \tab age. \cr
		\code{Q3.restaurants} \tab how often eat at restaurants. \cr
		\code{Q4.movies} \tab watching movies is fun. \cr
		\code{Q4.music} \tab I like listening to music. \cr
		\code{Q4.reading} \tab reading is an indispensable part of life. \cr
		\code{Q5.sources} \tab sources of information. \cr
		\code{Q6.interviewer} \tab interviewer. \cr
		\code{Q5.sources.internet} \tab dummy variable for response internet. \cr
		\code{Q5.sources.magazines} \tab dummy variable for response magazines. \cr
		\code{Q5.sources.newspapers} \tab dummy variable for response newspapers. \cr
		\code{Q5.sources.radio} \tab dummy variable for response radio. \cr
		\code{Q5.sources.TV} \tab dummy variable for response TV.}
}
\details{
This data set is obtained by importing file "scan_results_questionnaire.csv"
with function \code{\link{read.formscanner}}.
}
\author{Michela Battauz}
\seealso{
	\code{\link{read.formscanner}}
}
\examples{
data(questionnaire)
questionnaire
}
\keyword{datasets}
