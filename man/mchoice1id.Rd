\name{mchoice1id}
\alias{mchoice1id}
\docType{data}
\title{Simulated Test Responses}
\description{
This data set contains simulated multiple choice responses to 30 items.
}
\usage{data("mchoice1id")}
\format{
  A data frame with 1000 observations on the following 37 variables.
\tabular{rl}{
	\code{File.name} \tab file name. \cr
	\code{I1-I30} \tab responses given to the items. \cr
	\code{N1-N6} \tab concatenated, these form the identification number.}
}
\details{
	Columns \code{I1-I30} are the same of data set \code{\link{mchoice1}}.
	Column \code{File.name} of data set \code{\link{mchoice1}} can be obtained by concatenating columns \code{N1-N6}. 
	This data set shares 10 common items with the \code{\link{mchoice2id}} data set.
	Column names \code{I1-I30} uniquely identify the items.
}
\author{Michela Battauz}
\seealso{
	\code{\link{exam1}}, \code{\link{key}}, \code{\link{read.formscanner}}
}
\examples{
data(mchoice1id)
head(mchoice1id)
}
\keyword{datasets}
