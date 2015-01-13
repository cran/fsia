\name{exam1}
\alias{exam1}
\docType{data}
\title{Simulated Test Responses with Key}
\description{
This data set contains simulated multiple choice responses to 30 items and the corrected response for each item.
This is an instance of the output of function \code{\link{read.formscanner}}.
}
\usage{data("exam1")}
\format{
	A list with components
	\describe{
		\item{\code{data}}{a data frame with 1000 multiple choice responses to 30 items.}
		\item{\code{key}}{a data frame containing the correct responses to the items.}
	}
}
\details{
	\code{data} is taken from the \code{\link{mchoice1}} data set, \code{key} is taken from the \code{\link{key}} data set.
}
\author{Michela Battauz}
\seealso{
	\code{\link{key}}, \code{\link{mchoice1}}, \code{\link{read.formscanner}}
}
\examples{
data(exam1)
head(exam1$data)
exam1$key
}
\keyword{datasets}
