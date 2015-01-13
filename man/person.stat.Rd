\name{person.stat}
\alias{person.stat}
\title{Person Statistics}
\description{This function computes some statistics for each person.}
\usage{
person.stat(obj, columns)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{An object of class \code{fsdata} containing the data imported by function \code{\link{read.formscanner}}.}
  \item{columns}{A vector containing which columns to use. Columns can be specified by name or number.}
}
\value{
A data frame with the following variables.
\item{id}{person label.}
\item{score}{number of corrected responses for each person.}
\item{count}{number of total items for each person.}
\item{perc}{percentage of corrected responses.}
}
\author{Michela Battauz}
\seealso{
\code{\link{person.stat}}
}
\examples{
data(exam1)
pst <- person.stat(obj = exam1, col = 2:31)
head(pst)
summary(pst$score)
hist(pst$score, main = "Histogram of Scores", xlab = "")
}