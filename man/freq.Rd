\name{freq}
\alias{freq}
\alias{print.frlist}
\alias{plot.frlist}
\title{Absolute and Percentage Frequencies of the Responses to the Items.}
\description{
Calculates and plots the absolute or percentage frequencies of the responses to each item.
}
\usage{
freq(obj, columns, perc = FALSE)
\method{plot}{frlist}(x, display = TRUE, ...)
}
\arguments{
	\item{obj}{An object of class \code{fsdata} containing the data imported by function \code{\link{read.formscanner}}.}
  \item{columns}{A vector containing which columns to use. Columns can be specified by name or number.}
	\item{perc}{logical; if \code{TRUE} percentage frequencies are calculated.}
	\item{x}{An object of class \code{frlist} returned by function \code{freq}.}
	\item{display}{logical; if \code{TRUE} the frequencies are displayed on the plot.}
	\item{\dots}{further arguments passed to or from other methods.}
}
\value{
Function \code{freq} returns an abject of class \code{frlist} containing a list with components
\item{item}{the name of the item.}
\item{tab}{an object of class \code{table} containing the frequencies.}
\item{key}{the key of the item.}
}
\author{Michela Battauz}
\seealso{
\code{\link{read.formscanner}}
}
\examples{
data(exam1)
fr <- freq(obj = exam1, col = c("I2","I3"))
fr
plot(fr)
fr <- freq(obj = exam1, col = 2:11, perc = TRUE)
fr
par(mfrow = c(2,5))
plot(fr)
plot(fr, display = FALSE)
}
