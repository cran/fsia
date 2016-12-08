\name{read.formscanner}
\alias{read.formscanner}
\title{Import Data From FormScanner}
\description{
This function imports data from the FormScanner software.
}
\usage{
read.formscanner(file, col.names, conc = NULL, id = NULL, dummy = NULL)
}
\arguments{
	\item{file}{the name of the csv file to be imported.}
	\item{col.names}{the names of the columns. If \code{NULL} the names of the columns are those given in the csv file.}
	\item{conc}{a vector containing which columns to concatenate. Columns can be specified by name or number.}
	\item{id}{name of the column that uniquely identifies the row.}
	\item{dummy}{a vector containing the columns to convert to dummy variables. Columns can be specified by name or number.}
}
\details{
If some columns are concatenated using argument \code{conc}, 
the name of the newly created variable is the name of the first column. \cr

\code{\link{test}} and \code{\link{questionnaire}} are instances of the output of function \code{\link{read.formscanner}}.\cr

When \code{id} is obtained as the concatenation of different columns
using argument \code{conc}, \code{id} should be set equal to the first column concatenated.
}
\value{
A data frame.
}
\references{
Borsetta, A. (2016). FormScanner, [Computer Software],
URL http://sourceforge.net/projects/formscanner/.
}
\author{Michela Battauz}
\seealso{
	\code{\link{addkey}}, \code{\link{addweights}}
}
\examples{
# find the directory with package fsia
dir_pkg <- find.package("fsia")
# the example files are in the directory examples
# create the path
questionnaire_path <- file.path(dir_pkg, "examples", "scan_results_questionnaire.csv")
test_path <- file.path(dir_pkg, "examples", "scan_results_test.csv")
# import file "scan_results_questionnaire.csv"
questionnaire_imp<-read.formscanner(questionnaire_path, dummy = "Q5.sources")
questionnaire_imp
# questionnaire_imp is equal to the data questionnaire
# import file "scan_results_test.csv"
test_imp <- read.formscanner(test_path, conc = paste("id", 1:6, sep = ""), id = "id1")
test_imp
# test_imp is equal to the data test
}