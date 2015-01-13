\name{fsia-package}
\alias{fsia-package}
\alias{fsia}
\docType{package}
\title{Import and Analysis of OMR Data from FormScanner}
\description{
Import data of tests and questionnaires from FormScanner. FormScanner is an open source software that converts scanned images to data using optical mark recognition (OMR) and it can be downloaded from http://sourceforge.net/projects/formscanner/. The spreadsheet file created by FormScanner is imported in a convenient format to perform the analyses provided by the package. These analyses include the conversion of multiple responses to binary (correct/incorrect) data, the computation of the number of corrected responses for each subject or item, the computation and the graphical representation of the frequencies of the responses to each item and the report of the responses of one or a few persons.
}
\details{
\tabular{ll}{
Package: \tab fsia\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-13\cr
License: \tab GPL (>= 2)
}
Data of questionnaires and tests are often collected on paper forms.
FormScanner is an open source software that converts scanned images 
to data using optical mark recognition (OMR). 
Function \code{\link{read.formscanner}} of the fsia package 
can be used to import data from FormScanner in R.
If items have a correct response (key), data can be converted
in binary variables using function \code{\link{resp2binary}}.
In this case, the number of corrected responses for each person
and for each item can also be computed by using functions
\code{\link{person.stat}} and \code{\link{item.stat}}.
Function \code{\link{freq}} calculates the absolute or percentage 
frequencies of the responses to each item. The frequencies can
be printed on screen or plotted on a graph. In both cases, the
true responses (if any) are highlighted.
The responses given by one or a few persons can be displayed 
on a graph by using function \code{\link{report}}.
The key is shown on the right and wrong responses can be immediately
identified by the red colour.

The package includes various data sets for illustrative purposes.
The examples given in the documentation of function \code{\link{read.formscanner}}
export these data in the same format of FormScanner and then
import the files created.
Data sets \code{\link{mchoice1}} and \code{\link{mchoice2}}
contain simulated multiple choice responses to 30 items and present
10 items in common.
Data sets \code{\link{mchoice1id}} and \code{\link{mchoice2id}}
contain the same data of \code{mchoice1} and \code{mchoice2}
with the only exception that the identifier of the persons
can be obtained by concatenating a set of columns of the data set.
Data set \code{\link{key}} contains the correct responses of the 
items of all data sets of the fsia package.
Data set \code{\link{exam1}} is an instance of the output of
function \code{read.formscanner} and contains the data 
\code{mchoice1} and the corresponding key.
}
\author{
	Michela Battauz
	
	Maintainer: Michela Battauz <michela.battauz@uniud.it>
}
\references{
Borsetta, A. (2014). FormScanner [Computer Software],
URL http://sourceforge.net/projects/formscanner/.
}
\keyword{ package }
\examples{

\dontrun{
# IMPORT DATA FROM FORMSCANNER
# in this example
# the identification number is in column "File name"
# and the key is given in a separate file
data(mchoice1)
data(key)
# NOTE: the following line creates a file with the same format of FormScanner
write.csv2(mchoice1, "mchoice1.csv", row.names = FALSE)
# NOTE: the following line creates a file with the key
write.csv2(key, "key.csv", row.names = FALSE)
fs1 <- read.formscanner(file = "mchoice1.csv", keyfile = "key.csv", id = "File.name")
head(fs1$data)
fs1$key
# fs1 is equal to the data set exam1
}

data(exam1)

# CONVERT DATA IN BINARY VARIABLES
exam1bin <- resp2binary(obj = exam1, col = 2:31)

# PERSON STATISTICS
pst <- person.stat(obj = exam1, col = 2:31)
head(pst)
summary(pst$score)
hist(pst$score, main = "Histogram of Scores", xlab = "")

# ITEM STATISTICS
item.stat(obj = exam1, col = 2:31)

# FREQUENCIES OF THE RESPONSES
fr <- freq(obj = exam1, col = c("I2","I3"))
fr
plot(fr)

# RESPONSES OF TWO PERSONS
report(obj = exam1, col = 2:31, whichid = c("101844", "101272"))
}
