\name{fsia-package}
\alias{fsia-package}
\alias{fsia}
\docType{package}
\title{Import and Analysis of OMR Data from FormScanner}
\description{
Import data of tests and questionnaires from FormScanner. 
FormScanner is an open source software that converts scanned images 
to data using optical mark recognition (OMR) and it can be downloaded from 
<http://sourceforge.net/projects/formscanner/>. 
The spreadsheet file created by FormScanner is imported in a convenient 
format to perform the analyses provided by the package. 
These analyses include the conversion of multiple responses to binary (correct/incorrect) data, 
the computation of the number of corrected responses for each subject or item, 
scoring using weights,
the computation and the graphical representation of the frequencies of the responses 
to each item and the report of the responses of a few subjects.
}
\details{
\tabular{ll}{
Package: \tab fsia\cr
Type: \tab Package\cr
Version: \tab 1.1.1\cr
Date: \tab 2017-06-23\cr
License: \tab GPL-3
}
Data of questionnaires and tests are often collected on paper forms.
FormScanner is an open source software that converts scanned images 
to data using optical mark recognition (OMR). 
Function \code{\link{read.formscanner}} of the fsia package 
can be used to import data from FormScanner in R.
The correct response (key) can be specified using function 
\code{\link{addkey}}.
It is also possible to specify weights for each response using
function \code{\link{addweights}}.
If items have a key, data can be converted
to binary variables using function \code{\link{resp2binary}}.
In this case, the number of corrected responses for each person
and for each item can also be computed by using functions
\code{\link{person.stat}} and \code{\link{item.stat}}.
These functions can also be used to compute a score using the 
weights previously specified.
Function \code{\link{freq}} calculates the absolute or percentage 
frequencies of the responses to each item. The frequencies can
be printed on screen or plotted on a graph. In both cases, the
true responses (if any) are highlighted.
The responses given by one or a few subjects can be displayed 
on a graph by using function \code{\link{report}}.
The key is shown on the right and wrong responses can be immediately
identified by the red colour.

The package includes two data sets for illustrative purposes.
Data sets \code{\link{test}} and \code{\link{questionnaire}}
contain the result of importing csv files with function \code{\link{read.formscanner}}.
Data set \code{\link{key}} contains the correct responses of the 
items of the test data sets. Data set \code{\link{weights}} contains
the weights of each correct response, while data set \code{\link{weights_multiple}}
contains the weights of each response.
}
\author{
	Michela Battauz
	
	Maintainer: Michela Battauz <michela.battauz@uniud.it>
}
\references{
Borsetta, A. (2017). FormScanner [Computer Software],
URL http://sourceforge.net/projects/formscanner/.
}
\keyword{ package }
\examples{


# IMPORT DATA FROM FORMSCANNER
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

# ADD THE KEY
# create the path for file "key.csv"
key_path <- file.path(dir_pkg, "examples", "key.csv")
# add the key 
testk <- addkey(test_imp, keyfile = key_path)
testk$key

# ADD WEIGHTS
# create the path for file "weights.csv"
weights_path <- file.path(dir_pkg, "examples", "weights.csv")
# specify the weights for each correct response
testw <- addweights(testk, weightsfile = weights_path)
testw$weights
# create the path for file "weights_multiple.csv"
weights_mult_path <- file.path(dir_pkg, "examples", "weights_multiple.csv")
# specify the weights for each response
testwm <- addweights(test_imp, weightsfile = weights_mult_path)
testwm$weights

# CONVERT DATA TO BINARY VARIABLES
resp01 <- resp2binary(obj = testk, col = 2:41)
resp01[, 2:5]

# ASSIGN WEIGHTS TO RESPONSES
resps <- resp2scores(obj = testw, col =2:41)
resps[, 2:5]

# ASSIGN WEIGHTS TO RESPONSES (MULTIPLE WEIGHTS)
resps <- resp2scores(obj = testwm, col =2:41)
resps[, 2:5]

# PERSON STATISTICS (selected only 4 items)
pst <- person.stat(obj = testk, col = 2:5)
pst
pst <- person.stat(obj = testw, col = 2:5, weights = TRUE)
pst
pst <- person.stat(obj = testwm, col = 2:5, weights = TRUE)
pst

# ITEM STATISTICS
ist <- item.stat(obj = testk, col = 2:41)
head(ist)
ist <- item.stat(obj = testw, col = 2:41, weights = TRUE)
head(ist)
ist <- item.stat(obj = testwm, col = 2:41, weights = TRUE)
head(ist)

# FREQUENCIES OF THE RESPONSES
fr <- freq(obj = testk, col = c("Question03", "Question04"))
fr
par(mfrow = c(1, 2))
plot(fr, ask = FALSE)

# RESPONSES OF TWO SUBJECTS
par(mfrow = c(1, 2))
report(obj = testk, col = 2:11, whichid = c("102344", "245784"))
report(obj = testw, col = 2:11, whichid = c("102344", "245784"), weights = TRUE)
par(mfrow = c(1, 1))
report(obj = testwm, col = 2:11, whichid = c("102344", "245784"), weights = TRUE)
}
