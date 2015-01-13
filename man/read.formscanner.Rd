\name{read.formscanner}
\alias{read.formscanner}
\title{Import Data From FormScanner}
\description{
This function imports data from the FormScanner software.
}
\usage{
read.formscanner(file, conc = NULL, keyline = NULL, keyfile = NULL, keydata = NULL, id)
}
\arguments{
	\item{file}{the name of the file which the data are to be read from.}
	\item{conc}{a vector containing which columns to concatenate. Columns can be specified by name or number.}
	\item{keyline}{the number of line of the data containing the correct responses to the items. 
		This line is deleted from the data and used as key.}
	\item{keyfile}{the name of the file with the correct responses to the items. 
		Column names should match the names of the items.}
	\item{keydata}{an R dataframe containing the correct responses to the items. 
		Column names should match the names of the items.}
	\item{id}{name of the column that uniquely identifies the row.}
}
\details{
If some columns are concatenated using argument \code{conc}, 
the name of the newly created variable is the name of the first column. \cr

\code{\link{exam1}} is an instance of the output of function \code{\link{read.formscanner}}.\cr

Variable \code{id} can either be the column \code{File.name} or the concatenation of different columns
using argument \code{conc}. In the last case, \code{id} should be set equal to the first column concatenated.
FormScanner allows the user to change the file names of the images, so that the file name can be
set equal to the identifier of the form.
}
\value{
An object of class \code{fsdata} consisting in a list with components
\item{data}{a data frame with the responses.}
\item{key}{a data frame containing the correct responses to the items. 
	If there is not a correct response this can be \code{NULL}.}
}
\references{
Borsetta, A. (2014). FormScanner, [Computer Software],
URL http://sourceforge.net/projects/formscanner/.
}
\author{Michela Battauz}
\seealso{
	\code{\link{exam1}}, \code{\link{key}}, \code{\link{mchoice1}}, \code{\link{mchoice1id}}
}
\examples{
\dontrun{

# identification number is in column "File name"
# key is given in the first row
data(mchoice1)
data(key)
key1 <- key
key1$File.name <- 0
mchoice1k <- rbind(key1[, colnames(mchoice1)], mchoice1)
# NOTE: the following line creates a file with the same format of FormScanner
write.csv2(mchoice1k, "mchoice1k.csv", row.names = FALSE)
fs1 <- read.formscanner(file = "mchoice1k.csv", keyline = 1, id = "File.name")
head(fs1$data)
fs1$key

# identification number is in column "File name"
# key is given in a separate file
data(mchoice1)
data(key)
# NOTE: the following line creates a file with the same format of FormScanner
write.csv2(mchoice1, "mchoice1.csv", row.names = FALSE)
# NOTE: the following line creates a file with the key
write.csv2(key, "key.csv", row.names = FALSE)
fs1 <- read.formscanner(file = "mchoice1.csv", keyfile = "key.csv", id = "File.name")
head(fs1$data)
fs1$key
# in this case, fs1 is equal to the data set exam1


# identification number is in column "File name"
# key is given in an R dataset
data(mchoice1)
data(key)
# NOTE: the following line creates a file with the same format of FormScanner
write.csv2(mchoice1, "mchoice1.csv", row.names = FALSE)
fs1 <- read.formscanner(file = "mchoice1.csv", keydata = key, id = "File.name")
head(fs1$data)
fs1$key
# in this case, fs1 is equal to the data set exam1

# identification number is created by concatenating different fields
# key is given in the first row
data(mchoice1id)
data(key)
key1 <- key
key1$File.name <- "CCF27112014_01000"
key1$N1 <- key1$N2 <- key1$N3 <- key1$N4 <- key1$N5 <- key1$N6 <- 0
mchoice1idk <- rbind(key1[, colnames(mchoice1id)], mchoice1id)
# NOTE: the following line creates a file with the same format of FormScanner
write.csv2(mchoice1idk, "mchoice1idk.csv", row.names = FALSE)
fs1 <- read.formscanner(file = "mchoice1idk.csv", keyline = 1, id = "N1", 
	conc = c("N1", "N2", "N3", "N4", "N5", "N6"))
head(fs1$data)
fs1$key

# identification number is created by concatenating different fields
# key is given in a separate file
data(mchoice1id)
data(key)
# NOTE: the following line creates a file with the same format of FormScanner
write.csv2(mchoice1id, "mchoice1id.csv", row.names = FALSE)
# NOTE: the following line creates a file with the key
write.csv2(key, "key.csv", row.names = FALSE)
fs1 <- read.formscanner(file = "mchoice1id.csv", keyfile = "key.csv", id = "N1", 
	conc = c("N1", "N2", "N3", "N4", "N5", "N6"))
head(fs1$data)
fs1$key

# identification number is created by concatenating different fields
# key is given in an R dataset
data(mchoice1id)
data(key)
# NOTE: the following line creates a file with the same format of FormScanner
write.csv2(mchoice1id, "mchoice1id.csv", row.names = FALSE)
fs1 <- read.formscanner(file = "mchoice1id.csv", keydata = key, id = "N1", 
	conc = c("N1", "N2", "N3", "N4", "N5", "N6"))
head(fs1$data)
fs1$key

# identification number is created by concatenating different fields
# key is NULL
data(mchoice1id)
# NOTE: the following line creates a file with the same format of FormScanner
write.csv2(mchoice1id, "mchoice1id.csv", row.names = FALSE)
fs1 <- read.formscanner(file = "mchoice1id.csv", id = "N1", 
	conc = c("N1", "N2", "N3", "N4", "N5", "N6"))
head(fs1$data)
fs1$key
}
}