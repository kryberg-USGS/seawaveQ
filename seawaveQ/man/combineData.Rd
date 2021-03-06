% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combineData.R
\name{combineData}
\alias{combineData}
\title{Combine water-quality sample data and continuous ancillary 
variables}
\format{
A data frame with the number of rows equal to the number of rows
in the data frame indicated by qwdat. The number of columns depend 
on the two input data frames. Minimally there will be a station 
identification column, a dates column, a column of qualification codes,
and a column of water-quality data.
}
\usage{
combineData(qwdat, cqwdat, qwcols = c("staid", "dates", "R", "P"))
}
\arguments{
\item{qwdat}{is the dataset containing water-quality sample data with 
columns that begin with a P (or other user-defined indicator) followed 
by alphanumeric characters. These columns are concentration data. In 
addition there need to be columns that begin with an R (or other user-
defined indicator) followed by alphanumeric characters that match those 
of the associated concentration data. The R columns contain data 
qualification codes. See example datasets for more information about 
the data format, \link{IllRivValleyCty} and \link{qwMoRivOmaha}. This 
package assumes that the data are in micrograms per liter.}

\item{cqwdat}{is the dataset containing variables that can be used as 
explanatory variables for the seawaveQ model. See example dataset
for more information about the data format \link{cqwMoRivOmaha}. These 
are daily values with no missing values allowed between the first and 
the last date in the dataset.}

\item{qwcols}{is a character vector with column headings for 
a station (location) identifier, a dates column identifier, beginning 
of column headers for remarks code (default is R), and beginning of 
column headers for concentration data (default is P for parameter).}
}
\value{
A data frame
}
\description{
Function to combine water-quality sample data and continuous (daily) 
ancillary variables and drop unnecessary columns.
}
\note{
The columns indicated by qwcols[1:2] are used to
combine the datasets. The first column is the station identifier and 
the second column is the date column. These two column headings must 
be the same in the two datasets being combined and the dates in the 
datasets being combined must be of class Date and use the same 
format.
}
\examples{
data(swData)
MoRivOmaha <- combineData(qwdat = qwMoRivOmaha, cqwdat = cqwMoRivOmaha, 
qwcols = c("staid", "dates", "R", "P"))
}
\author{
Karen R. Ryberg and Aldo V. Vecchia
}
\keyword{manip}
