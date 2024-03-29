% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggd.file.R
\name{write.csv}
\alias{write.csv}
\alias{ggd.write.csv}
\alias{\S4method{write.csv}{GGD}}
\title{Write the composition}
\usage{
ggd.write.csv(obj, file = "")

\S4method{write.csv}{GGD}(file = "")
}
\arguments{
\item{obj}{The \code{\link[ggd]{GGD}} object to be saved.}

\item{file}{The name of the file or a \link[base]{connection} open for writing.
\code{""} indicates output to the console.}
}
\value{
An invisible NULL.
}
\description{
Writes the composition of a \code{\link[ggd]{GGD}} object as a CSV file.
This function provides a simple way to export a \code{\link[ggd]{GGD}} object,
regardless of the package version or R version.
}
\details{
\subsection{Accuracy of saved data}{
     Mean values and standard deviations are recorded to a maximum length of
     the 22nd decimal place.
     The accuracy is sufficient to reconstruct the original object almost completely
     (at least the value of each field can be \code{TRUE} with \code{\link[base]{==}})
     in most cases, and in most systems.
}
}
\examples{
 a <- ggd.set.cmp( data.frame( mean = c( 0.223, 0.219 ), sd = c( 2.265, 2.176 ) ),
                   grad = "v2" )
 a$mix.type
 a$cmp
 a$write.csv()
 ggd.write.csv( a )
}
\seealso{
\code{\link[ggd]{read.csv}}
}
