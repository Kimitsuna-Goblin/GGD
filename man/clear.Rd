% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggd.1.R
\name{clear}
\alias{clear}
\alias{\S4method{clear}{GGD}}
\title{Clear fields}
\usage{
\S4method{clear}{GGD}()
}
\value{
The cleared \code{\link[ggd]{GGD}} object itself (invisible).
}
\description{
Clears all fields. The lengths of all vector fields and the number of rows
in \code{cmp} field will be \code{0}.
}
\examples{
 a <- GGD$new()
 a   ## Normal Distribution with mix.type = 2
 a$clear()
 a   ## cleared
}
