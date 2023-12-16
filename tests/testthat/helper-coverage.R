################################################################################################
## This file is to print out coverage information to a file.
################################################################################################

passed.cov <- numeric()     ## memory for passed coverage check points

cov.trace.mode <- FALSE     ## If TRUE, print for points which has been already passed.

################################################################################################
#' Function to be put at the coverage check points
#'
#' Put this function at each coverage check points like \code{print.cov( "[001]" )}.
#' @param   text        A text stands for coverage check point.
#' @param   filename    A file name to output the passed coverage check points.
################################################################################################
print.cov <- function( text, filename = "./output.cov.txt" )
{
    if ( is.na( passed.cov[text] ) || cov.trace.mode )
    {
        con <- file( filename, open = "a" )
        writeLines( text, con )
        close( con )
    }

    if ( is.na( passed.cov[text] ) )
    {
        passed.cov[text] <<- 1
    }
    else
    {
        passed.cov[text] <<- passed.cov[text] + 1
    }
}

################################################################################################
#' Clear memory for passed coverage check points
################################################################################################
clear.passed.cov <- function()
{
    passed.cov <<- numeric()
}
