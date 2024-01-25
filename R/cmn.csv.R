################################################################################################
# Common functions about CSV files.
# @file         cmn.csv.R
# @author       Kimitsuna Ura
# @copyright    Copyright (C) 2024 Kimitsuna Ura
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Functions

################################################################################################
#' [Non-exported] Output a table to a file
#'
#' Outputs a data frame or a matrix to a file or \link[base]{connection} with the CSV format
#' using \link[base]{cat}.
#' The precision for real numbers can be indicated with \code{digits} argument.
#' @param   x           The object to be output, should be a matrix or data frame.
#' @param   file        A \link[base]{connection},
#'                      or a character string naming the file to print to.
#'                      If \code{""} (the default), it prints to the standard output connection,
#'                      the console unless redirected by \link[base]{sink}.
#' @param   top.text    A character string to be output as the first element of the header.
#' @param   append.str  A character string to be output after \code{x}.
#' @param   digits      The number of significant digits (see \link[base]{signif}) to print.
#'                      Integers from \code{1} to \code{22} with default \code{15} are allowed.
#' @return  An invisible \code{NULL}.
#' @examples
#'  ggd:::cat.table( data.frame( a = 1:6 * pi, b = pi^(1:6) ), top.text = "PI", digits = 10 )
################################################################################################
cat.table <- function( x, file = "", top.text = "", append.str = "", digits = 15 )
{
    digits.backup <- getOption( "digits" )

    tryCatch(
    {
        # Set digits option
        options( digits = as.integer( digits ) )

        # Output header
        cat( '"', file = file )
        if ( length( top.text ) > 0 )
        {
            cat( top.text, file = file, append = TRUE )
        }
        cat( '"', file = file, append = TRUE )

        if ( ncol( x ) > 0 )
        {
            cat( ',', file = file, append = TRUE )
            for ( i in 1:ncol( x ) )
            {
                cat( paste0( '"', colnames( x )[i], '"' ), file = file, append = TRUE )
                if ( i < ncol( x ) )
                {
                    cat( ",", file = file, append = TRUE )
                }
                else
                {
                    cat( "\n", file = file, append = TRUE )
                }
            }
        }

        # Output data
        if ( nrow( x ) > 0 )
        {
            for ( i in 1:nrow( x ) )
            {
                cat( '"', file = file, append = TRUE )
                cat( rownames( x )[i], file = file, append = TRUE )
                cat( '",', file = file, append = TRUE )
                for ( j in 1:ncol( x ) )
                {
                    cat( x[i, j], file = file, append = TRUE )
                    if ( j < ncol( x ) )
                    {
                        cat( ",", file = file, append = TRUE )
                    }
                    else
                    {
                        cat( "\n", file = file, append = TRUE )
                    }
                }
            }
        }

        if ( isTRUE( nchar( append.str ) > 0 ) )
        {
            cat( append.str, file = file, append = TRUE )
            cat( "\n", file = file, append = TRUE )
        }
    },
    finally = { options( digits = digits.backup ) } )

    return ( invisible( NULL ) )
}
