################################################################################################
# Common functions about data frames.
# @file         cmn.data.frame.R
# @author       Kimitsuna Ura
# @copyright    Copyright (C) 2024 Kimitsuna Ura
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Functions

################################################################################################
#' [Non-exported] Extract complete-case data in two columns
#'
#' Extracts complete-case data from the indicated data frame and makes a new data frame
#' with 2 columns named \code{"x"} and \code{y.name}.
#' @param   data        A data frame which represents a relation of x- and y-coordinates.
#'                      It should contain 2 columns for \code{x} and \code{y}.
#' @param   x           The column name or column number indicating \code{x} of \code{data},
#'                      the column of x-coordinates.
#' @param   y           The column name or column number indicating \code{y} of \code{data},
#'                      the column of y-coordinates.
#' @param   y.name      The column name of \code{y} column of the new data frame.
#' @return  A data frame with 2 columns named \code{"x"} and \code{y.name}.
#' @importFrom  stats   complete.cases
################################################################################################
extract.complete.x.y <- function( data, x, y, y.name )
{
    data.ext <- data.frame( x = data[[x]] )
    data.ext[y.name] <- data[[y]]
    return ( data.ext[complete.cases( data.ext ),] )
}
