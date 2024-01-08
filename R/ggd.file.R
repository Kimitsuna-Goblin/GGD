################################################################################################
# File I/O
# @file         ggd.file.R
# @author       Kimitsuna Ura
# @copyright    Copyright (C) 2024 Kimitsuna Ura
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Functions

################################################################################################
#' Read a composition
#'
#' Reads a CSV file recorded the composition of a \code{\link[ggd]{GGD}} object
#' and generates a \code{\link[ggd]{GGD}} object.
#' @export
#' @name    read.csv
#' @aliases ggd.read.csv
#' @aliases read.csv
#' @aliases \S4method{read.csv}{GGD}
#' @usage   ggd.read.csv(file)
#' @usage   \S4method{read.csv}{GGD}(file)
#' @param   file        The name of the CSV file which the composition of
#'                      a \code{\link[ggd]{GGD}} object is to be read from.
#'                      The \code{file} can be a readable text-mode \link[base]{connection}.
#'
#'                      The \code{file} must have a header consisting of
#'                      the value of \code{mix.type}, the character string of "mean"
#'                      and the character string of "sd".
#'                      The order of each column is not interchangeable.
#'                      The second and subsequent rows should be the name of the row
#'                      (such as \code{"nd.1"}, but not be cared)
#'                      and the mean value and standard deviation of the each components
#'                      of the \code{cmp} field.
#'
#'                      For more information about the properties of this argument,
#'                      see \code{file} argument of \link[utils]{read.table}.
#'
#' @return  The generated \code{\link[ggd]{GGD}} object (invisible for \code{GGD} method).
#'
#'          For \code{GGD} method: If failed to read, the object will be cleared.
#'
#' @seealso \code{\link[ggd]{write.csv}}
#'
#' @details
#' \subsection{Reading empty data}{
#'      If the read file is one which is written with a cleared object,
#'      \code{kind}/\code{kind.index} and \code{mix.type} fields of the generated object
#'      will be \code{NA},
#'      and \code{median} and \code{mean} and other numeric fields will be \code{NaN}.
#' }
#' \subsection{Illegal format file}{
#'      If the format of the read file was illegal and a \code{\link[ggd]{GGD}} object cannot
#'      be constructed, an error occurs and the \code{\link[ggd]{GGD}} object is cleared.
#'
#'      If the \code{mix.type} value stored in a file does not match the values for \code{cmp},
#'      an error may occur, or it may happen to work. It is possible to create such a file
#'      using \code{ggd:::}\code{\link[ggd]{cat.table}} function which is non-exported.
#'      However, it is not recommended to create such a non-conforming file anyway.
#' }
#' @importFrom  utils   read.csv
#' @examples
#'  csvfile <- tempfile( fileext = ".csv" )
#'  a <- ggd.set.cmp( data.frame( mean = c( -1.739, 1.195 ), sd = c( 1.175, 1.831 ) ),
#'                    grad = "h" )
#'  a$write.csv( csvfile )
#'
#'  ggd.read.csv( csvfile )
#'  b <- ggd.read.csv( csvfile )
#'  b$mix.type == a$mix.type
#'  b$cmp == a$cmp
#'
#'  a$clear()
#'  a$read.csv( csvfile )
#'  a
#'  a$mix.type == b$mix.type
#'  a$cmp == b$cmp
#'
#'  unlink( csvfile )
################################################################################################
ggd.read.csv <- function( file )
{
    table <- read.csv( file, header = FALSE, numerals = "no.loss" )
    if ( nrow( table ) < 1 || ncol( table ) < 3 )
    {
        stop( "Error: File format error." )
    }

    if ( !all( table[1, 2:3] == c( "mean", "sd" ) ) )
    {
        stop( "Error: Invalid file header." )
    }

    mix.type <- as.integer( table[1, 1] )
    if ( length( mix.type ) != 1 || !( is.na( mix.type ) || any( mix.type == 0:5 ) ) )
    {
        stop( "Error: The value of mix.type is invalid." )
    }

    if ( nrow( table ) == 1 )
    {
        cmp <- data.frame( mean = numeric(), sd = numeric() )
    }
    else
    {
        if ( isTRUE( mix.type == 5 ) )
        {
            cmp <- data.frame( mean = as.numeric( table[2:( nrow( table ) - 1 ), 2] ),
                                 sd = as.numeric( table[2:( nrow( table ) - 1 ), 3] ) )

            text <- gsub( "\\\\", '"', gsub( "\\\\n", "\n", table[nrow( table ), 2] ) )
            custom.d <- eval( parse( text = text ) )
        }
        else
        {
            cmp <- data.frame( mean = as.numeric( table[2:nrow( table ), 2] ),
                                 sd = as.numeric( table[2:nrow( table ), 3] ) )
            custom.d <- NULL
        }
    }

    if ( mix.type == 3 && nrow( cmp ) == 3 )
    {
        grad <- "v3"
    }
    else
    {
        grad <- "default"
    }

    return ( ggd.set.cmp( cmp, mix.type = mix.type, grad = grad, custom.d = custom.d ) )
}

GGD$methods(
    read.csv = function( file )
    {
        clear()
        obj <- ggd.read.csv( file )
        set.cmp( obj$cmp, this.mix.type = obj$mix.type,
                 grad = ifelse( ( obj$mix.type == 3 && nrow( obj$cmp ) == 3 ),
                                "v3", "default" ) )
    }
)

################################################################################################
#' Write the composition
#'
#' Writes the composition of a \code{\link[ggd]{GGD}} object as a CSV file.
#' This function provides a simple way to export a \code{\link[ggd]{GGD}} object,
#' regardless of the package or R version. See "Details" for more information.
#' @export
#' @name    write.csv
#' @aliases ggd.write.csv
#' @aliases write.csv
#' @aliases \S4method{write.csv}{GGD}
#' @usage   ggd.write.csv(obj, file = "")
#' @usage   \S4method{write.csv}{GGD}(file = "")
#' @param   obj     The \code{\link[ggd]{GGD}} object to be saved.
#' @param   file    The name of the file or a \link[base]{connection} for writing
#'                  the composition of the object. \code{""} indicates output to the console.
#' @return  An invisible NULL.
#' @seealso \code{\link[ggd]{read.csv}}
#'
#' @details
#' \subsection{Accuracy of saved data}{
#'      Mean values and standard deviations of the components are recorded to
#'      a maximum length of the 22nd decimal place.
#'      The accuracy is sufficient to reconstruct the original object almost completely
#'      (at least the value of each field can be \code{TRUE} with \code{"=="})
#'      in most cases, in most systems.
#' }
#' \subsection{For customized distribution}{
#'      Although it is possible to save an object of \code{"Customized Distribution"}
#'      (\code{mix.type = 5}), it is not always a good idea to use this method.
#'      This is because, \code{costom.d} is simply converted to a character string
#'      and saved in the last row of the CSV file, so it is not guaranteed that objects loaded
#'      with \code{\link[ggd]{read.csv}} will work well,
#'      particularly if \code{custom.d} calls your own functions.
#'      Instead, you can use \code{\link[base]{save}} or \code{\link[base]{save.image}}.
#' }
#'
#' @importFrom  utils   read.csv
#' @examples
#'  a <- ggd.set.cmp( data.frame( mean = c( 0.223, 0.219 ), sd = c( 2.265, 2.176 ) ),
#'                    grad = "v2" )
#'  a$mix.type
#'  a$cmp
#'  a$write.csv()
#'  ggd.write.csv( a )
################################################################################################
ggd.write.csv <- function( obj, file = "" )
{
    if ( isTRUE( obj$mix.type == 5 ) )
    {
        append.str <- paste0( '"","', gsub( '"', '\\\\"',
                                            paste( as.character( getSrcref( obj$custom.d ) ),
                                                   collapse = "\\n" ) ),
                              '",""' )
    }
    else
    {
        append.str <- ""
    }

    cat.table( obj$cmp, file, obj$mix.type, append.str, 22 )
}

GGD$methods(
    write.csv = function( file = "" )
    {
        ggd.write.csv( .self, file )
    }
)
