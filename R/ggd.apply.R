################################################################################################
# Applying functions to cmp field
# @file         ggd.apply.R
# @author       Kimitsuna-Goblin
# @copyright    Copyright (C) 2023 Ura Kimitsuna
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Functions

################################################################################################
#' Apply functions to elements of cmp
#'
#' Applies a function to each element in the \code{cmp} field.
#' Different functions can be applied to \code{mean} column and \code{sd} column.
#' Other fields will be adjusted accordingly.
#' @name    apply.cmp
#' @aliases apply.cmp
#' @aliases \S4method{apply.cmp}{GGD}
#' @usage   \S4method{apply.cmp}{GGD}(f.mean = NULL, f.sd = NULL)
#' @param   f.mean  A function to apply to elements in \code{mean} column.
#'                  If \code{NULL}, nothing is applied.
#' @param   f.sd    A function to apply to elements in \code{sd} column.
#'                  If \code{NULL}, nothing is applied.
#'                  See "Details" for more information.
#' @return  The processed \code{\link[ggd]{GGD}} object itself (invisible).
#' @seealso \code{\link[ggd]{round.cmp}}
#' @details
#'      Each function indicated with \code{f.mean} or \code{f.sd} can receive
#'      2 arguments (each argument name is arbitrary):
#'      \enumerate{
#'          \item A vector of the current values of \code{mean} or \code{sd} column.
#'          \item The \code{\link[ggd]{GGD}} object itself to be processed.
#'      }
#'      Therefore, the function for \code{f.mean} or \code{f.cmp} is hoped to be
#'      declared with 2 arguments like as "\code{function(mean, obj)}",
#'      however, if the function do not need the 2nd argument,
#'      you can declare with 1 arguments
#'      like as "\code{function(mean)}" or "\code{function(sd)}".
#'      For the values of the functions, each function must return a numeric vector
#'      with the same length of the 1st argument as new values of each column.
#'
#'      After both functions have been processed, the new values are actually
#'      set into the \code{cmp} field. Therefore, from within the functions to apply,
#'      it is possible to refer to the values of each field before applying,
#'      but not after.
#'
#'      Moreover, if an error occurs at \code{f.mean} or \code{f.sd},
#'      this method does not clear the object,
#'      but retains the values of all fields before the method is called.
#'
#'      The 2nd argument, which is the \code{\link[ggd]{GGD}} object itself, should be used for
#'      reference only, and it is not recommended to update the fields of it.
#'      The results of such updating are not guaranteed.
#'      It is strongly recommended NOT to use methods which return
#'      the object itself
#'      (e.g., \code{\link[ggd]{adjust.cmp}}, \code{\link[ggd]{round.cmp}}, etc.)
#'      for the object of the 2nd argument, since they may update the values of the fields.
#'
#'      This function does not change the number of rows of the \code{cmp} field,
#'      no matter what results are obtained.
#' @examples
#'  a <- ggd.set.cmp( data.frame( mean = c( -0.5, 0, 0.5 ), sd = c( 1, 0.8, 1.2 ) ) )
#'  a$cmp; c( a$mean, a$sd )
#'  ## +1 to mean.
#'  a$apply.cmp( function( mean ) mean + 1 )
#'  a$cmp; c( a$mean, a$sd )
#'  ## double sd of 2nd component.
#'  a$apply.cmp( f.sd = function( sd ) { sd[2] <- sd[2] * 2; sd } )
#'  a$cmp; c( a$mean, a$sd )
#'  ## rotate as index+1 for mean and index-1 for sd.
#'  a$apply.cmp( f.mean = function( mean ) mean[c( 2, 3, 1 )],
#'               f.sd = function( sd ) sd[c( 3, 1, 2 )] )
#'  a$cmp; c( a$mean, a$sd )
#'  ## swap mean and sd.
#'  a$apply.cmp( f.mean = function( m, obj ) obj$cmp$sd,
#'               f.sd = function( s, obj ) obj$cmp$mean )
#'  a$cmp; c( a$mean, a$sd )
################################################################################################
NULL
GGD$methods(
    apply.cmp = function( f.mean = NULL, f.sd = NULL )
    {
        new.cmp <- cmp

        if ( !is.null( f.mean ) )
        {
            new.cmp$mean <- apply.cmp.sub( f.mean, new.cmp$mean, .self )
        }

        if ( !is.null( f.sd ) )
        {
            new.cmp$sd <- apply.cmp.sub( f.sd, new.cmp$sd, .self )
        }

        return ( set.cmp( new.cmp, this.mix.type = mix.type,
                          grad = ifelse( ( mix.type == 3 && nrow( cmp ) == 3 ),
                                         "v3", "default" ) ) )
    }
)

################################################################################################
#' [Non-exported] Call applying function
#'
#' Calls a applying function to get new values of a column of \code{cmp} field.
#' @param   f       A function to apply to elements in a column of \code{cmp} field.
#' @param   values  A vector of current values of the column of \code{cmp} field.
#' @param   obj     The \code{\link[ggd]{GGD}} object which calls this function.
#' @return  A vector of new values of the column of \code{cmp} field.
#' @seealso \code{\link[ggd]{apply.cmp}}
################################################################################################
apply.cmp.sub <- function( f, values, obj )
{
    args <- names( formals( f ) )

    if ( length( args ) == 2 || any( args == "..." ) )
    {
        results <- f( values, obj )
    }
    else if ( length( args ) == 1 )
    {
        results <- f( values )
    }
    else
    {
        stop( paste( "Error: Function for", substitute( f ), "must accept 1 or 2 arguments." ) )
    }

    if ( length( results ) != length( values ) )
    {
        stop( paste( "Error: Length of value of", substitute( f ),
                            "is different from the input vector." ) )
    }

    return ( results )
}

################################################################################################
#' Rounding of numbers in cmp
#'
#' Rounds the values of all elements in the \code{cmp} field and adjusts other fields
#' accordingly.
#' \code{\link[base]{round}} and \code{\link[base]{signif}} functions can apply.
#' These functions does not change the number of rows of \code{cmp} field,
#' no matter what results are obtained.
#' If an error occurs, these functions do not clear the object, but retains the values of
#' all fields.
#' @name    round.cmp
#' @aliases round.cmp
#' @aliases signif.cmp
#' @aliases \S4method{round.cmp}{GGD}
#' @aliases \S4method{signif.cmp}{GGD}
#' @usage   \S4method{round.cmp}{GGD}(dg.mean = 0, dg.sd = dg.mean)
#' @usage   \S4method{signif.cmp}{GGD}(dg.mean = 6, dg.sd = dg.mean)
#' @param   dg.mean     A vector of integers indicating the numbers of
#'                      decimal places (\code{round.cmp}) or
#'                      significant digits (\code{signif.cmp}) for \code{mean} column.
#'                      It may work with any length. If the length is longer than
#'                      number of components or is not a divisor of number of components,
#'                      the extra length will be cut off.
#'                      If the length is \code{0} (including \code{NULL}),
#'                      mean values will not be rounded.
#' @param   dg.sd       A vector of integers similar to \code{dg.mean}, for \code{sd} column.
#'                      If the length is \code{0} (including \code{NULL}),
#'                      standard deviations will not be rounded.
#'                      See \code{\link[base]{round}} for more information.
#' @return  The processed \code{\link[ggd]{GGD}} object itself.
#' @seealso \code{\link[ggd]{apply.cmp}}, \code{\link[base]{round}}
#' @examples
#'  a <- ggd.set.cmp( data.frame( mean = c( 112.7865, 113.1647 ),
#'                                  sd = c( 7.936188, 11.092769 ) ) )
#'  a$cmp; a$kind; c( a$mean, a$sd )
#'
#'  a$round.cmp( 1, 2 )
#'  a$cmp; a$kind; c( a$mean, a$sd )
#'
#'  a$signif.cmp( 2 )
#'  a$cmp; a$kind; c( a$mean, a$sd )
#'
#'  ## For the IEC 60559 standard
#'  a <- ggd.set.cmp( data.frame( mean = c( 9.5, 10.5 ), sd = c( 0.35, 0.45 ) ) )
#'  a$round.cmp( 0, 1 ) ## mean will be c( 10, 10 ). sd depends on OS.
#'  a$cmp
#'
#'  ## It will be a normal distribution with 2 rows in cmp.
#'  a <- ggd.set.cmp( data.frame( mean = c( 1.12, 1.06 ), sd = c( 1.99, 2.07 ) ) )
#'  a$round.cmp()
#'  a$cmp; a$kind; c( a$mean, a$sd )
#'
#'  ## Only mean will be rounded.
#'  a <- ggd.set.cmp( data.frame( mean = c( -1.341789, 0.982471 ),
#'                                  sd = c( 1.897149, 2.412378 ) ) )
#'  a$signif.cmp( dg.mean = 2, dg.sd = NULL )
#'  a$cmp
################################################################################################
NULL
GGD$methods(
    round.cmp = function( dg.mean = 0, dg.sd = dg.mean )
    {
        return ( apply.math2.cmp( round, dg.mean, dg.sd, .self ) )
    }
)

GGD$methods(
    signif.cmp = function( dg.mean = 6, dg.sd = dg.mean )
    {
        return ( apply.math2.cmp( signif, dg.mean, dg.sd, .self ) )
    }
)

################################################################################################
#' [Non-exported] Apply a Math2 group function
#'
#' Applies a \link[methods]{Math2} group function
#' (\code{\link[base]{round}} or \code{\link[base]{signif}}) to the values of \code{cmp} field
#' of a \code{\link[ggd]{GGD}} object and adjusts other fields of the object accordingly.
#' @param   math2.f     A \link[methods]{Math2} group function.
#' @param   dg.mean     A vector of integer for the 1st argument of \code{math2.f}
#'                      to be used for \code{mean} column.
#' @param   dg.sd       A vector of integer similar to \code{dg.mean}, for \code{sd} column.
#' @param   obj         The \code{\link[ggd]{GGD}} object to be processed.
#' @return  A processed \code{\link[ggd]{GGD}} object.
#' @seealso \code{\link[ggd]{round.cmp}}
################################################################################################
apply.math2.cmp <- function( math2.f, dg.mean, dg.sd, obj )
{
    new.cmp <- obj$cmp
    if ( length( dg.mean ) > 0 && nrow( obj$cmp ) > 0 )
    {
        new.cmp$mean <- math2.f( obj$cmp$mean, dg.mean )[1:nrow( obj$cmp )]
    }

    if ( length( dg.sd ) > 0 && nrow( obj$cmp ) > 0 )
    {
        new.cmp$sd <- math2.f( obj$cmp$sd, dg.sd )[1:nrow( obj$cmp )]
        if ( !all( new.cmp$sd > 0 ) )
        {
            stop( "Error: sd must be positive." )
        }
    }

    # To retain the entity as the reference class of the obj object,
    # here we use obj$set.cmp method, not a generator function ggd.set.cmp.
    return ( obj$set.cmp( new.cmp, this.mix.type = obj$mix.type,
                          grad = ifelse( ( obj$mix.type == 3 && nrow( obj$cmp ) == 3 ),
                                         "v3", "default" ) ) )
}
