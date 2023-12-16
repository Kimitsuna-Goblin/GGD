################################################################################################
# Functions for the bisection method
# @file         bisection.R
# @version      1.0.0
# @author       Kimitsuna-Goblin
# @copyright    Copyright (C) 2023 Ura Kimitsuna
# @license      Released under the MIT license.
#               see https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#' [Non-exported] Bisection method
#'
#' Solves a equation via bisection method.
#' @param   f           The destination function to solve.
#' @param   interval    A numeric vector of length 2 indicating the range of x value
#'                      on which the solution is to be searched.
#' @param   ftol        The function value tolerance.
#'                      When the absolute function value at either edge of the step is smaller
#'                      than \code{ftol}, the convergence is declared.
#' @param   xtol        The relative steplength tolerance.
#'                      When the relative steplength of x value is smaller than \code{xtol},
#'                      the convergence is declared.
#' @return  Solution of the equation f = 0 with tolerances.
################################################################################################
bisection <- function( f, interval, ftol = .Machine$double.eps * 16, xtol = ftol )
{
    xs <- c( interval, ( interval[1] + interval[2] ) / 2 )
    fs <- f( xs )
    if ( any( abs( fs ) <= ftol ) )
    {
        return ( xs[abs( f( xs ) ) == min( abs( f( xs ) ) )][1] )
    }

    if ( fs[1] * fs[2] > 0 )
    {
        stop( "Error: f() values at end points not of opposite sign." )
    }

    if ( fs[1] > 0 )
    {
        a <- interval[1]
        interval[1] <- interval[2]
        interval[2] <- a
    }

    return ( bisection.sub( f, interval, ftol, xtol ) )
}

################################################################################################
#' [Non-exported] Subfunction for bisection method
#'
#' The entity that processes bisection method.
#' @param   f           The destination function to solve.
#' @param   interval    A vector of the range on which the solution is to be searched.
#' @param   ftol        The function value tolerance.
#'                      When the absolute function value at either edge of the step is smaller
#'                      than \code{ftol}, the convergence is declared.
#' @param   xtol        The relative steplength tolerance.
#'                      When the relative steplength of x value is smaller than \code{xtol},
#'                      the convergence is declared.
#' @return  Solution of the equation f = 0 with tolerances.
#' @seealso \code{\link[ggd]{bisection}}
################################################################################################
bisection.sub <- function( f, interval, ftol, xtol )
{
    mid <- ( interval[1] + interval[2] ) / 2
    ans <- f( mid )
    step <- abs( interval[1] - interval[2] )

    if ( abs( ans ) <= ftol )
    {
        return ( mid )
    }
    else if ( step * 2 / ( abs( interval[1] ) + abs( interval[2] ) ) <= xtol )
    {
        xs <- c( interval, mid )
        return ( xs[abs( f( xs ) ) == min( abs( f( xs ) ) )][1] )
    }
    else if ( ans > 0 )
    {
        bisection.sub( f, c( interval[1], mid ), xtol, ftol )
    }
    else
    {
        bisection.sub( f, c( mid, interval[2] ), xtol, ftol )
    }
}
