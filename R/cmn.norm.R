################################################################################################
# Functions about normal distribution
# @file         cmn.norm.R
# @author       Kimitsuna Ura
# @copyright    Copyright (C) 2024 Kimitsuna Ura
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Functions

################################################################################################
#' Sigma-unit distance from the mean of a normal distribution
#'
#' Calculates how many sigmas are the distance from the mean of a normal distribution
#' to the x-coordinate of the quantile which has the indicated probability.
#' The mean and standard deviation are not needed for this function
#' because the sigma-unit distance does not depend on them.
#' @export
#' @param   p           A vector of the probabilities of the quantiles.
#' @return  A vector of sigma-unit distances
#'          from the mean to the x-coordinates of the quantiles.
#' @importFrom  stats   qnorm
#' @examples
#'  sqnorm( 0.5 )               ## 0
#'  sqnorm( pnorm( -2, 0, 1 ) ) ## -2
#'  sqnorm( seq( 0, 1, 0.1 ) )  ## increces from -Inf to Inf
################################################################################################
sqnorm <- function( p )
{
    return ( qnorm( p, 0, 1 ) )
}

################################################################################################
#' Standard deviation of a normal distribution
#'
#' Calculates the standard deviation of the normal distribution
#' which has the indicated mean and passes through one other quantile.
#' @export
#' @param   mean    A vector of the mean values of the normal distributions.
#' @param   x       A vector of x-coordinates of the quantiles.
#'                  Each value of \code{x} must not be equal to the mean value.
#' @param   p       A vector of the probabilities for the quantiles.
#'                  In other word, the value of the cumulative distribution function of
#'                  a normal distribution for the \code{x}. The value must not be \code{0.5}.
#' @return  A vector of the standard deviations.
#' @examples
#'  sd.norm.mxp( 0, qnorm( 0.3, 0, 1 ), 0.3 )           ## 1
#'  sd.norm.mxp( rep( 0, 5 ), 1:5, pnorm( 1:5, 0, 1 ) ) ## 1 1 1 1 1
#'  sd.norm.mxp( c( -0.1, 0, 0.3 ), c( -0.3, -0.1, 0.4 ), c( 0.38, 0.47, 0.53 ) ) ## [2] == [3]
################################################################################################
sd.norm.mxp <- function( mean, x, p )
{
    return ( ( x - mean ) / sqnorm( p ) )
}

################################################################################################
#' Mean and standard deviation of a normal distribution
#'
#' Calculates the mean and standard deviation of the normal distribution
#' which passes through the indicated two quantiles.
#' This is the body of \code{ggd.trace.q(kind = "Normal Distribution")}.
#' @export
#' @param   x   The x-coordinates of the quantiles. It must be a vector with 2 elements.
#' @param   p   The probabilities for the quantiles.
#'              In other word, the values of the cumulative distribution function of
#'              a normal distribution for \code{x}. It must be also a vector with 2 elements.
#' @return  A list containing components
#'          \item{mean}{
#'              The mean value of the normal distribution.}
#'          \item{sd}{
#'              The standard deviation of the normal distribution.}
#' @examples
#'  ms.norm.xp( x = c( -1, 1 ), p = pnorm( c( -1, 1 ), 0, 1 ) ) ## list( mean = 0, sd = 1 )
#'  ms.norm.xp( x = c(  0, 1 ), p = pnorm( c(  0, 1 ), 0, 2 ) ) ## list( mean = 0, sd = 2 )
#'  ms.norm.xp( x = c( -2, 1 ), p = c( 0.3, 0.7 ) ) ## list( mean = -0.5, sd = about 2.86 )
################################################################################################
ms.norm.xp <- function( x, p )
{
    d <- sqnorm( p[2] ) - sqnorm( p[1] )

    return ( list( mean = ( sqnorm( p[2] ) * x[1] - sqnorm( p[1] ) * x[2] ) / d,
                   sd = ( x[2] - x[1] ) / d ) )
}
