################################################################################################
# Functions about normal distribution
# @file         cmn.norm.R
# @author       Kimitsuna-Goblin (Ura, Kimitsuna)
# @copyright    Copyright (C) 2023 Kimitsuna-Goblin (Ura, Kimitsuna)
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Functions

################################################################################################
#' Sigma-unit distance from the mean of a normal distribution
#'
#' Calculates how many sigmas are the distances from the mean of a normal distribution
#' to the values of the quantile function for the given probabilities.
#' The mean and standard deviation are not need for this function, because sigma-unit distances
#' do not depend on them.
#' @export
#' @param   p           A vector of probabilities.
#' @return  A vector of sigma-unit distances from the mean to the quantiles for
#'          the probabilities.
#' @importFrom  stats   qnorm
#' @examples
#'  sqnorm( 0.5 )   # 0
#'  sqnorm( pnorm( -2, 0, 1 ) ) # -2
#'  sqnorm( seq( 0, 1, 0.1 ) )  # increces from -Inf to Inf
################################################################################################
sqnorm <- function( p )
{
    return ( qnorm( p, 0, 1 ) )
}

################################################################################################
#' Standard deviation of a normal distribution
#'
#' Calculates the standard deviations of the normal distribution
#' satisfying the given mean and one other quantile.
#' @export
#' @param   mean    A vector of the mean values of the normal distributions.
#' @param   x       A vector of x-coordinates of the quantiles.
#'                  Each value of \code{x} must not be equal to the mean value.
#' @param   p       A vector of the probabilities for the quantiles.
#'                  In other word, the value of the cumulative distribution function of
#'                  a normal distribution for the \code{x}. The value must not be \code{0.5}.
#' @return  The vector of the standard deviations.
#' @examples
#'  sd.norm.mxp( 0, qnorm( 0.3, 0, 1 ), 0.3 )           # 1
#'  sd.norm.mxp( rep( 0, 5 ), 1:5, pnorm( 1:5, 0, 1 ) ) # 1 1 1 1 1
#'  sd.norm.mxp( c( -0.1, 0, 0.3 ), c( -0.3, -0.1, 0.4 ), c( 0.38, 0.47, 0.53 ) ) # [2] == [3]
################################################################################################
sd.norm.mxp <- function( mean, x, p )
{
    return ( ( x - mean ) / sqnorm( p ) )
}

################################################################################################
#' Mean and standard deviation of a normal distribution
#'
#' Calculates the mean and standard deviation of the normal distribution
#' satisfying the given two quantiles.
#' This is the body of \code{ggd.trace.q(kind = "Normal Distribution")}.
#' @export
#' @param   x   The x-coordinates of the quantiles. It must be a vector with 2 numerics.
#' @param   p   The probabilities for the quantiles.
#'              In other word, the values of the cumulative distribution function of
#'              a normal distribution for \code{x}. It must be also a vector with 2 numerics.
#' @return  A list containing components
#'          \item{mean}{
#'              The mean value of the normal distribution.}
#'          \item{sd}{
#'              The standard deviation of the normal distribution.}
#' @examples
#'  ms.norm.xp( x = c( -1, 1 ), p = pnorm( c( -1, 1 ), 0, 1 ) ) # list( mean = 0, sd = 1 )
#'  ms.norm.xp( x = c(  0, 1 ), p = pnorm( c(  0, 1 ), 0, 2 ) ) # list( mean = 0, sd = 2 )
#'  ms.norm.xp( x = c( -2, 1 ), p = c( 0.3, 0.7 ) ) # list( mean = 0.5, sd = 2.86 ) (about)
################################################################################################
ms.norm.xp <- function( x, p )
{
    d <- sqnorm( p[2] ) - sqnorm( p[1] )

    return ( list( mean = ( sqnorm( p[2] ) * x[1] - sqnorm( p[1] ) * x[2] ) / d,
                   sd = ( x[2] - x[1] ) / d ) )
}
