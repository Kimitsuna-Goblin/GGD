################################################################################################
# Calculation of mean value and standard deviation
# @file         ggd.mean.sd.R
# @author       Kimitsuna Ura
# @copyright    Copyright (C) 2024 Kimitsuna Ura
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Functions

################################################################################################
#' [Non-exported] Mean calculation
#'
#' Calculates the mean value of a GGD model.
#' This function does not use \code{\link[stats]{integrate}}, but \code{\link[stats]{pnorm}},
#' \code{\link[base]{sqrt}} and four arithmetic operations as needed.
#' @param   mix.type    The value of \code{mix.type}.
#' @param   means       The vector of the mean values of the normal distributions
#'                      of the components.
#' @param   sds         The vector of the standard deviations of the normal distributions
#'                      of the components.
#' @return  The mean value of a gradational Gaussian distribution model.
################################################################################################
calc.mean <- function( mix.type, means, sds )
{
    mean <- numeric()

    if ( mix.type == 1 )
    {
        mean <- ( means[1] + means[2] ) / 2
    }
    else if ( mix.type == 2 )
    {
        mean <- ( means[1] + means[2] - ( sds[1] - sds[2] ) / sqrt( pi ) ) / 2
    }
    else if ( mix.type == 3 )
    {
        if ( ( length( means ) == 2 ) ||
             ( length( means ) >= 3 && means[1] == means[3] && sds[1] == sds[3] ) )
        {
            mean <- means[1] - ( means[1] - means[2] ) * sqrt( 2 ) / 2
        }
        else
        {
            mean <- ( ( means[1] + means[3] ) * ( 2 - sqrt( 2 ) ) -
                      ( sds[1] - sds[3] ) * sqrt( 2 ) / sqrt( pi ) ) / 4 +
                    means[2] * sqrt( 2 ) / 2
        }
    }
    else if ( mix.type == 4 )
    {
        mean <- means[1] - ( means[1] - means[2] ) * sqrt( 2 ) / 2 -
                calc.mean.t4.sub( means[1:2], sds[1:2] ) +
                calc.mean.t4.sub( means[3:4], sds[3:4] )
    }

    return ( mean )
}

################################################################################################
#' [Non-exported] Sub-function of the mean calculation for mix.type = 4
#'
#' A sub-function which is used for calculating the mean value where \code{mix.type = 4}.
#' It calculates \eqn{\int_{-\infty}^{\infty} x \Psi_i(x) g_i(x) dx},
#' where
#' \eqn{\Psi_i(x) = \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi_{i,1}^*(x) +
#'                  \dfrac{1}{\sqrt{2}} \Phi_{i,2}^*(x)},
#' \eqn{g_i(x) = ( 1 - f_{i,1}(x) / f_{i,1}(\mu_{i,1}) ) f_{i,1}(x) +
#'               f_{i,2}(x)^2 / f_{i,2}(\mu_{i,2})}
#' \eqn{( i = 1, 2 )}.
#' This function does not use \code{\link[stats]{integrate}},
#' but \code{\link[stats]{pnorm}}, \code{\link[base]{sqrt}} and four arithmetic operations.
#' @param   means   The mean values of two normal distributions
#'                  \eqn{\mathcal{N}_{i,1}, \mathcal{N}_{i,2}}.
#' @param   sds     The standard deviations of two normal distributions
#'                  \eqn{\mathcal{N}_{i,1}, \mathcal{N}_{i,2}}.
#' @return  The value of \eqn{\int_{-\infty}^{\infty} x \Psi_i(x) g_i(x) dx}.
#' @importFrom  stats   pnorm
################################################################################################
calc.mean.t4.sub <- function( means, sds )
{
    d.mean <- means[1] - means[2]

    ( ( ( 4 + sqrt( 2 ) - 2 * sqrt( 6 ) ) * sds[1] + sqrt( 2 ) * sds[2] ) / 8 +
      sqrt( ( 2 * sds[1]^2 + sds[2]^2 ) / 8 ) *
      exp( -d.mean^2 / ( 2 * sds[1]^2 + sds[2]^2 ) ) -
      sqrt( sds[1]^2 + sds[2]^2 ) *
      exp( -d.mean^2 / ( sds[1]^2 + sds[2]^2 ) ) / 4 ) / sqrt( pi ) +
    ( means[1] + means[2] ) / 4 -
    d.mean *
    ( sqrt( 2 ) * pnorm( -d.mean / sqrt( sds[1]^2 + sds[2]^2 / 2 ), 0, 1 ) -
      pnorm( -sqrt( 2 ) * d.mean / sqrt( sds[1]^2 + sds[2]^2 ), 0, 1 ) ) / 2
}

################################################################################################
#' [Non-exported] Variance calculation
#'
#' Calculates the variance or the half variance of a GGD model.
#' This function does not use \code{\link[stats]{integrate}},
#' but \code{\link[stats]{dnorm}}, \code{\link[stats]{pnorm}}, \code{\link[base]{sqrt}}
#' and four arithmetic operations.
#'
#' For \code{mix.type = 4}, the half variance option of
#' \code{get.lv = TRUE} or \code{get.uv = TRUE} does not work. The reason is how to calculate
#' \eqn{\int_{-\infty}^{\mu} f_{i,1}(x) \Phi_{i,2}(x) dx} without numerical integral function
#' is still unknown for us.
#' @param   mix.type    The value of \code{mix.type} of the \code{\link[ggd]{GGD}} class.
#' @param   means       The mean values of the normal distributions of the components.
#' @param   sds         The standard deviations of the normal distributions of the components.
#' @param   mean        The mean of the whole distribution.
#' @param   symmetric   If \code{TRUE}, the distribution model is treated as symmetric.
#'                      This option may help reduce calculation errors.
#' @param   get.lv      If \code{TRUE}, this function calculates the lower half variance.
#' @param   get.uv      If \code{TRUE}, this function calculates the upper half variance.
#'
#'                      Only one of \code{get.lv = TRUE} and \code{get.uv = TRUE} are valid;
#'                      if both are \code{TRUE}, \code{get.lv} takes priority.
#'                      If both are \code{FALSE} (the default),
#'                      this function calculates the whole variance.
#' @return  The value of the whole/half variance.
#' @importFrom  stats   dnorm pnorm
################################################################################################
calc.v <- function( mix.type, means, sds,
                    mean = calc.mean( mix.type, means, sds ),
                    symmetric = FALSE, get.lv = FALSE, get.uv = FALSE )
{
    v <- 0

    if ( mix.type == 1 )
    {
        if ( symmetric )
        {
            if ( means[1] == means[2] )
            {
                if ( get.lv || get.uv )
                {
                    v <- ( sds[1]^2 + sds[2]^2 ) / 4
                }
                else
                {
                    v <- ( sds[1]^2 + sds[2]^2 ) / 2
                }
            }
            else # if ( sds[1] == sds[2] )
            {
                v <- ( means[1] - means[2] )^2 / 4 + sds[1]^2

                if ( get.lv || get.uv )
                {
                    v <- v / 2
                }
            }
        }
        else
        {
            v <- ( ( means[1] - means[2] )^2 / 2 + sds[1]^2 + sds[2]^2 ) / 2

            if ( get.lv || get.uv )
            {
                d <- dnorm( mean, means, sds )
                p <- pnorm( mean, means, sds )

                v.half <- ( sds[1]^2 * p[1] + sds[2]^2 * p[2] +
                            ( means[1] - means[2] )^2 * ( p[1] + p[2] ) / 4 -
                            ( means[1] - means[2] ) * ( sds[1]^2 * d[1] - sds[2]^2 * d[2] ) / 2 ) / 2

                if ( get.lv )
                {
                    v <- v.half
                }
                else
                {
                    v <- v - v.half
                }
            }
        }
    }
    else if ( mix.type == 2 )
    {
        if ( symmetric )
        {
            v <- ( means[1] - means[2] )^2 / 4 + sds[1]^2 -
                 ( means[1] - means[2] ) * sds[1] / sqrt( pi )

            if ( get.lv || get.uv )
            {
                v <- v / 2
            }
        }
        else
        {
            if ( get.lv || get.uv )
            {
                v <- calc.v.sub( 2, mean, means[2], sds[2], mean, 0 ) -
                     calc.v.sub( 2, mean, means[1], sds[1], mean, 2 )
            }

            if ( !get.lv )
            {
                v <- ( means[1]^2 + means[2]^2 + sds[1]^2 + sds[2]^2 ) / 2 - mean^2 +
                     ( sds[2] * means[2] - sds[1] * means[1] ) / sqrt( pi ) - v
            }
        }
    }
    else if ( mix.type == 3 )
    {
        if ( length( means ) == 3 && means[1] == means[3] && sds[1] == sds[3] )
        {
            means <- means[1:2]
            sds <- sds[1:2]
        }

        if ( symmetric )
        {
            if ( length( means ) == 2 )
            {
                v <- sds[1]^2 + ( sds[2]^2 - sds[1]^2 ) * sqrt( 2 ) / 4
            }
            else
            {
                v <- ( means[1] - mean )^2 * ( 2 - sqrt( 2 ) ) / 2 -
                     ( means[1] - mean ) * sds[1] * sqrt( 2 ) / sqrt( pi ) +
                     sds[1]^2 * ( 4 - sqrt( 2 ) ) / 4 + sds[2]^2 * sqrt( 2 ) / 4
            }

            if ( get.lv || get.uv )
            {
                v <- v / 2
            }
        }
        else if ( length( means ) == 2 )
        {
            if ( get.lv || get.uv )
            {
                d.1 <- dnorm( mean, means[1], sds[1] )
                p.1 <- pnorm( mean, means[1], sds[1] )
                dstar <- dnorm( mean, means[1:2], sds[1:2] * sqrt( 2 ) / 2 )
                pstar <- pnorm( mean, means[1:2], sds[1:2] * sqrt( 2 ) / 2 )
                d.mean <- means[1] - means[2]

                v <- ( d.mean^2 / 2 + sds[1]^2 ) * p.1 -
                     d.mean * sds[1]^2 * d.1 * sqrt( 2 ) / 2 -
                     ( ( d.mean^2 + sds[1]^2 ) * pstar[1] * sqrt( 2 ) -
                       d.mean * sds[1]^2 * dstar[1] -
                       ( d.mean^2 * ( 3 * sqrt( 2 ) - 4 ) + sds[2]^2 * sqrt( 2 ) ) * pstar[2] -
                       d.mean * sds[2]^2 * dstar[2] * ( sqrt( 2 ) - 1 ) ) / 4
            }

            if ( !get.lv )
            {
                v <- ( means[1] - means[2] )^2 * ( sqrt( 2 ) - 1 ) / 2 +
                     ( ( 4 - sqrt( 2 ) ) * sds[1]^2 + sqrt( 2 ) * sds[2]^2 ) / 4 - v
            }
        }
        else
        {
            if ( get.lv )
            {
                v <- calc.v.sub( 3, mean, means[1], sds[1], min( mean, means[1] ), k = 1 ) +
                     calc.v.sub( 3, mean, means[2], sds[2], mean, k = 2 )

                if ( means[3] < mean )
                {
                    v <- v + calc.v.sub( 3, mean, means[3], sds[3], mean, k = 3 ) -
                         ( means[3] - mean )^2 * ( 2 - sqrt( 2 ) ) / 4 +
                         ( means[3] - mean ) * sds[3] * sqrt( 2 ) / sqrt( pi ) / 2 -
                         sds[3]^2 * ( 4 - sqrt( 2 ) ) / 8
                }
            }
            else if ( get.uv )
            {
                v <- ( means[2] - mean )^2 * sqrt( 2 ) / 2 +
                     sds[2]^2 * sqrt( 2 ) / 4 -
                     calc.v.sub( 3, mean, means[2], sds[2], mean, k = 2 ) +
                     ( means[3] - mean )^2 * ( 2 - sqrt( 2 ) ) / 2 +
                     sds[3]^2 * ( 4 - sqrt( 2 ) ) / 4 -
                     calc.v.sub( 3, mean, means[3], sds[3], max( mean, means[3] ), k = 3 )

                if ( mean < means[1] )
                {
                    v <- v - calc.v.sub( 3, mean, means[1], sds[1], mean, k = 1 ) +
                         ( means[1] - mean )^2 * ( 2 - sqrt( 2 ) ) / 4 -
                         ( means[1] - mean ) * sds[1] * sqrt( 2 ) / sqrt( pi ) / 2 +
                         sds[1]^2 * ( 4 - sqrt( 2 ) ) / 8
                }
            }
            else
            {
                v <- ( ( means[1] - mean )^2 + ( means[3] - mean )^2 ) *
                     ( 2 - sqrt( 2 ) ) / 4 -
                     ( ( means[1] - mean ) * sds[1] - ( means[3] - mean ) * sds[3] ) *
                     sqrt( 2 ) / sqrt( pi ) / 2 +
                     ( sds[1]^2 + sds[3]^2 ) * ( 4 - sqrt( 2 ) ) / 8 +
                     ( ( means[2] - mean )^2 * 2 + sds[2]^2 ) * sqrt( 2 ) / 4
            }
        }
    }
    else if ( mix.type == 4 )
    {
        # For mix.type ==4, both get.lv and get.uv options are invalid.
        # Use calc.v.t4.via.integrate instead.

        if ( get.lv || get.uv )
        {
            stop( "Error: The get.lv and get.uv are not supported yet for mix.type = 4." )
        }

        v <- ( ( 2 - sqrt( 2 ) ) * means[1]^2 + ( 4 - sqrt( 2 ) ) * sds[1]^2 / 2 +
             sqrt( 2 ) * ( means[2]^2 + sds[2]^2 / 2 ) ) / 2 - mean^2 -
             calc.v.sub.t4( means[1:2], sds[1:2] ) +
             calc.v.sub.t4( means[3:4], sds[3:4] )
    }

    return ( v )
}

################################################################################################
#' [Non-exported] Sub-functions for variance calculation for mix.type = 2, 3
#'
#' A sub-function for calculating the variance of a GGD model.
#' The meaning of this function is depend on \code{mix.type} (see "Details").
#' This function dose not use \code{\link[stats]{integrate}}.
#' @param   mix.type    The value which represent the way to mix the normal distributions.
#' @param   mean        The mean of the whole distribution.
#' @param   mean.i      The mean value of the normal distribution of i-th component.
#' @param   sd.i        The standard deviation of the normal distribution of i-th component.
#' @param   x           The upper limit of the integral interval (see formulas in "Details").
#' @param   p.sum       The sum of the probabilities of two normal distributions of
#'                      the components. This argument is for \code{mix.type = 2}.
#' @param   k           The number indicating the normal distribution in the components.
#'                      This argument is for \code{mix.type = 3}.
#' @details
#'  Depending on the value of mix.type, the following calculations are performed without
#'  \code{\link[stats]{integrate}} but with \code{\link[stats]{dnorm}},
#'  \code{\link[stats]{pnorm}}, \code{\link[base]{sqrt}} and four arithmetic operations,
#'  respectively.
#'  The variance will be expressed with the sums and differences of the outputs of this function
#'  and sums or products with some simple terms.
#'
#'  In the following expressions,
#'  \eqn{\mu} is the mean of the whole distribution,
#'  \eqn{f_i(x)} is the probability density function of the normal distribution
#'  \eqn{\mathcal{N}(\mu_i, \sigma_i^2)},
#'  \eqn{\Phi_i(x)} is the cumulative distribution function of
#'  \eqn{\mathcal{N}(\mu_i, \sigma_i^2)}, and \eqn{\bar \Phi(x)} is the mean of
#'  two cumulative distribution functions \eqn{\Phi_1(x)} and \eqn{\Phi_2(x)}.
#'
#'  \describe{
#'      \item{mix.type = 2}{
#'          \deqn{\displaystyle
#'                \int_{-\infty}^x (t - \mu)^2 (\Phi_i(t) - \bar \Phi(q)) f_i(t) \ dt}}
#'
#'      \item{mix.type = 3}{
#'          \deqn{k = 1, 3 \ :
#'              \ \displaystyle
#'                \int_{-\infty}^x (t - \mu)^2 (1 - \dfrac{f_i(t)}{f_i(\mu_i)}) f_i(t) \ dt}
#'
#'          \deqn{k = 2 \ :
#'              \ \displaystyle
#'                \int_{-\infty}^x (t - \mu)^2 \dfrac{f_i(t)^2}{f_i(\mu_i)} f_i(t) \ dt}}
#'  }
#' @return  Calculated value of the expression shown in "Details".
#' @importFrom  stats   dnorm pnorm
################################################################################################
calc.v.sub <- function( mix.type, mean, mean.i, sd.i, x, p.sum = 0, k = 0 )
{
    v <- numeric()
    d.i <- dnorm( x, mean.i, sd.i )
    p.i <- pnorm( x, mean.i, sd.i )

    if ( mix.type == 2 )
    {
        pstar.i <- pnorm( x, mean.i, sd.i * sqrt( 2 ) / 2 )

        # To reduce cancellations of significant digits that occur when
        # the difference between the two normal distributions is small,
        # p.i of ( p.i^2 - p.sum * p.i ) is not taken out of parentheses.
        # However, cancellations still can occur.
        v <- ( ( mean.i - mean )^2 + sd.i^2 ) * ( p.i^2 - p.sum * p.i ) / 2 +
             ( mean.i - mean ) * sd.i * pstar.i / sqrt( pi ) -
             sd.i^2 * ( x + mean.i - 2 * mean ) * ( p.i - p.sum / 2 ) * d.i -
             sd.i^4 * d.i^2 / 2
    }
    else if ( mix.type == 3 )
    {
        dstar.i <- dnorm( x, mean.i, sd.i * sqrt( 2 ) / 2 )
        pstar.i <- pnorm( x, mean.i, sd.i * sqrt( 2 ) / 2 )

        v <- ( ( mean.i^2 + sd.i^2 / 2 ) * pstar.i -
               ( x + mean.i ) * sd.i^2 * dstar.i / 2 -
               mean * ( 2 * mean.i * pstar.i - sd.i^2 * dstar.i ) +
               mean^2 * pstar.i ) * sqrt( 2 ) / 2

        if ( k == 1 || k == 3 )
        {
            v <- ( mean.i^2 + sd.i^2 ) * p.i -
                 ( x + mean.i ) * sd.i^2 * d.i -
                 2 * mean * ( mean.i * p.i - sd.i^2 * d.i ) +
                 mean^2 * p.i - v
        }
    }

    return ( v )
}

################################################################################################
#' [Non-exported] A sub-function for variance calculation for mix.type = 4
#'
#' A sub-function for calculating the variance of a GGD model where \code{mix.type = 4}.
#' This function calculates \eqn{\int_{-\infty}^{\infty} x^2 \Psi_i(x) g_i(x) dx},
#' where
#' \eqn{\Psi_i(x) = \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi_{i,1}^*(x) +
#'                                  \dfrac{1}{\sqrt{2}} \Phi_{i,2}^*(x)},
#' \eqn{g_i(x) = ( 1 - f_{i,1}(x) / f_{i,1}(\mu_{i,1}) ) f_{i,1}(x) +
#'               f_{i,2}(x)^2 / f_{i,2}(\mu_{i,2})}
#' with fixing \eqn{i}. This function does not use \code{\link[stats]{integrate}},
#' but \code{\link[stats]{pnorm}}, \code{\link[base]{sqrt}} and four arithmetic operations.
#' @param   means   The mean values of the normal distributions
#'                  \eqn{\mathcal{N}_{i,1}} and \eqn{\mathcal{N}_{i,2}}.
#' @param   sds     The standard deviations of the normal distributions
#'                  \eqn{\mathcal{N}_{i,1}} and \eqn{\mathcal{N}_{i,2}}.
#' @return  The value of \eqn{\int_{-\infty}^{\infty} x^2 \Psi_i(x) g_i(x) dx}.
#' @importFrom  stats   pnorm
################################################################################################
calc.v.sub.t4 <- function( means, sds )
{
    d.mean <- means[1] - means[2]

    ( ( 6 - 4 * sqrt( 2 ) ) * means[1]^2 + ( 5 - 3 * sqrt( 2 ) ) * sds[1]^2 +
      ( 4 * sqrt( 2 ) - 2 ) * means[2]^2 + ( 2 * sqrt( 2 ) - 1 ) * sds[2]^2 ) / 8 +
    ( ( 4 + sqrt( 2 ) - 2 * sqrt( 6 ) ) * means[1] * sds[1] +
      sqrt( 2 ) * means[2] * sds[2] ) / sqrt( pi ) / 4 +
    ( sqrt( 2 ) * ( 2 * ( means[1]^2 - means[2]^2 + sds[1]^2 ) - sds[2]^2 ) *
      pnorm( d.mean / sqrt( sds[1]^2 + sds[2]^2 / 2 ), 0, 1 ) -
      ( 2 * ( means[1]^2 - means[2]^2 ) + sds[1]^2 - sds[2]^2 ) *
      pnorm( sqrt( 2 ) * d.mean / sqrt( sds[1]^2 + sds[2]^2 ), 0, 1 ) ) / 4 +
    ( means[1] + means[2] ) *
    ( sqrt( 4 * sds[1]^2 + 2 * sds[2]^2 ) *
      exp( -d.mean^2 / ( 2 * sds[1]^2 + sds[2]^2 ) ) -
      sqrt( sds[1]^2 + sds[2]^2 ) *
      exp( -d.mean^2 / ( sds[1]^2 + sds[2]^2 ) ) ) / sqrt( pi ) / 4
}

################################################################################################
#' [Non-exported] Half variance computation for mix.type = 4
#'
#' Using \code{\link[stats]{integrate}},
#' computes the lower/upper half variance for \code{mix.type = 4}.
#' Computing the whole variance with \code{\link[stats]{integrate}} is also enabled.
#' @param   means       The mean values of the normal distributions of the components.
#' @param   sds         The standard deviations of the normal distributions of the components.
#' @param   mean        The mean of the whole distribution.
#' @param   get.lv      If \code{TRUE}, computes the lower half variance.
#' @param   get.uv      If \code{TRUE}, computes the upper half variance.
#'                      Although it is not recommended to set
#'                      both \code{get.lv} and \code{get.uv} to \code{TRUE},
#'                      if both are \code{TRUE}, get.lv takes priority.
#'                      If both are \code{FALSE}, computes the whole variance.
#' @return  A list of the output of \code{\link[stats]{integrate}}
#'          as the result of the half/whole variance computation.
#' @importFrom  stats   dnorm pnorm integrate
################################################################################################
calc.v.t4.via.integrate <- function( means, sds, mean = calc.mean( 4, means, sds ),
                                     get.lv = FALSE, get.uv = FALSE )
{
    f <- function( x )
    {
        ( x - mean )^2 *
        ( ( 1 - pnorm( x, means[1], sds[1] ) +
                pnorm( x, means[1], sds[1] * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 -
                pnorm( x, means[2], sds[2] * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 ) *
          ( ( 1 - dnorm( x, means[1], sds[1] ) / dnorm( means[1], means[1], sds[1] ) ) *
                  dnorm( x, means[1], sds[1] ) +
                  dnorm( x, means[2], sds[2] )^2 / dnorm( means[2], means[2], sds[2] ) ) +
          ( pnorm( x, means[3], sds[3] ) -
            pnorm( x, means[3], sds[3] * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 +
            pnorm( x, means[4], sds[4] * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 ) *
          ( ( 1 - dnorm( x, means[3], sds[3] ) / dnorm( means[3], means[3], sds[3] ) ) *
                  dnorm( x, means[3], sds[3] ) +
                  dnorm( x, means[4], sds[4] )^2 / dnorm( means[4], means[4], sds[4] ) ) )
    }

    result <- list()
    if ( get.lv )
    {
        result <- integrate( f, -Inf, mean )
    }
    else if ( get.uv )
    {
        result <- integrate( f, mean, Inf )
    }
    else
    {
        result <- integrate( f, -Inf, Inf )
    }

    return ( result )
}

################################################################################################
#' Adjust the median, mean and sd fields
#'
#' Calculates the median, the mean and the standard deviation of the distribution model and
#' sets those values into the fields.
#' Before calling this method, you must set \code{cmp} field and \code{mix.type} field.
#' Normally, users of this class don't need to call this method directly.
#' @name    adjust.median.mean.sd
#' @aliases adjust.median.mean.sd
#' @aliases \S4method{adjust.median.mean.sd}{GGD}
#' @usage   \S4method{adjust.median.mean.sd}{GGD}()
#' @return  The \code{\link[ggd]{GGD}} object itself (invisible).
################################################################################################
NULL
GGD$methods(
    adjust.median.mean.sd = function()
    {
        means <- cmp$mean
        sds <- cmp$sd

        if ( length( means ) == 0 )
        {
            # No data
            median <<- mean <<- sd <<- lsd <<- usd <<- lsd.abs.error <<- usd.abs.error <<- NaN
        }
        else if ( is.normal() )
        {
            # Normal Distribution
            median <<- mean <<- means[1]
            sd <<- lsd <<- usd <<- sds[1]
            lsd.abs.error <<- usd.abs.error <<- 0
        }
        else
        {
            abst.mix.type <- mix.type   # abstract mix.type
            if ( mix.type == 4 )
            {
                if ( is.v2( strict = FALSE ) )
                {
                    abst.mix.type <- 3
                    means <- means[1:2]
                    sds <- sds[1:2]
                }
                else if ( is.h( strict = FALSE ) )
                {
                    abst.mix.type <- 2
                    means <- means[c( 1, 3 )]
                    sds <- sds[c( 1, 3 )]
                }
            }

            if ( is.symmetric() )
            {
                # Symmetric Distribution
                if ( length( means ) == 2 )
                {
                    if ( means[1] == means[2] )
                    {
                        median <<- mean <<- means[1]
                    }
                    else
                    {
                        median <<- mean <<- ( means[1] + means[2] ) / 2
                    }
                }
                else if ( length( means ) == 3 )
                {
                    median <<- mean <<- means[2]
                }
                else # if ( length( means ) == 4 )
                {
                    if ( means[1] == means[3] )
                    {
                        median <<- mean <<- means[1]
                    }
                    else if ( means[2] == means[4] )
                    {
                        median <<- mean <<- means[2]
                    }
                    else
                    {
                        # To average errors due to floating point,
                        # take the mean of 4 mean values, not of 2 mean values.
                        median <<- mean <<- sum( means ) / 4
                    }
                }

                # S.D. = lower S.D = upper S.D.
                sd <<- lsd <<- usd <<- sqrt( calc.v( abst.mix.type, means, sds, mean,
                                                     symmetric = TRUE ) )
                lsd.abs.error <<- usd.abs.error <<- 0
            }
            else
            {
                # Asymmetric Distribution
                if ( is.eq.mean() )
                {
                    median <<- means[1]
                }
                else
                {
                    median <<- bisection( function( x ) { p( x ) - 0.5 },
                                          c( min( means ), max( means ) ) )
                }

                mean <<- calc.mean( abst.mix.type, means, sds )

                if ( abst.mix.type == 4 )
                {
                    sd <<- sqrt( calc.v( 4, means, sds, mean ) )

                    lv <- calc.v.t4.via.integrate( means, sds, mean, get.lv = TRUE )
                    lsd <<- sqrt( 2 * lv$value )
                    lsd.abs.error <<- sqrt( 2 * lv$abs.error )

                    uv <- calc.v.t4.via.integrate( means, sds, mean, get.uv = TRUE )
                    usd <<- sqrt( 2 * uv$value )
                    usd.abs.error <<- sqrt( 2 * uv$abs.error )
                }
                else
                {
                    sd  <<- sqrt( calc.v( abst.mix.type, means, sds, mean ) )
                    lsd <<- sqrt( 2 * calc.v( abst.mix.type, means, sds, mean, get.lv = TRUE ) )
                    usd <<- sqrt( 2 * calc.v( abst.mix.type, means, sds, mean, get.uv = TRUE ) )

                    lsd.abs.error <<- usd.abs.error <<- 0
                }
            }
        }

        return ( invisible( .self ) )
    }
)
