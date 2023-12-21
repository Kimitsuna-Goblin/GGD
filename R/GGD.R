################################################################################################
# Gradational Gaussian Distribution class
# @file         GGD.R
# @version      1.0.0
# @author       Kimitsuna-Goblin
# @copyright    Copyright (C) 2023 Ura Kimitsuna
# @license      Released under the MIT license.
#               see https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Constants

## Square root of 2 pi
sqrt.2pi <- sqrt( 2 * pi )

## Handle of the probability density function for mix.type = 3
f.t3.d <- list( function( x, m, s )
                { ( 1 - dnorm( x, m, s ) * sqrt.2pi * s ) * dnorm( x, m, s ) },
                function( x, m, s )
                { dnorm( x, m, s )^2 * sqrt.2pi * s },
                0 )
## Handle of the cumulative distribution function for mix.type = 3
f.t3.p <- list( function( x, m, s )
                { pnorm( x, m, s ) - pnorm( x, m, s * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 },
                function( x, m, s )
                { pnorm( x, m, s * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 },
                ( ( 2 - sqrt( 2 ) ) / 4 ) )

## Kinds of supported distributions                                                # kind.index
kinds <- c( "Normal Distribution",                                                          # 1
            "Mean of Mean-Differed Sigma-Equaled 2 Normal Distributions",                   # 2
            "Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions",                   # 3
            "Mean of Mean-Differed Sigma-Differed 2 Normal Distributions",                  # 4
            "Mean-Differed Sigma-Equaled Horizontal Gradational Distribution",              # 5
            "Mean-Equaled Sigma-Differed Horizontal Gradational Distribution",              # 6
            "Mean-Differed Sigma-Differed Horizontal Gradational Distribution",             # 7
            "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution",              # 8
            "2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution",              # 9
            "2-Mean-Differed Sigma-Differed Vertical Gradational Distribution",             #10
            "3-Mean-Differed Sigma-Equaled Vertical Gradational Distribution",              #11
            "3-Mean-Equaled Sigma-Differed Vertical Gradational Distribution",              #12
            "3-Mean-Differed Sigma-Differed Vertical Gradational Distribution",             #13
            "Mean-Differed Sigma-Equaled Horizontal-Vertical Gradational Distribution",     #14
            "Mean-Equaled Sigma-Differed Horizontal-Vertical Gradational Distribution",     #15
            "Mean-Differed Sigma-Differed Horizontal-Vertical Gradational Distribution" )   #16

## Matching order of ggd:::kinds for regular expressions
kinds.match.order <- c( 1L, 4L, 2L, 3L, 7L, 5L, 6L, 10L, 8L, 9L, 13L, 11L, 12L, 16L, 14L, 15L )

## TeX templates
##
## In these templates, with write.comma = TRUE option,
## single ";" will be replaced to a comma (",") and
## double ";;" will be replaced to a period (".").
## And with write.comma = FALSE option, all of ";"s are erased.
##
## Remark, "\\right." is not a period but an invisible bracket.

## TeX templates (common modules)
tex.form.header <- "\\begin{align}"
tex.form.footer <- "\\end{align}"

## TeX templates (probability density functions)
tex.d.main <- list(
        # mix.type = 0
        "g(x) &= f(x);\\\\",
        # mix.type = 1
        "g(x) &= \\dfrac{1}{2} ( f_1(x) + f_2(x) );\\\\",
        # mix.type = 2
        "g(x) &= \\left( 1 - \\Phi_1(x) \\right) f_1(x) + \\Phi_2(x) f_2(x);\\\\",
        # mix.type = 3
        list( # 2 components
              paste0( "g(x) &= \\left( 1 - \\dfrac{f_1(x)}{f_1(\\mu_1)} \\right) f_1(x) + ",
                              "\\dfrac{f_2(x)}{f_2(\\mu_2)} f_2(x);\\\\" ),
              # 3 components
              "g(x) &= g_1(x) + g_2(x) + g_3(x) ;\\\\" ),
        # mix.type = 4
        "g(x) &= \\left( 1 - \\Psi_1(x) \\right) g_1(x) + \\Psi_2(x) g_2(x);\\\\" )

tex.d.sub <- list(
        # mix.type = 0
        paste0(
            "f(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma^2}} ",
                    "\\exp \\left( -\\dfrac{(x - \\mu)^2}{2 \\sigma^2} \\right);\\\\" ),
        # mix.type = 1
        paste0(
            "f_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
                      "\\exp \\left( -\\dfrac{(x - \\mu_i)^2}{2 \\sigma_i^2} \\right);\\\\" ),
        # mix.type = 2
        paste0(
            "f_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
                      "\\exp \\left( -\\dfrac{(x - \\mu_i)^2}{2 \\sigma_i^2} \\right);\\\\" ),
        # mix.type = 3
        list( # 2 components and common
              paste0(
                 "f_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
                           "\\exp \\left( -\\dfrac{(x - \\mu_i)^2}{2 \\sigma_i^2} ",
                           "\\right);\\\\" ),
              # 3 components only
              c( "g_1(x) &= \\left\\lbrace",
                           "\\begin{array}{ll}",
                 paste0(   "\\left( 1 - \\dfrac{f_1(x)}{f_1(\\mu_1)} \\right) ",
                           "f_1(x) & (x \\leq \\mu_1);\\\\" ),
                           "0 & (x > \\mu_1);\\\\",
                           "\\end{array} \\right.\\\\",
                 "g_2(x) &= \\dfrac{f_2(x)}{f_2(\\mu_2)} f_2(x);\\\\",
                 paste0(
                 "g_3(x) &= \\left\\lbrace \\begin{array}{ll}",
                           "0 & (x < \\mu_3);\\\\" ),
                 paste0(   "\\left( 1 - \\dfrac{f_3(x)}{f_3(\\mu_3)} \\right) f_3(x) & ",
                           "(x \\geq \\mu_3);" ),
                           "\\end{array} \\right.\\\\" ) ),
        # mix.type = 4
        c(  paste0(
            "g_i(x) &= \\left( 1 - ",
                      "\\dfrac{f_{i,1}(x)}{f_{i,1}(\\mu_{i,1})} \\right) f_{i,1}(x) + ",
                      "\\dfrac{f_{i,2}(x)}{f_{i,2}(\\mu_{i,2})} f_{i,2}(x);\\\\" ),
            paste0(
            "f_{i,j}(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_{i,j}^2}} ",
                    "\\exp \\left( -\\dfrac{(x - \\mu_{i,j})^2}{2 \\sigma_{i,j}^2} ",
                    "\\right);\\\\" ) ) )

## TeX templates (cumulative distribution functions)
tex.p.main <- list(
        # mix.type = 0
        "\\Psi(x) &= \\Phi(x);\\\\",
        # mix.type = 1
        "\\Psi(x) &= \\dfrac{1}{2} ( \\Phi_1(x) + \\Phi_2(x) );\\\\",
        # mix.type = 2
        paste0( "\\Psi(x) &= \\Phi_1(x) - \\dfrac{1}{2} \\Phi_1(x)^2 + ",
                            "\\dfrac{1}{2} \\Phi_2(x)^2;\\\\" ),
        # mix.type = 3
        list( # 2 components
              paste0( "\\Psi(x) &= \\Phi_1(x) - \\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_1(x) + ",
                                  "\\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_2(x);\\\\" ),
              # 3 components
              "\\Psi(x) &= \\Psi_1(x) + \\Psi_2(x) + \\Psi_3(x);\\\\" ),
        # mix.type = 4
        paste0( "\\Psi(x) &= \\Psi_1(x) - \\dfrac{1}{2} \\Psi_1(x)^2 + ",
                            "\\dfrac{1}{2} \\Psi_2(x)^2;\\\\" ) )

tex.p.sub <- list (
        # mix.type = 0
        paste0(
            "\\Phi(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma^2}} ",
                        "\\int_{-\\infty}^{x} ",
                        "\\exp \\left( -\\dfrac{(t - \\mu)^2}{2 \\sigma^2} \\right) dt;\\\\" ),
        # mix.type = 1
        paste0(
            "\\Phi_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
                          "\\int_{-\\infty}^{x} ",
                          "\\exp \\left( -\\dfrac{(t - \\mu_i)^2}{2 \\sigma_i^2} \\right) dt;",
                          "\\\\" ),
        # mix.type = 2
        paste0(
            "\\Phi_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
                          "\\int_{-\\infty}^{x} ",
                          "\\exp \\left( ",
                          "-\\dfrac{(t - \\mu_i)^2}{2 \\sigma_i^2} \\right) dt;\\\\" ),
        # mix.type = 3
        list( # 2 components and common
              c( paste0(
                 "\\Phi_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
                               "\\int_{-\\infty}^{x} ",
                               "\\exp \\left( -\\dfrac{(t - \\mu_i)^2}{2 \\sigma_i^2} \\right) ",
                               "dt;\\\\" ),
                 paste0(
                 "\\Phi^\\ast_i(x) &= \\dfrac{1}",
                                     "{\\sqrt{2 \\pi \\left( ",
                                     "%begin-frac-env%\\dfrac{\\sigma_i}{\\sqrt{2}} ",
                                     "%end-frac-env%\\right)^2}} ",
                                     "\\int_{-\\infty}^{x} ",
                                     "\\exp \\left( %begin-frac-env%-\\dfrac{(t - \\mu_i)^2}",
                                     "{2 \\left( %begin-frac-env%\\dfrac{\\sigma_i}",
                                     "{\\sqrt{2}} %end-frac-env%\\right)^2} %end-frac-env%",
                                     "\\right) dt;\\\\" ) ),
              # 3 components only
              c( paste0(
                 "\\Psi_1(x) &= \\mathrm{min} \\left( ",
                               "\\Phi_1(x) - \\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_1(x), \\ ",
                               "\\dfrac{2 - \\sqrt{2}}{4} \\right);\\\\" ),
                 "\\Psi_2(x) &= \\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_2(x);\\\\",
                 paste0(
                 "\\Psi_3(x) &= \\mathrm{max} \\left( ",
                               "0, \\ \\Phi_3(x) - \\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_3(x) - ",
                               "\\dfrac{2 - \\sqrt{2}}{4} \\right);\\\\" ) ) ),
        # mix.type = 4
        c(  paste0(
            "\\Psi_i(x) &= \\Phi_{i,1}(x) - ",
                          "\\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_{i,1}(x) + ",
                          "\\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_{i,2}(x);\\\\" ),
            paste0(
            "\\Phi_{i,j}(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_{i,j}^2}} ",
                              "\\int_{-\\infty}^{x} ",
                              "\\exp \\left( -\\dfrac{(t - \\mu_{i,j})^2}{2 \\sigma_{i,j}^2} ",
                              "\\right) dt;\\\\" ),
            paste0(
            "\\Phi^\\ast_{i,j}(x) &= \\dfrac{1}",
                                    "{\\sqrt{2 \\pi \\left( %begin-frac-env%",
                                    "\\dfrac{\\sigma_{i,j}}{\\sqrt{2}} %end-frac-env%",
                                    "\\right)^2}} \\int_{-\\infty}^{x} ",
                                    "\\exp \\left( %begin-frac-env%",
                                    "-\\dfrac{(t - \\mu_{i,j})^2}",
                                    "{2 \\left( %begin-frac-env%\\dfrac{\\sigma_{i,j}}",
                                    "{\\sqrt{2}} %end-frac-env%\\right)^2} %end-frac-env%",
                                    "\\right) dt;\\\\" ) ) )

## TeX templates
# variables
tex.val.sub <- list(
        # mix.type = 0
        c( "\\\\",
           "& \\begin{array}{ll}",
           "\\mu = mean; & \\sigma = sd;;",
           "\\end{array}" ),
        # mix.type = 1, 2, 3 (2 components)
        c( "\\\\",
           "& \\begin{array}{ll}",
           "\\mu_1 = mean.1; & \\sigma_1 = sd.1;\\\\",
           "\\mu_2 = mean.2; & \\sigma_2 = sd.2;;",
           "\\end{array}" ),
        # mix.type = 3 (3 components)
        c( "\\\\",
           "& \\begin{array}{ll}",
           "\\mu_1 = mean.1; & \\sigma_1 = sd.1;\\\\",
           "\\mu_2 = mean.2; & \\sigma_2 = sd.2;\\\\",
           "\\mu_3 = mean.3; & \\sigma_3 = sd.3;;",
           "\\end{array}" ),
        # mix.type = 4
        c( "\\\\",
           "& \\begin{array}{ll}",
           "\\mu_{1,1} = mean.1.1; & \\sigma_{1,1} = sd.1.1;\\\\",
           "\\mu_{1,2} = mean.1.2; & \\sigma_{1,2} = sd.1.2;\\\\",
           "\\mu_{2,1} = mean.2.1; & \\sigma_{2,1} = sd.2.1;\\\\",
           "\\mu_{2,2} = mean.2.2; & \\sigma_{2,2} = sd.2.2;;",
           "\\end{array}" ) )

# environments for fractions
tex.begin.frac.env = c( "array"     = "\\\\begin{array}{c} ",
                        "aligned"   = "\\\\begin{aligned} ",
                        "gathered"  = "\\\\begin{gathered} ",
                        "default"   = "" )

tex.end.frac.env = c( "array"       = "\\\\end{array} ",
                      "aligned"     = "\\\\end{aligned} ",
                      "gathered"    = "\\\\end{gathered} ",
                      "default"     = "" )

################################################################################################
#  Classes, Functions

################################################################################################
# Functions for computing mean, variance and standard deviation

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
#'                  Each value of \code{x} must not be eqaul to the mean value.
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
#' satisfying the given two quantiles (x-probability points).
#' This is the body of \code{ggd.trace.q(kind = "Normal Distribution")}.
#' @export
#' @param   x       The x-coordinates of the quantiles. It must be a vector with 2 numerics.
#' @param   p       The probabilities for the quantiles.
#'                  In other word, the values of the cumulative distribution function of
#'                  a normal distribution for the x. It must be also a vector with 2 numerics.
#' @return  A list containing components
#'          \item{mean}{
#'                  The mean value of the normal distribution.}
#'          \item{sd}{
#'                  The standard deviation of the normal distribution.}
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

################################################################################################
#' [Non-exported] Subfunction of the mean calculation for mix.type = 4
#'
#' A subfunction which is used for calculating the mean value where \code{mix.type = 4}.
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
#' [Non-exported] Subfunctions for variance calculation for mix.type = 2, 3
#'
#' A subfunction for culculating the variance of a GGD model.
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
#' [Non-exported] A subfunction for variance calculation for mix.type = 4
#'
#' A subfunction for culculating the variance of a GGD model where \code{mix.type = 4}.
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
#' @param   get.lv      If \code{TRUE}, this function culculates the lower half variance.
#' @param   get.uv      If \code{TRUE}, this function culculates the upper half variance.
#'
#'                      Only one of \code{get.lv = TRUE} and \code{get.uv = TRUE} are valid;
#'                      if both are \code{TRUE}, \code{get.lv} takes priority.
#'                      If both are \code{FALSE} (the default),
#'                      this function culculates the whole variance.
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
# Functions for getting the kind of distribution

################################################################################################
#' Index number of the kind of distribution
#'
#' Gets index numbers which represent the kinds of distributions.
#' @export
#' @param   objs        A vector or a list of elements indicating the kind of distribution.
#'                      Each element must be a character string of a regular expression pattern
#'                      matching to an element of \code{ggd:::kinds} or an index number of
#'                      \code{ggd:::kinds}, or a \code{\link[ggd]{GGD}} object, or a \code{NA}.
#'
#'                      If a character string is indicated as an element,
#'                      the string matches only one element of \code{ggd:::kinds} of
#'                      the first element along with the index order of
#'                      \code{ggd:::kinds.match.order} which is
#'                      \code{ c(1L, 4L, 2L, 3L, 7L, 5L, 6L, 10L, 8L, 9L, 13L, 11L, 12L,
#'                               16L, 14L, 15L)}.
#'
#'                      The order of \code{ggd:::kinds.match.order} is designed,
#'                      while \code{ggd:::kinds} is ordered by intuitive degrees of freedom,
#'                      for practical purposes so that \code{"Mean-Differed Sigma-Differed"}
#'                      model, which has more degrees of freedom than the others of
#'                      the same type, can be matched first.
#'
#'                      If an element is \code{NA}, \code{NA_integer_} will be returned for it.
#'
#' @param   undef.err   A logical;
#'                      If \code{TRUE}, an error occur if a not-\code{NA} element of \code{objs}
#'                      does not match any element of \code{ggd:::kinds}.
#'                      If \code{FALSE}, \code{NA_integer_} will be returned for it.
#'
#' @return  The vector of index numbers (integers) which represent the kinds of distributions.
#'          If \code{NULL} is given for \code{objs}, \code{NULL} will be returned.
#'
#' @importFrom  stats   complete.cases
#' @seealso \code{\link[ggd]{ggd.kind}}
#' @examples
#'  ggd.kind.index( ggd:::kinds )
#'  ggd.kind.index( 1:16 )
#'  ggd.kind.index( 6 )
#'  ggd.kind.index( "3-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )
#'
#'  ggd.kind.index( "Horizontal" )              ## 7, not 5 nor a vector of 5:7.
#'  ggd.kind.index( "Mean-Eq.*Vertical" )       ## 9, not 12 nor a vector of c( 9, 12 ).
#'  ggd.kind.index( "3-Mean-Eq.*Vertical" )     ## 12
#'
#'  ## If undef.err = FALSE (default),
#'  ## NA will be returned if the specified distribution is not supported.
#'  ggd.kind.index( "Poisson Distribution" )    ## NA
#'
#'\dontrun{
#'  ## If undef.err = TRUE is specified, an error will occur.
#'  ggd.kind.index( "Poisson", undef.err = TRUE )
#'
#'  ## Error: 'Poisson' does not match any character strings of kinds.}
#'
#'  a <- GGD$new()
#'  ggd.kind.index( a )
#'
#'  df <- data.frame( x = seq( -2, 2, 0.2 ),
#'                    freq = c(     57,    277,   1002,   3178,   9646,  22109, 42723,
#'                               80646, 117625, 139181, 162319, 150870, 109947, 78736,
#'                               46616,  21058,   9211,   3466,    976,    260,    61 ) )
#'  result <- ggd.nls.freq.all( df )
#'  ggd.kind.index( result$obj )
################################################################################################
ggd.kind.index <- function( objs, undef.err = FALSE )
{
    if ( inherits( objs, "GGD" ) )
    {
        objs <- list( objs )
    }

    results <- vapply( as.list( objs ), function( obj )
    {
        result <- NULL

        if ( inherits( obj, "GGD" ) )
        {
            result <- obj$kind.index
        }
        else if ( is.numeric( obj[[1]] ) )
        {
            if ( complete.cases( obj ) )
            {
                if ( any( obj == 1:length( kinds ) ) )
                {
                    result <- as.integer( obj )
                }
                else if ( undef.err )
                {
                    stop( paste( "Error: kind for index", obj, "is undefined." ) )
                }
                else
                {
                    result <- NA_integer_
                }
            }
            else
            {
                result <- NA_integer_
            }
        }
        else
        {
            if ( complete.cases( obj ) )
            {
                # Remark, grep returns integer(0) if the pattern is not found.
                # And integer(0)[1] gives NA_integer_.
                result <- kinds.match.order[grep( obj, kinds[kinds.match.order] )[1]]
                if ( is.na( result ) && ( undef.err ) )
                {
                    stop( paste0( "Error: '", obj,
                                  "' does not match any character strings of kinds." ) )
                }
            }
            else
            {
                result <- NA_integer_
            }
        }

        return ( result )
    }, 0L )

    return ( results )
}

################################################################################################
#' String of the kind of distribution
#'
#' Gets the strings which represent the kinds of distributions.
#' @export
#' @param   objs    A vector or a list of elements indicating the kind of distribution.
#'                  Each element must be a character string of a regular expression pattern
#'                  matching to an element of \code{ggd:::kinds} or an index number of
#'                  \code{ggd:::kinds}, or a \code{\link[ggd]{GGD}} object, or a \code{NA}.
#'
#'                  If character strings are indicated, each string matches only one element of
#'                  \code{ggd:::kinds} of the first element along with the index order of
#'                  \code{ggd:::kinds.match.order} which is
#'                  \code{ c(1L, 4L, 2L, 3L, 7L, 5L, 6L, 10L, 8L, 9L, 13L, 11L, 12L,
#'                           16L, 14L, 15L)}.
#'
#'                  The order is designed, while \code{ggd:::kinds} is ordered by
#'                  intuitive degrees of freedom, for practical purposes so that
#'                  the \code{"Mean-Differed Sigma-Differed"} model, which has more degrees of
#'                  freedom than the others of the same type, can be matched first.
#'
#' @return  The vector of character strings which represent the kinds of distributions,
#'          which are elements of \code{ggd:::kinds}.
#'          If an element of \code{objs} does not match any element of \code{ggd:::kinds},
#'          \code{NA_character_} will be returned for it.
#'          If \code{NULL} is given for \code{objs}, \code{NULL} will be returned.
#'
#' @seealso \code{\link[ggd]{ggd.kind.index}}
#' @examples
#'  ggd.kind( ggd:::kinds )
#'  ggd.kind( 1:16 )
#'  ggd.kind( 6 )
#'  ggd.kind( "3-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )
#'
#'  ggd.kind( "Horizontal" )
#'  ggd.kind( "Mean-Eq.*Vertical" )
#'  ggd.kind( "3-Mean-Eq.*Vertical" )
#'
#'  a <- GGD$new()
#'  ggd.kind( a )
#'
#'  df <- data.frame( x = seq( -2, 2, 0.2 ),
#'                    freq = c(     57,    277,   1002,   3178,   9646,  22109, 42723,
#'                               80646, 117625, 139181, 162319, 150870, 109947, 78736,
#'                               46616,  21058,   9211,   3466,    976,    260,    61 ) )
#'  result <- ggd.nls.freq.all( df )
#'  ggd.kind( result$obj )
################################################################################################
ggd.kind <- function( objs )
{
    return ( kinds[ggd.kind.index( objs )] )
}

################################################################################################
#' Get mix.type according to grad
#'
#' Gets the value for \code{mix.type} field of \code{GGD} object which is appropriate to
#' given \code{grad} value.
#' If \code{grad = "default"}, the return value will be the value of \code{mix.type} argument
#' or the value appropriate for \code{kind} argument.
#' @export
#' @param   grad        A character string indicating the method of gradation.
#'
#'                      \code{"h"} for horizontal, \code{"v"} for vertical,
#'                      and \code{"hv"} for horizontal-vertical.
#'                      The number after \code{"v"} is the number of components.
#'                      Numberless \code{"v"} is an alias for \code{"v2"}.
#'
#'                      \code{"normal"} is for a normal distribution.
#'                      \code{"default"} is for depending on other arguments.
#'
#' @param   kind        A character string or a numeric value or a \code{\link[ggd]{GGD}} object
#'                      which indicates the kind of distribution model.
#'                      This argument is enabled when the length of \code{mix.type} argument
#'                      is \code{0}, and \code{grad} is \code{"default"}.
#'
#'                      The matching method of this argument follows that of elements of
#'                      the \code{objs} argument of the \link[ggd]{ggd.kind.index}.
#'
#'                      If \code{NA} is indicated, \code{NA_integer_} will be returned.
#'                      If indicated character string pattern or index number does not match
#'                      to any element of \code{ggd:::kind}, an error will occur.
#'
#' @param   mix.type    Default value which is returned if \code{grad} is \code{"default"}.
#'                      Although return value will become in integer class,
#'                      this function does not check if the value of this argument is valid.
#'
#' @return  An integer of \code{mix.type} value appropriate for a distribuion model
#'          represented by \code{grad} value or the default value indicated other arguments.
#'          If the indicated default value is \code{NULL},
#'          \code{integer(0)} is returned when \code{grad} is \code{"default"}.
#'
#'          Although the length of \code{grad} argument must be 1 (or 0 as the default),
#'          but lengths of other arguments are not checked in this function consciously.
#'          So if \code{grad} is \code{"default"}, a vector of 2 or more length can be returned.
#'          That means, the length of other arguments or the return value must be checked
#'          by the caller of this function if necessary.
#'
#' @seealso \code{\link[ggd]{ggd.mix.type.for.kind.index}}, \code{\link[ggd]{ggd.ncmp.for}}
#' @examples
#'  ggd.mix.type.for( grad = "normal" )                 ## 0
#'  ggd.mix.type.for( grad = "h" )                      ## 2
#'  ggd.mix.type.for( grad = "v2" )                     ## 3
#'  ggd.mix.type.for( kind = "Normal" )                 ## 0
#'  ggd.mix.type.for( grad = "default", kind = NA )     ## NA
#'  ggd.mix.type.for( grad = "def", mix.type = NULL )   ## integer(0)
#'  ggd.mix.type.for( grad = "def", mix.type = -1 )     ## -1 (invalid value as mix.type)
################################################################################################
ggd.mix.type.for <- function( grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ),
                              kind = NULL, mix.type = NULL )
{
    grad <- match.arg( grad )
    if ( grad == "v" )
    {
        grad <- "v2"
    }

    if      ( grad == "normal" )            mix.type <- 0L
    else if ( grad == "h" )                 mix.type <- 2L
    else if ( substr( grad, 1, 1 ) == "v")  mix.type <- 3L
    else if ( grad == "hv" )                mix.type <- 4L
    else if ( length( mix.type ) == 0 && !is.null( kind ) )
    {
        mix.type <- ggd.mix.type.for.kind.index( ggd.kind.index( kind, undef.err = TRUE ) )
    }

    return ( as.integer( mix.type ) )
}

################################################################################################
#' Get mix.type according to kind.index
#'
#' Gets a vector of integers indicating \code{mix.type}s
#' which are appropriate to the indicated destributions by index numbers.
#' @export
#' @param   kind.index      A vector of index numbers of the kinds of distributions.
#'                          Values in \code{1:length( ggd:::kinds )} and \code{NA} are
#'                          allowed.
#' @return  A vector of integers of the mix.type which accepts the indicated
#'          kind of distribution.
#' @seealso \code{\link[ggd]{ggd.mix.type.for}}
#' @examples
#'  ggd.mix.type.for.kind.index( 1:7 )
################################################################################################
ggd.mix.type.for.kind.index <- function( kind.index )
{
    kind.index <- vapply( as.numeric( kind.index ), function( kind.index )
    {
        if ( is.na( kind.index ) )
        {
            NA_integer_
        }
        else if ( !any( kind.index == 1:length( kinds ) ) )
        {
            stop( paste( "Error: kind.index", kind.index, "is undefined." ) )
        }
        else
        {
            as.integer( kind.index )
        }
    }, 0L )

    return ( as.integer( ( kind.index + 1 ) %/% 3 - ifelse( kind.index > 10, 1, 0 ) ) )
}

################################################################################################
#' Get number of components according to the grad value and others
#'
#' Gets the number of components for \code{cmp} field of \code{GGD} object
#' which is appropriate to given \code{grad} or \code{mix.type} argument.
#'
#' This function does not have \code{kind} argument.
#' So if you want to apply to \code{kind},
#' use \code{\link[ggd]{ggd.mix.type.for}} to get appropriate \code{mix.type} value.
#' @export
#' @param   grad        A character string indicating the method of gradation.
#'
#'                      \code{"h"} for horizontal, \code{"v"} for vertical,
#'                      and \code{"hv"} for horizontal-vertical.
#'                      The number after \code{"v"} is the number of components.
#'                      Numberless \code{"v"} is an alias for \code{"v2"}.
#7
#'                      \code{"normal"} is for a normal distribution.
#'                      \code{"default"} is for depending on \code{mix.type}.
#'
#' @param   mix.type    A numeric value for \code{mix.type} field of \code{GGD} object.
#'                      If the length is longer than 1, only the first element is valid.
#'
#' @return  Appropriate number of components.
#'          If \code{grad} is \code{"default"} and \code{mix.type} is invalid,
#'          \code{NA} will be returned.
#'
#' @seealso \code{\link[ggd]{ggd.mix.type.for}}
#' @examples
#'  ggd.ncmp.for( grad = "normal" )                 ## 1
#'  ggd.ncmp.for( grad = "h" )                      ## 2
#'  ggd.ncmp.for( grad = "v3" )                     ## 3
#'  ggd.ncmp.for( grad = "default", mix.type = 4 )  ## 4
#'  ggd.ncmp.for( mix.type = NA )                   ## 0
#'  ggd.ncmp.for( mix.type = -1 )                   ## NA (invalid mix.type)
################################################################################################
ggd.ncmp.for <- function( grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ),
                          mix.type = 2 )
{
    grad <- match.arg( grad )
    if ( grad == "v" )
    {
        grad <- "v2"
    }

    if      ( grad == "normal" )            ncmp <- 1
    else if ( grad == "h" || grad == "v2" ) ncmp <- 2
    else if ( grad == "v3" )                ncmp <- 3
    else if ( grad == "hv" )                ncmp <- 4
    else
    {
        mix.type <- as.integer( mix.type )
        if ( length( mix.type ) == 0 || is.null( mix.type[1] ) || is.na( mix.type[1] ) )
        {
            ncmp <- 0
        }
        else if ( mix.type[1] == 0 )            ncmp <- 1
        else if ( any( mix.type[1] == 1:3 ) )   ncmp <- 2   # "v3" is not indicated.
        else if ( mix.type[1] == 4 )            ncmp <- 4
        else ncmp <- NA_integer_
    }

    return ( ncmp )
}

################################################################################################
# Class and methods (and sub-functions)

################################################################################################
#' Gradational Gaussian Distribution class
#'
#' The class provides the Gradational Gaussian Distribution.
#' @export      GGD
#' @exportClass GGD
#' @field   kind.index      An interger; the index number of the kind of the distribution model.
#' @field   kind            A string; the name of the kind of the distribution model.
#' @field   mix.type        An interger which represents how to mix normal distributions
#'                          of the components. It classificates the distribution model.
#'
#'                          The type of the distribution model and the row number of \code{cmp}
#'                          field will be as follows with this value:
#'                          \itemize{
#'                              \item \code{0} : Normal distribution.
#'                                        \code{cmp} has only 1 row.
#'                              \item \code{1} : Mean of 2 normal distributions.
#'                                        \code{cmp} has 2 rows.
#'                              \item \code{2} : Horizontal gradational distribution.
#'                                        \code{cmp} has 2 rows.
#'                              \item \code{3} : Vertical gradational distribution.
#'                                        \code{cmp} has 2 or 3 rows.
#'                              \item \code{4} : Horizontal-vertical gradational distribution.
#'                                               \code{cmp} has 4 rows.
#'                          }
#'
#'                          With \code{mix.type = 1}, the distribution model is not
#'                          a gradational Gaussian distribution (GGD), but a kind of
#'                          Gaussian mixture model (GMM).
#'                          This is provided for comparing GGD with GMM.
#'
#' @field   cmp             A data frame with 2 numeric columns which shows
#'                          the normal distributions of the components.
#'
#'                          \code{mean} column represents the mean values of the components,
#'                          and \code{sd} column represents the standard deviations.
#'
#'                          Where \code{mix.type} is in \code{0:3},
#'                          it has 1 to 3 rows named like "\code{n.i}".
#'                          Where \code{mix.type = 4},
#'                          it has 4 rows named like "\code{n.i.j}".
#'
#' @field   median          A numeric; the median of the distribution.
#' @field   mean            A numeric; the mean of the distribution.
#' @field   sd              A numeric; the standard deviation of the distribution.
#' @field   lsd             A numeric; the semi-standard deviation lower than mean.
#' @field   usd             A numeric; the semi-standard deviation upper than mean.
#' @field   lsd.abs.error   A numeric;
#'                          the estimated modulus of the absolute error for \code{lsd}.
#' @field   usd.abs.error   A numeric;
#'                          the estimated modulus of the absolute error for \code{usd}.
#'
#'                          Where \code{mix.type = 4}, to compute the semi-standard deviations,
#'                          \code{\link[stats]{integrate}} function is used.
#'                          And the modulus of the absolute errors which
#'                          \code{\link[stats]{integrate}} function has reported
#'                          will be set into these \code{*.abs.error} fields.
#'
#' @seealso \code{\link[ggd]{set.cmp}},
#'          \code{\link[ggd]{nls.freq}}, \code{\link[ggd]{ggd.nls.freq.all}},
#'          \code{\link[ggd]{trace.q}},
#'          \href{https://github.com/Kimitsuna-Goblin/GGD}{README.md} (GitHub)
#' @details
#'  \subsection{Overview about GGD}{
#'      The \bold{gradational Gaussian distribution (GGD)} is one of
#'      continuous distribution models for mainly modeling asymmetric unimodal data
#'      which do not follow a normal distribution.
#'
#'      The GGD is alike the Gaussian mixture model (GMM) but different.
#'      The GMM is represented by linear combinations of some normal distributions,
#'      and is often used for clustering of mixed data.
#'      On the other hand, the GGD is a distribution model of
#'      which mixes some normal distributions with gradually changing ratio
#'      along the x-axis or y-axis directions,
#'      and it treats data which do not follow a normal distribution as they are.
#'
#'      About the \bold{horizontal gradational Gaussian distribution}
#'      \eqn{\mathcal{G}[\mathcal{N}_1 \rightarrow \mathcal{N}_2]},
#'      it is expressed as
#'      \deqn{\mathcal{G}[\mathcal{N}_1 \rightarrow \mathcal{N}_2] =
#'            h_1(x) \ \mathcal{N}_1 + h_2(x) \ \mathcal{N}_2}
#'
#'      where \eqn{h_1} is the mixing ratio decreasing gradually along x-axis as
#'      \deqn{x:-\infty \to \infty \ \Rightarrow \ h_1(x):1 \to 0,}
#'      and \eqn{h_2} is the mixing ratio increasing gradually as
#'      \deqn{x:-\infty \to \infty \ \Rightarrow \ h_2(x):0 \to 1.}
#'
#'      Because \eqn{\mathcal{N}_1} is dominant on the left (lower) side of x-axis,
#'      and \eqn{\mathcal{N}_2} is on the right (upper) side,
#'      we call \eqn{\mathcal{N}_1} the \bold{left- (lower-) side} distribution,
#'      and \eqn{\mathcal{N}_2} the \bold{right- (upper-) side} distribution.
#'
#'      In this package, we use \eqn{h_1(x) = 1 - \Phi_1(x)} and \eqn{h_2(x) = \Phi_2(x)},
#'      where \eqn{\Phi_1} and \eqn{\Phi_2} are the cumulative distribution functions
#'      of \eqn{\mathcal{N}_1} and \eqn{\mathcal{N}_2}, respectively.
#'
#'      About the \bold{vertical gradational Gaussian distribution}
#'      \eqn{\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2]},
#'      it is expressed as
#'      \deqn{\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2] =
#'            v_1(x) \ \mathcal{N}_1 + v_2(x) \ \mathcal{N}_2.}
#'
#'      Now, let \eqn{f_1} and \eqn{f_2} to be the probability density functions of
#'      \eqn{\mathcal{N}_1} and \eqn{\mathcal{N}_2}, respectively.
#'      Then, the mixing ratio \eqn{v_1} decreases gradually along y-axis as
#'      \deqn{f_1(x):0 \to \max_{x \in (-\infty, \infty)}(f_1(x))
#'                   \ \Rightarrow \ v_1(x):1 \to 0,}
#'
#'      and \eqn{v_2} increases gradually as
#'      \deqn{f_2(x):0 \to \max_{x \in (-\infty, \infty)}(f_2(x))
#'                   \ \Rightarrow \ v_2(x):0 \to 1.}
#'
#'      Here, we call \eqn{\mathcal{N}_1} the \bold{tail-side} distribution,
#'      and \eqn{\mathcal{N}_2} the \bold{top-side} distribution.
#'
#'      In this package, we use \eqn{v_1(x) = 1 - f_1(x) / f_1(\mu_1)}
#'      and \eqn{v_2(x) = f_2(x) / f_2(\mu_1)},
#'      where \eqn{\mu_1} and \eqn{\mu_2} are the mean values of
#'      \eqn{\mathcal{N}_1} and  \eqn{\mathcal{N}_2}, respectively.
#'
#'      You can devide the tail-side distribution along x-axis into left (lower) side
#'      and right (upper) side.
#'
#'      In this case, the distribution
#'      \eqn{\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2 \downarrow \mathcal{N}_3]}
#'      is expressed as
#'      \deqn{\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2 \downarrow \mathcal{N}_3] =
#'            v_1(x) \ \mathcal{N}_1 + v_2(x) \ \mathcal{N}_2 + v_3(x) \ \mathcal{N}_3,}
#'
#'      then the mixing ratio \eqn{v_1} decreases gradually as
#'      \deqn{x:-\infty \to \mu_1 \ \Rightarrow \ v_1(x):1 \to 0, \
#'            v_1(x) = 0 \ \ \mathrm{where} \ \ x > \mu_1,}
#'
#'      and \eqn{v_3} increases gradually as
#'      \deqn{v_3(x) = 0 \ \ \mathrm{where} \ \ x < \mu_3, \
#'            x:\mu_3 \to \infty \ \Rightarrow \ v_3(x):0 \to 1.}
#'
#'      Then, \eqn{v_2(x)} for top side is defined as same as
#'      with 2 components.
#'
#'      Here, \eqn{\mu_1} and \eqn{\mu_3} are the mean values of
#'      \eqn{\mathcal{N}_1} and  \eqn{\mathcal{N}_3}, respectively.
#'      We call \eqn{\mathcal{N}_1} the \bold{left- (lower-) tail-side} distribution,
#'      and \eqn{\mathcal{N}_3} the \bold{right- (upper-) tail-side} distribution,
#'      regardless whether \eqn{\mu_1} and \eqn{\mu_3} is greater.
#'
#'      About the \bold{horizontal-vertical gradational Gaussian distribution}
#'      \eqn{\mathcal{G}[\mathcal{G}_1 \rightarrow \mathcal{G}_2]},
#'      two vertical GGDs \eqn{\mathcal{G}_1} and \eqn{\mathcal{G}_2} can mixture
#'      as same as normal distributions for the horizontal GGD as
#'      \deqn{\mathcal{G}[\mathcal{G}_1 \rightarrow \mathcal{G}_2] =
#'            h_1(x) \ \mathcal{G}_1 + h_2(x) \ \mathcal{G}_2,}
#'      \deqn{\mathcal{G}_1 =
#'                  \mathcal{G}[\mathcal{N}_{1,1} \uparrow \mathcal{N}_{1,2}] =
#'                  v_{1,1}(x) \ \mathcal{N}_{1,1} + v_{1,2}(x) \ \mathcal{N}_{2,1},}
#'      \deqn{\mathcal{G}_2 =
#'                  \mathcal{G}[\mathcal{N}_{2,1} \uparrow \mathcal{N}_{2,2}] =
#'                  v_{2,1}(x) \ \mathcal{N}_{2,1} + v_{2,2}(x) \ \mathcal{N}_{2,2}.}
#'
#'      These are the kinds of distribution models supported by this package.
#'  }
#'  \subsection{The probability density function and the cumulative distribution function}{
#'      In \code{GGD} class, \code{mix.type} is very important field that determines
#'      how the normal distributions of the components (shown in \code{cmd}) are mixed.
#'      In other words, \code{mix.type} determines the format of
#'      the probability density function and the cumulative distribution function.
#'
#'      The probability density function \eqn{g(x)}
#'      and the cumulative distribution function \eqn{\Psi(x)}
#'      of each distribution model are defined as follows:
#'
#'      \describe{
#'          \item{\code{mix.type = 0} (normal distribution)}{\deqn{
#'              g(x) = f_1(x) \\
#'              \Psi(x) = \Phi_1(x) }}
#'
#'          \item{\code{mix.type = 1} (mean of 2 normal distributions;
#'                              a Gaussian mixture model, not a GGD)}{\deqn{
#'              g(x) = \dfrac{f_1(x) + f_2(x)}{2} \ \\
#'              \Psi(x) = \dfrac{\Phi_1(x) + \Phi_2(x)}{2} }}
#'
#'          \item{\code{mix.type = 2} (horizontal GGD)}{\deqn{
#'              g(x) = \left( 1 - \Phi_1(x) \right) f_1(x)
#'                      + \Phi_2(x) f_2(x) \\
#'              \ \\
#'              \Psi(x) = \Phi_1(x) - \dfrac{\Phi_1(x)^2}{2} +
#'                          \dfrac{\Phi_2(x)^2}{2} \quad \; \; }}
#'
#'          \item{\code{mix.type = 3}, \code{grad = "v2"} (2-component-vertical GGD)}{\deqn{
#'              g(x) = \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x) +
#'                     \dfrac{f_2(x)^2}{f_2(\mu_2)} \\
#'              \ \\
#'              \Psi(x) = \Phi_1(x) - \dfrac{\Phi^*_1(x)}{\sqrt{2}} +
#'                        \dfrac{\Phi^*_2(x)}{\sqrt{2}} \qquad \; \: }}
#'
#'          \item{\code{mix.type = 3}, \code{grad = "v3"} (3-component-vertical GGD)}{\deqn{
#'              g(x) = g_1(x) + g_2(x) + g_3(x) \hspace{14.5em} \\
#'              \ \\
#'              g_1(x) = \left( 1 - \dfrac{f_1(x)}{f_1(\mu_1)} \right) f_1(x)
#'                       \quad (x \leq \mu_1; \ \mathrm{otherwise} \ 0) \\
#'              \ \\
#'              g_2(x) = \dfrac{f_2(x)^2}{f_2(\mu_2)} \hspace{15.6em} \\
#'              \ \\
#'              g_3(x) = \left( 1 - \dfrac{f_3(x)}{f_3(\mu_3)} \right) f_3(x)
#'                       \quad (x \geq \mu_3; \ \mathrm{otherwise} \, 0) \\
#'              \ \\
#'              \Psi(x) = \min \left( \Phi_1(x) - \dfrac{\Phi^*_1(x)}{\sqrt{2}},
#'                                  \ \dfrac{2 - \sqrt{2}}{4} \right) \hspace{10em} \\
#'                        + \dfrac{\Phi^*_2(x)}{\sqrt{2}}
#'                        + \max \left( 0, \ \Phi_3(x) - \dfrac{\Phi^*_3(x)}{\sqrt{2}} -
#'                                      \dfrac{2 - \sqrt{2}}{4} \right) }}
#'
#'          \item{\code{mix.type = 4} (horizontal-vertical GGD)}{\deqn{
#'              g(x) = \left( 1 - \Psi_1(x)\ \right) g_1(x) + \Psi_2(x) g_2(x)
#'                     \hspace{6em} \ \ \\
#'              \ \\
#'              g_i(x) = \left( 1 - \dfrac{f_{i,1}(x)}{f_{i,1}(\mu_{i,1})} \right)
#'                       f_{i,1}(x) + \dfrac{f_{i,2}(x)^2}{f_{i,2}(\mu_{i,2})} \\
#'              \ \\
#'              \Psi(x) = \Psi_1(x) - \dfrac{\Psi_1(x)^2}{2} +
#'                                    \dfrac{\Psi_2(x)^2}{2} \hspace{8.5em} \\
#'              \ \\
#'              \Psi_i(x) = \Phi_{i,1}(x) - \dfrac{\Phi^*_{i,1}(x)}{\sqrt{2}} +
#'                          \dfrac{\Phi^*_{i,2}(x)}{\sqrt{2}} \hspace{3em} \ }}
#'      }
#'
#'      Where \eqn{f_i} is the probability density function of
#'      the normal distribution \eqn{\mathcal{N}_i},
#'      \eqn{\Phi_i} and \eqn{\Phi_{i,j}} are the cumulative distribution functions of
#'      \eqn{\mathcal{N}_i} and \eqn{\mathcal{N}_{i,j}},
#'      and \eqn{\mu_i} is the mean value of \eqn{\mathcal{N}_i},
#'      and \eqn{\Phi^*_i} and \eqn{\Phi^*_{i,j}} are the cumulative distribution function
#'      of the normal distributions \eqn{\mathcal{N}(\mu_i, (\sigma_i / \sqrt{2})^2)} and
#'      \eqn{\mathcal{N}(\mu_{i,j}, (\sigma_{i,j} / \sqrt{2})^2)}, respectively.
#'
#'      Note, for each of above GGD models,
#'      we can say that \eqn{\Psi(x)} is a \eqn{0 \to 1} monotonical increasing function.
#'      This is because \eqn{g(x)}, the derivative of \eqn{\Psi(x)}, has \eqn{g(x) \geq 0} for
#'      \eqn{\forall x \in (-\infty, \infty)}, moreover \eqn{\Psi(-\infty) = 0},
#'      and \eqn{\Psi(\infty) = 1}.
#'  }
################################################################################################
GGD <- setRefClass(

    # Class name
    Class = "GGD",

    # Fields
    fields = list(
        kind.index  = "integer",    # The index number of the kind of the distribution model.
        kind        = "character",  # The name of the kind of the distribution model.
        mix.type    = "integer",    # The value which represent how to mix normal distributions.
        cmp         = "data.frame", # The mean and sd of the component normal distributions.

        median      = "numeric",    # The median value of the distribution.
        mean        = "numeric",    # The mean of the distribution.
        sd          = "numeric",    # The standard deviation of the distribution.
        lsd         = "numeric",    # The semi-standard deviation lower than mean.
        usd         = "numeric",    # The semi-standard deviation upper than mean.
        lsd.abs.error = "numeric",  # The estimated modulus of the absolute error for lsd.
        usd.abs.error = "numeric"   # The estimated modulus of the absolute error for usd.
    )
)

################################################################################################
#' Constructor
#'
#' Generates a \code{\link[ggd]{GGD}} object.
#' The fields are initialized as a standard normal distribution model \eqn{\mathcal{N}(0, 1)}
#' with \code{mix.type = 2}, which represents a horizontal gradational distribution;
#' a basic model of the GGD.
#' To indicate components for a new \code{\link[ggd]{GGD}} object,
#' use \code{\link[ggd]{ggd.set.cmp}} or connect \code{\link[ggd]{set.cmp}} method as examples.
#' There are some other object generators:
#' \code{\link[ggd]{ggd.nls.freq}}, \code{\link[ggd]{ggd.nls.freq.all}} and
#' \code{\link[ggd]{ggd.trace.q}}.
#' @name    initialize
#' @aliases new
#' @aliases \S4method{new}{GGD}
#' @aliases \S4method{initialize}{GGD}
#' @usage   \S4method{new}{GGD}()
#' @seealso \code{\link[ggd]{ggd.set.cmp}},
#'          \code{\link[ggd]{ggd.nls.freq}}, \code{\link[ggd]{ggd.nls.freq.all}},
#'          \code{\link[ggd]{ggd.trace.q}}
#' @examples
#'  a <- GGD$new()
#'  a
#'  rm( a )
#'  a <- GGD$new()$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1.108, 0.703 ) ) )
#'  a
################################################################################################
NULL
GGD$methods(
    initialize = function()
    {
        initFields( kind.index      = 1L,
                    kind            = kinds[1],
                    mix.type        = 2L,
                    cmp             = data.frame( mean = c( 0, 0 ), sd = c( 1, 1 ) ),
                    median          = 0,
                    mean            = 0,
                    sd              = 1,
                    usd             = 1,
                    lsd             = 1,
                    lsd.abs.error   = 0,
                    usd.abs.error   = 0 )
        adjust.cmp.rownames()
    }
)

################################################################################################
#' Clear fields
#'
#' Clears all fields.
#' The lengths of all vector fields and the number of rows in the \code{cmp} field will be 0.
#' @name    clear
#' @aliases clear
#' @aliases \S4method{clear}{GGD}
#' @usage   \S4method{clear}{GGD}()
#' @return  The cleared \code{\link[ggd]{GGD}} object itself (invisible).
#' @examples
#'  a <- GGD$new()
#'  a   ## Normal Distribution with mix.type = 2
#'  a$clear()
#'  a   ## cleared
################################################################################################
NULL
GGD$methods(
    clear = function()
    {
        mix.type    <<- integer()
        cmp         <<- data.frame( mean = numeric(), sd = numeric() )
        adjust.kind.index()

        median <<- mean <<- sd <<- lsd <<- usd <<- numeric()
        lsd.abs.error <<- usd.abs.error <<- numeric()

        return ( invisible( .self ) )
    }
)

################################################################################################
#' Setting components
#'
#' Sets the normal distributions of the components to the \code{cmp} field.
#' All fields of the \code{\link[ggd]{GGD}} object will be set as appropriate according to
#' given components and other arguments, respectively.
#' Remark! Never set any values into the \code{cmp} fields directly without using this method.
#' @export
#' @name    set.cmp
#' @aliases ggd.set.cmp
#' @aliases set.cmp
#' @aliases \S4method{set.cmp}{GGD}
#' @usage   ggd.set.cmp(cmp, kind = NULL, mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"))
#' @usage   \S4method{set.cmp}{GGD}(this.cmp = .self$cmp,
#'          this.kind = NULL, this.mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"))
#'
#' @param   cmp         A data frame for setting into the \code{cmp} field.
#'
#'                      It must have just 2 columns named "\code{mean}" and "\code{sd}",
#'                      and its rows must be less than or equals to 4.
#'                      Row names are not required.
#'
#'                      The \code{mean} column represents the mean values of
#'                      the normal distributions of the components.
#'
#'                      The \code{sd} column represents the standard deviations of
#'                      the normal distributions of the components.
#'
#'                      Having no rows is allowed,
#'                      but indicating \code{NULL} or having no columns is not allowed.
#'
#' @param   kind        A character string or a numeric value or a \code{\link[ggd]{GGD}} object
#'                      which indicates the kind of the distribution model to create.
#'
#'                      The matching method of this argument follows that of elements of
#'                      the \code{objs} argument of the \link[ggd]{ggd.kind.index}.
#'                      \code{NA} is allowed when \code{cmp} has no rows.
#'
#'                      If \code{mix.type} argument or other than \code{"default"} for
#'                      \code{grad} argument is indicated, this argument will be ignored.
#'
#' @param   mix.type    A numeric value represents how to mix the normal distributions.
#'
#'                      The type of the distribution model and the number of rows in \code{cmp}
#'                      will be represented with \code{mix.type} value as follows:
#'                      \itemize{
#'                          \item \code{0} : Normal distribution. \code{cmp} has only 1 row.
#'                          \item \code{1} : Mean of 2 normal distributions (not a GGD).
#'                                           \code{cmp} has 2 rows.
#'                          \item \code{2} : Horizontal gradational distribution.
#'                                           \code{cmp} has 2 rows.
#'                          \item \code{3} : Vertical gradational distribution.
#'                                           \code{cmp} has 2 or 3 rows.
#'                          \item \code{4} : Horizontal-vertical gradational distribution.
#'                                           \code{cmp} has 4 rows.
#'                      }
#'
#'                      If the number of rows in \code{cmp} argument is different from
#'                      the number shown above, the \code{cmp} field will be
#'                      redundant/simplified to have the number of rows as above,
#'                      if possible. If not possible, an error will occur.
#'
#'                      You can indicate \code{mix.type = NA} only if \code{cmp} has no rows.
#'
#'                      If other than \code{"default"} for \code{grad} argument is indicated,
#'                      this argument will be ignored.
#'
#' @param   grad        A character string indicating the method of gradation.
#'
#'                      \code{"h"} for horizontal, \code{"v"} for vertical,
#'                      and \code{"hv"} for horizontal-vertical.
#'                      The number after \code{"v"} is the number of components.
#'                      Numberless \code{"v"} is an alias for \code{"v2"}.
#'
#'                      \code{"normal"} is for a normal distribution,
#'                      then also, \code{'grad = "no"'} can be read as "no gradation".
#'
#'                      \code{"default"} is, if \code{kind} or \code{mix.type} argument
#'                      is given, follows it, otherwise it depends on the number of columns
#'                      in the \code{cmp} argument. If the number of columns in cmp is \code{2},
#'                      the current \code{mix.type} is retained or horizontal (default) is used.
#'
#' @param   this.cmp    A data frame for setting into the \code{cmp} field.
#'                      It is equivalent to \code{cmp} argument for \code{ggd.set.cmp}.
#'
#' @param   this.kind   A string or a numeric value or a \code{\link[ggd]{GGD}} object
#'                      which indicates the kind of the distribution model.
#'                      It is equivalent to \code{kind} argument for \code{ggd.set.cmp}.
#'
#' @param   this.mix.type   A numeric value represents how to mix the normal distributions.
#'                          It is equivalent to \code{mix.type} argument for \code{ggd.set.cmp}.
#'
#' @return  The \code{\link[ggd]{GGD}} object itself (invisible for \code{GGD} method).
#'
#'          For \code{GGD} method: If an error occur, each value of field will not be changed.
#'
#' @importFrom  methods     new
#'
#' @details
#'  \subsection{About "kind" and "mix.type"}{
#'      In this function, \code{[this.]kind} argument is, unlike \code{\link[ggd]{trace.q}}
#'      and \code{\link[ggd]{nls.freq}} methods, used only to determine how to mix
#'      the normal distributions of the components.
#'      That is, \code{[this.]kind} argument is used only to determine the new value of
#'      \code{mix.type} field, and has no effect on the unification of the mean values and
#'      the standard deviations of the components.
#'
#'      So, the new value of \code{kind} field of the object may not be matched with
#'      the indicated value of \code{[this.]kind} argument.
#'      For example, if you indicate \code{[this.]kind = "Mean-Eq.*Horizontal"} and
#'      \code{[this.]cmp = data.frame(mean = c(0, 1), sd = c(0.8, 1.2))},
#'      the new \code{kind} field will be
#'      \code{"Mean-Differed Sigma-Differed Horizontal Gradational Distribution"},
#'      which is not matched with \code{"Mean-Eq.*Horizontal"}.
#'
#'      In such a case, i.e. if indicated \code{[this.]kind} does not match
#'      to the new value of \code{kind} field, a warning will occur.
#'      But when a \code{\link[ggd]{GGD}} object is indicated for \code{[this.]kind},
#'      no warning will occur if the unification of the mean values and standard deviations of
#'      the components are different between the indicated object and the result.
#'      Indicated object is regarded as just for specifying new \code{mix.type}.
#'
#'      It is not recommended but if you indicate both of
#'      \code{[this.]kind} and \code{[this.]mix.type} at once with not-\code{NULL} values,
#'      \code{[this.]kind} will be ignored.
#'
#'      If both \code{[this.]kind} and \code{[this.]mix.type} arguments are \code{NULL}
#'      and \code{grad} argument is \code{"default"},
#'      the new value of \code{mix.type} field will be decided according to
#'      number of rows in \code{[this.]cmp} as:
#'      \itemize{
#'          \item \code{nrow([this.]cmp) = 0} : \code{mix.type} will be \code{NA}.
#'          \item \code{nrow([this.]cmp) = 1} : \code{mix.type} will be \code{0}.
#'          \item \code{nrow([this.]cmp) = 2} : If the current \code{mix.type} field
#'                                              is in \code{1:3}, it will be retained.
#'                                              Otherwize, \code{mix.type} will be \code{2}.
#'          \item \code{nrow([this.]cmp) = 3} : \code{mix.type} will be \code{3}.
#'          \item \code{nrow([this.]cmp) = 4} : \code{mix.type} will be \code{4}.
#'      }
#'  }
#'
#' @examples
#'  ## Normal Distribution
#'  a <- ggd.set.cmp( data.frame( mean = 0, sd = 1.5 ) )
#'  a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## "normal" for "grad" argument is allowed, though, it does little work in this function.
#'  a <- ggd.set.cmp( data.frame( mean = 1, sd = 2 ), grad = "normal" )
#'  a$mix.type; a$cmp
#'
#'  ## Where the number of the cmp field rows is 2,
#'  ## it is recommended to indicate "grad" or "mix.type" or "kind" to avoid confusing.
#'  rm( a )
#'  a <- ggd.set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1.2, 0.8 ) ),
#'                    kind = "Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions" )
#'  a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Changing to mix.type = 2 : Horizontal Gradational Distribution
#'  a$set.cmp( a$cmp, this.mix.type = 2 )
#'  a$mix.type; a$cmp
#'
#'  ## You can also write as:
#'  a$set.cmp( a$cmp, grad = "h" )
#'  a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## kind.index = 9 : 2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution
#'  a$set.cmp( a$cmp, this.kind = 9 )
#'  a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## 3-Mean-Differed Sigma-Differed Vertical Gradational Distribution
#'  a$set.cmp( data.frame( mean = c( -0.5, 0, 0.5 ), sd = c( 1.2, 0.8, 1.2 ) ) )
#'  a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Mean-Equaled Sigma-Differed Horizontal-Vertical Gradational Distribution
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ), sd = c( 0.7, 0.5, 2.0, 1.5 ) ) )
#'  a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## If "cmp" field can be simplified, "mix.type" field may be set to lower type.
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ), sd = c( 1, 0.7, 1, 0.7 ) ) )
#'  a$mix.type  ## 3 (not 4)
#'  a$cmp       ## with 2 rows
#'
#'  ## If you want not to simplify "mix.type" and "cmp" fields,
#'  ## indicate "mix.type" / "this.mix.type" or one of other arguments for the condition.
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ), sd = c( 1, 0.7, 1, 0.7 ) ),
#'             this.mix.type = 4 )
#'  a$mix.type  ## 4
#'  a$cmp       ## with 4 rows
#'
#'  ## You can also write as:
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ), sd = c( 1, 0.7, 1, 0.7 ) ),
#'             grad = "hv" )
#'  a$mix.type; a$cmp
#'
#'  ## The "kind" / "this.kind" argument can also indicate
#'  ## how to mix the normal distributions of the components.
#'  ##
#'  ## However, if the components indicated by "cmp" or "this.cmp" are inconsistent
#'  ## with the value of "kind" / "this.kind" argument,
#'  ## the "kind" and "kind.index" fields will be set to different values with a warning.
#'  ## For avoiding confusing, you should use "mix.type" / "this.mix.type" or "grad" instead.
#'
#'  ## This sample will work with a warning.
#'  a$set.cmp(
#'      this.cmp = data.frame( mean = c( 0, 0, 0, 0 ), sd = c( 1, 0.7, 1, 0.7 ) ),
#'      this.kind = "Mean-Equaled Sigma-Differed Horizontal-Vertical" )
#'  a$mix.type  ## 4
#'  a$kind      ## "2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution"
################################################################################################
ggd.set.cmp <- function( cmp, kind = NULL, mix.type = NULL,
                         grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ) )
{
    obj <- GGD$new()
    return ( withVisible( obj$set.cmp( this.cmp      = cmp,
                                       this.kind     = kind,
                                       this.mix.type = mix.type,
                                       grad          = grad ) )$value )
}

GGD$methods(
    set.cmp = function( this.cmp = .self$cmp,
                        this.kind = NULL, this.mix.type = NULL,
                        grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ) )
    {
        # Error-check for this.cmp.
        #
        #   This process must be executed before initializing the fields
        #   for safty of the data copy process
        #    (strictly, some processes using this.cmp must be executed before initializing).
        if ( !is.data.frame( this.cmp ) )
        {
            stop( "Error: cmp must be a data frame." )
        }

        if ( ncol( this.cmp ) != 2 || !all( names( this.cmp ) == c( "mean", "sd" ) ) )
        {
            stop( "Error: cmp must have 2 columns named 'mean' and 'sd'" )
        }

        if ( nrow( this.cmp ) > 4 )
        {
            stop( "Error: The number of cmp rows is too large." )
        }

        if ( !all( complete.cases( this.cmp ) ) )
        {
            stop( "Error: Rows of cmp must be complete cases." )
        }

        if ( !is.numeric( this.cmp$mean ) || !is.numeric( this.cmp$sd ) )
        {
            stop( "Error: Elements of cmp must be numeric." )
        }

        if ( !all( is.finite( c( this.cmp$mean, this.cmp$sd ) ) ) )
        {
            stop( "Error: Elements of cmp must be finite." )
        }

        if ( !all( this.cmp$sd > 0 ) )
        {
            stop( "Error: sd must be positive." )
        }

        grad <- match.arg( grad )
        if ( grad == "v" )
        {
            grad <- "v2"
        }

        # Get kind.index in order to check now error of this.kind argument,
        # and to check later whether the values of indicated and result are same.
        this.kind.index <- NULL
        if ( !is.null( this.kind ) )
        {
            this.kind.index <- ggd.kind.index( this.kind, undef.err = TRUE )
            if ( length( this.kind.index ) > 1 )
            {
                stop( paste( "Error: kind should be valid single value or a GGD object." ) )
            }
        }

        # Check this.mix.type
        new.mix.type <- ggd.mix.type.for( grad, kind = this.kind, mix.type = this.mix.type )
        if ( length( new.mix.type ) > 0 )
        {
            if ( length( new.mix.type ) == 1 && is.na( new.mix.type ) )
            {
                if ( nrow( this.cmp ) > 0 )
                {
                    stop( paste( "Error: NA for kind or mix.type can be specified",
                                        "only if cmp has no rows." ) )
                }
            }
            else if ( length( new.mix.type ) > 1 || !any( new.mix.type == 0:4 ) )
            {
                stop( paste( "Error: mix.type should be single integer from 0 to 4." ) )
            }
        }

        # Check if the indicated new mix.type is suitable for nrow(this.cmp)
        tent.mix.type <- NA_integer_   # tentative (current) mix.type
        if ( nrow( this.cmp ) == 0 )
        {
            tent.mix.type <- NA_integer_
        }
        else if ( nrow( this.cmp ) == 1 )
        {
            tent.mix.type <- 0L
        }
        else if ( nrow( this.cmp ) == 2 )
        {
            if ( length( new.mix.type ) > 0 && any( new.mix.type == 1:3 ) )
            {
                tent.mix.type <- as.integer( new.mix.type )
            }
            else if ( length( mix.type ) > 0 && !is.null( mix.type ) &&
                      !is.na( mix.type ) && any( mix.type == 1:3 ) )
            {
                tent.mix.type <- mix.type
            }
            else
            {
                tent.mix.type <- 2L
            }
        }
        else
        {
            tent.mix.type <- as.integer( nrow( this.cmp ) )
        }

        backup <- copy()        # backup for rollback

        # Fix current mix.type field and set indicated cmp field.
        mix.type <<- tent.mix.type
        cmp <<- this.cmp

        # Adjust mix.type and cmp fields.
        result <- try( adjust.cmp( new.mix.type, grad ), silent = TRUE )
        if ( inherits( result, "try-error" ) )
        {
            # rollback
            mix.type    <<- backup$mix.type
            cmp         <<- backup$cmp

            if ( grad != "default" )
            {
                indicated <- "grad"
            }
            else if ( !is.null( this.mix.type ) )
            {
                indicated <- "mix.type"
            }
            else
            {
                indicated <- "kind"
            }
            stop( paste( "Error: Indicated", indicated, "is not appropriate for cmp." ) )
        }

        # Set kind and kind.index.
        adjust.kind.index()
        if ( length( this.kind.index ) > 0 && !is.na( this.kind.index ) &&
             length( this.kind ) > 0 && !inherits( this.kind, "GGD" ) &&
             ( ( is.numeric( this.kind[[1]] ) &&
                 kind.index != this.kind[[1]] ) ||
               ( is.character( this.kind[[1]] ) &&
                 length( grep( this.kind[[1]], kind ) ) == 0 ) ) )
        {
            warning( paste( "Warning: Indicated kind does not match to the result."  ) )
        }

        # Set median, mean, sd and its family.
        return ( adjust.median.mean.sd() )
    }
)

################################################################################################
#' Adjust the cmp field
#'
#' Simplifies the components in \code{cmp} field with retaining
#' the substance of the distribution. Also, you can make \code{cmp} field redundant inversely.
#' The \code{mix.type} field will be change accordingly.
#' @name    adjust.cmp
#' @aliases adjust.cmp
#' @aliases \S4method{adjust.cmp}{GGD}
#' @usage   \S4method{adjust.cmp}{GGD}(this.mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"))
#'
#' @param   this.mix.type   A numeric value to set \code{mix.type} field as an integer.
#'
#'                      If \code{NULL} (the default), \code{mix.type} field will be set to
#'                      the smallest number which can represent the distribution model.
#'                      Normally, it should be an integer from \code{0} to \code{4}.
#'                      \code{NA} is allowed only if \code{cmp} field has no rows.
#'
#' @param   grad        A character string indicating the method of gradation.
#'
#'                      \code{"h"} for horizontal, \code{"v"} for vertical,
#'                      and \code{"hv"} for horizontal-vertical.
#'                      The number after \code{"v"} is the number of components.
#'                      Numberless \code{"v"} is an alias for \code{"v2"}.
#'
#'                      \code{"normal"} is for a normal distribution.
#'                      \code{"default"} is for depending on \code{this.mix.type} argument.
#'
#'                      If other than \code{"default"} is indicated,
#'                      \code{this.mix.type} argument is ignored.
#'                      If the current distribution model cannot be represented by
#'                      the indicated model, an error will occur.
#'
#' @return  The adjusted \code{\link[ggd]{GGD}} object itself (invisible).
#'
#' @seealso \code{\link[ggd]{set.cmp}}
#'
#' @details
#'  \subsection{What this method can and cannot do}{
#'      This method may change the value of \code{mix.type} field and
#'      the number of rows in \code{cmp} field, but retains the substance of the distribution
#'      and does not change other fields.
#'
#'      When \code{this.mix.type} is given, \code{mix.type} field will be set to the
#'      given value if the substance of the distribution will not change.
#'      For example, when \code{mix.type = 2} and \code{cmp} has 2 rows,
#'      you can indicate \code{this.mix.type = 4} to enhance \code{cmp} field into 4 rows
#'      with retaining the substance of the distribution.
#'
#'      If you want to change the substance of the distribution,
#'      for example, if you want to change from a horizontal gradation to a vertical gradation,
#'      use \code{\link[ggd]{set.cmp}} instead.
#'  }
#'
#' @examples
#'  ## Usually the cmp field is simplified automatically,
#'  ## so you do not have to call adjust.cmp by your own.
#'  ## For example, in this case,
#'  ## the number of components is simplified to 1 automatically.
#'  a <- ggd.set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1.1, 1.1 ) ) )
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 0 represents a Normal Distribution.
#'
#'  ## GGD$new() makes 2 components of normal distributions for convenience.
#'  ## You can use adjust.cmp in order to adjust the cmp field of a new object to 1 component.
#'  a <- GGD$new()
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 2 represents a Horizontal Gradational Model.
#'
#'  a$adjust.cmp()
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 0 represents a Normal Distribution.
#'
#'  ## You can also write as:
#'  a <- GGD$new()$adjust.cmp()
#'  a$kind; a$mix.type; a$cmp
#'
#'  ## If you want to give redundancy to the components, you can also use adjust.cmp.
#'  ## Normal Distribution with 2 components.
#'  a$adjust.cmp( this.mix.type = 1 )
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 1 represents a Gaussian Mixture Model.
#'
#'  ## Normal Distribution with 3 components.
#'  a$adjust.cmp( grad = "v3" )
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 3 represents a Vertical Gradational Model.
#'
#'  ## Normal Distribution with 4 components.
#'  a$adjust.cmp( grad = "hv" )
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 4 represents a H-V Gradational Model.
################################################################################################
NULL
GGD$methods(
    adjust.cmp = function( this.mix.type = NULL,
                           grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ) )
    {
        grad <- match.arg( grad )
        if ( grad == "v" )
        {
            grad <- "v2"
        }

        this.mix.type <- ggd.mix.type.for( grad, mix.type = this.mix.type )

        if ( length( this.mix.type ) > 1 ||
             ( length( this.mix.type ) == 1 &&
               !is.na( this.mix.type ) && !any( this.mix.type == 0:4 ) ) )
        {
            stop( paste( "Error: mix.type should be single integer from 0 to 4." ) )
        }

        if ( nrow( cmp ) == 0 )
        {
            if ( length( this.mix.type ) > 0 && !is.na( this.mix.type ) )
            {
                stop( paste( "Error: Not appropriate for",
                                    "current distribution model" ) )
            }

            mix.type <<- as.integer( this.mix.type )
            cmp <<- data.frame( mean = numeric(), sd = numeric() )
        }
        else
        {
            if ( length( this.mix.type ) > 0 && is.na( this.mix.type ) )
            {
                stop( paste( "Error: NA for mix.type can be specified only if",
                                    "cmp has no rows." ) )
            }

            # First, optimize the components of the "cmp" field.
            current.mix.type <- integer()   # current essential mix type; the distribution model
            cmp.rows <- nrow( cmp )
            means <- cmp$mean
            sds <- cmp$sd

            if ( cmp.rows > 4 )
            {
                # 5 and more rows in "cmp" field are not allowed.
                warning( paste( "Warning: The number of the cmp field rows is too large.",
                                         "5th and after rows are discarded." ) )
                cmp.rows <- 4
                means <- means[1:4]
                sds <- sds[1:4]
            }

            if ( is.normal() )
            {
                # Normal Distribution
                current.mix.type <- 0L
                means <- means[1]
                sds <- sds[1]
            }
            else
            {
                # Not a normal distribution
                if ( cmp.rows == 4 )
                {
                    if ( is.v2( strict = TRUE ) )
                    {
                        # Reduce to vertical gradation (mix.type = 3).
                        current.mix.type <- 3L
                        means <- means[1:2]
                        sds <- sds[1:2]
                    }
                    else if ( is.h( strict = TRUE ) )
                    {
                        # Reduce to horizontal gradation (mix.type = 2).
                        current.mix.type <- 2L
                        means <- means[c( 1, 3 )]
                        sds <- sds[c( 1, 3 )]
                    }
                    else
                    {
                        current.mix.type <- 4L
                    }
                }
                else if ( cmp.rows == 3 )
                {
                    current.mix.type <- 3L
                    if ( is.v2( strict = TRUE ) )
                    {
                        # Vertical Gradation of 2 normal distributions
                        means <- means[1:2]
                        sds <- sds[1:2]
                    }
                }
                else # if ( cmp.rows == 2 )
                {
                    # When nrow( cmp ) == 2 and not a normal distribution,
                    # if current mix.type is strange, an error occur
                    # even if a valid this.mix.type is indicated.
                    #
                    # Because it may affect to the substance of the distribution
                    # to set the valid mix.type.
                    if ( length( mix.type ) == 0 || is.na( mix.type ) || !any( mix.type == 1:3 ) )
                    {
                        stop( "Error: Cannot identify current mix.type." )
                    }

                    current.mix.type <- mix.type
                }
            }

            new.mix.type <- current.mix.type    # the new value for mix.type

            # Second, give redundancy to the "cmp" if this.mix.type or grad argument is given.
            if ( length( this.mix.type ) > 0 )
            {
                # new.ncmp: number of cmp rows to have
                new.ncmp <- ggd.ncmp.for( grad, mix.type = this.mix.type )

                # The normal distribution can enhanse to any other type.
                # On the other hand, that of mix.type = 1 cannot enhanse to any other type.
                if ( current.mix.type == 0 )
                {
                    new.mix.type <- as.integer( this.mix.type )
                    if ( new.ncmp == 2 )
                    {
                        means <- rep( means[1], 2 )
                        sds <- rep( sds[1], 2 )
                    }
                    else if ( new.ncmp == 3 )
                    {
                        means <- rep( means[1], 3 )
                        sds <- rep( sds[1], 3 )
                    }
                    else if ( new.ncmp == 4 )
                    {
                        means <- rep( means[1], 4 )
                        sds <- rep( sds[1], 4 )
                    }
                }
                else if ( current.mix.type == 2 )
                {
                    if ( new.ncmp == 4 )
                    {
                        # Horizontal
                        new.mix.type <- 4L
                        means <- means[c( 1, 1, 2, 2 )]
                        sds <- sds[c( 1, 1, 2, 2 )]
                    }
                }
                else if ( current.mix.type == 3 && length( means ) == 2 )
                {
                    if ( new.ncmp == 3 )
                    {
                        new.mix.type <- 3L
                        means <- means[c( 1, 2, 1 )]
                        sds <- sds[c( 1, 2, 1 )]
                    }
                    else if ( new.ncmp == 4 )
                    {
                        # Vertical
                        new.mix.type <- 4L
                        means <- means[c( 1, 2, 1, 2 )]
                        sds <- sds[c( 1, 2, 1, 2 )]
                    }
                }

                if ( new.mix.type != this.mix.type )
                {
                    stop( paste( "Error: Not appropriate for",
                                        "current distribution model" ) )
                }

                if ( new.mix.type == 3 && grad == "v2" && length( means ) == 3 )
                {
                    stop( paste( "Error: Not appropriate for",
                                        "current distribution model" ) )
                }
            }

            mix.type <<- as.integer( new.mix.type )
            cmp <<- data.frame( mean = means, sd = sds )
        }

        return ( adjust.cmp.rownames() )
    }
)

################################################################################################
#' Adjust each row name of the cmp field.
#'
#' Sets each row name of \code{cmp} field according to \code{mix.type} field.
#' Normally, users of this class don't need to call this method directly.
#' @name    adjust.cmp.rownames
#' @aliases adjust.cmp.rownames
#' @aliases \S4method{adjust.cmp.rownames}{GGD}
#' @usage   \S4method{adjust.cmp.rownames}{GGD}()
#' @return  The adjusted \code{\link[ggd]{GGD}} object itself (invisible).
################################################################################################
NULL
GGD$methods(
    adjust.cmp.rownames = function()
    {
        # Using direct string vectors instead of rownames( cmp, prefix = "n." )
        # to avoid creating strange names when the number of the cmp rows is invalid.
        if ( length( mix.type ) == 0 || is.na( mix.type ) )
        {
            rownames( cmp ) <<- c()
        }
        else if ( mix.type == 0 )
        {
            rownames( cmp ) <<- "n.1"
        }
        else if ( mix.type == 1 )
        {
            rownames( cmp ) <<- c( "n.1", "n.2" )
        }
        else if ( mix.type == 2 )
        {
            rownames( cmp ) <<- c( "n.1", "n.2" )
        }
        else if ( mix.type == 3 )
        {
            if ( nrow( cmp ) == 2 )
            {
                rownames( cmp ) <<- c( "n.1", "n.2" )
            }
            else
            {
                rownames( cmp ) <<- c( "n.1", "n.2", "n.3" )
            }
        }
        else if ( mix.type == 4 )
        {
            rownames( cmp ) <<- c( "n.1.1", "n.1.2", "n.2.1", "n.2.2" )
        }
        else
        {
            stop( "Error: mix.type is invalid." )
        }

        return ( invisible( .self ) )
    }
)

################################################################################################
#' Adjust the kind.index field
#'
#' Sets \code{kind.index} and \code{kind} fields according to
#' \code{mix.type} and \code{cmp} fields.
#' Normally, users of this class don't need to call this method directly.
#' @name    adjust.kind.index
#' @aliases adjust.kind.index
#' @aliases \S4method{adjust.kind.index}{GGD}
#' @usage   \S4method{adjust.kind.index}{GGD}()
#' @return  The adjusted \code{\link[ggd]{GGD}} object itself (invisible).
################################################################################################
NULL
GGD$methods(
    adjust.kind.index = function()
    {
        index <- integer()

        if ( nrow( cmp ) > 0 )
        {
            if ( is.normal() )
            {
                index <- 1L # Normal Distribution
            }
            else
            {
                real.mix.type <- mix.type   # intrinsic mix.type
                if ( nrow( cmp ) == 4 )
                {
                    if ( is.h( strict = TRUE ) )
                    {
                        # Horizontal GGD
                        real.mix.type <- 2L
                    }
                    else if ( is.v2( strict = TRUE ) )
                    {
                        # Vertical GGD
                        real.mix.type <- 3L
                    }
                }

                if ( real.mix.type == 1 )
                {
                    if ( is.eq.sd() )
                    {
                        # Mean of Mean-Differed Sigma-Equaled 2 Normal Distributions
                        index <- 2L
                    }
                    else if ( is.eq.mean() )
                    {
                        # Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions
                        index <- 3L
                    }
                    else
                    {
                        # Mean of Mean-Differed Sigma-Differed 2 Normal Distributions
                        index <- 4L
                    }
                }
                else if ( real.mix.type == 2 )
                {
                    if ( is.eq.sd() )
                    {
                        # Mean-Differed Sigma-Equaled Horizontal Gradational Distribution
                        index <- 5L
                    }
                    else if ( is.eq.mean() )
                    {
                        # Mean-Equaled Sigma-Differed Horizontal Gradational Distribution
                        index <- 6L
                    }
                    else
                    {
                        # Mean-Differed Sigma-Differed Horizontal Gradational Distribution
                        index <- 7L
                    }
                }
                else if ( real.mix.type == 3 )
                {
                    if ( is.v2( strict = TRUE ) )
                    {
                        if ( is.eq.sd() )
                        {
                            # 2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution
                            index <- 8L
                        }
                        else if ( is.eq.mean() )
                        {
                            # 2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution
                            index <- 9L
                        }
                        else
                        {
                            # 2-Mean-Differed Sigma-Differed Vertical Gradational Distribution
                            index <- 10L
                        }
                    }
                    else
                    {
                        if ( is.eq.sd() )
                        {
                            # 3-Mean-Differed Sigma-Equaled Vertical Gradational Distribution
                            index <- 11L
                        }
                        else if ( is.eq.mean() )
                        {
                            # 3-Mean-Equaled Sigma-Differed Vertical Gradational Distribution
                            index <- 12L
                        }
                        else
                        {
                            # 3-Mean-Differed Sigma-Differed Vertical Gradational Distribution
                            index <- 13L
                        }
                    }
                }
                else if ( real.mix.type == 4 )
                {
                    if ( is.eq.sd() )
                    {
                        # Mean-Differed Sigma-Equaled Horizontal-Vertical Gradational Distribution
                        index <- 14L
                    }
                    else if ( is.eq.mean() )
                    {
                        # Mean-Equaled Sigma-Differed Horizontal-Vertical Gradational Distribution
                        index <- 15L
                    }
                    else
                    {
                        # Mean-Differed Sigma-Differed Horizontal-Vertical Gradational Distribution
                        index <- 16L
                    }
                }
                else
                {
                    stop( "Error: mix.type is invalid." )
                }
            }
        }

        kind.index <<- index
        if ( length( index ) == 0 )
        {
            if ( length( mix.type ) == 1 && is.na( mix.type ) )
            {
                kind.index <<- NA_integer_
                kind <<- NA_character_
            }
            else
            {
                kind <<- character()
            }
        }
        else
        {
            kind <<- kinds[index]
        }

        return ( invisible( .self ) )
    }
)

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

################################################################################################
#' Read a composition from a CSV file
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
#' @return  The ganarated \code{\link[ggd]{GGD}} object (invisible for \code{GGD} method).
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
    if ( length( mix.type ) != 1 || !( is.na( mix.type ) || any( mix.type == 0:4 ) ) )
    {
        stop( "Error: The value of mix.type is invalid." )
    }

    if ( nrow( table ) == 1 )
    {
        cmp <- data.frame( mean = numeric(), sd = numeric() )
    }
    else
    {
        cmp <- data.frame( mean = as.numeric( table[2:nrow( table ), 2] ),
                             sd = as.numeric( table[2:nrow( table ), 3] ) )
    }

    if ( mix.type == 3 && nrow( cmp ) == 3 )
    {
        grad <- "v3"
    }
    else
    {
        grad <- "default"
    }

    return ( ggd.set.cmp( cmp, mix.type = mix.type, grad = grad ) )
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
#' Write the composition to a CSV file
#'
#' Writes the composition of a \code{\link[ggd]{GGD}} object as a CSV file.
#' Mean values and standard deviations of the components are recorded to a maximum length of
#' the 22nd decimal place.
#' The accuracy is sufficient to reconstruct the original object almost completely
#' (at least the value of each field can be \code{TRUE} with \code{"=="})
#' in most cases, in most systems.
#' So, this function provides a simple way to export a \code{\link[ggd]{GGD}} object,
#' regardless of the package or R version.
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
    cat.table( obj$cmp, file, obj$mix.type, 22 )
}

GGD$methods(
    write.csv = function( file = "" )
    {
        ggd.write.csv( .self, file )
    }
)

################################################################################################
#' Quantile tracing
#'
#' Constructs a \code{\link[ggd]{GGD}} object which traces all of the given quantiles
#' accurately with its cumulative distribution function.
#' @export
#' @name    trace.q
#' @aliases ggd.trace.q
#' @aliases trace.q
#' @aliases \S4method{trace.q}{GGD}
#' @usage   ggd.trace.q(quantiles, x = "x", p = "p",
#'          kind = NULL, mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"),
#'          eq.mean = logical(), eq.sd = logical(), control = list())
#' @usage   \S4method{trace.q}{GGD}(quantiles, x = "x", p = "p",
#'          this.kind = NULL, this.mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"),
#'          eq.mean = logical(), eq.sd = logical(), control = list())
#' @param   quantiles   A data frame which represents the quantiles to be traced.
#'
#'                      It must contain at least 2 numeric columns for \code{x} and \code{p}.
#'                      Column \bold{\code{x}} is for the x-coordinates,
#'                      and column \bold{\code{p}} is for the probabilities of the quantiles.
#'
#'                      Rows which contain \code{NA} or \code{NaN} are ignored.
#'                      The value of each row must not duplicated
#'                      (except for \code{NA} or \code{NaN}),
#'                      and must be valid as a set of quantiles.
#'                      You do not have to sort the order of quantiles.
#'                      The number of valid rows must be in range of from 2 to 8.
#'
#'                      A valid example:
#'                      \code{data.frame(x = c(0, -2, 2.3), p = c(0.5, 0.25, 0.75))}.
#'
#'                      Column names and column numbers for \code{x} and \code{p} are flexible.
#'                      You can specify them with next two arguments.
#'
#' @param   x           The column name or column number for x-coordinates in \code{quantiles}.
#'
#' @param   p           The column name or column number for probabilities in \code{quantiles}.
#'
#' @param   kind        A character string or a numeric value or a \code{\link[ggd]{GGD}} object
#'                      which indicates the kind of the distribution model.
#'
#'                      The matching method of this argument follows that of elements of
#'                      the \code{objs} argument of the \link[ggd]{ggd.kind.index}.
#'
#'                      This argument gives the conditions of
#'                      the value of \code{mix.type} field,
#'                      and of whether the mean values and standard deviations of the components
#'                      should be aligned to the same value.
#'
#'                      Indicating \code{mix.type} argument or
#'                      indicating other than \code{"default"} for \code{grad} argument
#'                      or \code{TRUE}/\code{FALSE} for \code{eq.mean} or \code{eq.sd}
#'                      can overwrite the conditions of this argument.
#'
#' @param   this.kind   A string or a numeric value or a \code{\link[ggd]{GGD}} object
#'                      which indicates the kind of distribution model to construct.
#'
#'                      This argument will work as same as \code{kind} argument
#'                      of the generator function (signature '\code{NULL}').
#'
#'                      If this method is called without \code{this.kind} argument,
#'                      \code{grad}, or other conditions, it attempt to retain the value of
#'                      \code{mix.type} field as much as possible, but not the value of
#'                      \code{kind} field, i.e., the condition whether the mean value or
#'                      standard deviation of each component is aligned may not be retained.
#'                      If you want to retain the conditions as well,
#'                      indicate the object itself to \code{this.kind} argument like as
#'                      \code{obj$trace.q(quantiles, this.kind = obj)}.
#'
#' @param   mix.type    A numeric value to set into \code{mix.type} field as an integer.
#'                      It should be an integer from \code{0} to \code{4} or \code{NULL}.
#'
#'                      Each value represents the following type of distribution:
#'                      \itemize{
#'                          \item \code{0} : Normal distribution.
#'                          \item \code{1} : Mean of 2 normal distributions (not a GGD).
#'                          \item \code{2} : Horizontal gradation of 2 normal distributions.
#'                          \item \code{3} : Vertical gradation of 2 or 3 normal distributions.
#'                                           The 2-component model has priority.
#'                          \item \code{4} : Horizontal-Vertical gradation
#'                                           with 4 (2x2) normal distributions.
#'                      }
#'                      If \code{NULL} (and if \code{kind} and \code{grad = "default"}),
#'                      it tries to trace the quantiles with changing
#'                      the \code{mix.type} according to the priority sequence:
#'                      2 > 4 > 3 (2 components) > 3 (3 components) > 1 > 0.
#'
#'                      If other than \code{"defaule"} for \code{grad} argument is indicated,
#'                      this argument will be ignored.
#'
#' @param   this.mix.type   A numeric value to set into \code{mix.type} field as an integer.
#'                          It should be an integer from \code{0} to \code{4} or \code{NULL}.
#'
#'                      If \code{NULL}, the current \code{mix.type} field will be retained
#'                      (and number of components, too) if it could trace the quantiles.
#'                      But if could not, it tries to trace the quantiles
#'                      with changing \code{mix.type} as same as the generator function
#'                      (signature '\code{NULL}').
#'                      Thus, the priority sequence of \code{mix.type} is
#'                      current > 2 > 4 > 3 (2 components) > 3 (3 components) > 1 > 0.
#'
#' @param   grad        A character string indicating the method of gradation.
#'
#'                      \code{"h"} for horizontal, \code{"v"} for vertical,
#'                      and \code{"hv"} for horizontal-vertical.
#'                      The number after \code{"v"} is the number of components.
#'                      Numberless \code{"v"} is an alias for \code{"v2"}.
#'
#'                      \code{"normal"} is for a normal distribution.
#'                      \code{"default"} is for depending on other arguments.
#'
#'                      If other than \code{"default"} is indicated,
#'                      this function constructs a distribution model
#'                      according to this argument with ignoring \code{[this.]mix.type} argument
#'                      and overwriting the type indicated by \code{[this.]kind} argument.
#'
#' @param   eq.mean     A logical. It works when \code{mix.type} is not 0.
#'
#'                      If \code{TRUE}, it forces all of the mean values of the components
#'                      to be equal.
#'                      This condition reduces the degrees of freedom,
#'                      so allowed number of quantiles will be restricted.
#'                      See "Details" for more information.
#'
#'                      If \code{FALSE}, it constructs mean-differed components.
#'                      In this case, mean-equaled components are rarely constructed.
#'
#'                      If \code{logical(0)} (the default),
#'                      Normally, as \code{FALSE}, it constructs mean-differed components.
#'                      But if that fails, it tries to construct mean-equaled components
#'                      as \code{TRUE} (especially, when the median is specified as a quantile
#'                      to trace with a vertical gradational distribution).
#'
#'                      \code{TRUE} and \code{FALSE} can overwrite the condition indicated by
#'                      \code{kind} or \code{this.kind} argument.
#'
#' @param   eq.sd       A logical. It works when \code{mix.type} is not 0.
#'
#'                      If \code{TRUE}, it forces all of the standard deviations of
#'                      the components to be equal.
#'                      This condition reduces the degrees of freedom,
#'                      so allowed number of quantiles will be restricted.
#'                      See "Details" for more information.
#'
#'                      If \code{FALSE} or \code{logical(0)},
#'                      it constructs sigma-differed components.
#'                      In this case, sigma-equaled components are rarely constructed.
#'
#'                      \code{TRUE} and \code{FALSE} can overwrite the condition indicated by
#'                      \code{kind} or \code{this.kind} argument.
#'
#' @param   control     A list of \code{control} option for \code{\link[nleqslv]{nleqslv}}.
#'                      See the "Control options" at \code{\link[nleqslv]{nleqslv}}
#'                      for more information.
#'
#'                      By default, this list is empty, but if there is a high probability
#'                      of a "Jacobian is singular" error due to the condition of the quantiles,
#'                      \code{allowSingular = TRUE} is set implicitly.
#'                      However, if \code{allowSingular} is given to this argument,
#'                      the given option takes priority.
#'
#' @return  A list containing components (invisible for \code{GGD} method)
#'          \item{obj}{
#'                  The \code{\link[ggd]{GGD}} object which traces the quantiles.
#'                  For \code{\link[ggd]{GGD}} method, the \code{\link[ggd]{GGD}} object itself.}
#'          \item{nleqslv.out}{
#'                  The list of the output of \code{\link[nleqslv]{nleqslv}}
#'                  which has successed to solve tracing quantiles.}
#'
#'          For \code{GGD} method: If an error occur, the object will be cleared in most cases.
#'
#' @importFrom  methods     new
#' @seealso \code{\link[ggd]{nls.freq}}
#'
#' @details
#'  \subsection{Allowed number of quantiles}{
#'      The allowed numbers of quantiles depends on the value of \code{mix.type} argument/field.
#'      Because the value determines the number of components in \code{cmd} field,
#'      and resultingly determines the degrees of freedom of the distribution model.
#'      The allowed numbers of quantiles are as follows:
#'      \itemize{
#'          \item \code{mix.type = 0} : 2 quantiles only.
#'          \item \code{mix.type = 1} : 3 or 4 quantiles.
#'          \item \code{mix.type = 2} : 3 or 4 quantiles.
#'          \item \code{mix.type = 3} : 3 to 6 quantiles.
#'          \item \code{mix.type = 4} : 5 to 8 quantiles.
#'      }
#'      About \code{mix.type = 3},
#'      if \code{grad = "v2"}, 3 or 4 quantiles are allowed.
#'      If \code{grad = "v3"}, 3 to 6 quantiles are allowed.
#'
#'      Note, if \code{mix.type = 3} is indicated with \code{grad = "default"},
#'      it tries both \code{grad = "v2"} and \code{"v3"} cases
#'      and give priority to \code{"v2"}.
#'  }
#'  \subsection{Tightening with eq.mean / eq.sd}{
#'      If either \code{eq.mean = TRUE} or \code{eq.sd = TRUE} is indicated,
#'      as each of them reduces the degrees of freedom,
#'      the number of quantiles is restricted as follows:
#'      \itemize{
#'          \item \code{mix.type = 1} : 3 quantiles only.
#'          \item \code{mix.type = 2} : 3 quantiles only.
#'          \item \code{mix.type = 3} : 3 or 4 quantiles.
#'          \item \code{mix.type = 4} : 5 quantiles only.
#'      }
#'      About \code{mix.type = 3},
#'      with \code{grad = "v2"}, it works for 3 quantiles only.
#'      With \code{grad = "v3"}, it works for 3 or 4 quantiles.
#'
#'      If both \code{eq.mean} and \code{eq.sd} are \code{TRUE},
#'      a normal distribution will be constructed and number of quantiles must be 2.
#'  }
#'
#' @examples
#'  ## Mean of 2 Normal Distributions Example:
#'  ##  If mix.type = 1, it constructs a mean of 2 normal distributions.
#'  ##  This model is not a gradational Gaussian distribution (GDD),
#'  ##  but a kind of Gaussian mixture model (GMM).
#'  ##  The number of quantiles must be 3 or 4.
#'  result <- ggd.trace.q( data.frame(
#'                         x = c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.75 ) ),
#'                         p = c( 0.1, 0.5, 0.6 ) ),
#'                         mix.type = 1 )
#'  result
#'  plot( seq( -3, 3, 0.01 ), result$obj$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Horizontal Gradational Example:
#'  ##  If mix.type = 2 or grad = "h",
#'  ##  it constructs a horizontal gradational distribution.
#'  ##  The number of p of the quantiles must be 3 or 4.
#'  a <- ggd.trace.q( data.frame( x = c( -0.67, 0, 0.53 ), p = c( 0.25, 0.5, 0.75 ) ),
#'                    mix.type = 2 )$obj
#'  a
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## eq.sd Example:
#'  ##  If eq.sd = TRUE, it constructs a distribution model with components of which
#'  ##  the standard deviations are all equal.
#'  a$trace.q(
#'          data.frame( x = c( -0.64, -0.25, 0 ), p = c( 0.25, 0.4, 0.5 ) ),
#'          this.mix.type = 2, eq.sd = TRUE )
#'  a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Vertical Gradational Example:
#'  ##  If grad = "v2", it constructs a vertical gradation of 2 normal distributions.
#'  ##  The number of quantiles must be 3 or 4.
#'  a$trace.q(
#'          data.frame( x = c( -1.28, -0.23, 0 ), p = c( 0.1, 0.4, 0.5 ) ),
#'          grad = "v2" )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## eq.mean Example:
#'  ##  If eq.mean = TRUE, it constructs a mean-equaled distribution model.
#'
#'  a$trace.q(
#'          data.frame( x = c( -1.28, -0.42, 0 ), p = c( 0.1, 0.3, 0.5 ) ),
#'          grad = "v2", eq.mean = TRUE )
#'  a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## 3-Mean/Sigma-Differed Vertical Gradational Example:
#'  ##  If grad = "v3", it constructs a vertical gradational distribution
#'  ##  with different components for left-tail side and right-tail side.
#'  ##  The number of quantiles must be from 3 to 6.
#'  a <- ggd.trace.q(
#'          data.frame( x = c( -1.92, -0.20, 0.20, 1.92 ), p = c( 0.1, 0.4, 0.6, 0.9 ) ),
#'          grad = "v3" )$obj
#'  a
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Horizontal-Vertical Gradational Examples:
#'  ##  If mix.type = 4 or grad = "hv",
#'  ##  it constructs a horizontal-vertical gradational distribution.
#'  ##  The number of quantiles must be from 5 to 8.
#'  a <- ggd.trace.q(
#'          data.frame(
#'              x = c( -1.38, -0.76, -0.28, 0.02, 0.36, 1.10, 2.79 ),
#'              p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ) ),
#'          mix.type = 4 )$obj
#'  a
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  a$trace.q(
#'      data.frame(
#'          x = c( -1.40, -0.96, -0.61, -0.30, 0.32, 0.72, 1.23, 2.21 ),
#'          p = c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ) ),
#'      grad = "hv" )
#'  a
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
################################################################################################
ggd.trace.q <- function( quantiles, x = "x", p = "p",
                         kind = NULL, mix.type = NULL,
                         grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ),
                         eq.mean = logical(), eq.sd = logical(), control = list() )
{
    obj <- GGD$new()
    return ( withVisible( obj$trace.q( quantiles     = quantiles,
                                       x             = x,
                                       p             = p,
                                       this.kind     = kind,
                                       this.mix.type = mix.type,
                                       grad          = grad,
                                       eq.mean       = eq.mean,
                                       eq.sd         = eq.sd,
                                       control       = control ) )$value )
}

GGD$methods(
    trace.q = function( quantiles, x = "x", p = "p",
                        this.kind = NULL, this.mix.type = NULL,
                        grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ),
                        eq.mean = logical(), eq.sd = logical(), control = list() )
    {
        result <- NULL              # result of solving-function (typically nleqslv)
        new.mix.type <- integer()   # mix.type of the new GGD object
        control.backup <- control   # backup of control list for retrying

        ########################################################
        # Check arguments

        # Note:
        # In this function, when a error occur,
        # we clear the fields as possible as we can.
        #
        # Because this function does not directly set specified values to the fields,
        # if the fields are not cleared and contain some normal values,
        # users may let the subsequent processes take place without noticing the error
        # (during the development phase, the author actually experienced such mistakes).

        # Check data frame.
        if ( !is.data.frame( quantiles ) )
        {
            clear()
            stop( "Error: quantiles must be a data frame." )
        }

        if ( length( x ) != 1 || ( !is.numeric( x ) && !is.character( x ) ) || is.na( x ) )
        {
            clear()
            stop( "Error: Argument x must be a column name or an index number." )
        }
        else if ( is.numeric( x ) )
        {
            x <- as.integer( x )
            if ( !any( x == 1:ncol( quantiles ) ) )
            {
                clear()
                stop( "Error: Illegal column number for x." )
            }
        }

        if ( length( p ) != 1 || ( !is.numeric( p ) && !is.character( p ) ) || is.na( p ) )
        {
            clear()
            stop( "Error: Argument p must be a column name or an index number." )
        }
        else if ( is.numeric( p ) )
        {
            p <- as.integer( p )
            if ( !any( p == 1:ncol( quantiles ) ) )
            {
                clear()
                stop( "Error: Illegal column number for p." )
            }
        }

        if ( is.null( quantiles[[x]] ) || is.null( quantiles[[p]] ) )
        {
            clear()
            stop( paste0( "Error: Column '",
                                  ifelse( is.null( quantiles[[x]] ), x, p ),
                                  "' is undefined." ) )
        }

        grad <- withCallingHandlers( match.arg( grad ), error = function( e ) clear() )
        if ( grad == "v" )
        {
            grad <- "v2"
        }

        # Discard NA and NaN.
        qt <- extract.complete.x.y( quantiles, x, p, "p" )

        if ( nrow( qt ) == 0 )
        {
            clear()
            stop( "Error: No valid rows in quantiles." )
        }
        else if ( nrow( qt ) < 2 )
        {
            clear()
            stop( "Error: Illegal number of quantiles." )
        }

        # Check the range of probability p.
        if ( length( qt$p[( qt$p < 0 | qt$p > 1 )] ) > 0 )
        {
            warning( paste( "Warning: There is an out-of-range" , p, "value." ) )
        }

        # Warning if the x-coordinates are infinite and the probabilities are not 0 nor 1.
        if ( length( qt$x[( qt$x == -Inf & qt$p != 0 ) | ( qt$x != -Inf & qt$p == 0 ) |
                          ( qt$x == Inf  & qt$p != 1 ) | ( qt$x != Inf  & qt$p == 1 )] ) > 0 )
        {
            warning( paste( "Warning: Wrong position for upper or lower limit of",
                                      x, "or", p, "value." ) )
        }

        ################################################################
        # Make qt to an ascending-sorted quantiles excluding the median and infinity quantiles
        # then make it suitable as the condition for nleqslv.

        # Often, the median quantile can be excluded from the condition for nleqslv,
        # in particular, if eq.mean = TRUE.
        # Because if the mean values of all of the components are aligned to the same value,
        # median will obviously be equal to the value.

        # Get the order of the quantiles.
        qt.order <- order( qt$p )

        # Error if the x-coordinates are not in the same order as the probabilities.
        if ( any( qt$p[qt.order[1:( nrow( qt ) - 1 )]] >= qt$p[qt.order[2:nrow( qt )]] ) ||
             any( qt$x[qt.order[1:( nrow( qt ) - 1 )]] >= qt$x[qt.order[2:nrow( qt )]] ) )
        {
            clear()
            stop( paste( "Error: Order of", x, "and", p, "must be along,",
                                "and", x, "and", p, "must not duplicated." ) )
        }

        # Sort qt and remove invalid quantiles.
        # qt.with.median will be used when the median has to be added to the condition.
        x.ordered <- qt$x[qt.order]
        p.ordered <- qt$p[qt.order]
        qt.with.median <- data.frame( x = x.ordered[p.ordered > 0 & p.ordered < 1 &
                                                    x.ordered > -Inf & x.ordered < Inf],
                                      p = p.ordered[p.ordered > 0 & p.ordered < 1 &
                                                    x.ordered > -Inf & x.ordered < Inf] )

        # Get the number of valid quantiles.
        qt.num <- nrow( qt.with.median ) # the number of quantiles
        if ( qt.num < 2 || qt.num > 8 )
        {
            clear()
            stop( "Error: Illegal number of valid quantiles." )
        }

        # Get the median if it is specified.
        is.set.median <- any( qt.with.median$p == 0.5 )
        if ( is.set.median )
        {
            median <<- qt.with.median$x[qt.with.median$p == 0.5][1]
        }

        # Get ordered quantiles without median.
        qt.no.median <- qt.with.median[( qt.with.median$p != 0.5 ),]

        ################################################################
        # Deciding the new mix.type.

        # Get grad value needed for tracing processes with this.kind.
        if ( !is.null( this.kind ) )
        {
            this.kind.index <- withCallingHandlers(
                                ggd.kind.index( this.kind, undef.err = TRUE ),
                                error = function( e ) clear() )
            if ( length( this.kind.index ) > 1 )
            {
                clear()
                stop( "Error: kind should be valid single value or a GGD object." )
            }
            else if ( length( this.kind.index ) == 0 )
            {
                this.kind.index <- NA_integer_
            }

            if ( !is.na( this.kind.index ) )
            {
                if ( grad == "default" )
                {
                    if ( any( this.kind.index == 8:10 ) )
                    {
                        grad <- "v2"
                    }
                    else if ( any( this.kind.index == 11:13 ) )
                    {
                        grad <- "v3"
                    }
                }
                if ( length( eq.mean ) == 0 )
                {
                    eq.mean <- ( this.kind.index %% 3 == 0 )
                }
                if ( length( eq.sd ) == 0 )
                {
                    eq.sd   <- ( this.kind.index %% 3 == 2 )
                }
            }
        }

        # Since eq.sd is 2-valued (TRUE/FALSE) logical, set FALSE if length is 0
        # (eq.mean is 3-valued).
        eq.mean <- if ( length( eq.mean ) == 0 ) logical() else isTRUE( eq.mean )
        eq.sd <- isTRUE( eq.sd )

        if ( isTRUE( eq.mean ) && eq.sd )
        {
            grad <- "normal"
        }

        is.v3.ind <- ( grad == "v3" )   # The flag if grad = "v3" is indicated as the argument.
                                        # This is used for retrying.

        # Note: Error check for this.kind has already done at ggd.kind.index.
        #       So, now this.kind has a valid value or the length of it is 0.
        new.mix.type <- ggd.mix.type.for( grad, kind = this.kind, mix.type = this.mix.type )

        # Check indicated new mix.type.
        if ( length( new.mix.type ) != 1 || is.na( new.mix.type ) ||
             !any( new.mix.type == 0:4 ) )
        {
            if ( !is.null( this.mix.type ) )
            {
                clear()
                stop( "Error: mix.type should be single integer from 0 to 4." )
            }
            else if ( !is.null( this.kind ) )
            {
                # This code will run if this.kind = character( 0 ).
                clear()
                stop( "Error: kind should be valid single value or a GGD object." )
            }
            else
            {
                # If come here, both this.kind and this.mix.type are NULL. It is OK.
            }
        }

        # New mix.type indicated by arguments has got now.
        # If the value has not been got yet, decide it with number of quantiles.
        if ( length( new.mix.type ) == 0 )
        {
            if ( qt.num == 2 )
            {
                # If 2 quantiles, only mix.type = 0 is allowed.
                new.mix.type <- 0
            }
            else
            {
                ################################################################
                # Sequential tracing with changing mix.type
                if ( qt.num < 5 )
                {
                    # mix.type = 4 and 0 are skipped because error shall occur always.
                    mix.type.seq <- c( 2, 3, 1 )
                }
                else # if ( qt.num in 5:8 )
                {
                    # mix.type = 2, 1 and 0 are skipped because error shall occur always.
                    mix.type.seq <- c( 4, 3 )
                }

                # If current mix.type is valid, move the value to the top of the sequence.
                if ( length( mix.type ) > 0 && !is.na( mix.type ) &&
                     any( mix.type == mix.type.seq ) )
                {
                    mix.type.seq <- c( mix.type,
                                       mix.type.seq[mix.type.seq != mix.type] )

                    # In order to attempt to retain number of rows of cmp field when mix.type = 3,
                    # make sequences of values to set grad values.
                    if ( mix.type == 3 && nrow( cmp ) == 2 )
                    {
                        grad.seq <- c( "v2", rep( "default", length( mix.type.seq - 1 ) ) )
                    }
                    else if ( mix.type == 3 && nrow( cmp ) == 3 )
                    {
                        grad.seq <- c( "v3", rep( "default", length( mix.type.seq - 1 ) ) )
                    }
                    else
                    {
                        grad.seq <- rep( "default", length( mix.type.seq ) )
                    }
                }
                else
                {
                    grad.seq <- rep( "default", length( mix.type.seq ) )
                }

                # Because of sequential processing, vapply should not be used here.
                for( i in 1:length( mix.type.seq ) )
                {
                    result <- withCallingHandlers(
                                try( .self$trace.q( quantiles       = qt.with.median,
                                                    this.mix.type   = mix.type.seq[i],
                                                    grad            = grad.seq[i],
                                                    eq.mean         = eq.mean,
                                                    eq.sd           = eq.sd,
                                                    control         = control ),
                                    silent = TRUE ),
                                message = function( m )
                                {
                                    if ( is.na( charmatch( "Message:", m[[1]] ) ) )
                                    {
                                        # An error message has thrown.
                                        msg <- paste0(
                                                "At mix.type = ", mix.type.seq[i],
                                                ", an error has occurred" )

                                        if ( i != length( mix.type.seq ) )
                                        {
                                            msg <- paste( msg, "(then retried)" )
                                        }

                                        message( paste0( msg, ":" ) )
                                    }

                                } )
                    if ( !inherits( result, "try-error" ) )
                    {
                        break
                    }
                }
                if ( inherits( result, "try-error" ) )
                {
                    clear()
                    stop( result )
                }
                else
                {
                    return ( invisible( result ) )
                }
            }
        }

        ################################################################
        # Check if the number of quantiles is allowed.
        if ( isTRUE( eq.mean ) || eq.sd )
        {
            if ( new.mix.type == 3 && grad == "v2" )
            {
                if ( !qt.num == 3 )
                {
                    clear()
                    stop( paste( "Error: Illegal number of quantiles for grad = \"v2\"",
                                 "and either eq.mean or eq.sd is TRUE." ) )
                }
            }
            else if ( !( ( new.mix.type == 0 && qt.num == 2 ) ||
                         ( new.mix.type == 1 && qt.num == 3 ) ||
                         ( new.mix.type == 2 && qt.num == 3 ) ||
                         ( new.mix.type == 3 && any( qt.num == 3:4 ) ) ||
                         ( new.mix.type == 4 && qt.num == 5 ) ) )
            {
                clear()
                stop( paste( "Error: Illegal number of quantiles for mix.type =",
                             new.mix.type, "and either eq.mean or eq.sd is TRUE." ) )
            }
        }
        else
        {
            if ( new.mix.type == 3 && grad == "v2" )
            {
                if ( !any( qt.num == 3:4 ) )
                {
                    clear()
                    stop( "Error: Illegal number of quantiles for grad = \"v2\"." )
                }
            }
            else if ( !( ( new.mix.type == 0 && qt.num == 2 ) ||
                         ( new.mix.type == 1 && any( qt.num == 3:4 ) ) ||
                         ( new.mix.type == 2 && any( qt.num == 3:4 ) ) ||
                         ( new.mix.type == 3 && any( qt.num == 3:6 ) ) ||
                         ( new.mix.type == 4 && any( qt.num == 5:8 ) ) ) )
            {
                clear()
                stop( paste( "Error: Illegal number of quantiles for mix.type =",
                              paste0( new.mix.type, "." ) ) )
            }
        }

        ################################################################
        # If new.mix.type = 0,
        # culculate the mean and the standard deviation directly.
        if ( new.mix.type == 0 )
        {
            result <- ms.norm.xp( qt.with.median$x, qt.with.median$p )
            set.cmp( data.frame( result ), this.mix.type = new.mix.type )

            return ( invisible( list( obj = .self, nleqslv.out = NULL ) ) )
        }

        ################################################################
        # Determine the maximum number of retries of tracing processes.
        max.retry <- 0      # Number of retries allowed
        retry.msg <- NULL   # Message when tracing has failed once then going to retry.

        if ( length( eq.mean ) == 0 && !eq.sd &&
              ( ( new.mix.type == 1 && qt.num == 3 ) ||
                ( new.mix.type == 2 && qt.num == 3 ) ||
                ( new.mix.type == 3 && ( qt.num == 3 ||
                                         ( qt.num == 4 && grad != "v2" ) ) ) ||
                ( new.mix.type == 4 && qt.num == 5 ) ) )
        {
            # At first, it tries to trace with mean-differed components,
            # and next, it tries with mean-equaled components.
            # So once retry is allowed.
            max.retry <- 1
        }

        if ( new.mix.type == 3 && qt.num <= 4 && grad != "v2" )
        {
            if ( grad != "v3" && !( qt.num == 4 && ( isTRUE( eq.mean ) || eq.sd ) ) )
            {
                # At first, it tries to trace with "v2",
                # and next, it tries with "v3".
                # So once more retry is allowed.
                max.retry <- max.retry + 1

                if ( qt.num == 3 && length( eq.mean ) == 0 && !eq.sd )
                {
                    # Both in mean-differed phase and mean-equaled phase
                    # this process will be done.
                    # So once more retry is allowed.
                    max.retry <- max.retry + 1
                }
            }

            if ( !isFALSE( eq.mean ) && !eq.sd && is.set.median )
            {
                # When median is specified with a quantile, in eq.mean and "v3" phase,
                # a logical stepwise tracing without allowSingular option and
                # another computational tracing with allowSingular option will be try.
                # So once more retry is allowed.
                max.retry <- max.retry + 1
            }
        }

        if ( new.mix.type == 4 && qt.num <= 6 && !isTRUE( eq.mean ) && !eq.sd )
        {
            # At first, it tries to trace with a horizontal gradation of
            # 2 mean-equaled vertical gradational distributions (6 degrees of freedom),
            # and next, it tries to trace with 4 mean-differed components
            # (8 degrees of freedom). So once more retry is allowed.
            max.retry <- max.retry + 1
        }

        ################################################################
        # Construct a GGD object which can trace the quantiles.

        #  To avoid that standard deviations fall into negative values
        #  during nleqslv iterations,
        #  we use "square of square root" of standard deviations
        #   (And it seems that the convergence is a little faster
        #    than using standard deviations directly).

        # This case-separated loop process must run in serial, not in parallel.
        for ( retry in 0:max.retry )
        {
            x.0 <- NULL             # initial vector for nleqslv.
            f <- NULL               # objective function.
            get.cmp.result <- NULL  # function which gives cmp with the result of nleqslv.
            qt <- qt.no.median      # the condition of quantiles for nleqslv

            # Define the initial values and the objective function of nleqslv.
            #   In the folloing code,
            #   mean.i and sd.i (i = 1,2,...) are index number of x,
            #   which is the input vector of f and also output vector of nleqslv.
            if ( new.mix.type == 1 )
            {
                if ( eq.sd )
                {
                    # Where eq.sd = TRUE, all standard deviations are equaled.
                    # So each of the distances from the median of the distribution
                    # to the mean of each of the normal distributions will be equaled.
                    #
                    # So we seek the median (if unknown),
                    # and the distance to the mean, and the standard deviation.
                    if ( is.set.median )
                    {
                        x.0 <- c( 0, sqrt( ms.norm.xp( qt$x[1:2], qt$p[1:2] )$sd ) )

                        median.1 <- 0
                        dist.1 <- 1
                        sd.1 <- 2
                    }
                    else
                    {
                        preudos <- ms.norm.xp( qt$x[c( 1, 3 )], qt$p[c( 1, 3 )] )
                        x.0 <- c( qnorm( 0.5, preudos$mean, preudos$sd ), 0,
                                  sqrt( preudos$sd ) )

                        median.1 <- 1
                        dist.1 <- 2
                        sd.1 <- 3
                    }

                    f <- function( x )
                    {
                        ( pnorm( qt$x, ifelse( median.1 == 0, median, x[median.1] ) - x[dist.1],
                                       x[sd.1]^2 ) +
                          pnorm( qt$x, ifelse( median.1 == 0, median, x[median.1] ) + x[dist.1],
                                       x[sd.1]^2 ) ) / 2 - qt$p
                    }

                    get.cmp.result <- function( result )
                    {
                        med.result <- ifelse( median.1 == 0, median, result$x[median.1] )

                        data.frame( mean = c( med.result - result$x[dist.1],
                                              med.result + result$x[dist.1] ),
                                    sd = rep( result$x[sd.1]^2, 2 ) )
                    }
                }
                else
                {
                    if ( qt.num == 3 )
                    {
                        if ( isTRUE( eq.mean ) )
                        {
                            # Same mean, different standard deviations.
                            if ( is.set.median )
                            {
                                # Mean is excluded from the condition
                                # and the mean values are equaled to the median.
                                x.0 <- c( 0.9, 1.1 )

                                mean.1 <- mean.2 <- 0
                                sd.1 <- 1
                                sd.2 <- 2
                            }
                            else
                            {
                                pseudo.mean <- ms.norm.xp( qt$x[c( 1, 3 )],
                                                           qt$p[c( 1, 3 )] )$mean
                                x.0 <- c( pseudo.mean, 0.9, 1.1 )

                                mean.1 <- mean.2 <- 1
                                sd.1 <- 2
                                sd.2 <- 3
                            }
                        }
                        else
                        {
                            # Process as 4 quantiles.
                            # (allowSingular = TRUE due to insufficient conditions)
                            qt <- data.frame( x = qt.with.median$x[c( 1, 2, 2, 3 )],
                                              p = qt.with.median$p[c( 1, 2, 2, 3 )] )
                            pseudos <- list( ms.norm.xp( qt$x[1:2], qt$p[1:2] ),
                                             ms.norm.xp( qt$x[3:4], qt$p[3:4] ) )
                            x.0 <- c( pseudos[[1]]$mean, pseudos[[2]]$mean,
                                      sqrt( pseudos[[1]]$sd ), sqrt( pseudos[[2]]$sd ) )

                            mean.1 <- 1
                            mean.2 <- 2
                            sd.1 <- 3
                            sd.2 <- 4

                            if ( !is.list( control ) || is.null( control$allowSingular ) )
                            {
                                control <- append( control, list( allowSingular = TRUE ) )
                            }
                        }
                    }
                    else # if ( qt.num == 4 )
                    {
                        # 4 quantiles
                        qt <- qt.with.median
                        pseudos <- list( ms.norm.xp( qt$x[c( 1, 4 )], qt$p[c( 1, 4 )] ),
                                         ms.norm.xp( qt$x[2:3], qt$p[2:3] ) )
                        x.0 <- c( pseudos[[1]]$mean, pseudos[[2]]$mean,
                                  sqrt( pseudos[[1]]$sd ), sqrt( pseudos[[2]]$sd ) )

                        mean.1 <- 1
                        mean.2 <- 2
                        sd.1 <- 3
                        sd.2 <- 4
                    }

                    f <- function( x )
                    {
                        ( pnorm( qt$x, ifelse( mean.1 == 0, median, x[mean.1] ),
                                       x[sd.1]^2 ) +
                          pnorm( qt$x, ifelse( mean.2 == 0, median, x[mean.2] ),
                                       x[sd.2]^2 ) ) / 2 - qt$p
                    }

                    get.cmp.result <- function( result )
                    {
                        data.frame( mean = ifelse( c( mean.1, mean.2 ) == 0,
                                                   rep( median, 2 ),
                                                   result$x[c( mean.1, mean.2 )] ),
                                    sd = result$x[c( sd.1, sd.2 )]^2 )
                    }
                }
            }
            else if ( new.mix.type == 2 )
            {
                if ( is.set.median )
                {
                    pseudo.mean <- median
                }
                else
                {
                    pseudo.mean <- ms.norm.xp( qt$x[c( 1, 3 )], qt$p[c( 1, 3 )] )$mean
                }

                if ( eq.sd )
                {
                    # Where eq.sd = TRUE, all standard deviations are equaled.
                    # So each of the distances from the median of the distribution
                    # to the mean of each of the normal distributions will be equaled.
                    #
                    # So we seek the median (if unknown),
                    # and the distance to the mean, and the standard deviation.
                    if ( is.set.median )
                    {
                        x.0 <- c( 0, sqrt( ( sd.norm.mxp( pseudo.mean, qt$x[1], qt$p[1] )
                                             + sd.norm.mxp( pseudo.mean, qt$x[2], qt$p[2] ) ) *
                                           15 / 32 ) )
                        median.1 <- 0
                        dist.1 <- 1
                        sd.1 <- 2
                    }
                    else
                    {
                        x.0 <- c( pseudo.mean, 0,
                                  sqrt( ( sd.norm.mxp( pseudo.mean, qt$x[1], qt$p[1] )
                                          + sd.norm.mxp( pseudo.mean, qt$x[2], qt$p[2] ) ) *
                                        15 / 32 ) )
                        median.1 <- 1
                        dist.1 <- 2
                        sd.1 <- 3
                    }

                    f <- function( x )
                    {
                        p1 <- pnorm( qt$x,
                                     ifelse( median.1 == 0, median, x[median.1] ) - x[dist.1],
                                     x[sd.1]^2 )
                        p2 <- pnorm( qt$x,
                                     ifelse( median.1 == 0, median, x[median.1] ) + x[dist.1],
                                     x[sd.1]^2 )

                        return ( p1 - ( p1 * p1 - p2 * p2 ) / 2 - qt$p )
                    }

                    get.cmp.result <- function( result )
                    {
                        med.result <- ifelse( median.1 == 0, median, result$x[median.1] )

                        data.frame( mean = c( med.result - result$x[dist.1],
                                              med.result + result$x[dist.1] ),
                                    sd = rep( result$x[sd.1]^2, 2 ) )
                    }
                }
                else
                {
                    if ( qt.num == 3 )
                    {
                        if ( isTRUE( eq.mean ) )
                        {
                            # Same mean, different standard deviations.
                            if ( is.set.median )
                            {
                                x.0 <- c( 1, 1 )
                            }
                            else
                            {
                                x.0 <- c( pseudo.mean, 1, 1 )
                            }

                            mean.1 <- mean.2 <- ifelse( is.set.median, 0, 1 )
                            sd.1 <- mean.2 + 1
                            sd.2 <- mean.2 + 2
                        }
                        else
                        {
                            # Process as 4 quantiles.
                            # (allowSingular = TRUE due to insufficient conditions)
                            qt <- data.frame( x = qt.with.median$x[c( 1, 2, 2, 3 )],
                                              p = qt.with.median$p[c( 1, 2, 2, 3 )] )
                            x.0 <- c( pseudo.mean, pseudo.mean, 1, 1 )

                            mean.1 <- 1
                            mean.2 <- 2
                            sd.1 <- 3
                            sd.2 <- 4

                            if ( !is.list( control ) || is.null( control$allowSingular ) )
                            {
                                control <- append( control, list( allowSingular = TRUE ) )
                            }
                        }
                    }
                    else # if ( qt.num == 4 )
                    {
                        # 4 quantiles
                        qt <- qt.with.median
                        pseudos <- list( ms.norm.xp( qt$x[1:2], qt$p[1:2] ),
                                         ms.norm.xp( qt$x[3:4], qt$p[3:4] ) )
                        x.0 <- c( pseudos[[1]]$mean, pseudos[[2]]$mean,
                                  sqrt( pseudos[[1]]$sd ), sqrt( pseudos[[2]]$sd ) )

                        mean.1 <- 1
                        mean.2 <- 2
                        sd.1 <- 3
                        sd.2 <- 4
                    }

                    f <- function( x )
                    {
                        p1 <- pnorm( qt$x, ifelse( mean.1 == 0, median, x[mean.1] ), x[sd.1]^2 )
                        p2 <- pnorm( qt$x, ifelse( mean.2 == 0, median, x[mean.2] ), x[sd.2]^2 )

                        return ( p1 - ( p1 * p1 - p2 * p2 ) / 2 - qt$p )
                    }

                    get.cmp.result <- function( result )
                    {
                        data.frame( mean = c( ifelse( mean.1 == 0, median, result$x[mean.1] ),
                                              ifelse( mean.2 == 0, median, result$x[mean.2] ) ),
                                    sd = result$x[c( sd.1, sd.2 )]^2 )
                    }
                }
            }
            else if ( new.mix.type == 3 && grad != "v3" &&
                      ( qt.num == 3 ||
                        ( qt.num == 4 && ( !isTRUE( eq.mean ) && !eq.sd ) ) ) )
            {
                ########################################
                # new.mix.type = 3, 2 components
                if ( eq.sd )
                {
                    # Set start values for mean values and the standard deviaton
                    # by 2 quantiles other than median.
                    pseudo <- ms.norm.xp( qt$x[1:2], qt$p[1:2] )

                    x.0 <- c( pseudo$mean,
                              ifelse( is.set.median, median, pseudo$mean ),
                              sqrt( pseudo$sd ) )
                    qt <- qt.with.median

                    mean.1 <- 1
                    mean.2 <- 2
                    sd.1 <- sd.2 <- 3

                    if ( grad != "v2" )
                    {
                        retry.msg <- paste( "Message: Tracing with 2 components has failed.",
                                            "Tracing with 3 components has been retried." )
                    }
                }
                else if ( isTRUE( eq.mean ) )
                {
                    # Standardly trace 3 quantiles with mean-equaled components.
                    if ( is.set.median )
                    {
                        # Median is specified:
                        #   Unify mean values to the median and seek standard deviations.
                        x.0 <- c( 1, 1 )

                        mean.1 <- mean.2 <- 0
                        sd.1 <- 1
                        sd.2 <- 2
                    }
                    else
                    {
                        # Median is not specified:
                        #   Seek the mean value to align and standard deviations.
                        pseudo <- ms.norm.xp( qt$x[c( 1, 3 )], qt$p[c( 1, 3 )] )
                        x.0 <- c( pseudo$mean, sqrt( pseudo$sd ), sqrt( pseudo$sd ) )

                        mean.1 <- mean.2 <- 1
                        sd.1 <- 2
                        sd.2 <- 3
                    }

                    if ( grad != "v2" )
                    {
                        retry.msg <- paste( "Message: Tracing with 2 components has failed.",
                                            "Tracing with 3 components has been retried." )
                    }
                }
                else
                {
                    # Process as 4 quantiles.
                    if ( qt.num == 3 )
                    {
                        qt <- data.frame( x = qt.with.median$x[c( 1, 2, 2, 3 )],
                                          p = qt.with.median$p[c( 1, 2, 2, 3 )] )

                        # allowSingular = TRUE due to insufficient conditions.
                        if ( !is.list( control ) || is.null( control$allowSingular ) )
                        {
                            control <- append( control, list( allowSingular = TRUE ) )
                        }
                    }
                    else
                    {
                        qt <- qt.with.median
                    }

                    retry.msg <- "Message: Tracing with 2 components has failed."
                    l <- try( v2.qt4.cmp( qt, control, ( retry < max.retry || qt.num == 3 ) ),
                              silent = TRUE )
                    if ( inherits( l, "try-error" ) && qt.num == 3 )
                    {
                        if ( length( grep( "Failed to construct components",
                                           as.character( attr( l, "condition" ) ) ) ) == 0 )
                        {
                            # Critical or programming error.
                            clear()
                            stop( l )
                        }

                        # If failed, where 3 quantiles, it tries crossover-tracing.
                        message( paste( "Message: Tracing with allowSingular option",
                                        "has failed.",
                                        "Crossover-tracing has been retried." ) )
                        retry.msg <- "Message: Crossover-tracing has failed."
                        l <- try( v2.crossover( qt.with.median, control ),
                                  silent = TRUE )
                    }

                    if ( inherits( l, "try-error" ) )
                    {
                        if ( grad == "v2" )
                        {
                            if ( qt.num == 3 && isFALSE( eq.mean ) )
                            {
                                message( "Crossover-tracing has failed." )
                                clear()
                                stop( l )
                            }
                            else if ( qt.num == 4 )
                            {
                                clear()
                                stop( l )
                            }

                            # Retry with eq.mean = TRUE.
                            message( paste( retry.msg,
                                            "Tracing with mean-equaled components",
                                            "has been retried." ) )
                            retry.msg <- NULL
                            eq.mean <- TRUE
                            next
                        }
                        else
                        {
                            # Retry with 3 mean-differed componetnts.
                            #
                            # Note: Twice retrying with eq.mean = TRUE
                            #       (with 2 components and 3 components) will be done
                            #       after failing mean-differed-3-component-tracing.
                            message( paste( retry.msg,
                                            "Tracing with 3 components has been retried." ) )
                            retry.msg <- NULL
                            grad <- "v3"
                            next
                        }
                    }
                    else
                    {
                        set.cmp( l$cmp, this.mix.type = new.mix.type, grad = grad )
                        result <- l$result
                        break
                    }
                }

                # Normal tracing function for "v2"
                f <- function( x )
                {
                    x.mean.1 <- ifelse( mean.1 == 0, median, x[mean.1] )
                    x.mean.2 <- ifelse( mean.2 == 0, median, x[mean.2] )

                    p <- pnorm( qt$x, x.mean.1, x[sd.1]^2 )
                    p.a1 <- pnorm( qt$x, x.mean.1,  x[sd.1]^2 * sqrt( 2 ) / 2 )
                    p.a2 <- pnorm( qt$x, x.mean.2, x[sd.2]^2 * sqrt( 2 ) / 2 )

                    return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - qt$p )
                }

                get.cmp.result <- function( result )
                {
                    ggd.get.t3.cmp( ifelse( c( mean.1, mean.2, mean.1 ) == 0, rep( median, 3 ),
                                            c( result$x[mean.1],
                                               result$x[mean.2],
                                               result$x[mean.1] ) ),
                                    c( result$x[sd.1]^2, result$x[sd.2]^2, result$x[sd.1]^2 ),
                                    grad = grad )
                }
            }
            else if ( new.mix.type == 3 )
            {
                ########################################
                # new.mix.type = 3, 3 components
                if ( qt.num <= 4 && isTRUE( eq.mean ) &&
                     is.set.median && retry < max.retry )
                {
                    # grad = "v3" and median is specified:
                    #   Mean values will be aligned to median.
                    #   To seek standard deviations,
                    #   the cases are divided according to the number of quantiles
                    #   with p < 0.5 (#lower) and p > 0.5 (#upper).

                    num.lower <- length( qt$p[qt$p < 0.5] )
                    num.upper <- length( qt$p[qt$p > 0.5] )

                    if ( num.lower >= 3 || num.upper >= 3 )
                    {
                        # If there are more than 3 quantiles in one side,
                        # process as same as 4 quantiles case. -> next loop
                        next
                    }
                    else
                    {
                        # Stepwise tracing without allowSingular option
                        #
                        # Dividing cases according to #lower and #upper.
                        if ( num.lower == 1 && num.upper == 1 )
                        {
                            # ( #lower, #upper ) = ( 1, 1 )
                            # The SD of the top-side is set as
                            # the mean of the SD of the left-tail-side and right-tail-side.
                            x.0 <- c( 1, 1 )

                            f <- function( x )
                            {
                                x.ave <- ( x[1]^2 + x[2]^2 ) / 2

                                p <- pnorm( qt$x, median, x^2 )
                                p.a1 <- pnorm( qt$x, median, x^2 * sqrt( 2 ) / 2 )
                                p.a2 <- pnorm( qt$x, median, x.ave * sqrt( 2 ) / 2 )

                                return ( p - p.a1 * sqrt( 2 ) / 2 +
                                             p.a2 * sqrt( 2 ) / 2 - qt$p )
                            }

                            get.cmp.result <- function( result )
                            {
                                sds <- c( result$x[1]^2,
                                          ( result$x[1]^2 + result$x[2]^2 ) / 2,
                                          result$x[2]^2 )
                                ggd.get.t3.cmp( rep( median, 3 ), sds, grad )
                            }
                        }
                        else
                        {
                            # ( #lower, #upper ) = ( 2, 0 ), ( 0, 2 ), ( 2, 1 ) or ( 1, 2 )

                            # Get the SDs of the normal distributions
                            # through both quantles on the side with 2 quantiles.
                            #
                            # Here, we use 2 variables for indexes of qt,
                            # and 2 variables for indexes of vector x of
                            # the objective function f(x).
                            #
                            # for qt (nrow:3),
                            # qt.lower: the index of on the side with the smaller x-coordinate,
                            # qt.upper: the index of with the larger x-coordinate.
                            #
                            # for x of f(x) (length:2),
                            # xi.outer: the index of the farther quantile from the median,
                            # xi.inner: the index of the closer quantile to the median.

                            # ( #lower == 2 and ( #upper == 0 or 1 (i.e. any) ) ) ||
                            # ( #lower == 0 and #upper == 2 ) => qt.lower = 1
                            qt.lower <- ifelse( ( num.lower == 2 || num.lower == 0 ), 1, 2 )
                            qt.upper <- ifelse( ( num.lower == 2 || num.lower == 0 ), 2, 3 )
                            qt.x <- c( qt$x[qt.lower], qt$x[qt.upper] )
                            qt.p <- c( qt$p[qt.lower], qt$p[qt.upper] )
                            xi.outer <- ifelse( num.lower == 2, 1, 2 )
                            xi.inner <- ifelse( num.lower == 2, 2, 1 )

                            x.0 <- c( 1, 1 )

                            f <- function( x )
                            {
                                p <- pnorm( qt.x, median, x[xi.outer]^2 )
                                p.a1 <- pnorm( qt.x, median, x[xi.outer]^2 * sqrt( 2 ) / 2 )
                                p.a2 <- pnorm( qt.x, median, x[xi.inner]^2 * sqrt( 2 ) / 2 )

                                return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - qt.p )
                            }

                            get.cmp.result <- function( result )
                            {
                                sds <- numeric()

                                if ( num.upper == 0 || num.lower == 0 )
                                {
                                    # ( #lower, #upper ) = ( 2, 0 ) or ( 0, 2 )
                                    sds[1] <- result$x[1]^2
                                    sds[2] <- result$x[xi.inner]^2
                                    sds[3] <- result$x[2]^2
                                }
                                else
                                {
                                    # ( #lower, #upper ) = ( 2, 1 ) or ( 1, 2 )
                                    # i.last: index of qt for the quantile on the other side.

                                    # First, get 2 SDs of the normal distribuions which
                                    # trace each of 2 quantiles on the 2-quantiles side.
                                    if ( num.lower == 2 )
                                    {
                                        sds[1] <- result$x[xi.outer]^2
                                        sds[2] <- result$x[xi.inner]^2

                                        i.last <- 3
                                    }
                                    else
                                    {
                                        sds[2] <- result$x[xi.inner]^2
                                        sds[3] <- result$x[xi.outer]^2

                                        i.last <- 1
                                    }

                                    # Check whether the last SD can be found.
                                    #
                                    # If the remained probability after
                                    # \Phi^*_{i.last}(x[i.last])/sqrt(2)
                                    # is grater than 0 and less than (2 - sqrt(2))/4,
                                    # the last SD can be found.
                                    p.remain <- qt$p[i.last] -
                                                pnorm( qt$x[i.last], median,
                                                       sds[2] * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2
                                    if ( qt$p[i.last] < 0.5 )
                                    {
                                        if ( p.remain <= 0 )
                                        {
                                            # Cannot be found because p[i.last] is too low.
                                            # => If the x[i.last] were smaller,
                                            #    it might be possible to find it.
                                            message( paste(
                                                "Message:",
                                                "The probability of the lowest quantile",
                                                "is too small. Or, the lowest quantile",
                                                "may be too near from the median." ) )
                                            return ( NULL )
                                        }
                                        else if ( p.remain >= ( 0.5 - 0.25 * sqrt( 2 ) ) )
                                        {
                                            # Cannot be found because p[i.last] is too high
                                            # (too close to 0.5).
                                            # => If the x[i.last] were closer to the median,
                                            #    it might be possible to find it.
                                            message( paste(
                                                "Message:",
                                                "The probability of the lowest quantile",
                                                "is too near to 0.5. Or, the lowest quantile",
                                                "may be too far from the median." ) )
                                            return ( NULL )
                                        }
                                    }
                                    else
                                    {
                                        if ( p.remain <= ( 0.5 - 0.25 * sqrt( 2 ) ) )
                                        {
                                            # Cannot be found because p[i.last] is too low
                                            # (too close to 0.5).
                                            # => If the x[i.last] were closer to the median,
                                            #    it might be possible to find it.
                                            message( paste(
                                                "Message:",
                                                "The probability of the highest quantile",
                                                "is too near to 0.5. Or, the highest quantile",
                                                "may be too far from the median." ) )
                                            return ( NULL )
                                        }
                                        else if ( p.remain >= ( 1 - sqrt( 2 ) / 2 ) )
                                        {
                                            # Cannot be found because p[i.last] is too high.
                                            # => If the x[i.last] were grater,
                                            #    it might be possible to find it.
                                            message( paste(
                                                "Message:",
                                                "The probability of the highest quantile",
                                                "is too large. Or, the highest quantile",
                                                "may be too near from the median." ) )
                                            return ( NULL )
                                        }
                                    }

                                    if ( sds[2] == sd.norm.mxp( median, qt$x[i.last],
                                                                        qt$p[i.last] ) )
                                    {
                                        # The last SD equals to the SD of the top-side.
                                        sds[i.last] <- sds[2]
                                    }
                                    else
                                    {
                                        # Decide search range for the last SD.
                                        if ( qt$p[i.last] < 0.5 )
                                        {
                                            sd.sup <- sd.norm.mxp( median, qt$x[i.last],
                                                            ( 2 + sqrt( 2 ) ) * p.remain )
                                            sd.inf <- sd.norm.mxp( median, qt$x[i.last],
                                                            p.remain )
                                        }
                                        else
                                        {
                                            sd.sup <- sd.norm.mxp( median, qt$x[i.last],
                                                            ( 2 + sqrt( 2 ) ) * p.remain )
                                            sd.inf <- sd.norm.mxp( median, qt$x[i.last],
                                                            p.remain + sqrt( 2 ) / 2 )
                                        }

                                        # Search the last SD.
                                        sds[i.last] <- bisection(
                                        function( x )
                                        {
                                            pnorm( qt$x[i.last], median, x ) -
                                            pnorm( qt$x[i.last], median, x * sqrt( 2 ) / 2 ) *
                                            sqrt( 2 ) / 2 - p.remain
                                        }, c( sd.inf, sd.sup ) )
                                    }
                                }

                                return ( ggd.get.t3.cmp( rep( median, 3 ), sds, grad ) )
                            }
                        }
                        retry.msg <- paste( "Logical stepwise tracing has failed.",
                                            "Tracing with allowSingular option",
                                            "has been retried." )
                    }
                }
                else if ( qt.num <= 4 )
                {
                    # grad = "v3", process as 4 quantiles.
                    if ( qt.num == 3 )
                    {
                        qt <- data.frame( x = qt.with.median$x[c( 1, 2, 2, 3 )],
                                          p = qt.with.median$p[c( 1, 2, 2, 3 )] )

                        # allowSingular = TRUE if 3 quantiles.
                        if ( !is.list( control ) || is.null( control$allowSingular ) )
                        {
                            control <- append( control, list( allowSingular = TRUE ) )
                        }
                    }
                    else
                    {
                        qt <- qt.with.median
                    }

                    retry.msg <- paste( "Message: Tracing with mean-differed components",
                                        "is failed.",
                                        "Tracing with mean-equaled components",
                                        "has been retried." )
                    l <- try( v3.qt4.cmp( qt, isTRUE( eq.mean ), eq.sd, control,
                                          ( retry < max.retry ), grad ),
                              silent = TRUE )
                    if ( inherits( l, "try-error" ) )
                    {
                        if ( retry < max.retry )
                        {
                            message( retry.msg )
                            retry.msg <- NULL
                            eq.mean <- TRUE
                            control <- control.backup
                            if ( qt.num == 3 && !is.v3.ind )
                            {
                                grad <- "default"
                            }
                            next
                        }
                        else
                        {
                            clear()
                            stop( l )
                        }
                    }

                    set.cmp( l$cmp, this.mix.type = new.mix.type, grad = grad )
                    result <- l$result
                    break
                }
                else # if ( any( qt.num == 5:6 ) )
                {
                    # grad = "v3", process as 6 quantiles.

                    # Get start values for means and standard deviations
                    # from the quantiles other than the median.
                    # Remark that median is not in qt here.
                    if ( qt.num == 5 )
                    {
                        qt.near <- data.frame( x = qt$x[order( abs( qt$p - 0.5 ) )][1:2],
                                               p = qt$p[order( abs( qt$p - 0.5 ) )][1:2] )

                        pseudos.2 <- ms.norm.xp( qt.near$x, qt.near$p )
                    }
                    else
                    {
                        pseudos.2 <- ms.norm.xp( qt$x[3:4], qt$p[3:4] )
                    }

                    pseudos <- list( ms.norm.xp( qt$x[1:2], qt$p[1:2] ),
                                     pseudos.2,
                                     ms.norm.xp( qt$x[( nrow( qt ) - 1 ):nrow( qt )],
                                                 qt$p[( nrow( qt ) - 1 ):nrow( qt )] ) )

                    x.0 <- c( pseudos[[1]]$mean, pseudos[[2]]$mean, pseudos[[3]]$mean,
                              sqrt( c( pseudos[[1]]$sd, pseudos[[2]]$sd, pseudos[[3]]$sd ) ) )

                    # For tracing, add median to qt here if needed.
                    if ( qt.num == 5 )
                    {
                        qt <- data.frame( x = qt.with.median$x[c( 1, 2, 3, 3, 4, 5 )],
                                          p = qt.with.median$p[c( 1, 2, 3, 3, 4, 5 )] )

                        # allowSingular = TRUE if 5 quantiles.
                        if ( !is.list( control ) || is.null( control$allowSingular ) )
                        {
                            control <- append( control, list( allowSingular = TRUE ) )
                        }
                    }
                    else
                    {
                        qt <- qt.with.median
                    }

                    f <- function( x )
                    {
                        c( dp.t3( qt$x, x[1:3], x[4:6]^2, f.t3.p ) - qt$p )
                    }

                    get.cmp.result <- function( result )
                    {
                        ggd.get.t3.cmp( result$x[1:3], result$x[4:6]^2, grad )
                    }
                }
            }
            else if ( new.mix.type == 4 && ( qt.num == 5 || qt.num == 6 ) &&
                      ( isTRUE( eq.mean ) || eq.sd || retry == 0 ) )
            {
                # new.mix.type = 4 with 5 or 6 quantiles.
                # Note, there can be a case where eq.mean == TRUE and retry > 0.

                # Tracing with a horizontal gradational distribution of
                # 2 mean- or sigma-equaled vertical gradational distributions.

                # Get start values for means and standard deviations
                # of the left-side and right-side distributions
                # via ggd.trace.q with new.mix.type = 3.
                # Here, we don't give "control" to ggd.trace.q intentionally.

                # Substring of the failed message for initial guessing
                msg.sub <- "mean-differed"
                if ( eq.sd )
                {
                    msg.sub <- "sigma-equaled"
                }
                else if ( isTRUE( eq.mean ) )
                {
                    msg.sub <- "mean-equaled"
                }

                # As the start values for means,
                # if the median is specified, it is taken;
                # if not, the x-coordinate of the 3rd quantile counting from the lowest
                # probability is taken.
                if ( any( qt.with.median$p[1:3] == 0.5 ) )
                {
                    qt.d.1 <- qt.with.median[1:3,]
                }
                else
                {
                    d.1.p <- c( qt.with.median$p[1:2], 0.5 )
                    qt.d.1 <- data.frame( x = qt.with.median$x[order( d.1.p )],
                                          p = d.1.p[order( d.1.p )] )
                }

                # Suppress Messages for getting start values
                # to avoid confusing with messages for main nleqslv.
                d.1 <- suppressMessages(
                            try( ggd.trace.q( qt.d.1, mix.type = 3, grad = "v2",
                                              eq.mean = eq.mean, eq.sd = eq.sd )$obj,
                                 silent = TRUE ), "message" )
                if ( inherits( d.1, "try-error" ) )
                {
                    message( paste( "Message: Initial guessing with 3-quantile-tracing",
                                             "for left-side", msg.sub, "components has failed.",
                                             "2-quantile-tracing has used instead." ) )
                    if ( isTRUE( eq.mean ) && is.set.median )
                    {
                        pseudos <- list( ms.norm.xp( c( qt.with.median$x[1], median ),
                                                     c( qt.with.median$p[1], 0.5 ) ),
                                         ms.norm.xp( c( qt.with.median$x[2], median ),
                                                     c( qt.with.median$p[2], 0.5 ) ) )
                    }
                    else
                    {
                        pseudos <- list( ms.norm.xp( c( qt.with.median$x[c( 1, 2 )] ),
                                                     c( qt.with.median$p[c( 1, 2 )] ) ),
                                         ms.norm.xp( c( qt.with.median$x[c( 2, 3 )] ),
                                                     c( qt.with.median$p[c( 2, 3 )] ) ) )
                    }
                    d.1 <- GGD$new()
                    d.1$set.cmp(
                            ggd.get.t3.cmp(
                                c( pseudos[[1]]$mean, pseudos[[2]]$mean, pseudos[[1]]$mean ),
                                c( pseudos[[1]]$sd, pseudos[[2]]$sd, pseudos[[1]]$sd ),
                                grad = "default" ), grad = "v2" )
                }

                # As the start values for means, if the median is specified, it is taken;
                # if not, the x-coordinate of the 3rd quantile counting from the highest
                # probability is taken.
                if ( any( qt.with.median$p[nrow( qt.with.median ) + ( -2:0 )] == 0.5 ) )
                {
                    qt.d.2 <- qt.with.median[nrow( qt.with.median ) + ( -2:0 ),]
                }
                else
                {
                    d.2.p <- c( 0.5, qt.with.median$p[nrow( qt.with.median ) + ( -1:0 )] )
                    qt.d.2 <- data.frame( x = qt.with.median$x[order( d.2.p ) +
                                                               nrow( qt.with.median ) - 3],
                                          p = d.2.p[order( d.2.p )] )
                }

                # Suppress Messages for getting start values
                # to avoid confusing with messages for main nleqslv.
                d.2 <- suppressMessages(
                            try( ggd.trace.q( qt.d.2, mix.type = 3, grad = "v2",
                                              eq.mean = eq.mean, eq.sd = eq.sd )$obj,
                                 silent = TRUE ), "message" )
                if ( inherits( d.2, "try-error" ) )
                {
                    message( paste( "Message: Initial guessing with 3-quantile-tracing",
                                             "for right-side", msg.sub, "components has failed.",
                                             "2-quantile-tracing has used instead." ) )
                    rows <- nrow( qt.with.median )
                    if ( isTRUE( eq.mean ) && is.set.median )
                    {
                        pseudos <- list( ms.norm.xp( c( qt.with.median$x[rows], median ),
                                                     c( qt.with.median$p[rows], 0.5 ) ),
                                         ms.norm.xp( c( qt.with.median$x[rows - 1], median ),
                                                     c( qt.with.median$p[rows - 1], 0.5 ) ) )
                    }
                    else
                    {
                        pseudos <- list( ms.norm.xp(
                                            c( qt.with.median$x[c( rows, rows - 1 )] ),
                                            c( qt.with.median$p[c( rows, rows - 1 )] ) ),
                                         ms.norm.xp(
                                            c( qt.with.median$x[c( rows - 1, rows - 2 )] ),
                                            c( qt.with.median$p[c( rows - 1, rows - 2 )] ) ) )
                    }
                    d.2 <- GGD$new()
                    d.2$set.cmp(
                            ggd.get.t3.cmp(
                                c( pseudos[[1]]$mean, pseudos[[2]]$mean, pseudos[[1]]$mean ),
                                c( pseudos[[1]]$sd, pseudos[[2]]$sd, pseudos[[1]]$sd ),
                                grad = "default" ), grad = "v2" )
                }

                # Make main functions for the tracing
                if ( eq.sd )
                {
                    qt <- qt.with.median

                    # length( x.0 ) == 5
                    x.0 <- c( d.1$cmp$mean[1], d.1$cmp$mean[2],
                              d.2$cmp$mean[1], d.2$cmp$mean[2],
                              sqrt( ( d.1$cmp$sd[2] + d.2$cmp$sd[2] ) / 2 ) )

                    mean.1 <- 1
                    mean.2 <- 2
                    mean.3 <- 3
                    mean.4 <- 4
                    sd.1 <- sd.2 <- sd.3 <- sd.4 <- 5
                }
                else if ( isTRUE( eq.mean ) )
                {
                    if ( is.set.median )
                    {
                        # length( x.0 ) == 4 (all mean values are equal to the median)
                        x.0 <- sqrt( c( d.1$cmp$sd[1], d.1$cmp$sd[2],
                                        d.2$cmp$sd[1], d.2$cmp$sd[2] ) )

                        mean.1 <- mean.2 <- mean.3 <- mean.4 <- 0
                        sd.1 <- 1
                        sd.2 <- 2
                        sd.3 <- 3
                        sd.4 <- 4
                    }
                    else
                    {
                        # length( x.0 ) == 5
                        x.0 <- c( ( d.1$median + d.2$median ) / 2,
                                  sqrt( c( d.1$cmp$sd[1], d.1$cmp$sd[2],
                                           d.2$cmp$sd[1], d.2$cmp$sd[2] ) ) )

                        mean.1 <- mean.2 <- mean.3 <- mean.4 <- 1
                        sd.1 <- 2
                        sd.2 <- 3
                        sd.3 <- 4
                        sd.4 <- 5
                    }
                }
                else
                {
                    # Different means: treat as 6 quantiles
                    if ( qt.num == 5 )
                    {
                        qt <- data.frame( x = qt.with.median$x[c( 1, 2, 3, 3, 4, 5 )],
                                          p = qt.with.median$p[c( 1, 2, 3, 3, 4, 5 )] )

                        # allowSingular = TRUE if 5 quantiles.
                        if ( !is.list( control ) || is.null( control$allowSingular ) )
                        {
                            control <- append( control, list( allowSingular = TRUE ) )
                        }
                    }
                    else
                    {
                        qt <- qt.with.median
                    }

                    # length( x.0 ) == 6
                    x.0 <- c( d.1$mean, d.2$mean,
                              sqrt( c( d.1$cmp$sd[1], d.1$cmp$sd[2],
                                       d.2$cmp$sd[1], d.2$cmp$sd[2] ) ) )

                    mean.1 <- mean.2 <- 1
                    mean.3 <- mean.4 <- 2
                    sd.1 <- 3
                    sd.2 <- 4
                    sd.3 <- 5
                    sd.4 <- 6

                    retry.msg <- paste(
                                    "Message: Tracing with 6 degrees of freedom has failed.",
                                    "Tracing with 8 degrees of freedom has been retried." )
                }

                f <- function( x )
                {
                    means <- ifelse( rep( mean.1 == 0, 4 ), rep( median, 4 ),
                                     x[c( mean.1, mean.2, mean.3, mean.4 )] )
                    sds <- x[c( sd.1, sd.2, sd.3, sd.4 )]^2

                    p.1 <- f.t3.p[[1]]( qt$x, means[1], sds[1] ) +
                           f.t3.p[[2]]( qt$x, means[2], sds[2] )
                    p.2 <- f.t3.p[[1]]( qt$x, means[3], sds[3] ) +
                           f.t3.p[[2]]( qt$x, means[4], sds[4] )

                    return ( p.1 - p.1^2 / 2 + p.2^2 / 2 - qt$p )
                }

                get.cmp.result <- function( result )
                {
                    means <- ifelse( rep( mean.1 == 0, 4 ), rep( median, 4 ),
                                     result$x[c( mean.1, mean.2, mean.3, mean.4 )] )
                    sds <- result$x[c( sd.1, sd.2, sd.3, sd.4 )]^2

                    return ( data.frame( mean = means, sd = sds ) )
                }
            }
            else if ( new.mix.type == 4 )
            {
                # new.mix.type = 4.

                # Tracing with 4 mean-differed sigma-differed components.

                # Get start values for means and standard deviations.

                # Where 7 or 8 quantiles, to get initial guesses,
                # take 2 ggd.trace.q processes with grad = "v2".
                #
                # In these v2-ggd.trace.q processes,
                # it tends to reduce the success rate to give 4 quantiles for the conditions,
                # so we give 3 quantiles here.
                # If failed, the normal distributions through 2 of the given quantiles
                # are taken for initial guesses.

                qt.x <- c( qt.with.median$x[1:3],
                           qt.with.median$x[ceiling( qt.num / 2 )],
                           qt.with.median$x[ceiling( ( qt.num + 1 ) / 2 )],
                           qt.with.median$x[( qt.num - 2 ):qt.num] )

                qt.p <- c( qt.with.median$p[1:3],
                           qt.with.median$p[ceiling( qt.num / 2 )],
                           qt.with.median$p[ceiling( ( qt.num + 1 ) / 2 )],
                           qt.with.median$p[( qt.num - 2 ):qt.num] )

                # Suppress Messages for getting start values
                # to avoid confusing with messages for main nleqslv.
                d.1 <- suppressMessages(
                            try( ggd.trace.q( data.frame( x = qt.x[c( 1, 2, 4 )],
                                                          p = qt.p[c( 1, 2, 4 )] ),
                                              mix.type = 3, grad = "v2" )$obj, silent = TRUE ),
                            "message" )
                if ( inherits( d.1, "try-error" ) )
                {
                    message( paste( "Message: Initial guessing with 3-quantile-tracing",
                                    "for left-side components has failed.",
                                    "2-quantile-tracing has used instead." ) )
                    pseudos <- list( ms.norm.xp( qt.x[c( 1, 3 )], qt.p[c( 1, 3 )] ),
                                     ms.norm.xp( qt.x[c( 2, 4 )], qt.p[c( 2, 4 )] ) )

                    d.1 <- GGD$new()
                    d.1$set.cmp(
                            ggd.get.t3.cmp(
                                c( pseudos[[1]]$mean, pseudos[[2]]$mean, pseudos[[1]]$mean ),
                                c( pseudos[[1]]$sd, pseudos[[2]]$sd, pseudos[[1]]$sd ),
                                grad = "default" ),
                            grad = "v2" )
                }

                # Suppress Messages for getting start values
                # to avoid confusing with messages for main nleqslv.
                d.2 <- suppressMessages(
                            try( ggd.trace.q( data.frame( x = qt.x[c( 5, 7, 8 )],
                                                          p = qt.p[c( 5, 7, 8 )] ),
                                              mix.type = 3, grad = "v2" )$obj, silent = TRUE ),
                            "message" )
                if ( inherits( d.2, "try-error" ) )
                {
                    message( paste( "Message: Initial guessing with 3-quantile-tracing",
                                    "for right-side components has failed.",
                                    "2-quantile-tracing has used instead." ) )
                    pseudos <- list( ms.norm.xp( qt.x[c( 6, 8 )], qt.p[c( 6, 8 )] ),
                                     ms.norm.xp( qt.x[c( 5, 7 )], qt.p[c( 5, 7 )] ) )

                    d.2 <- GGD$new()
                    d.2$set.cmp(
                            ggd.get.t3.cmp(
                                c( pseudos[[1]]$mean, pseudos[[2]]$mean, pseudos[[1]]$mean ),
                                c( pseudos[[1]]$sd, pseudos[[2]]$sd, pseudos[[1]]$sd ),
                                grad = "default" ),
                            grad = "v2" )
                }

                # allowSingular = TRUE if less than 8 quantiles.
                if ( qt.num < 8 && ( !is.list( control ) || is.null( control$allowSingular ) ) )
                {
                    control <- append( control, list( allowSingular = TRUE ) )
                }

                x.0 <- c( d.1$cmp$mean[1], sqrt( d.1$cmp$sd[1] ),
                          d.1$cmp$mean[2], sqrt( d.1$cmp$sd[2] ),
                          d.2$cmp$mean[1], sqrt( d.2$cmp$sd[1] ),
                          d.2$cmp$mean[2], sqrt( d.2$cmp$sd[2] ) )

                f <- function( x )
                {
                    sds <- x[seq( 2, 8, 2 )]^2
                    p.1 <- f.t3.p[[1]]( qt.x, x[1], sds[1] ) + f.t3.p[[2]]( qt.x, x[3], sds[2] )
                    p.2 <- f.t3.p[[1]]( qt.x, x[5], sds[3] ) + f.t3.p[[2]]( qt.x, x[7], sds[4] )

                    return ( p.1 - p.1^2 / 2 + p.2^2 / 2 - qt.p )
                }

                get.cmp.result <- function( result )
                {
                    data.frame( mean = result$x[c( 1, 3, 5, 7 )],
                                sd = result$x[c( 2, 4, 6, 8 )]^2 )
                }
            }

            # Call nleqslv and get the result.
            if ( !is.null( x.0 ) )
            {
                cmp.result <- NULL
                result <- try( nleqslv( x.0, f, control = control ), silent = TRUE )
                if ( inherits( result, "try-error" ) )
                {
                    clear()
                    stop( paste( "Error: Critical error at nleqslv:\n",
                                         attr( result, "condition" ) ) )
                }
                else if ( result$termcd == 1 )
                {
                    cmp.result <- get.cmp.result( result )
                }

                if ( !is.null( cmp.result ) )
                {
                    # Succeeded: get out the loop.
                    set.cmp( cmp.result, this.mix.type = new.mix.type, grad = grad )
                    break
                }
                else
                {
                    if ( retry < max.retry )
                    {
                        if ( result$termcd != 1 )
                        {
                            message( paste( "nleqslv has once failed. Message:",
                                            result$message ) )
                        }

                        if ( is.null( retry.msg ) && length( eq.mean ) == 0 && !eq.sd &&
                              ( ( new.mix.type == 1 && qt.num == 3 ) ||
                                ( new.mix.type == 2 && qt.num == 3 ) ||
                                ( new.mix.type == 3 && qt.num == 3 ) ||
                                (      grad == "v3" && qt.num == 4 ) ||
                                ( new.mix.type == 4 && qt.num == 5 ) ) )
                        {
                            retry.msg <- paste( "Message: Tracing with",
                                                "mean-differed components is failed.",
                                                "Tracing with mean-equaled components",
                                                "has been retried." )
                            eq.mean <- TRUE
                        }

                        if ( length( grep( "with 3 components", retry.msg ) ) > 0 )
                        {
                            grad <- "v3"
                        }

                        if ( length( grep( "allowSingular", retry.msg ) ) > 0 &&
                             ( !is.list( control ) || is.null( control$allowSingular ) ) )
                        {
                            control <- append( control, list( allowSingular = TRUE ) )
                        }
                        else
                        {
                            control <- control.backup
                        }

                        message( retry.msg )
                        retry.msg <- NULL
                    }
                    else
                    {
                        message( paste( "nleqslv has failed. Message:", result$message ) )
                        clear()
                        stop( "Error: Failed to construct components." )
                    }
                }
            }
        }

        return ( invisible( list( obj = .self, nleqslv.out = result ) ) )
    }
)

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

################################################################################################
#' [Non-exported] PDF/CDF for mix.type = 3
#'
#' Calculates the values of the probability density function or
#' the cumulative distribution function of the GGD model with \code{mix.type = 3}.
#' Both \code{means} and \code{sds} vectors need 3 elements for this function;
#' so where with two components, you must set \code{means[3]} and \code{sds[3]}
#' the same values of \code{means[1]} and \code{sds[1]}.
#' @param   x           A vector of x-coordinates.
#' @param   means       The vector of mean values of the 3 components.
#' @param   sds         The vector of sd values of the 3 components.
#' @param   f.t3        A function handle,
#'                      \code{ggd:::f.t3.d} for PDF or \code{ggd:::f.t3.p} for CDF.
#' @return  The vector of values of the probability density function
#'          or the cumulative distribution function.
################################################################################################
dp.t3 <- function( x, means, sds, f.t3 )
{
    results <- vapply( x, function( x )
    {
        result <- f.t3[[2]]( x, means[2], sds[2] )

        if ( x < means[1] )
        {
            result <- result + f.t3[[1]]( x, means[1], sds[1] )
        }
        else
        {
            result <- result + f.t3[[3]]
        }

        if ( x > means[3] )
        {
            result <- result + f.t3[[1]]( x, means[3], sds[3] ) - f.t3[[3]]
        }

        return ( result )
    }, 0 )

    return ( results )
}

################################################################################################
#' [Non-exported] Crossover-tracing (3 quantiles, vertical gradation of 2 normal distributions)
#'
#' Gets the data frame for \code{cmp} field where \code{mix.type = 3} and tracing 3 quantiles
#' with mean-differed 2 normal distribuions of the components by crossover-tracing.
#' @param   qt          A data frame; the quantiles to be traced,
#'                      If the median is specified, median must be included.
#' @param   control     The \code{control} option for \code{\link[nleqslv]{nleqslv}}.
#' @return  A list containing components
#'          \item{cmp}{
#'                  The data frame for \code{cmp} field.}
#'          \item{result}{
#'                  The output of \code{\link[nleqslv]{nleqslv}}.}
#' @details
#'  \subsection{Crossover-tracing}{
#'      Let cumulative distribution functions of 2 mean-differed normal distributions
#'      crossover at one of quantiles (except for median),
#'      and seek the mean values and standard deviations as tracing the other 2 quantiles.
#'  }
#' @importFrom  stats       pnorm
#' @importFrom  nleqslv     nleqslv
################################################################################################
v2.crossover <- function( qt, control )
{
    if ( nrow( qt ) != 3 )
    {
        stop( paste( "Error: nrow(qt) must be 3 for v2.crossover. nrow:",
                             nrow( qt ) ) )
    }

    successed <- FALSE
    cmp <- result <- NULL

    # First, make crossover at the middle (2nd) quantile.
    # If failed, then make crossover at other quantiles.
    #
    # This loop process must run serial, not parallel.
    for ( i in c( 2, 3, 1 ) )
    {
        if ( qt$p[i] == 0.5 )
        {
            next
        }

        p.i <- c( 1:3 )[-i]
        means <- c( ms.norm.xp( qt$x[c( p.i[1], i )],
                                qt$p[c( p.i[1], i )] )$mean,
                    ms.norm.xp( qt$x[c( i, p.i[2] )],
                                qt$p[c( i, p.i[2] )] )$mean )

        result <- try( nleqslv( means,
                                f <- function( x )
                                {
                                    x <- ifelse( sd.norm.mxp( x, qt$x[i], qt$p[i] ) < 0,
                                                 2 * qt$x[i] - x, x )
                                    sds <- sd.norm.mxp( x, qt$x[i], qt$p[i] )

                                    p    <- pnorm( qt$x[p.i], x[1], sds[1] )
                                    p.a1 <- pnorm( qt$x[p.i], x[1],
                                                   sds[1] * sqrt( 2 ) / 2 )
                                    p.a2 <- pnorm( qt$x[p.i], x[2],
                                                   sds[2] * sqrt( 2 ) / 2 )

                                    return ( p - p.a1 * sqrt( 2 ) / 2 +
                                                 p.a2 * sqrt( 2 ) / 2 - qt$p[p.i] )
                                }, control = control ), silent = TRUE )
        if ( inherits( result, "try-error" ) )
        {
            stop( paste( "Error: Critical error at nleqslv:\n",
                                 attr( result, "condition" ) ) )
        }
        else if ( result$termcd == 1 )
        {
            means <- ifelse( sd.norm.mxp( result$x, qt$x[i], qt$p[i] ) < 0,
                             2 * qt$x[i] - result$x, result$x )
            sds <- sd.norm.mxp( means, qt$x[i], qt$p[i] )

            cmp <- ggd.get.t3.cmp( c( means[1], means[2], means[1] ),
                                   c( sds[1], sds[2], sds[1] ), "v2" )
            successed <- TRUE
            break
        }

        message( paste( "nleqslv has once failed. Message:",
                        result$message ) )
        message( paste0( "Message: Tracing with crossing over #", i,
                        " quantile has failed.",
                        " The result may distort heavily." ) )
    }

    if ( !successed )
    {
        stop( "Error: Failed to construct components." )
    }

    return ( list( cmp = cmp, result = result ) )
}

################################################################################################
#' [Non-exported] Gets cmp field (4 quantiles, vertical gradation of 2 normal distributions)
#'
#' Gets the data frame for \code{cmp} field where \code{mix.type = 3} and tracing 4 quantiles
#' with 2 normal distribuions of the components.
#' @param   qt          A data frame; the quantiles to be traced.
#' @param   control     The \code{control} option for \code{\link[nleqslv]{nleqslv}}.
#' @param   retriable   A logical; the flag if retrying is enable.
#' @return  A list containing components
#'          \item{cmp}{
#'                  The data frame for \code{cmp} field.}
#'          \item{result}{
#'                  The output of \code{\link[nleqslv]{nleqslv}}.}
#' @importFrom  stats       pnorm
#' @importFrom  nleqslv     nleqslv
################################################################################################
v2.qt4.cmp <- function( qt, control, retriable )
{
    if ( nrow( qt ) != 4 )
    {
        stop( paste( "Error: nrow(qt) must be 4 for v2.qt4.cmp. nrow:",
                             nrow( qt ) ) )
    }

    # Get start values of nleqslv
    #
    # Sort the quantiles in order of closeness to the median,
    # then calculate mean values and standard deviations of the components
    # through the 2 quantiles farther/closer to the median, respectively.

    qt <- data.frame( x = qt$x[order( abs( qt$p - 0.5 ) )],
                      p = qt$p[order( abs( qt$p - 0.5 ) )] )
    if ( qt$p[1] == qt$p[2] )
    {
        # If there are 2 same quantiles of the nearest to the median in qt,
        # we use normal distributions through quantiles of the nearest and the farthest
        # and through the quantiles of the nearest and the mid-distance.
        #
        # This can be the case when 3 quantiles are specified.
        pseudo.far  <- ms.norm.xp( qt$x[c( 2, 4 )], qt$p[c( 2, 4 )] )
        pseudo.near <- ms.norm.xp( qt$x[c( 1, 3 )], qt$p[c( 1, 3 )] )
    }
    else
    {
        pseudo.far  <- ms.norm.xp( qt$x[3:4], qt$p[3:4] )
        pseudo.near <- ms.norm.xp( qt$x[1:2], qt$p[1:2] )
    }

    # Define the start values and the objective function for nleqslv.
    x.0 <- c( pseudo.far$mean, pseudo.near$mean,
              sqrt( pseudo.far$sd ), sqrt( pseudo.near$sd ) )

    f <- function( x )
    {
        p <- pnorm( qt$x, x[1], x[3]^2 )
        p.a1 <- pnorm( qt$x, x[1], x[3]^2 * sqrt( 2 ) / 2 )
        p.a2 <- pnorm( qt$x, x[2], x[4]^2 * sqrt( 2 ) / 2 )

        return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - qt$p )
    }

    # Call nleqslv.
    result <- try( nleqslv( x.0, f, control = control ), silent = TRUE )
    if ( inherits( result, "try-error" ) )
    {
        stop( paste( "Error: Critical error at nleqslv:\n",
                             attr( result, "condition" ) ) )
    }
    else if ( result$termcd == 1 )
    {
        means <- c( result$x[1], result$x[2], result$x[1] )
        sds <- c( result$x[3]^2, result$x[4]^2, result$x[3]^2 )
    }
    else
    {
        if ( retriable )
        {
            message( paste( "nleqslv has once failed. Message:", result$message ) )
        }
        else
        {
            message( paste( "nleqslv has failed. Message:", result$message ) )
        }
        stop( "Error: Failed to construct components." )
    }

    return ( list( cmp = ggd.get.t3.cmp( means, sds, grad = "default" ), result = result ) )
}

################################################################################################
#' [Non-exported] Gets cmp field (4 quantiles, vertical gradation of 3 normal distributions)
#'
#' Gets the data frame for \code{cmp} field where \code{mix.type = 3} and tracing 4 quantiles
#' with 3 normal distribuions of the components.
#' @param   qt          A data frame; the quantiles to be traced.
#' @param   eq.mean     A logical; the flag to be equal all of the mean values.
#' @param   eq.sd       A logical; the flag to be equal all of the standard deviations.
#' @param   grad        A character string indicating the method of gradation.
#'                      If "v3", constructing with 3 components is enforcedly,
#'                      even if it is possible to construct with 2 components.
#' @param   control     The \code{control} option for \code{\link[nleqslv]{nleqslv}}.
#' @param   retriable   A logical; the flag if retrying is enable.
#' @return  A list containing components
#'          \item{cmp}{
#'                  The data frame for \code{cmp} field.}
#'          \item{result}{
#'                  The output of \code{\link[nleqslv]{nleqslv}}.}
#' @importFrom  nleqslv     nleqslv
################################################################################################
v3.qt4.cmp <- function( qt, eq.mean, eq.sd, control, retriable, grad )
{
    if ( nrow( qt ) != 4 )
    {
        stop( paste( "Error: nrow(qt) must be 4 for v3.qt4.cmp. nrow:", nrow( qt ) ) )
    }

    # Get start values of nleqslv.
    #
    # Sort the quantiles in order of closeness to the median,
    # then calculate mean values and standard deviations of the normal distributions
    # through the 2 quantiles farther/closer to the median, respectively.

    qt <- data.frame( x = qt$x[order( abs( qt$p - 0.5 ) )],
                      p = qt$p[order( abs( qt$p - 0.5 ) )] )
    if ( qt$p[1] == qt$p[2] )
    {
        # If there are 2 same quantiles of the nearest to the median in qt,
        # we use normal distributions through quantiles of the nearest and the farthest
        # and through the quantiles of the nearest and the mid-distance.
        #
        # This can be the case when 3 quantiles are specified.
        pseudos <- list( ms.norm.xp( qt$x[c( 1, 3 )], qt$p[c( 1, 3 )] ),
                         ms.norm.xp( qt$x[c( 2, 4 )], qt$p[c( 2, 4 )] ) )
    }
    else
    {
        pseudos <- list( ms.norm.xp( qt$x[1:2], qt$p[1:2] ),
                         ms.norm.xp( qt$x[3:4], qt$p[3:4] ) )
    }

    pseudos.2 <- list( mean = ( pseudos[[1]]$mean + pseudos[[2]]$mean ) / 2,
                         sd = ( pseudos[[1]]$sd + pseudos[[2]]$sd ) / 2 )

    # Tracing.
    if ( eq.mean )
    {
        x.0 <- c( pseudos.2$mean, sqrt( pseudos[[1]]$sd ),
                                  sqrt( pseudos.2$sd ),
                                  sqrt( pseudos[[2]]$sd ) )

        mean.1 <- mean.2 <- mean.3 <- 1
        sd.1 <- 2
        sd.2 <- 3
        sd.3 <- 4
    }
    else if ( eq.sd )
    {
        x.0 <- c( pseudos[[1]]$mean,
                  pseudos.2$mean,
                  pseudos[[2]]$mean, sqrt( pseudos.2$sd ) )

        mean.1 <- 1
        mean.2 <- 2
        mean.3 <- 3
        sd.1 <- sd.2 <- sd.3 <- 4
    }
    else
    {
        # try as 6 quantiles with allowSingular = TRUE.
        qt <- data.frame( x = c( qt$x[1:2], qt$x ), p = c( qt$p[1:2], qt$p ) )

        x.0 <- c( pseudos[[1]]$mean,        pseudos.2$mean,         pseudos[[2]]$mean,
                  sqrt( pseudos[[1]]$sd ),  sqrt( pseudos.2$sd ),   sqrt( pseudos[[2]]$sd ) )

        mean.1 <- 1
        mean.2 <- 2
        mean.3 <- 3
        sd.1 <- 4
        sd.2 <- 5
        sd.3 <- 6

        if ( !is.list( control ) || is.null( control$allowSingular ) )
        {
            control <- append( control, list( allowSingular = TRUE ) )
        }
    }

    result <- try( nleqslv( x.0,
                            f <- function( x )
                            {
                                means <- c( x[mean.1], x[mean.2], x[mean.3] )
                                sds   <- c( x[sd.1],   x[sd.2],   x[sd.3] )^2
                                return ( dp.t3( qt$x, means, sds, f.t3.p ) - qt$p )
                            },
                            control = control ), silent = TRUE )
    if ( inherits( result, "try-error" ) )
    {
        stop( paste( "Error: Critical error at nleqslv:\n",
                             attr( result, "condition" ) ) )
    }
    else if ( result$termcd == 1 )
    {
        means <- c( result$x[mean.1], result$x[mean.2], result$x[mean.3] )
        sds   <- c( result$x[sd.1],   result$x[sd.2],   result$x[sd.3] )^2
    }
    else
    {
        if ( retriable )
        {
            message( paste( "nleqslv has once failed. Message:", result$message ) )
        }
        else
        {
            message( paste( "nleqslv has failed. Message:", result$message ) )
        }
        stop( "Error: Failed to construct components." )
    }

    return ( list( cmp = ggd.get.t3.cmp( means, sds, grad ), result = result ) )
}

################################################################################################
#' Get cmp field for mix.type = 3
#'
#' Gets a data frame for \code{cmp} field for \code{mix.type = 3}.
#' Each of \code{means} and \code{sds} arguments must has 3 elements in order of
#' [1] left-tail side, [2] top side, [3] right-tail side.
#' @export
#' @param   means       A vector of mean values of the 3 normal distributions of the components.
#' @param   sds         A vector of standard deviations of the 3 normal distributions of
#'                      the components.
#' @param   grad        A character string indicating the method of gradation.
#'                      If "v3", the number of components is forced to be 3.
#' @return  The data frame for \code{cmp} field.
#' @examples
#'  means <- c( 0.2, 0, 0.2 ); sds <- c( 1.2, 0.9, 1.2 )
#'  ggd.get.t3.cmp( means, sds )                    # 2 components
#'  ggd.get.t3.cmp( means, sds, grad = "v3" )       # 3 components
#'
#'  means <- rep( 0.5, 3 ); sds <- rep( 2.5, 3 )
#'  ggd.get.t3.cmp( means, sds )                    # 2 components even if normal distribution
################################################################################################
ggd.get.t3.cmp <- function( means, sds, grad = c( "default", "v2", "v3" ) )
{
    grad <- match.arg( grad )

    if ( grad != "v3" && means[1] == means[3] && sds[1] == sds[3] )
    {
        cmp <- data.frame( mean = means[1:2], sd = sds[1:2] )
    }
    else
    {
        cmp <- data.frame( mean = means[1:3], sd = sds[1:3] )
    }

    return ( cmp )
}

################################################################################################
#' Approximating a frequency distribution
#'
#' With the non-linear least squares (\code{\link[stats]{nls}}),
#' constructs a \code{\link[ggd]{GGD}} object which (locally) most closely approximates
#' the given frequency distribution. "Locally" means that if the start value is modified,
#' more closely approximating model may be generated.
#' The outliers of the frequency distribution will not be excluded in this function.
#' If necessary, outliers should be excluded by pre-processing.
#' @export
#' @name    nls.freq
#' @aliases ggd.nls.freq
#' @aliases nls.freq
#' @aliases \S4method{nls.freq}{GGD}
#' @usage   ggd.nls.freq(data, x = "x", freq = "freq", total = NULL,
#'          kind = NULL, mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"),
#'          eq.mean = logical(), eq.sd = logical(),
#'          start.level = 100, start = NULL, control = list(),
#'          not.use.nls = FALSE, method = NULL, ...)
#' @usage   \S4method{nls.freq}{GGD}(data, x = "x", freq = "freq", total = NULL,
#'          this.kind = NULL, this.mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"),
#'          eq.mean = logical(), eq.sd = logical(),
#'          start.level = 100, start = NULL, control = list(),
#'          not.use.nls = FALSE, method = NULL, ...)
#'
#' @param   data        A data frame which represents the frequency distribution.
#'                      It must contain at least 2 numeric columns for \code{x} and \code{freq}.
#'
#'                      Column \bold{\code{x}} is for the x-coordinates.
#'                      Each value expected to be a numeric value which represents the cell of
#'                      the frequency distribution, the x-coordinate at the center of a cell
#'                      of the frequency distribution.
#'                      The values must be arranged in ascending order, and not be duplicated.
#'
#'                      Column \bold{\code{freq}} is for the frequencies following \code{x}.
#'                      The values of frequencies must be positive.
#'                      Both integers and real numbers are allowed for the values.
#'
#'                      Rows which contain \code{NA} or \code{NaN} for \code{x} or \code{freq}
#'                      are ignored. The number of rows should be large enough;
#'                      it is recommended that there are more than 8 valid rows.
#'                      At least, 3 valid rows must be contained.
#'
#'                      Column names and column numbers for \code{x} and \code{freq}
#'                      are flexible. You can specify them with next two arguments.
#'
#' @param   x           The column name or column number for x-coordinates in \code{data}.
#'
#' @param   freq        The column name or column number for frequencies in \code{data}.
#'
#' @param   total       Total value of the frequencies.
#'
#'                      If \code{NULL} (the default),
#'                      the total of \code{freq}, i.e., \code{\link[base]{sum}(data[[freq]])}
#'                      (on \code{\link[stats]{complete.cases}} of \code{x} and \code{freq})
#'                      is used for it.
#'
#' @param   kind        A character string or a numeric value or a \code{\link[ggd]{GGD}} object
#'                      which indicates the kind of distribution model for approximating
#'                      the frequency distribution.
#'
#'                      The matching method of this argument follows that of elements of
#'                      the \code{objs} argument of the \link[ggd]{ggd.kind.index}.
#'
#'                      This argument gives the conditions of
#'                      the value of \code{mix.type} field,
#'                      and of whether the mean values and standard deviations of the components
#'                      should be aligned to the same value.
#'
#'                      Indicating \code{mix.type} argument or
#'                      indicating other than \code{"default"} for \code{grad} argument
#'                      or \code{TRUE}/\code{FALSE} for \code{eq.mean} or \code{eq.sd}
#'                      can overwrite the conditions of this argument.
#'
#' @param   this.kind   A string or a numeric value or a \code{\link[ggd]{GGD}} object
#'                      which indicates the kind of distribution model for approximating
#'                      the frequency distribution.
#'
#'                      This argument will work as same as \code{kind} argument
#'                      of the generator function (signature '\code{NULL}').
#'
#'                      When this method is called without \code{this.kind} argument
#'                      or other conditions, it attempt to retain the value of
#'                      \code{mix.type} field as much as possible, but not the value of
#'                      \code{kind} field, i.e., the condition whether the mean value or
#'                      standard deviation of each component is aligned may not be retained.
#'                      If you want to retain these conditions as well,
#'                      indicate the object itself to \code{this.kind} argument like as
#'                      \code{obj$nls.freq(data, this.kind = obj)}.
#'
#' @param   mix.type    A numeric value to set into \code{mix.type} field of
#'                      the \code{\link[ggd]{GGD}} object as an integer.
#'                      It should be an integer from \code{0} to \code{4} or \code{NULL}.
#'
#'                      The type of the distribution model will be as:
#'                      \itemize{
#'                          \item 0: Normal distribution.
#'                          \item 1: Mean of 2 normal distributions.
#'                          \item 2: Horizontal gradation of 2 normal distributions.
#'                          \item 3: Vertical gradation of 2 or 3 normal distributions.
#'                                   The 2-component model has priority.
#'                          \item 4: Horizontal-Vertical gradation
#'                                   with 4 (2x2) normal distributions.
#'                      }
#'
#'                      If other than \code{"default"} for \code{grad} argument is indicated,
#'                      this argument will be ignored.
#'
#' @param   this.mix.type   A numeric value to set into \code{mix.type} field as an integer.
#'                          This argument will work as same as \code{mix.type} of
#'                          the generator function (signature '\code{NULL}').
#'
#'                      If both of \code{this.kind} and \code{this.mix.type} are not given
#'                      and \code{grad} argument is \code{"default"},
#'                      the current value of \code{mix.type} field will be retained,
#'                      and number of components will also.
#'                      But furthermore, the object has been cleared,
#'                      the \code{mix.type} field will be set to \code{2}, the initial value.
#'
#' @param   grad        A character string indicating the method of gradation.
#'
#'                      \code{"h"} for horizontal, \code{"v"} for vertical,
#'                      and \code{"hv"} for horizontal-vertical.
#'                      The number after \code{"v"} is the number of components.
#'                      Numberless \code{"v"} is an alias for \code{"v2"}.
#'
#'                      \code{"normal"} is for a normal distribution.
#'                      \code{"default"} is for depending on values of other arguments.
#'
#'                      If other than \code{"default"} is indicated,
#'                      this function will create a \code{\link[ggd]{GGD}} object
#'                      according to this argument with ignoring \code{[this.]mix.type} argument
#'                      and overwriting the type indicated by \code{[this.]kind} argument.
#'
#' @param   eq.mean     A logical. If \code{TRUE}, all of the mean values of of the components
#'                      are forced to be equal.
#'
#'                      If \code{FALSE} or \code{logical(0)},
#'                      the mean values of the components can be dirrerent to each other,
#'                      and may be equal in very rare cases.
#'
#'                      \code{TRUE} and \code{FALSE} can overwrite the condition indicated by
#'                      \code{kind} or \code{this.kind} argument.
#'
#' @param   eq.sd       A logical. If \code{TRUE}, all of the standard deviations of
#'                      the components are forced to be equal.
#'
#'                      If \code{FALSE} or \code{logical(0)},
#'                      the standard deviation of the components can be dirrerent to
#'                      each other, and may be equal in very rare cases.
#'
#'                      If both \code{eq.mean} and \code{eq.sd} are \code{TRUE},
#'                      a normal distribution will be generated.
#'
#'                      \code{TRUE} and \code{FALSE} can overwrite the condition indicated by
#'                      \code{kind} or \code{this.kind} argument.
#'
#' @param   start.level A numeric value of integer from \code{0} to \code{3} and \code{100}
#'                      with default \code{100}; the level at which to guess the initial
#'                      \code{start} parameters of \code{\link[stats]{nls}}.
#'
#'          Details for each level are as:
#'          \itemize{
#'              \item \code{0}:
#'                       The mean and the standard deviation of the frequency distribution
#'                       are used as initial values.
#'              \item \code{1}:
#'                       In addition to level \code{0}, if it is likely to be better guess,
#'                       it computes the mean values or standard deviations in ranges of
#'                       local x-coordinates where the effect of each of components is likely
#'                       to be heavy, and uses them as initial values.
#'              \item \code{2}:
#'                       In addition to level \code{0}, if it is likely to be better guess,
#'                       it uses the mean values or standard deviations of normal distributions
#'                       tracing two of quantiles which are generated with
#'                       the frequency distribution as initial values.
#'              \item \code{3}:
#'                       It uses the mean values and standard deviations of the components
#'                       of a \code{\link[ggd]{GGD}} object tracing some (2, 3 or 5) quantiles
#'                       which are generated with the frequency distribution as initial values.
#'                       If tracing fails, level \code{2} is used instead.
#'              \item \code{100}:
#'                       Try all of above levels and adopt the result with the highest
#'                       \code{\link[stats]{cor}} value.
#'          }
#'
#'          The higher the level in the range of from \code{0} to \code{3},
#'          the more likely it is that the initial values will model
#'          the frequency distribution in closer,
#'          but the accuracy of the result may not along with the level.
#'          It is possible that \code{\link[stats]{nls}} will succeed at level \code{1}
#'          and fail at level \code{3} for the same data.
#'
#' @param   start       A list of \code{start} argument for \code{\link[stats]{nls}}.
#'
#'                      You can provide your own \code{start} for \code{\link[stats]{nls}},
#'                      the mean values
#'                      (names are like: \code{mean}, \code{mean.i} or \code{mean.i.j})
#'                      and the \bold{square root} of the standard deviations
#'                      (names are like: \code{sqrt.sd}, \code{sqrt.sd.i} or \code{sqrt.sd.i.j})
#'                      of the normal distributions of the components.
#'
#'                      Depending on the kind of the distribution model,
#'                      the name of the parameters are different.
#'                      You can use \code{\link[ggd]{ggd.start.template}}
#'                      to get the template of the list and know the names of the parameters.
#'
#'                      If a not-\code{NULL} list is indicated for this argument,
#'                      \code{start.level} argument is ignored.
#'
#' @param   control     The list for \code{control} argument of \code{\link[stats]{nls}}.
#'                      See \code{\link[stats]{nls.control}} for more information.
#'
#' @param   not.use.nls A logical.
#'                      This argument is enabled when \code{start.level} argument is
#'                      other than \code{100}.
#'
#'                      If \code{TRUE}, this function does not use \code{\link[stats]{nls}} and
#'                      it outputs a distribution model using initial values directly.
#'                      If \code{FALSE}, this function uses \code{\link[stats]{nls}}.
#'
#'                      You can use \code{not.use.nls = TRUE} to check the initial values
#'                      when an error has occurred at this function.
#'
#' @param   method      The \code{method} argument for \code{\link[stats]{cor}},
#'                      which represents the correlation coefficient method.
#'                      This argument is used only if \code{start.level = 100}.
#'                      If \code{NULL}, it uses the default method of \code{\link[stats]{cor}}.
#'                      See \code{\link[stats]{cor}} for more information.
#'
#' @param   ...         Each argument for \code{\link[stats]{nls}} can be indicated.
#'                      See "Arguments" of \code{\link[stats]{nls}} for more information.
#'
#' @return  A list containing components (invisible for \code{GGD} method)
#'          \item{obj}{
#'                  Generated \code{\link[ggd]{GGD}} object which most (at least locally)
#'                  closely approximates the given frequency distribution.
#'                  If \code{\link[stats]{nls}} has failed, it will be a cleared object.
#'                  For \code{\link[ggd]{GGD}} method, the \code{\link[ggd]{GGD}} object itself.}
#'          \item{nls.out}{
#'                  The list of the output of \code{\link[stats]{nls}}.
#'                  See "Value" of \code{\link[stats]{nls}} for more information.}
#'          \item{start.level}{
#'                  An integer of the indicated value of \code{start.level}.
#'                  If not-\code{NULL} \code{start} argument is indicated,
#'                  it will be \code{NA_integer_}.}
#'          \item{start}{
#'                  The used \code{start} argument for the initial values
#'                  in \code{\link[stats]{nls}}.}
#'          \item{start.obj}{
#'                  A \code{\link[ggd]{GGD}} object corresponding to the initial values.
#'                  That is, a \code{\link[ggd]{GGD}} object in which each parameter in
#'                  the used \code{start} argument is directly given to the field.}
#'          \item{cor}{
#'                  A \code{\link[ggd]{GGD}} object corresponding to the initial values.
#'                  That is, a \code{\link[ggd]{GGD}} object in which each parameter in
#'                  This component is given only if \code{start.level = 100}.}
#'          \item{errors}{
#'                  A list of information about errors which have occurred
#'                  at \code{\link[stats]{nls}}.
#'                  This component is given only if \code{start.level = 100}.
#'                  Each element in the list contains:
#'                  \itemize{
#'                      \item level: The level of initial guessing.
#'                                   That is, the value of \code{start.level}
#'                                   other than \code{100}.
#'                      \item message: The error message.
#'                  }}
#'          \item{warnings}{
#'                  A list of information about warnings which have occurred
#'                  at \code{\link[stats]{nls}}.
#'                  This component is given only if \code{start.level = 100}.
#'                  The composition of each element is as same as for \code{errors}.}
#'
#'          For \code{GGD} method: If an error occur, the object will be cleared in most cases.
#'
#' @importFrom  methods     new
#' @importFrom  stats       complete.cases
#' @seealso \code{\link[stats]{nls}}, \code{\link[stats]{nls.control}},
#'          \code{\link[ggd]{ggd.nls.freq.all}}, \code{\link[ggd]{ggd.start.template}}
#'
#' @details
#'  \subsection{Why the standard deviations for "start" are square-rooted?}{
#'      You know a standard deviation must be a non-zero positive value.
#'      But if you use standard deviations directly in the formula for \code{\link[stats]{nls}},
#'      they will sometimes drop into negative values while the Gauss-Newton algorithm
#'      and the algorithm will fail, even if it can reach to convergence when done well.
#'
#'      So, to avoid such failures, we use square roots of standard deviations and
#'      take squares of them in the formula for \code{\link[stats]{nls}}.
#'  }
#'
#' @examples
#'  ## Preparing
#'  df <- data.frame( x      = seq( -2, 2, 0.2 ),
#'                    freq   = c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'                                7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'                                3698,  2740,  2549,  2284,  1499,  1147,   918 ),
#'                    x.2    = seq( -20, 20, 2 ),
#'                    freq.2 = c( .000974, .003797, .008523, .023142, .045017, .081743, .120990,
#'                                .142527, .124627, .106294, .078625, .059378, .045690, .042958,
#'                                .035760, .030938, .015675, .012516, .008139, .005114, .003582 ) )
#'
#'  plot.freq.and.d <- function( obj, x, freq )
#'  {
#'      xlim <- c( min( x ), max( x ) )
#'      ylim <- c( 0, max( ggd:::get.d.freq( x, freq ) ) * 1.2 )
#'      plot( x, ggd:::get.d.freq( x, freq ), xlim = xlim, ylim = ylim, xlab = "", ylab = "" )
#'      par( new = TRUE )
#'      plot( seq( min( x ), max( x ), 0.01 ), obj$d( seq( min( x ), max( x ), 0.01 ) ),
#'            type = "l", xlim = xlim, ylim = ylim )
#'  }
#'
#'  ## Examples
#'  ggd.nls.freq( df, grad = "normal" )
#'  a <- ggd.nls.freq( df, grad = "no" )$obj
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  a$nls.freq( df, this.mix.type = 1 )
#'  a
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  a$nls.freq( df,
#'              this.kind = "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution" )
#'  a
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  ## overwriting "Sigma-Differed" after "kind = a"
#'  b <- ggd.nls.freq( df, kind = a, eq.sd = FALSE )
#'  b
#'  plot.freq.and.d( b$obj, df$x, df$freq )
#'
#'  ## You can set start parameters if you want.
#'  a$nls.freq( df, this.kind = 14, control = list( warnOnly = TRUE ) )
#'  a
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  start.list <- ggd.start.template( a )
#'  start.list
#'
#'  start.list$mean.1.1 <- -0.671
#'  start.list$mean.1.2 <- -0.198
#'  start.list$mean.2.1 <- 0.293
#'  start.list$mean.2.2 <- -0.198
#'  start.list$sqrt.sd <- sqrt( 0.640 ) ## sqrt.sd is the sqrt of the standard deviation.
#'  a$nls.freq( df, this.kind = 14, start = start.list )
#'  a
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  ## When you use a GGD object consecutively,
#'  ## the field values set to the object in the previous session are kept
#'  ## (if no error has occurred).
#'  a$nls.freq( df, grad = "hv", eq.mean = TRUE )
#'  a
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  a$nls.freq( df, eq.mean = FALSE )   ## grad = "hv" (mix.type = 4) is retained.
#'  a
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  ## Using "x.2" for x and "freq.2" for freq.
#'  a <- ggd.nls.freq( df, x = "x.2", freq = "freq.2" )$obj
#'  a   ## default value of mix.type is 2
#'  plot.freq.and.d( a, df$x.2, df$freq.2 )
################################################################################################
ggd.nls.freq <- function( data, x = "x", freq = "freq", total = NULL,
                          kind = NULL, mix.type = NULL,
                          grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ),
                          eq.mean = logical(), eq.sd = logical(),
                          start.level = 100, start = NULL, control = list(),
                          not.use.nls = FALSE, method = NULL, ... )
{
    obj <- GGD$new()
    return ( withVisible( obj$nls.freq( data            = data,
                                        x               = x,
                                        freq            = freq,
                                        total           = total,
                                        this.kind       = kind,
                                        this.mix.type   = mix.type,
                                        grad            = grad,
                                        eq.mean         = eq.mean,
                                        eq.sd           = eq.sd,
                                        start.level     = start.level,
                                        start           = start,
                                        control         = control,
                                        not.use.nls     = not.use.nls,
                                        method          = method, ... ) )$value )
}

GGD$methods(
    nls.freq = function( data, x = "x", freq = "freq", total = NULL,
                         this.kind = NULL, this.mix.type = NULL,
                         grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ),
                         eq.mean = logical(), eq.sd = logical(),
                         start.level = 100, start = NULL, control = list(),
                         not.use.nls = FALSE, method = NULL, ...)
    {
        # Note:
        # In this function, when a error occur,
        # we clear the fields as possible as we can.
        #
        # Because this function does not directly set specified values to the fields,
        # if the fields are not cleared and contain some normal values,
        # users may let the subsequent processes take place without noticing the error.
        # During the development phase, the developer actually experienced such mistakes.

        result <- list( obj = NULL, nls.out = NULL,
                        start.level = NULL, start = NULL, start.obj = NULL )

        # Check errors of data frame and discard NA and NaN.
        data.ext <- withCallingHandlers( extract.freq.data( data, x, freq ),
                                         error = function( e ) clear() )

        # Get total.
        if ( is.null( total ) )
        {
            total <- sum( data.ext$freq )
        }
        else if ( length( total ) != 1 || !is.numeric( total ) || is.na( total ) ||
                  is.infinite( total ) || total <= 0 )
        {
            clear()
            stop( "Error: total should be positive finite single value." )
        }

        ################################################################
        # Check options and get new mix.type according to the priority.
        grad <- withCallingHandlers( match.arg( grad ), error = function( e ) clear() )
        if ( grad == "v" )
        {
            grad <- "v2"
        }

        if ( !is.null( this.kind ) )
        {
            this.kind.index <- withCallingHandlers(
                                    ggd.kind.index( this.kind, undef.err = TRUE ),
                                    error = function( e ) clear() )
            if ( length( this.kind.index ) > 1 )
            {
                clear()
                stop( "Error: kind should be valid single value or a GGD object." )
            }
            else if ( length( this.kind.index ) == 1 && !is.na( this.kind.index ) )
            {
                if ( is.null( this.mix.type ) )
                {
                    this.mix.type <- ggd.mix.type.for.kind.index( this.kind.index )

                    if ( grad == "default" )
                    {
                        if ( any( this.kind.index == 8:10 ) )
                        {
                            grad <- "v2"
                        }
                        else if ( any( this.kind.index == 11:13 ) )
                        {
                            grad <- "v3"
                        }
                    }
                }

                if ( length( eq.mean ) == 0 )
                {
                    eq.mean <- ( this.kind.index %% 3 == 0 )
                }
                if ( length( eq.sd ) == 0 )
                {
                    eq.sd   <- ( this.kind.index %% 3 == 2 )
                }
            }
        }

        # Since 2-valued (TRUE/FALSE) logical, set FALSE if length is 0.
        eq.mean <- isTRUE( eq.mean )
        eq.sd   <- isTRUE( eq.sd )

        if ( eq.mean && eq.sd )
        {
            # Both eq.mean and eq.sd are TRUE => generate a normal distribution.
            grad <- "normal"
        }

        # If this.mix.type is not given (neither this.kind),
        # get the value from the current mix.type field.
        if ( is.null( this.mix.type ) && is.null( this.kind ) )
        {
            if ( length( mix.type ) > 0 && complete.cases( mix.type ) )
            {
                this.mix.type <- mix.type[1]
            }
            else
            {
                # If mix.type field is cleared or invalid, use the default value 2.
                this.mix.type <- 2
            }
        }

        new.mix.type <- withCallingHandlers(
                            ggd.mix.type.for( grad,
                                              kind = this.kind, mix.type = this.mix.type ),
                            error = function( e ) clear() )
        if ( length( new.mix.type ) != 1 || is.na( new.mix.type ) ||
             !any( new.mix.type == 0:4 ) )
        {
            clear()
            if ( !is.null( this.kind ) )
            {
                # This code will run if this.kind = character( 0 ).
                stop( "Error: kind should be valid single value or a GGD object." )
            }
            else
            {
                stop( "Error: mix.type should be single integer from 0 to 4." )
            }
        }

        # When new mix.type is going to be 3, if grad = "default",
        # priority is given to "v2". "v3" is for only current number of components is 3.
        if ( new.mix.type == 3 && grad == "default" )
        {
            if ( nrow( cmp ) == 3 )
            {
                grad <- "v3"
            }
            else
            {
                grad <- "v2"
            }
        }

        ################################################################
        # Check start.level.
        if ( is.null( start ) )
        {
            if ( length( start.level ) != 1 || !is.numeric( start.level ) ||
                 is.na( start.level ) || !any( start.level == c( 0:3, 100 ) ) )
            {
                clear()
                stop( "Error: start.level should be single integer in 0:3 or 100." )
            }
        }
        else
        {
            start.level <- NA_integer_
        }

        if ( isTRUE( start.level == 100 ) )
        {
            ################################################################
            # Loop with each level.
            result <- try( nls.freq.level.100( data.ext, total, this.kind, this.mix.type,
                                               grad, eq.mean, eq.sd, control,
                                               not.use.nls = FALSE, method, ... ),
                           silent = TRUE )
            if ( inherits( result, "try-error" ) )
            {
                clear()
                stop( result )
            }

            # Replace result$obj with .self.
            set.cmp( result$obj$cmp, this.kind = result$obj )
            result$obj <- .self
        }
        else
        {
            ################################################################
            # Get start paramaters for nls.
            params <- get.nls.params( data.ext$x, data.ext$freq, total,
                                      new.mix.type, grad, eq.mean, eq.sd, start.level )

            # Output start paramaters.
            result$start.level <- as.integer( start.level )
            if ( is.null( start ) )
            {
                result$start <- params$start
            }
            else
            {
                result$start <- start
            }
            result$start.obj <- ggd.set.cmp( get.cmp.with.nls.coef( unlist( result$start ),
                                                                    new.mix.type, grad,
                                                                    eq.mean, eq.sd ),
                                             mix.type = new.mix.type, grad = grad )

            ################################################################
            # Get the result components.
            result.cmp <- NULL
            if ( isTRUE( not.use.nls ) )
            {
                # Output using start paramaters directly without nls.
                result.cmp <- result$start.obj$cmp
            }
            else
            {
                # Execute nls.
                result$nls.out <- try( nls(
                                    params$formula,
                                    data = list( d = get.d.freq( data.ext$x,
                                                            data.ext$freq,
                                                            total ),
                                                 x = data.ext$x ),
                                                 start = result$start,
                                                 control = control, ... ), silent = TRUE )
                if ( inherits( result$nls.out, "try-error" ) )
                {
                    clear()
                    stop( paste( "nls has failed. Message:", result$nls.out ) )
                }
                else
                {
                    result.cmp <- get.cmp.with.nls.coef( coef( result$nls.out ),
                                                         new.mix.type, grad, eq.mean, eq.sd )
                }
            }

            ################################################################
            # Update fields with the result.
            set.cmp( result.cmp, this.mix.type = new.mix.type, grad = grad )

            result$obj <- .self
        }

        return ( invisible( result ) )
    }
)

################################################################################################
#' [Non-exported] Extract complete-case frequency distribution data
#'
#' Checks errors of the indicated frequency distribution data frame and makes
#' a complete-case data frame with 2 columns named \code{"x"} and \code{"freq"}.
#' @param   data    A data frame which represents a relation of x-coordinates and frequencies.
#'                  It should contain 2 columns for \code{x} and \code{freq}.
#' @param   x       The column name or column number indicating \code{x} of \code{data},
#'                  the column of x-coordinates.
#' @param   freq    The column name or column number indicating \code{freq} of \code{data},
#'                  the column of frequencies.
#' @return  A data frame with 2 columns named \code{"x"} and \code{"freq"}.
#' @seealso \code{\link[ggd]{extract.complete.x.y}}
################################################################################################
extract.freq.data <- function( data, x, freq )
{
    if ( !is.data.frame( data ) )
    {
        stop( "Error: data must be a data frame." )
    }

    if ( length( x ) != 1 || ( !is.numeric( x ) && !is.character( x ) ) || is.na( x ) )
    {
        stop( "Error: Argument x must be a column name or an index number." )
    }
    else if ( is.numeric( x ) )
    {
        x <- as.integer( x )
        if ( is.na( x ) || !any( x == 1:ncol( data ) ) )
        {
            stop( "Error: Illegal column number for x." )
        }
    }

    if ( length( freq ) != 1 || ( !is.numeric( freq ) && !is.character( freq ) ) ||
         is.na( freq ) )
    {
        stop( "Error: Argument freq must be a column name or an index number." )
    }
    else if ( is.numeric( freq ) )
    {
        freq <- as.integer( freq )
        if ( is.na( freq ) || !any( freq == 1:ncol( data ) ) )
        {
            stop( "Error: Illegal column number for freq." )
        }
    }

    if ( is.null( data[[x]] ) || is.null( data[[freq]] ) )
    {
        stop( paste0( "Error: Column '",
                              ifelse( is.null( data[[x]] ), x, freq ), "' is undefined." ) )
    }

    # Discard NA and NaN.
    data.ext <- extract.complete.x.y( data, x, freq, "freq" )
    if ( nrow( data.ext ) < 3 )
    {
        stop( "Error: The row number of data is too small." )
    }

    # Check if rows are sorted.
    if ( !all( data.ext$x[1:nrow( data.ext ) - 1] < data.ext$x[2:nrow( data.ext )] ) )
    {
        stop( paste( "Error: Rows of", x, "must have been sorted in ascending order,",
                            "and must not duplicated." ) )
    }

    return ( data.ext )
}

################################################################################################
#' [Non-exported] Probability density values from a frequency distribution
#'
#' Gets the probability density value at each of the x-coordinates based on
#' a frequency distribution.
#' @param   x       A vector of x-coordinates. Duplicate values are not allowed.
#'
#' @param   freq    A vector of frequencies following \code{x}.
#'                  The values of frequencies must be positive.
#'                  Both integers and real numbers are allowed for the values.
#'
#' @param   total   Total value of the frequencies.
#'
#'                  If \code{NULL} (the default),
#'                  the total of \code{freq}, i.e., \code{\link[base]{sum}(data[[freq]])}
#'                  (on \code{\link[stats]{complete.cases}} of \code{x} and \code{freq})
#'                  is used for it.
#'
#' @return  The vector of expected probability density value at each of x-coordinates.
#' @examples
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'             7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'             3698,  2740,  2549,  2284,  1499,  1147,   918 )
#'  ggd:::get.d.freq( x, freq )
#'  plot( x, ggd:::get.d.freq( x, freq ) )
################################################################################################
get.d.freq <- function( x, freq, total = sum( freq ) )
{
    d <- vapply( 1:length( x ), function( i )
    {
        if ( i == 1 )
        {
            x.width.lower <- ( x[2] - x[1] ) / 2
        }
        else
        {
            x.width.lower <- ( x[i] - x[i - 1] ) / 2
        }

        if ( i == length( x ) )
        {
            x.width.upper <- ( x[i] - x[i - 1] ) / 2
        }
        else
        {
            x.width.upper <- ( x[i + 1] - x[i] ) / 2
        }

        freq[i] / total / ( x.width.lower + x.width.upper )
    }, 0 )

    return ( d )
}

################################################################################################
#' [Non-exported] Probabilities of frequency distribution
#'
#' Gets a vector of probabilities that the random variable is less than or equal to the value of
#' x of the frequency distribution.
#' This function assumes that each x of the frequency distribution is at the center of a cell
#' and that an equal number of samples both above and below are excluded outside the range
#' of the frequency distribution.
#' @param   freq        A vector of frequencies.
#'                      The x-coordinates corresponding to these frequencies are expected to be
#'                      at the center of the cells of the frequency distribution.
#' @param   total       Total value of the frequencies.
#' @return  A vector of probabilities.
################################################################################################
get.p.freq <- function( freq, total )
{
    ps <- numeric( length( freq ) )
    if ( total < sum( freq ) )
    {
        total <- sum( freq )
    }

    # This process must be run sequentially.
    p <- ( 1 - sum( freq ) / total ) / 2
    for ( i in 1:length( freq ) )
    {
        if ( i == 1 )
        {
            ps[1] <- p <- p + freq[1] / total / 2
        }
        else
        {
            ps[i] <- p <- p + ( freq[i - 1] + freq[i] ) / total / 2
        }
    }

    return ( ps )
}

################################################################################################
#' [Non-exported] Exclude small frequencies of edges
#'
#' Excludes data with \code{0} or extremely small frequency on both edges of the range of
#' the x-coordinates from data of a frequency distribution.
#' @usage   exclude.freq.edge(x, freq)
#' @param   x               A vector of x-coordinates of a frequency distribution.
#' @param   freq            A vector of frequencies of a frequency distribution.
#' @return  A list containing components
#'          \item{x}{
#'                  The vectors of x-coordinates after the cutoff.}
#'          \item{freq}{
#'                  The vectors of frequencies after the cutoff.}
################################################################################################
exclude.freq.edge <- function( x, freq )
{
    min.i <- min( ( 1:length( freq ) )[freq > .Machine$double.eps * max( freq )] )
    max.i <- max( ( 1:length( freq ) )[freq > .Machine$double.eps * max( freq )] )

    return ( list( x = x[min.i:max.i], freq = freq[min.i:max.i] ) )
}

################################################################################################
#' [Non-exported] Parameters for nls
#'
#' Gets a list of main arguments for \code{\link[stats]{nls}} (except for \code{data}),
#' based on the configuration arguments.
#' @param   x           A vector of x-coordinates arranged in ascending order
#'                      and non-duplicated.
#' @param   freq        A vector of frequencies following \code{x}.
#' @param   total       Total value of the frequencies.
#' @param   mix.type    The value for \code{mix.type} field.
#' @param   grad        A character string indicating the method of gradation.
#'                      If "v3", constructing with 3 components is enforcedly,
#'                      even if it is possible to construct with 2 components.
#'
#'                      In this function, \code{grad} argument have no priority
#'                      over \code{mix.type} argument.
#'                      Therefore, \code{grad = "v2"} or \code{"v3"} works only if
#'                      \code{mix.type = 3} is indicated.
#'                      And if \code{mix.type = 3} is indicated,
#'                      either \code{grad = "v2"} or \code{"v3"} must be indicated.
#'
#' @param   eq.mean     A logical; the flag whether to make all of the mean values of
#'                      the normal distributions of the components to be equal.
#' @param   eq.sd       A logical; the flag whether to make all of the standard deviations of
#'                      the normal distributions of the components to be equal.
#' @param   start.level An integer from \code{0} to \code{3} or \code{NA};
#'                      the level at which to guess
#'                      the initial \code{start} parameters of \code{\link[stats]{nls}}.
#'
#'          Details for each level are as:
#'          \itemize{
#'              \item from \code{0} to \code{3}:
#'                      Same as \code{start.level} in arguments of \code{\link[ggd]{nls.freq}}.
#'              \item \code{NA}: All of mean values and standard deviations are \code{NA_real_}.
#'          }
#' @return  A list containing \code{formula} and \code{start} for the arguments of
#'          \code{\link[stats]{nls}}.
#' @importFrom  stats   dnorm pnorm
################################################################################################
get.nls.params <- function( x, freq, total, mix.type, grad, eq.mean, eq.sd, start.level )
{
    fm <- NULL          # formula for return value
    start <- list()     # start for return value

    # Mean and standard deviation of the data
    data.mean <- sum( x * freq ) / total
    data.sd   <- sqrt( sum( ( data.mean - x )^2 * freq ) / total )

    # Exclude data with 0 or extremely small frequency on both edges of
    # the range of the x-coordinates.
    x.freq <- exclude.freq.edge( x, freq )

    # Quarter the vectors of the frequency distribution and get initial values.
    #
    # The initial values are as:
    #   means, sqrt.sds:
    #       The mean values and sqrt of standard deviations corresponding to each of the four
    #       quarterings of the range of the frequency distribution in ascending order of x.
    #
    #   mean.lower/upper/inner/outer, sqrt.sd.lower/upper/inner/outer:
    #       The mean values and sqrt of standard deviations corresponding to each of half ranges
    #       of the frequency distribution.
    #
    #   means.mid:
    #       Four mean values corresponding to each of the four quarterings of the range,
    #       but closer to mean of the frequency distribution than "means".
    #       These are used when we do not want the initial mean values to be dispersed.
    #
    #   sqrt.sd.mid:
    #       Global or local standard deviation corresponding to around center of the range of
    #       the frequency distribution.
    if ( is.na( start.level ) )
    {
        # Level NA: all parameters are NA.
        means.mid <- means <- rep( NA_real_, 4 )
        sqrt.sd.mid <- sqrt.sds <- rep( NA_real_, 4 )

        mean.lower <- mean.upper <- mean.inner <- mean.outer <- NA_real_
        sqrt.sd.lower <- sqrt.sd.upper <- sqrt.sd.inner <- sqrt.sd.outer <- NA_real_
    }
    else if ( start.level == 0 )
    {
        # Level 0: use global mean value and standard deviation for all initial values.
        means <- rep( data.mean, 4 )
        sqrt.sds <- rep( sqrt( data.sd ), 4 )

        mean.lower <- mean.upper <- mean.inner <- mean.outer <- data.mean
        sqrt.sd.lower <- sqrt.sd.upper <- sqrt.sd.inner <- sqrt.sd.outer <- sqrt( data.sd )

        means.mid <- rep( data.mean, 4 )
        sqrt.sd.mid <- sqrt( data.sd )
    }
    else if ( start.level == 1 )
    {
        # Level 1: compute local mean values and standard deviations.
        sep <- separate.data.quarter( x.freq$x, x.freq$freq, data.mean )

        mean.lower <- sum( sep$x.lower * sep$data.lower ) / sum( sep$data.lower )
        mean.upper <- sum( sep$x.upper * sep$data.upper ) / sum( sep$data.upper )
        mean.inner <- sum( sep$x.inner * sep$data.inner ) / sum( sep$data.inner )
        mean.outer <- sum( sep$x.outer * sep$data.outer ) / sum( sep$data.outer )

        sqrt.sd.lower <- ( sum( ( sep$x.lower - data.mean )^2 * sep$data.lower ) /
                           sum( sep$data.lower ) )^0.25
        sqrt.sd.upper <- ( sum( ( sep$x.upper - data.mean )^2 * sep$data.upper ) /
                           sum( sep$data.upper ) )^0.25
        sqrt.sd.inner <- ( sum( ( sep$x.inner - data.mean )^2 * sep$data.inner ) /
                           sum( sep$data.inner ) )^0.25
        sqrt.sd.outer <- ( sum( ( sep$x.outer - data.mean )^2 * sep$data.outer ) /
                           sum( sep$data.outer ) )^0.25

        means <- c( ( mean.lower + mean.outer ) / 2,
                    ( mean.lower + mean.inner ) / 2,
                    ( mean.upper + mean.inner ) / 2,
                    ( mean.upper + mean.outer ) / 2 )
        sqrt.sds <- vapply( 1:4, function( i )
        {
            ( sum( ( sep$x[[i]] - data.mean )^2 * sep$data[[i]] ) /
              sum( sep$data[[1]] ) )^0.25
        }, 0 )

        means.mid <- rep( data.mean, 4 )
        sqrt.sd.mid <- sqrt( data.sd )
    }
    else if ( start.level >= 2 )
    {
        # Level 2: pick up 2 quantiles for each component.
        freq.ps <- get.p.freq( x.freq$freq, total )
        sep <- separate.data.quarter( x.freq$x, freq.ps, data.mean )
        lengths <- vapply( 1:4, function( i ) length( sep$x[[i]] ), 0 )

        ms <- lapply( 1:4, function( i )
        {
            ms.norm.xp( c( sep$x[[i]][1],    sep$x[[i]][lengths[i]] ),
                        c( sep$data[[i]][1], sep$data[[i]][lengths[i]] ) )
        } )

        means <- vapply( 1:4, function( i ) ms[[i]]$mean, 0 )
        sqrt.sds <- vapply( 1:4, function( i ) sqrt( ms[[i]]$sd ), 0 )

        mean.lower <- ( ms[[1]]$mean + ms[[2]]$mean ) / 2
        mean.upper <- ( ms[[3]]$mean + ms[[4]]$mean ) / 2
        mean.inner <- ( ms[[2]]$mean + ms[[3]]$mean ) / 2
        mean.outer <- ( ms[[1]]$mean + ms[[4]]$mean ) / 2

        sqrt.sd.lower <- sqrt( ( ms[[1]]$sd + ms[[2]]$sd ) / 2 )
        sqrt.sd.upper <- sqrt( ( ms[[3]]$sd + ms[[4]]$sd ) / 2 )
        sqrt.sd.inner <- sqrt( ( ms[[2]]$sd + ms[[3]]$sd ) / 2 )
        sqrt.sd.outer <- sqrt( ( ms[[1]]$sd + ms[[4]]$sd ) / 2 )

        means.mid <- rep( data.mean, 4 )
        sqrt.sd.mid <- sqrt( data.sd )

        if ( start.level == 3 )
        {
            # Level 3: tracing quantiles.
            # Overwriting the guessing to level 2 guessing.
            if ( mix.type == 0 )
            {
                ms <- ms.norm.xp( c( sep$x[[1]][lengths[1]], sep$x[[4]][1] ),
                                  c( sep$data[[1]][lengths[1]], sep$data[[4]][1] ) )

                means.mid <- rep( ms$mean, 4 )
                sqrt.sd.mid <- sqrt( ms$sd )
            }
            else
            {
                if ( mix.type == 4 )
                {
                    qt <- data.frame( x = c( sep$x[[1]][1],
                                             sep$x[[2]][1],
                                             sep$x[[3]][1],
                                             sep$x[[3]][lengths[3]],
                                             sep$x[[4]][lengths[4]] ),
                                      p = c( sep$data[[1]][1],
                                             sep$data[[2]][1],
                                             sep$data[[3]][1],
                                             sep$data[[3]][lengths[3]],
                                             sep$data[[4]][lengths[4]] ) )
                }
                else
                {
                    qt <- data.frame( x = c( sep$x[[1]][lengths[1]],
                                             sep$x[[3]][1],
                                             sep$x[[4]][1] ),
                                      p = c( sep$data[[1]][lengths[1]],
                                             sep$data[[3]][1],
                                             sep$data[[4]][1] ) )
                }

                obj <- suppressMessages(
                            try( ggd.trace.q( qt, mix.type = mix.type,
                                              grad = ifelse( mix.type != 3, "default", grad ),
                                              eq.mean = eq.mean, eq.sd = eq.sd )$obj,
                                 silent = TRUE ), "message" )
                if ( inherits( obj, "try-error" ) )
                {
                    warning( paste( "Warning: Level 3 initial guessing has failed.",
                                             "Level 2 has been used instead." ) )
                }
                else
                {
                    if ( mix.type == 4 )
                    {
                        means.mid <- means <- obj$cmp$mean[c( 1, 2, 4, 3 )]
                        sqrt.sds <- sqrt( obj$cmp$sd[c( 1, 2, 4, 3 )] )
                        sqrt.sd.mid <- ( sqrt.sds[2] + sqrt.sds[3] ) / 2
                    }
                    else
                    {
                        means.mid <- means <- obj$cmp$mean[c( 1, 2, 2, nrow( obj$cmp ) )]
                        sqrt.sds <- sqrt( obj$cmp$sd[c( 1, 2, 2, nrow( obj$cmp ) )] )

                        mean.lower <- means[1]
                        mean.upper <- means[4]
                        mean.inner <- means[2]
                        mean.outer <- means[1]

                        sqrt.sd.lower <- sqrt.sds[1]
                        sqrt.sd.upper <- sqrt.sds[4]
                        sqrt.sd.inner <- sqrt.sds[2]
                        sqrt.sd.outer <- sqrt.sds[1]

                        sqrt.sd.mid <- sqrt.sds[2]
                    }
                }
            }
        }
    }

    if ( mix.type == 0 )
    {
        # via Normal Distribution
        fm <- d ~ dnorm( x, mean, sqrt.sd^2 )

        start <- list( mean = means.mid[1], sqrt.sd = sqrt.sd.mid )
    }
    else
    {
        if ( mix.type == 1 )
        {
            # via Mean of 2 Normal Distributions
            if ( eq.sd )
            {
                fm <- d ~ ( dnorm( x, mean.1, sqrt.sd^2 ) + dnorm( x, mean.2, sqrt.sd^2 ) ) / 2
                start <- list( mean.1 = mean.lower,
                               mean.2 = mean.upper,
                               sqrt.sd = sqrt.sd.mid )
            }
            else if ( eq.mean )
            {
                fm <- d ~ ( dnorm( x, mean, sqrt.sd.1^2 ) + dnorm( x, mean, sqrt.sd.2^2 ) ) / 2
                start <- list( mean = means.mid[1],
                               sqrt.sd.1 = sqrt.sd.outer,
                               sqrt.sd.2 = sqrt.sd.inner )
            }
            else
            {
                fm <- d ~ ( dnorm( x, mean.1, sqrt.sd.1^2 ) + dnorm( x, mean.2, sqrt.sd.2^2 ) ) / 2
                start <- list( mean.1 = means.mid[1],
                               mean.2 = means.mid[2],
                               sqrt.sd.1 = sqrt.sd.outer,
                               sqrt.sd.2 = sqrt.sd.inner )
            }
        }
        else if ( mix.type == 2 )
        {
            # via Horizontal Gradation of 2 Normal Distributions
            if ( eq.sd )
            {
                fm <- d ~ ( 1 - pnorm( x, mean.1, sqrt.sd^2 ) ) *
                                dnorm( x, mean.1, sqrt.sd^2 ) +
                          pnorm( x, mean.2, sqrt.sd^2 ) * dnorm( x, mean.2, sqrt.sd^2 )

                start <- list( mean.1 = mean.lower,
                               mean.2 = mean.upper,
                               sqrt.sd  = sqrt.sd.mid )
            }
            else if ( eq.mean )
            {
                fm <- d ~ ( 1 - pnorm( x, mean, sqrt.sd.1^2 ) ) *
                                dnorm( x, mean, sqrt.sd.1^2 ) +
                          pnorm( x, mean, sqrt.sd.2^2 ) * dnorm( x, mean, sqrt.sd.2^2 )

                start <- list( mean = means.mid[1],
                               sqrt.sd.1 = sqrt.sd.lower,
                               sqrt.sd.2 = sqrt.sd.upper )
            }
            else
            {
                fm <- d ~ ( 1 - pnorm( x, mean.1, sqrt.sd.1^2 ) ) *
                                dnorm( x, mean.1, sqrt.sd.1^2 ) +
                          pnorm( x, mean.2, sqrt.sd.2^2 ) * dnorm( x, mean.2, sqrt.sd.2^2 )

                start <- list( mean.1 = means.mid[1],
                               mean.2 = means.mid[2],
                               sqrt.sd.1 = sqrt.sd.lower,
                               sqrt.sd.2 = sqrt.sd.upper )
            }
        }
        else if ( mix.type == 3 )
        {
            if ( grad == "v2" )
            {
                # via Vertical Gradation of 2 Normal Distributions
                if ( eq.sd )
                {
                    fm <- d ~ dp.t3( x, c( mean.1, mean.2, mean.1 ),
                                        c( sqrt.sd^2, sqrt.sd^2, sqrt.sd^2 ), f.t3.d )

                    start <- list( mean.1 = mean.outer,
                                   mean.2 = mean.inner,
                                   sqrt.sd = sqrt.sd.mid )
                }
                else if ( eq.mean )
                {
                    fm <- d ~ dp.t3( x, c( mean, mean, mean ),
                                        c( sqrt.sd.1^2, sqrt.sd.2^2, sqrt.sd.1^2 ), f.t3.d )

                    start <- list( mean = means.mid[1],
                                   sqrt.sd.1 = sqrt.sd.outer,
                                   sqrt.sd.2 = sqrt.sd.inner )
                }
                else
                {
                    fm <- d ~ dp.t3( x, c( mean.1, mean.2, mean.1 ),
                                        c( sqrt.sd.1^2, sqrt.sd.2^2, sqrt.sd.1^2 ), f.t3.d )

                    start <- list( mean.1 = means.mid[1],
                                   mean.2 = means.mid[2],
                                   sqrt.sd.1 = sqrt.sd.outer,
                                   sqrt.sd.2 = sqrt.sd.inner )
                }
            }
            else
            {
                # via Vertical Gradation of 3 Normal Distributions
                if ( eq.sd )
                {
                    fm <- d ~ dp.t3( x, c( mean.1, mean.2, mean.3 ),
                                        c( sqrt.sd^2, sqrt.sd^2, sqrt.sd^2 ), f.t3.d )

                    start <- list( mean.1 = means[1],
                                   mean.2 = mean.inner,
                                   mean.3 = means[4],
                                   sqrt.sd = sqrt.sd.mid )
                }
                else if ( eq.mean )
                {
                    fm <- d ~ dp.t3( x, c( mean, mean, mean ),
                                        c( sqrt.sd.1^2, sqrt.sd.2^2, sqrt.sd.3^2 ), f.t3.d )

                    start <- list( mean      = means.mid[1],
                                   sqrt.sd.1 = sqrt.sds[1],
                                   sqrt.sd.2 = sqrt.sd.inner,
                                   sqrt.sd.3 = sqrt.sds[4] )
                }
                else
                {
                    fm <- d ~ dp.t3( x, c( mean.1, mean.2, mean.3 ),
                                        c( sqrt.sd.1^2, sqrt.sd.2^2, sqrt.sd.3^2 ), f.t3.d )

                    start <- list( mean.1 = means.mid[1],
                                   mean.2 = ( means.mid[2] + means.mid[3] ) / 2,
                                   mean.3 = means.mid[4],
                                   sqrt.sd.1 = sqrt.sds[1],
                                   sqrt.sd.2 = sqrt.sd.inner,
                                   sqrt.sd.3 = sqrt.sds[4] )
                }
            }
        }
        else # if ( mix.type == 4 )
        {
            # via Horizontal-Vertical Gradation of 4 (2x2) normal distributions
            if ( eq.sd )
            {
                fm <- d ~ ( 1 - f.t3.p[[1]]( x, mean.1.1, sqrt.sd^2 ) -
                                f.t3.p[[2]]( x, mean.1.2, sqrt.sd^2 ) ) *
                          ( f.t3.d[[1]]( x, mean.1.1, sqrt.sd^2 ) +
                            f.t3.d[[2]]( x, mean.1.2, sqrt.sd^2 ) ) +
                          ( f.t3.p[[1]]( x, mean.2.1, sqrt.sd^2 ) +
                            f.t3.p[[2]]( x, mean.2.2, sqrt.sd^2 ) ) *
                          ( f.t3.d[[1]]( x, mean.2.1, sqrt.sd^2 ) +
                            f.t3.d[[2]]( x, mean.2.2, sqrt.sd^2 ) )

                start <- list( mean.1.1 = means[1],
                               mean.1.2 = means[2],
                               mean.2.1 = means[4],
                               mean.2.2 = means[3],
                               sqrt.sd = sqrt.sd.mid )
            }
            else if ( eq.mean )
            {
                fm <- d ~ ( 1 - f.t3.p[[1]]( x, mean, sqrt.sd.1.1^2 ) -
                                f.t3.p[[2]]( x, mean, sqrt.sd.1.2^2 ) ) *
                          ( f.t3.d[[1]]( x, mean, sqrt.sd.1.1^2 ) +
                            f.t3.d[[2]]( x, mean, sqrt.sd.1.2^2 ) ) +
                          ( f.t3.p[[1]]( x, mean, sqrt.sd.2.1^2 ) +
                            f.t3.p[[2]]( x, mean, sqrt.sd.2.2^2 ) ) *
                          ( f.t3.d[[1]]( x, mean, sqrt.sd.2.1^2 ) +
                            f.t3.d[[2]]( x, mean, sqrt.sd.2.2^2 ) )

                start <- list( mean = ( means.mid[2] + means.mid[3] ) / 2,
                               sqrt.sd.1.1 = sqrt.sds[1],
                               sqrt.sd.1.2 = sqrt.sds[2],
                               sqrt.sd.2.1 = sqrt.sds[4],
                               sqrt.sd.2.2 = sqrt.sds[3] )
            }
            else
            {
                fm <- d ~ ( 1 - f.t3.p[[1]]( x, mean.1.1, sqrt.sd.1.1^2 ) -
                                f.t3.p[[2]]( x, mean.1.2, sqrt.sd.1.2^2 ) ) *
                          ( f.t3.d[[1]]( x, mean.1.1, sqrt.sd.1.1^2 ) +
                            f.t3.d[[2]]( x, mean.1.2, sqrt.sd.1.2^2 ) ) +
                          ( f.t3.p[[1]]( x, mean.2.1, sqrt.sd.2.1^2 ) +
                            f.t3.p[[2]]( x, mean.2.2, sqrt.sd.2.2^2 ) ) *
                          ( f.t3.d[[1]]( x, mean.2.1, sqrt.sd.2.1^2 ) +
                            f.t3.d[[2]]( x, mean.2.2, sqrt.sd.2.2^2 ) )

                start <- list( mean.1.1 = means.mid[1],
                               mean.1.2 = means.mid[2],
                               mean.2.1 = means.mid[4],
                               mean.2.2 = means.mid[3],
                               sqrt.sd.1.1 = sqrt.sds[1],
                               sqrt.sd.1.2 = sqrt.sds[2],
                               sqrt.sd.2.1 = sqrt.sds[4],
                               sqrt.sd.2.2 = sqrt.sds[3] )
            }
        }
    }

    return ( list( formula = fm, start = start ) )
}

################################################################################################
#' [Non-exported] Quartering data
#'
#' Divides numeric data into 4 groups as:
#' at first, it divides the data into two groups by the mean of the data,
#' and next, divides each data into two groups so that the number of elements are equal to
#' each other.
#' If there is an x-coordinate that is exactly equal to the mean,
#' put that element in both the 2nd and 3rd part.
#' If the first dividing makes a group with odd number of elements, put the element at
#' the dividing point in both the lower and upper x-coordinate groups after second dividing.
#' @param   x           A vector of x-coordinates arranged in ascending order
#'                      and non-duplicated.
#' @param   data        A vector of numeric values of the data with respect to x-coordinates.
#' @param   data.mean   Mean value of \code{data}.
#' @return  A list containing components
#'          \item{x}{
#'                  The vectors of x-coordinates of the 4 groups.}
#'          \item{data}{
#'                  The vectors of data of the 4 groups.}
#'          \item{[x|data].lower}{
#'                  The vector of x-coordinates or data integrated the lower-side 2 groups.}
#'          \item{[x|data].upper}{
#'                  The vector of x-coordinates or data integrated the upper-side 2 groups.}
#'          \item{[x|data].outer}{
#'                  The vector of x-coordinates or data integrated the outer-side 2 groups.}
#'          \item{[x|data].inner}{
#'                  The vector of x-coordinates or data integrated the inner-side 2 groups.}
#' @importFrom  utils       head tail
################################################################################################
separate.data.quarter <- function( x, data, data.mean )
{
    xl <- dl <- list( numeric(), numeric(), numeric(), numeric() )

    x.lower <- x[x <= data.mean]
    x.upper <- x[x >= data.mean]
    data.lower <- data[x <= data.mean]
    data.upper <- data[x >= data.mean]

    xl[[1]] <- head( x.lower, ceiling( length( x.lower ) / 2 ) )
    xl[[2]] <- tail( x.lower, ceiling( length( x.lower ) / 2 ) )
    xl[[3]] <- head( x.upper, ceiling( length( x.upper ) / 2 ) )
    xl[[4]] <- tail( x.upper, ceiling( length( x.upper ) / 2 ) )

    dl[[1]] <- head( data.lower, ceiling( length( data.lower ) / 2 ) )
    dl[[2]] <- tail( data.lower, ceiling( length( data.lower ) / 2 ) )
    dl[[3]] <- head( data.upper, ceiling( length( data.upper ) / 2 ) )
    dl[[4]] <- tail( data.upper, ceiling( length( data.upper ) / 2 ) )

    if ( xl[[2]][length( xl[[2]] )] == xl[[3]][1] )
    {
        x.inner <- c( head( xl[[2]], length( xl[[2]] ) - 1 ), xl[[3]] )
        data.inner  <- c( head( dl[[2]], length( dl[[2]] ) - 1 ), dl[[3]] )
    }
    else
    {
        x.inner <- c( xl[[2]], xl[[3]] )
        data.inner  <- c( dl[[2]], dl[[3]] )
    }

    x.outer <- c( xl[[1]], xl[[4]] )
    data.outer  <- c( dl[[1]], dl[[4]] )

    return ( list( x            = xl,           data        = dl,
                   x.lower      = x.lower,      x.upper     = x.upper,
                   data.lower   = data.lower,   data.upper  = data.upper,
                   x.outer      = x.outer,      x.inner     = x.inner,
                   data.outer   = data.outer,   data.inner  = data.inner ) )
}

################################################################################################
#' [Non-exported] Making cmp field with the result of nls
#'
#' Makes a data frame for \code{cmp} field of a \code{\link[ggd]{GGD}} object
#' with the result of \code{\link[stats]{nls}}.
#' @param   coefs       The numeric vector got by \code{\link[stats]{coef}}
#'                      with the result of \code{\link[stats]{nls}}.
#' @param   mix.type    The value for \code{mix.type} field.
#' @param   grad        A character string indicating the method of gradation.
#'                      If "v3", constructing with 3 components is enforcedly,
#'                      even if it is possible to construct with 2 components.
#'
#'                      In this function, \code{grad} argument has no priority over
#'                      \code{mix.type} argument.
#'                      Therefore, \code{grad = "v2"} or \code{"v3"} works
#'                      only if \code{mix.type = 3} is indicated.
#'                      And if \code{mix.type = 3} is indicated,
#'                      either \code{grad = "v2"} or \code{"v3"} must be indicated.
#'
#' @param   eq.mean     A logical; the flag whether to make all of the mean values of
#'                      the normaldistributions of the components to be equal.
#' @param   eq.sd       A logical; the flag whether to make all of the standard deviations of
#'                      the normal distributions of the components to be equal.
#' @return  A data frame for \code{cmp} field
#'          according to the result of \code{\link[stats]{nls}}.
################################################################################################
get.cmp.with.nls.coef <- function( coefs, mix.type, grad, eq.mean, eq.sd )
{
    cmp <- list()
    if ( mix.type == 0 )
    {
        # Normal Distribution
        cmp <- data.frame( mean = unname( coefs["mean"] ),
                           sd = unname( coefs["sqrt.sd"] )^2 )
    }
    else if ( mix.type == 1 || mix.type == 2 || ( mix.type == 3 && grad == "v2" ) )
    {
        # Mean of 2 Normal Distributions and
        # Horizontal or Vertical Gradation of 2 Normal Distributions
        if ( eq.mean )
        {
            mean.1 <- mean.2 <- unname( coefs["mean"] )
        }
        else
        {
            mean.1 <- unname( coefs["mean.1"] )
            mean.2 <- unname( coefs["mean.2"] )
        }

        if ( eq.sd )
        {
            sd.1 <- sd.2 <- unname( coefs["sqrt.sd"] )^2
        }
        else
        {
            sd.1 <- unname( coefs["sqrt.sd.1"] )^2
            sd.2 <- unname( coefs["sqrt.sd.2"] )^2
        }

        cmp <- data.frame( mean = c( mean.1, mean.2 ),
                           sd = c( sd.1, sd.2 ) )
    }
    else if ( mix.type == 3 )
    {
        # Vertical Gradation of 3 Normal Distributions
        if ( eq.sd )
        {
            cmp <- ggd.get.t3.cmp( c( unname( coefs["mean.1"] ),
                                      unname( coefs["mean.2"] ),
                                      unname( coefs["mean.3"] ) ),
                                   rep( unname( coefs["sqrt.sd"] )^2, 3 ), grad )
        }
        else if ( eq.mean )
        {
            cmp <- ggd.get.t3.cmp( rep( unname( coefs["mean"] ), 3 ),
                                   c( unname( coefs["sqrt.sd.1"] )^2,
                                      unname( coefs["sqrt.sd.2"] )^2,
                                      unname( coefs["sqrt.sd.3"] )^2 ), grad )
        }
        else
        {
            cmp <- ggd.get.t3.cmp( c( unname( coefs["mean.1"] ),
                                      unname( coefs["mean.2"] ),
                                      unname( coefs["mean.3"] ) ),
                                   c( unname( coefs["sqrt.sd.1"] )^2,
                                      unname( coefs["sqrt.sd.2"] )^2,
                                      unname( coefs["sqrt.sd.3"] )^2 ), grad )
        }
    }
    else # if ( mix.type == 4 )
    {
        # Horizontal-Vertical Gradation of 4 Normal Distributions
        if ( eq.sd )
        {
            cmp <- data.frame( mean = c( unname( coefs["mean.1.1"] ),
                                         unname( coefs["mean.1.2"] ),
                                         unname( coefs["mean.2.1"] ),
                                         unname( coefs["mean.2.2"] ) ),
                               sd = rep( unname( coefs["sqrt.sd"] )^2, 4 ) )
        }
        else if ( eq.mean )
        {
            cmp <- data.frame( mean = rep( unname( coefs["mean"] ), 4 ),
                               sd = c( unname( coefs["sqrt.sd.1.1"] )^2,
                                       unname( coefs["sqrt.sd.1.2"] )^2,
                                       unname( coefs["sqrt.sd.2.1"] )^2,
                                       unname( coefs["sqrt.sd.2.2"] )^2 ) )
        }
        else
        {
            cmp <- data.frame( mean = c( unname( coefs["mean.1.1"] ),
                                         unname( coefs["mean.1.2"] ),
                                         unname( coefs["mean.2.1"] ),
                                         unname( coefs["mean.2.2"] ) ),
                               sd = c( unname( coefs["sqrt.sd.1.1"] )^2,
                                       unname( coefs["sqrt.sd.1.2"] )^2,
                                       unname( coefs["sqrt.sd.2.1"] )^2,
                                       unname( coefs["sqrt.sd.2.2"] )^2 ) )
        }
    }

    return ( cmp )
}

################################################################################################
#' Correlation between GGD and a frequency distribution
#'
#' Computes correlation coefficients between Gradational Gaussian Distributions and
#' a frequency distribution.
#' Note, in this function, the arguments to give the frequency distribution are
#' two numeric vectors like as other non-exported functions, not a data frame.
#' @export
#' @param   objs    A \code{\link[ggd]{GGD}} object or a list of \code{\link[ggd]{GGD}} objects.
#' @param   x       The vector of the x-coordinates of the frequency distribution.
#' @param   freq    The vector of the frequencies of the frequency distribution.
#' @param   total   Total value of the frequencies.
#' @param   method  The \code{method} argument for \code{\link[stats]{cor}},
#'                  which represents the correlation coefficient method.
#'                  If \code{NULL}, it uses the default method of \code{\link[stats]{cor}}.
#'                  See \code{\link[stats]{cor}} for more information.
#' @return  A vector of correlation coefficients.
#'          Its order follows the order of the elements of the \code{objs} argument.
#'          If \code{cmp} field of an \code{\link[ggd]{GGD}} object in \code{objs} has no rows,
#'          \code{NA_real_} is set to the element.
#' @importFrom  stats   cor
#' @examples
#'  df <- data.frame( x     = seq( -2, 2, 0.2 ),
#'                    freq  = c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'                               7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'                               3698,  2740,  2549,  2284,  1499,  1147,   918 ) )
#'  a <- ggd.nls.freq( df, kind = "Mean-Eq.*Vertical" )$obj
#'  ggd.cor.vs.freq( a, df$x, df$freq )
#'
#'  objs <- list( ggd.nls.freq( df, kind = "Mean-Eq.*Vertical" )$obj,
#'                ggd.nls.freq( df, kind = "Sigma-Eq.*Vertical" )$obj,
#'                ggd.nls.freq( df, kind = "Mean-Diff.*Sigma-Diff.*Vertical" )$obj )
#'  ggd.cor.vs.freq( objs, df$x, df$freq )
################################################################################################
ggd.cor.vs.freq <- function( objs, x, freq, total = sum( freq ), method = NULL )
{
    if ( inherits( objs, "GGD" ) )
    {
        objs <- list( objs )
    }

    if ( is.null( method ) )
    {
        cor.f <- function( cd, fd, m ) cor( cd, fd )
    }
    else
    {
        cor.f <- function( cd, fd, m ) cor( cd, fd, method = m )
    }

    cors <- suppressWarnings(
                tryCatch(
                    vapply( objs,
                            function( obj )
                            {
                                if ( length( obj$mix.type ) == 0 )
                                {
                                    return ( NA_real_ )
                                }

                                cor.f( obj$d( x ), get.d.freq( x, freq, total ), method )
                            }, 0 ) ) )

    return ( cors )
}

################################################################################################
#' [Non-exported] Loop nls for start.level = 100
#'
#' Execute a loop of \code{\link[stats]{nls}} with changing the level of initial guessing.
#' This is the main process of \code{\link[ggd]{nls.freq}} with \code{start.level = 100}.
#' @param   data        A data frame which represents the frequency distribution.
#'                      It must contain 2 numeric columns named \code{x} and \code{freq}.
#' @param   total       Total value of the frequencies.
#' @param   kind        An object indicating the \code{kind} of \code{\link[ggd]{GGD}} object.
#' @param   mix.type    A numeric value to set into \code{mix.type} field of
#'                      the \code{\link[ggd]{GGD}} object as an integer.
#'                      It is an integer from \code{0} to \code{4} or \code{NULL}.
#' @param   grad        A character string indicating the method of gradation.
#'                      See \code{\link[ggd]{nls.freq}} for more information.
#' @param   eq.mean     A logical. If \code{TRUE}, all of the mean values of of the components
#'                      are forced to be equal.
#' @param   eq.sd       A logical. If \code{TRUE}, all of the standard deviations of
#'                      the components are forced to be equal.
#' @param   control     The list for \code{control} argument of \code{\link[stats]{nls}}.
#'                      See \code{\link[stats]{nls.control}} for more information.
#' @param   method      The \code{method} argument for \code{\link[stats]{cor}},
#'                      which represents the correlation coefficient method.
#' @param   ...         Each argument for \code{\link[stats]{nls}} can be indicated.
#'                      See "Arguments" of \code{\link[stats]{nls}} for more information.
#' @return  A list conforming the return value of \code{\link[ggd]{nls.freq}}.
#' @seealso \code{\link[ggd]{nls.freq}}
################################################################################################
nls.freq.level.100 <- function( data, total, kind, mix.type,
                                grad, eq.mean, eq.sd, control, method, ... )
{
    outl <- errl <- wrnl <- NULL

    outl <- lapply( 0:3, function( level )
    {
        suppressMessages(
            suppressWarnings(
                tryCatch(
                    withCallingHandlers(
                        ggd.nls.freq( data          = data,
                                      total         = total,
                                      kind          = kind,
                                      mix.type      = mix.type,
                                      grad          = grad,
                                      eq.mean       = eq.mean,
                                      eq.sd         = eq.sd,
                                      start.level   = level,
                                      control       = control, ... ),
                        warning = function( w )
                        {
                            wrnl <<- append( wrnl, list( list( level = level,
                                                               message = conditionMessage( w ) ) ) )
                        } ),
                    error = function( e )
                    {
                        errl <<- append( errl, list( list( level = level,
                                                           message = conditionMessage( e ) ) ) )
                    } ) ) )
    } )


    cors <- ggd.cor.vs.freq( lapply( outl, function( out ) out$obj ),
                             data$x, data$freq, total, method )
    max.cor <- max( ifelse( is.na( cors ), -Inf, cors ) )
    if ( max.cor == -Inf )
    {
        messages <- paste( unlist(
                            lapply( errl,
                            function( e )
                            {
                                paste0( "level: ", e$level, "\n", e$message )
                            } ) ), collapse="\n" )
        stop( paste0( "Error: All of nls functions have failed. Error messages are:\n",
                              messages ) )
    }

    max.i <- ( 1:length( cors ) )[max.cor == ifelse( is.na( cors ), -Inf, cors )][1]
    result <- append( outl[[max.i]],
                      list( cor = cors, errors = errl, warnings = wrnl ) )
    names( result$cor ) <- paste0( "level.", 0:3 )

    # Report warnings if the "result" is a result with warnings.
    lapply( wrnl, function( w )
    {
        if ( w$level == result$start.level )
        {
            warning( w$message )
        }
    } )

    return ( result )
}

################################################################################################
#' Approximating a frequency distribution with all of supported models
#'
#' Approximates the given frequency distribution with all of distribution models
#' available in this package (the number of models is 16), and compare their accuracies.
#' The accuracy is checked by the correlation coefficients with the frequency distribution
#' computed by \code{\link[stats]{cor}}.
#'
#' The output lists are ordered by \code{ggd:::kinds} as:
#' \enumerate{
#'      \item   Normal Distribution
#'      \item   Mean of Mean-Differed Sigma-Equaled 2 Normal Distributions
#'      \item   Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions
#'      \item   Mean of Mean-Differed Sigma-Differed 2 Normal Distributions
#'      \item   Mean-Differed Sigma-Equaled Horizontal Gradational Distribution
#'      \item   Mean-Equaled Sigma-Differed Horizontal Gradational Distribution
#'      \item   Mean-Differed Sigma-Differed Horizontal Gradational Distribution
#'      \item   2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution
#'      \item   2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution
#'      \item   2-Mean-Differed Sigma-Differed Vertical Gradational Distribution
#'      \item   3-Mean-Differed Sigma-Equaled Vertical Gradational Distribution
#'      \item   3-Mean-Equaled Sigma-Differed Vertical Gradational Distribution
#'      \item   3-Mean-Differed Sigma-Differed Vertical Gradational Distribution
#'      \item   Mean-Differed Sigma-Equaled Horizontal-Vertical Gradational Distribution
#'      \item   Mean-Equaled Sigma-Differed Horizontal-Vertical Gradational Distribution
#'      \item   Mean-Differed Sigma-Differed Horizontal-Vertical Gradational Distribution
#' }
#' Each index numbers of above list are
#' also used for \code{kind.index} field of \code{\link[ggd]{GGD}} class.
#'
#' This function generates 16 \code{\link[ggd]{GGD}} objects and
#' calls \code{\link[ggd]{nls.freq}} method 16 times.
#' By default, \code{\link[ggd]{nls.freq}} is called with \code{warnOnly = TRUE},
#' so \code{\link[ggd]{nls.freq}} does not generate errors, but generates warnings often.
#' When a warning occur, this function generates another warning like
#' "Warning for kind = xx :" to inform which \code{kind.index} gets a poor result
#' (poor, but may be accurate enough).
#' So when one warning has occurred, two warnings will occur eventually.
#'
#' If you indicate \code{warnOnly = FALSE} in \code{control} argument
#' and overwrite \code{warnOnly} option, \code{\link[ggd]{nls.freq}} can generate errors.
#' If an error occur in one of \code{\link[ggd]{nls.freq}} processes,
#' this function throws messages like "Error for kind = xx :" and "Error in ..."
#' instead of throwing error and does not stop,
#' then tries other \code{\link[ggd]{nls.freq}} processes.
#' For the result of error-occurred \code{kind.index}, a cleared \code{\link[ggd]{GGD}} object
#' will be got as an element of \code{obj} (see "Value").
#'
#' @export
#' @param   data    A data frame which represents the frequency distribution.
#'                  It must contain at least 2 numeric columns for \code{x} and \code{freq}.
#'                  See \code{\link[ggd]{nls.freq}} for more information.
#'
#'                  Column names and indexes for \code{x} and \code{freq} are flexible.
#'                  You can specify them with next two arguments.
#'
#' @param   x       The column name or index number for the column of x-coordinates
#'                  in \code{data}.
#'
#' @param   freq    The column name or index number for the column of frequencies
#'                  in \code{data}.
#'
#' @param   total   Total value of the frequencies.
#'                  See \code{\link[ggd]{nls.freq}} for more information.
#'
#' @param   start.level A numeric value of integer from \code{0} to \code{3} and \code{100}
#'                      with default \code{1};
#'                      the level at which to guess the initial \code{start} parameters
#'                      of \code{\link[stats]{nls}}.
#'                      See \code{\link[ggd]{nls.freq}} for more information.
#'
#' @param   start   A \bold{list of lists} with the length of 16
#'                  for each of the \code{start} arguments for \code{\link[stats]{nls}}
#'                  as initial values.
#'                  Each element (a list) will give to the \code{start} argument of
#'                  \code{\link[stats]{nls}} one by one.
#'
#'                  For initial values for which \code{NULL} is indicated as the list
#'                  in the \code{start} argument, internally computed initial values
#'                  depending on \code{start.level} are used.
#'
#'                  As an auxiliary tool for making a list of 16 lists,
#'                  you can use \code{\link[ggd]{ggd.init.start}} function
#'                  which returns a list of 16 \code{NULL}s.
#'                  And to get a template list for each list, you can use
#'                  \code{\link[ggd]{ggd.start.template}} function.
#'
#'                  In addition, \code{\link[ggd]{ggd.kind}} and
#'                  \code{\link[ggd]{ggd.kind.index}} functions may help you whether
#'                  each index number of \code{start} represents what kind of distribution.
#'                  See "Examples" for usages of these tools.
#'
#' @param   control The \code{control} argument for \code{\link[stats]{nls}}.
#'                  See \code{\link[stats]{nls.control}} for more information.
#'
#' @param   not.use.nls A logical. This argument is enabled when \code{start.level} argument is
#'                      other than \code{100}.
#'
#'                  If \code{TRUE}, this function does not use \code{\link[stats]{nls}} and
#'                  it outputs adistribution models using initial values directly.
#'                  If \code{FALSE}, this function uses \code{\link[stats]{nls}}.
#'
#'                  You can use \code{not.use.nls = TRUE} to check the initial values
#'                  when an error has occurred at this function.
#'
#' @param   method  The \code{method} argument for \code{\link[stats]{cor}},
#'                  which represents the correlation coefficient method.
#'                  If \code{NULL}, it uses the default method of \code{\link[stats]{cor}}.
#'                  See \code{\link[stats]{cor}} for more information.
#'
#' @param   ...     Each argument for \code{\link[stats]{nls}} can be indicated.
#'                  See "Arguments" of \code{\link[stats]{nls}} for more information.
#'
#' @return  A list containing components (invisible for \code{GGD} method)
#'          \item{best}{
#'                  The \code{\link[ggd]{GGD}} object which has got
#'                  the highest correlation coefficient.
#'                  That is, the most approximate to the given frequency distribution
#'                  in all of the supported distribution models.
#'                  If there are some models which have got the same highest correlation
#'                  coefficient, the object with the earlier \code{kind.index} is
#'                  given priority.}
#'
#'          \item{best.cor}{
#'                  The correlation coefficient of \code{best} for the given
#'                  frequency distribution.}
#'
#'          \item{obj}{
#'                  The list of all 16 \code{\link[ggd]{GGD}} objects
#'                  ordered by \code{kind.index}; the index number in \code{ggd:::kinds}.
#'                  If an error has occured, the element will be a cleared object.}
#'
#'          \item{cor}{
#'                  The vector of the correlation coefficient of each model for the given
#'                  frequency distribution.
#'                  \code{NA_real_} will be given for an error case or an extremely bad result.}
#'
#'          \item{detail}{
#'                  The list of the all outputs of each \code{\link[ggd]{nls.freq}}.
#'                  Normally, each element is a list of the output of
#'                  \code{\link[ggd]{nls.freq}}.
#'                  If an error has occured, the element will be an error condition object.
#'                  See "Value" of \code{\link[ggd]{nls.freq}} for more information.}
#'
#' @importFrom  methods     new
#' @seealso \code{\link[ggd]{nls.freq}}, \code{\link[stats]{cor}},
#'          \code{\link[ggd]{ggd.init.start}}, \code{\link[ggd]{ggd.start.template}}
#' @examples
#'  ## Preparing.
#'  plot.freq.and.d <- function( obj, x, freq )
#'  {
#'      xlim <- c( min( x ), max( x ) )
#'      ylim <- c( 0, max( ggd:::get.d.freq( x, freq ) ) * 1.2 )
#'      plot( x, ggd:::get.d.freq( x, freq ), xlim = xlim, ylim = ylim, xlab = "", ylab = "" )
#'      par( new = TRUE )
#'      plot( seq( min( x ), max( x ), 0.01 ), obj$d( seq( min( x ), max( x ), 0.01 ) ),
#'            type = "l", xlim = xlim, ylim = ylim )
#'  }
#'
#'  df <- data.frame(
#'              x = seq( -2, 2, 0.2 ),
#'              freq = c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'                        7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'                        3698,  2740,  2549,  2284,  1499,  1147,   918 ) )
#'
#'  ## Try ggd.nls.freq.all.
#'  ## Here, "start.level = 1" is specified to get less-than-ideal results
#'  ## to explain how to use "start" argument.
#'  result <- ggd.nls.freq.all( df, start.level = 1 )
#'
#'  ## Show the results.
#'  result$cor
#'  result$best.cor
#'  result$best
#'
#'  ## Check that the value of cor for kind = 14 is very low.
#'  result$cor[[14]]
#'
#'  ## Let's try to increase the value of cor by changing the initial values.
#'  ##
#'  ## There is an easy and good way to solve this problem,
#'  ## that is to remove "start.level = 1".
#'  ## But here, we use "start" argument with remaining "start.level" for explaining.
#'  ##
#'  ## First, to see what kind = 14 is, display the kind.
#'  result$obj[[14]]$kind
#'
#'  ## Also, using ggd.kind, you can get the character string for the index.
#'  ggd.kind( 14 )
#'
#'  ## Inversely, using ggd.kind.index, you can get index for each kind.
#'  ggd.kind.index( "Mean-Differed Sigma-Equaled Horizontal-Vertical" )
#'
#'  ## Show the cmp field of kind = 14 and plots.
#'  result$obj[[14]]$cmp
#'  plot.freq.and.d( result$obj[[14]], df$x, df$freq )
#'
#'  ## Now, for the initial values, we are going to use the result of
#'  ## kind = "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution".
#'  ggd.kind.index( "2-Mean-Differed Sigma-Equaled Vertical" )  ## 8
#'  result$obj[[8]]$cmp
#'
#'  ## Display the parameters for the initial values for kind = 14.
#'  ggd.start.template( 14 )
#'
#'  ## Set the initial values for kind = 14.
#'  start.list <- ggd.init.start()
#'  start.list[[14]] <- ggd.start.template( 14 )
#'  start.list[[14]]$mean.1.1 <- result$obj[[8]]$cmp$mean[1]
#'  start.list[[14]]$mean.1.2 <- result$obj[[8]]$cmp$mean[2]
#'  start.list[[14]]$mean.2.1 <- result$obj[[8]]$cmp$mean[1]
#'  start.list[[14]]$mean.2.2 <- result$obj[[8]]$cmp$mean[2]
#'  start.list[[14]]$sqrt.sd <- sqrt( result$obj[[8]]$cmp$sd[1] )   ## Set sqrt for SD.
#'
#'  ## Retry ggd.nls.freq.all.
#'  result <- ggd.nls.freq.all( df, start.level = 1, start = start.list )
#'  result$cor
#'  result$cor[[14]]
#'  result$obj[[14]]$cmp
#'  plot.freq.and.d( result$obj[[14]], df$x, df$freq )
################################################################################################
ggd.nls.freq.all <- function( data, x = "x", freq = "freq", total = NULL,
                              start.level = 100, start = NULL,
                              control = list( maxiter = 300, warnOnly = TRUE ),
                              not.use.nls = FALSE, method = NULL, ... )
{
    # Check errors and discard NA and NaN from data.
    data.ext <- extract.freq.data( data, x, freq )

    # Get total.
    if ( is.null( total ) )
    {
        total <- sum( data.ext$freq )
    }
    else if ( length( total ) != 1 || !is.numeric( total ) || is.na( total ) ||
              is.infinite( total ) || total <= 0 )
    {
        stop( "Error: total should be positive finite single value." )
    }

    # Check start.level.
    if ( length( start.level ) != 1 || !is.numeric( start.level ) || is.na( start.level ) ||
         !any( start.level == c( 0:3, 100 ) ) )
    {
        stop( "Error: start.level should be single integer in 0:3 or 100." )
    }

    # Initialize the output object list.
    objs <- list()

    # Initialize start.
    if ( is.null( start ) )
    {
        start <- ggd.init.start()
    }

    # Add warnOnly control.
    if ( is.null( control ) )
    {
        control <- list( warnOnly = TRUE )
    }
    else if ( is.null( control$warnOnly ) )
    {
        control <- append( control, list( warnOnly = TRUE ) )
    }

    # Execule nls.
    # If an error occuer, the error message is displayed but other processes continue.
    results <- lapply( 1:length( kinds ),
    function( i )
    {
        # Remark if an error occur at nls.freq, obj will be a cleared object (not NULL).
        obj <- GGD$new()
        result <- withCallingHandlers(
                    try( obj$nls.freq( data             = data.ext,
                                       x                = "x",
                                       freq             = "freq",
                                       total            = total,
                                       this.kind        = i,
                                       this.mix.type    = NULL,
                                       grad             = "default",
                                       eq.mean          = logical(),
                                       eq.sd            = logical(),
                                       start.level      = start.level,
                                       start            = start[[i]],
                                       control          = control,
                                       not.use.nls      = not.use.nls,
                                       method           = method, ... ),
                         silent = TRUE ),
                    warning = function( w )
                    {
                        warning( paste( "Warning for kind =", i, ":" ) )
                    } )
        if ( inherits( result, "try-error" ) )
        {
            message( paste( "Error for kind =", i, ":" ) )
            message( result )
            result <- attr( result, "condition" )
        }

        list( obj, result )
    } )

    # Summarize the results.
    objs <- lapply( results, function( result ) result[[1]] )
    details <- lapply( results, function( result ) result[[2]] )

    cors <- ggd.cor.vs.freq( objs, data.ext$x, data.ext$freq, total, method )

    best.cor <- max( cors, na.rm = TRUE )

    return ( list( best = objs[( !is.na( cors ) ) & ( cors == best.cor )][[1]],
                   best.cor = best.cor, obj = objs, cor = cors, detail = details ) )
}

################################################################################################
#' Initialize start for ggd.nls.freq.all
#'
#' Generates a list of 16 (= \code{length(ggd:::kinds)}) \code{NULL} lists.
#'
#' If you want to indicate some \code{start} lists for \code{\link[ggd]{ggd.nls.freq.all}}
#' by yourself, you can give the lists to the elements of the returned list of this function,
#' and give it to \code{\link[ggd]{ggd.nls.freq.all}} as \code{start} argument.
#'
#' You don't have to set all elements of the returned list by yourself,
#' since internally calculated default values will be used for \code{start} of
#' \code{\link[stats]{nls}} where the elements of the list are \code{NULL}.
#'
#' The format of each element can be got with \code{\link[ggd]{ggd.start.template}}.
#' In addition, \code{\link[ggd]{ggd.kind}} and \code{\link[ggd]{ggd.kind.index}} functions
#' may help you to know the kind of distribution which the index number represents.
#' @export
#' @return  An all-\code{NULL} list with the length of 16 (= \code{length(ggd:::kinds)}).
#' @seealso \code{\link[ggd]{ggd.nls.freq.all}}, \code{\link[ggd]{ggd.start.template}}
#' @examples
#'  ## Let's approximate this frequency distribution.
#'  df <- data.frame(
#'              x = seq( -2, 2, 0.2 ),
#'              freq = c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'                        7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'                        3698,  2740,  2549,  2284,  1499,  1147,   918 ) )
#'
#'  ## get the initial list
#'  start.list <- ggd.init.start()
#'  start.list
#'
#'  ## check the parameters for ggd:::kinds[14].
#'  ggd.start.template( 14 )
#'
#'  ## set the start parameters of ggd:::kinds[14] to get better result
#'  start.list[[14]]$mean.1.1 <-  0.426831
#'  start.list[[14]]$mean.1.2 <- -0.130757
#'  start.list[[14]]$mean.2.1 <-  0.426831
#'  start.list[[14]]$mean.2.2 <- -0.130757
#'  start.list[[14]]$sqrt.sd <- sqrt( 0.812744 )
#'
#'  ## try ggd.nls.freq.all (the result of ggd:::kinds[14] will be better than ggd:::kinds[8])
#'  result <- ggd.nls.freq.all( df, start.level = 1, start = start.list )
#'  result$cor[8]
#'  result$cor[14]
################################################################################################
ggd.init.start <- function()
{
    return ( lapply( 1:length( kinds ), function( x ) NULL ) )
}

################################################################################################
#' Template for start of nls
#'
#' Gets the template for start list for \code{\link[stats]{nls}}
#' when you want to provide your own \code{start} parameters for \code{\link[ggd]{nls.freq}}
#' or \code{\link[ggd]{ggd.nls.freq.all}}.
#' This function can output only one template list at once.
#' So if you want to get several templates for different distribution kinds,
#' call this function one by one for each distribution kind.
#' @export
#' @param   target      A variable that identifies the kind of distribution to which
#'                      the template applies. The length should be 1.
#'                      The type of the variable is valid for \code{\link[ggd]{GGD}} object,
#'                      the string of an element of \code{ggd:::kinds}, or its index number.
#' @return  A list containing components any of
#'          \item{mean}{
#'                  The start value for mean values common to all normal distributions
#'                  of the components. 0 is preset.}
#'          \item{mean.i}{
#'                  The start value for the mean value of i-th normal distribution.
#'                  0 is preset.}
#'          \item{mean.i.j}{
#'                  The start value for mean value of i,j-th normal distribution
#'                  in the 2x2 components. 0 is preset.}
#'          \item{sqrt.sd}{
#'                  The sqrt of the start value for standard deviations common to
#'                  all normal distributions of the components. 1 is preset.}
#'          \item{sqrt.sd.i}{
#'                  The sqrt of the start values for the standard deviation of
#'                  i-th normal distribution. 1 is preset.}
#'          \item{sqrt.sd.i.j}{
#'                  The sqrt of the start values for the standard deviation of
#'                  i,j-th normal distribution in the 2x2 components. 1 is preset.}
#'
#'          If \code{target} represents unsupported distribution kind, \code{NULL} is returned.
#' @seealso \code{\link[ggd]{nls.freq}}, \code{\link[ggd]{ggd.nls.freq.all}},
#'          \code{\link[ggd]{ggd.init.start}}
#' @examples
#'  ## preparing
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'             7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'             3698,  2740,  2549,  2284,  1499,  1147,   918 )
#'
#'  ## set the start parameters
#'  start <- ggd.start.template( 14 )
#'  start   ## check the parameters for the start of ggd:::kinds[14]
#'
#'  start$mean.1.1 <- -0.671
#'  start$mean.1.2 <- -0.198
#'  start$mean.2.1 <-  0.293
#'  start$mean.2.2 <- -0.198
#'  start$sqrt.sd <- sqrt( 0.640 )  ## sqrt.sd is the square root of the standard deviation.
#'
#'  ## try ggd.nls.freq
#'  ggd.nls.freq( data.frame( x, freq ), start = start, kind = 14 )$obj
################################################################################################
ggd.start.template <- function( target )
{
    kind.index <- ggd.kind.index( target, undef.err = FALSE )[1]
    if ( is.na( kind.index ) || is.null( kind.index ) )
    {
        return ( NULL )
    }

    temp <- list()
    if ( kind.index == 1 )
    {
        temp <- list( mean = 0, sqrt.sd = 1 )
    }
    else if ( kind.index == 2 )
    {
        temp <- list( mean.1 = 0, mean.2 = 0, sqrt.sd = 1 )
    }
    else if ( kind.index == 3 )
    {
        temp <- list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
    }
    else if ( kind.index == 4 )
    {
        temp <- list( mean.1 = 0, mean.2 = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
    }
    else if ( kind.index == 5 )
    {
        temp <- list( mean.1 = 0, mean.2 = 0, sqrt.sd = 1 )
    }
    else if ( kind.index == 6 )
    {
        temp <- list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
    }
    else if ( kind.index == 7 )
    {
        temp <- list( mean.1 = 0, mean.2 = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
    }
    else if ( kind.index == 8 )
    {
        temp <- list( mean.1 = 0, mean.2 = 0, sqrt.sd = 1 )
    }
    else if ( kind.index == 9 )
    {
        temp <- list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
    }
    else if ( kind.index == 10 )
    {
        temp <- list( mean.1 = 0, mean.2 = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
    }
    else if ( kind.index == 11 )
    {
        temp <- list( mean.1 = 0, mean.2 = 0, mean.3 = 0, sqrt.sd = 1 )
    }
    else if ( kind.index == 12 )
    {
        temp <- list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1, sqrt.sd.3 = 1 )
    }
    else if ( kind.index == 13 )
    {
        temp <- list(    mean.1 = 0,    mean.2 = 0,    mean.3 = 0,
                      sqrt.sd.1 = 1, sqrt.sd.2 = 1, sqrt.sd.3 = 1 )
    }
    else if ( kind.index == 14 )
    {
        temp <- list( mean.1.1 = 0, mean.1.2 = 0, mean.2.1 = 0, mean.2.2 = 0, sqrt.sd = 1 )
    }
    else if ( kind.index == 15 )
    {
        temp <- list( mean = 0,
                      sqrt.sd.1.1 = 1, sqrt.sd.1.2 = 1, sqrt.sd.2.1 = 1, sqrt.sd.2.2 = 1 )
    }
    else if ( kind.index == 16 )
    {
        temp <- list( mean.1.1 = 0, mean.1.2 = 0, sqrt.sd.1.1 = 1, sqrt.sd.1.2 = 1,
                      mean.2.1 = 0, mean.2.2 = 0, sqrt.sd.2.1 = 1, sqrt.sd.2.2 = 1 )
    }

    return ( temp )
}

################################################################################################
#' Judge if all mean values are equal
#'
#' Checks if the mean values of all normal distributions of components are equal.
#' @name    is.eq.mean
#' @aliases is.eq.mean
#' @aliases \S4method{is.eq.mean}{GGD}
#' @usage   \S4method{is.eq.mean}{GGD}()
#' @return  \code{TRUE} if all \code{mean} values in \code{cmp} field are equal,
#'          otherwise \code{FALSE}.
#' @examples
#'  a <- GGD$new()
#'  a$kind          ## Normal Distribution
#'  a$is.eq.mean()  ## TRUE
#'
#'  a$trace.q(
#'      data.frame( x = c( -1.92, -0.20, 0.20, 1.92 ), p = c( 0.1, 0.4, 0.6, 0.9 ) ),
#'      grad = "v2" )
#'  a$kind          ## 2-Mean-Differed Sigma-Differed Vertical Gradational Distribution
#'  a$is.eq.mean()  ## FALSE
#'
#'  a <- GGD$new()
#'  a$set.cmp( data.frame( mean = rep( 0, 3 ), sd = c( 1.2, 0.5, 1.3 ) ) )
#'  a$kind          ## 3-Mean-Equaled Sigma-Differed Vertical Gradational Distribution
#'  a$is.eq.mean()  ## TRUE
################################################################################################
NULL
GGD$methods(
    is.eq.mean = function()
    {
        return ( nrow( cmp ) > 0 && all( cmp$mean == cmp$mean[1] ) )
    }
)

################################################################################################
#' Judge if all standard deviations are equal
#'
#' Checks if the standard deviations of all normal distributions of components are equal.
#' @name    is.eq.sd
#' @aliases is.eq.sd
#' @aliases is.eq.sigma
#' @aliases \S4method{is.eq.sd}{GGD}
#' @aliases \S4method{is.eq.sigma}{GGD}
#' @usage   \S4method{is.eq.sd}{GGD}()
#' @usage   \S4method{is.eq.sigma}{GGD}()
#' @return  \code{TRUE} if \code{sd} (= sigma) values in \code{cmp} field are equal,
#'          otherwise \code{FALSE}.
#' @examples
#'  a <- GGD$new()
#'  a$kind          ## Normal Distribution
#'  a$is.eq.sd()    ## TRUE
#'  a$is.eq.sigma() ## TRUE (This method is just a synonym of is.eq.sd)
#'
#'  a$trace.q(
#'      data.frame( x = c( -1.92, -0.20, 0.20, 1.92 ), p = c( 0.1, 0.4, 0.6, 0.9 ) ),
#'      grad = "v2" )
#'  a$kind          ## 2-Mean-Differed Sigma-Differed Vertical Gradational Distribution
#'  a$is.eq.sd()    ## FALSE
#'  a$is.eq.sigma() ## FALSE
#'
#'  a$trace.q(
#'      data.frame( x = c( -0.67, 0, 0.53 ), p = c( 0.25, 0.5, 0.70 ) ),
#'      this.mix.type = 2, eq.sd = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'  a$is.eq.sd()    ## TRUE
#'  a$is.eq.sigma() ## TRUE
################################################################################################
NULL
GGD$methods(
    is.eq.sd = function()
    {
        return ( nrow( cmp ) > 0 && all( cmp$sd == cmp$sd[1] ) )
    }
)

GGD$methods(
    is.eq.sigma = function()
    {
        return ( is.eq.sd() )
    }
)

################################################################################################
#' Judge if normal distribution
#'
#' Refering only \code{cmp} field, checks if the distribution is a normal distribution.
#' Note, this function does not check \code{kind} and \code{kind.index} fields.
#' @name    is.normal
#' @aliases is.normal
#' @aliases \S4method{is.normal}{GGD}
#' @usage   \S4method{is.normal}{GGD}()
#' @return  \code{TRUE} if the object shows a normal distribution, otherwise \code{FALSE}.
#' @examples
#'  a <- GGD$new()
#'  a$is.normal()   ## TRUE
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1.1, 1.1 ) ), this.mix.type = 2 )
#'  a$is.normal()   ## TRUE
#'
#'  a$trace.q(
#'      data.frame( x = c( -1.92, -0.20, 0.20, 1.92 ), p = c( 0.1, 0.4, 0.6, 0.9 ) ),
#'      grad = "v2" )
#'  a$kind; a$cmp
#'  a$is.normal()   ## FALSE
################################################################################################
NULL
GGD$methods(
    is.normal = function()
    {
        return ( is.eq.mean() && is.eq.sd() )
    }
)

################################################################################################
#' Judge if horizontal gradational distribution
#'
#' Refering \code{mix.type} and \code{cmp} field, checks if the distribution is constructed as
#' a horizontal gradational distribution.
#' When \code{mix.type = 4}, if it is essentially a horizontal gradational distribution,
#' this function returns \code{TRUE}.
#' Note, this function does not check \code{kind} and \code{kind.index} fields.
#' @name    is.h
#' @aliases is.h
#' @aliases \S4method{is.h}{GGD}
#' @usage   \S4method{is.h}{GGD}(strict = FALSE)
#' @param   strict  If \code{TRUE}, this function returns \code{TRUE} only if the model
#'                  is a horizontal gradation with 2 different normal distributions.
#'
#'                  If \code{FALSE}, this function also returns \code{TRUE}
#'                  when the model is a normal distribution.
#'
#' @return  \code{TRUE} if the object shows a kind of horizontal gradational distribution,
#'          otherwise \code{FALSE}.
#' @examples
#'  a <- GGD$new()
#'  a$is.h()    ## TRUE
#'
#'  a$set.cmp( data.frame( mean = c( 0, 1 ), sd = c( 1.1, 1.2 ) ), this.mix.type = 2 )
#'  a$is.h()    ## TRUE
#'
#'  a$trace.q(
#'      data.frame( x = c( -1.92, -0.20, 0.20, 1.92 ), p = c( 0.1, 0.4, 0.6, 0.9 ) ),
#'      grad = "v2" )
#'  a$kind      ## 2-Mean-Differed Sigma-Differed Vertical Gradational Distribution
#'  a$is.h()    ## FALSE
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0, 1, 1 ),
#'                         sd = c( 1.1, 1.1, 1.2, 1.2 ) ), this.mix.type = 4 )
#'  a$is.h()    ## TRUE -- this is a horizontal gradational distribution
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1, 1 ) ), this.mix.type = 2 )
#'  a$is.h( strict = FALSE )    ## TRUE  -- this is a horizontal gradational distribution
#'  a$is.h( strict = TRUE )     ## FALSE -- this is a normal distribution
################################################################################################
NULL
GGD$methods(
    is.h = function( strict = FALSE )
    {
        if ( is.normal() )
        {
            return ( !strict )
        }
        else
        {
            return ( isTRUE( mix.type == 2 ) ||
                     ( isTRUE( mix.type == 4 ) && nrow( cmp ) == 4 &&
                       all( cmp$mean[c( 1, 3 )] == cmp$mean[c( 2, 4 )] ) &&
                       all(   cmp$sd[c( 1, 3 )] == cmp$sd[c( 2, 4 )] ) ) )
        }
    }
)

################################################################################################
#' Judge if vertical gradation of 2 normal distributions
#'
#' Refering \code{mix.type} and \code{cmp} field, checks if the distribution is constructed as
#' a vertical gradation of 2 normal distributions.
#' Even when \code{cmp} field has 3 rows with \code{mix.type = 3}, if the components of
#' the left-tail side and the right-tail side are the same, this function returns \code{TRUE},
#' because such a distribution can be reduced to 2-component model.
#' Also, if it is \code{mix.type = 4}
#' and essentially a vertical gradation of 2 normal distributions,
#' this function returns \code{TRUE}.
#' Note, this function does not check \code{kind} and \code{kind.index} fields.
#' @name    is.v2
#' @aliases is.v2
#' @aliases \S4method{is.v2}{GGD}
#' @usage   \S4method{is.v2}{GGD}(strict = FALSE)
#' @param   strict  If \code{TRUE}, this function returns \code{TRUE} only if the model
#'                  is a vertical gradation with 2 different normal distributions.
#'
#'                  If \code{FALSE}, this function also returns \code{TRUE} when the model
#'                  is a normal distribution.
#'
#' @return  \code{TRUE} if the object shows a kind of vertical gradation of
#'          2 normal distributions, otherwise \code{FALSE}.
#' @examples
#'  a <- GGD$new()
#'  a$kind; a$cmp
#'  a$is.v2()       ## TRUE
#'  a$is.v2( TRUE ) ## FALSE
#'
#'  a$trace.q(
#'      data.frame( x = c( -1.92, -0.20, 0.20, 1.92 ), p = c( 0.1, 0.4, 0.6, 0.9 ) ),
#'      grad = "v2" )
#'  a$kind; a$cmp
#'  a$is.v2( TRUE ) ## TRUE
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ),
#'                         sd = c( 1.1, 0.8, 1.1, 0.8 ) ), this.mix.type = 4 )
#'  a$is.v2( TRUE ) ## TRUE -- this is a vertical gradational distribution
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1, 1 ) ), this.mix.type = 3 )
#'  a$is.v2()       ## TRUE  -- this is a vertical gradational distribution
#'  a$is.v2( TRUE ) ## FALSE -- this is a normal distribution
################################################################################################
NULL
GGD$methods(
    is.v2 = function( strict = FALSE )
    {
        if ( is.normal() )
        {
            return ( !strict )
        }
        else
        {
            return ( ( isTRUE( mix.type == 3 ) &&
                       ( nrow( cmp ) == 2 ||
                         ( nrow( cmp ) == 3 && cmp$mean[1] == cmp$mean[nrow( cmp )] &&
                                                 cmp$sd[1] == cmp$sd[nrow( cmp )] ) ) ) ||
                      ( isTRUE( mix.type == 4 ) && nrow( cmp ) == 4 &&
                        all( cmp$mean[1:2] == cmp$mean[3:4] ) &&
                          all( cmp$sd[1:2] == cmp$sd[3:4] ) ) )
        }
    }
)

################################################################################################
#' Judge if vertical gradation of 3 normal distributions
#'
#' Refering \code{mix.type} and \code{cmp} field, checks if the distribution is constructed as
#' a vertical gradation of 3 normal distributions.
#' Note! when \code{mix.type = 4},
#' if the right-top-side and left-top-side components are the same
#' and both tail-side components are different from each other,
#' this method returns \code{FALSE}.
#' Because it is not a vertical gradation of 3 normal distributions, but it contains
#' an element of the horizontal gradation of the 2 tail-side components.
#' Note, this function does not check \code{kind} and \code{kind.index} fields.
#' @name    is.v3
#' @aliases is.v3
#' @aliases \S4method{is.v3}{GGD}
#' @usage   \S4method{is.v3}{GGD}(strict = FALSE)
#' @param   strict  If \code{TRUE}, this function returns \code{TRUE} only if
#'                  the distribution is a vertical gradation with different left-tail-side and
#'                  right-tail-side components.
#'                  Even when the \code{cmp} field has 3 rows with \code{mix.type = 3},
#'                  if the 1st and the 3rd components are the same, it returns \code{FALSE}.
#'
#'                  If \code{FALSE}, this function also returns \code{TRUE} for
#'                  a normal distribution or a vertical gradation of 2 normal distributions.
#'
#' @return  \code{TRUE} if the object shows a kind of vertical gradation of
#'          3 normal distributions, otherwise \code{FALSE}.
#' @examples
#'  a <- GGD$new()
#'  a$kind; a$cmp
#'  a$is.v3()                   ## TRUE
#'  a$is.v3( strict = TRUE )    ## FALSE
#'
#'  a$trace.q(
#'      data.frame( x = c( -1.92, -0.20, 0.21, 1.98 ), p = c( 0.1, 0.4, 0.6, 0.9 ) ),
#'      grad = "v3" )
#'  a$kind; a$cmp
#'  a$is.v3()                   ## TRUE
#'  a$is.v3( strict = TRUE )    ## TRUE
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ),
#'                         sd = c( 1.1, 0.8, 1.2, 0.8 ) ), this.mix.type = 4 )
#'  a$mix.type  ## 4
#'  a$is.v3()   ## FALSE -- see "Note!" at Description
################################################################################################
NULL
GGD$methods(
    is.v3 = function( strict = FALSE )
    {
        if ( is.v2() )
        {
            return ( !strict )
        }
        else
        {
            return ( isTRUE( mix.type == 3 ) && nrow( cmp ) == 3 &&
                     ( cmp$mean[1] != cmp$mean[nrow( cmp )] ||
                         cmp$sd[1] != cmp$sd[nrow( cmp )] ) )
        }
    }
)

################################################################################################
#' Judge if horizontal-vertical gradation
#'
#' Refering \code{mix.type} and \code{cmp} field, checks if the distribution is constructed as
#' a horizontal-vertical gradation of 4 (2x2) normal distributions.
#' @name    is.hv
#' @aliases is.hv
#' @aliases \S4method{is.hv}{GGD}
#' @usage   \S4method{is.hv}{GGD}(strict = FALSE)
#' @param   strict      If \code{TRUE}, this function returns \code{TRUE} only if the model is
#'                      precisely a horizontal-vertical gradational distribution and
#'                      it cannot be represented by any other simpler models.
#'
#'                      If \code{FALSE}, this function also returns \code{TRUE} for
#'                      a normal distribution, a horizontal gradational distribution
#'                      and a 2-component vertical gradational distribution.
#'
#' @return  \code{TRUE} if the object shows a kind of horizontal-vertical gradation of
#'          4 (2x2) normal distributions, therwise \code{FALSE}.
#' @examples
#'  a <- GGD$new()
#'  a$kind; a$cmp
#'  a$is.hv()       ## TRUE
#'  a$is.hv( TRUE ) ## FALSE
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ),
#'                         sd = c( 1.1, 0.8, 1.2, 0.7 ) ), this.mix.type = 4 )
#'  a$is.hv()       ## TRUE
#'  a$is.hv( TRUE ) ## TRUE
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0, 1, 1 ),
#'                         sd = c( 1.1, 1.1, 1.2, 1.2 ) ), this.mix.type = 4 )
#'  a$is.hv()       ## TRUE
#'  a$is.hv( TRUE ) ## FALSE -- this is a horizontal gradational distribution
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ),
#'                         sd = c( 1.1, 0.8, 1.1, 0.8 ) ), this.mix.type = 4 )
#'  a$is.hv()       ## TRUE
#'  a$is.hv( TRUE ) ## FALSE -- this is a vertical gradational distribution
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ),
#'                         sd = c( 1.1, 0.8, 1.2, 0.8 ) ), this.mix.type = 4 )
#'  a$is.hv()       ## TRUE
#'  a$is.hv( TRUE ) ## TRUE -- this is NOT a vertical gradational distribution
################################################################################################
NULL
GGD$methods(
    is.hv = function( strict = FALSE )
    {
        if ( is.h() || is.v2() )
        {
            return ( !strict )
        }
        else
        {
            return ( isTRUE( mix.type == 4 ) && !is.h() && !is.v2() )
        }
    }
)

################################################################################################
#' Judge if symmetric distribution
#'
#' Refering \code{mix.type} and \code{cmp} field,
#' checks if the probability density function is symmectric about the mean.
#'
#' This function judges that the distribution model is symmetric if either:
#' \enumerate{
#'      \item   a normal distribution.
#'      \item   a mean of 2 normal distributions or a horizontal gradational distribution,
#'              where the standard deviations of the two normal distributions of the components
#'              are equal to each other.
#'      \item   a mean of 2 normal distributions or a vertical gradational distribution
#'              of 2 normal distributions, where the mean values of the two normal distributions
#'              of the components are equal to each other.
#'      \item   a vertical gradational distribution of 3 normal distributions,
#'              where the mean of the mean values of the 2 tail-side components is equal to
#'              the mean value of the top-side component, and the standard deviations of
#'              the both tail-side components are equal to each other.
#'      \item   a horizontal-vertical gradational distribution,
#'              where the two vertical gradational distributions of the components are
#'              symmetric about the mean of the distribution.
#' }
#' @name    is.symmetric
#' @aliases is.symmetric
#' @aliases \S4method{is.symmetric}{GGD}
#' @usage   \S4method{is.symmetric}{GGD}()
#' @return  \code{TRUE} if the probability density function of the distribution model is
#'          symmectric about the mean, otherwise \code{FALSE}.
#' @examples
#'  a <- GGD$new()
#'  a$is.symmetric()    ## TRUE
#'
#'  a$set.cmp( data.frame( mean = c( -0.35, 0.35 ), sd = c( 1, 1 ) ), grad = "h" )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'  a$is.symmetric()    ## TRUE
#'
#'  a$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1.1, 0.8 ) ), grad = "v2" )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'  a$is.symmetric()    ## TRUE
#'
#'  a$adjust.cmp( grad = "hv" )
#'  a$is.symmetric()    ## TRUE
#'
#'  a$set.cmp( data.frame( mean = c( -0.8, -0.2, 0.8, 0.2 ), sd = c( 1.2, 0.9, 1.2, 0.9 ) ) )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'  a$is.symmetric()    ## TRUE
#'
#'  a$set.cmp( data.frame( mean = c( -0.8, -0.2, 0.2, 0.8 ), sd = c( 1.2, 0.9, 1.2, 0.9 ) ) )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'  a$is.symmetric()    ## FALSE
################################################################################################
NULL
GGD$methods(
    is.symmetric = function()
    {
        return( is.normal() ||
                ( is.eq.mean()  && ( isTRUE( mix.type == 1 ) ||
                                     is.v2( strict = FALSE ) ) ) ||
                ( is.eq.sd()    && ( isTRUE( mix.type == 1 ) ||
                                     is.h( strict = FALSE ) ) ) ||
                ( isTRUE( mix.type == 3 ) && nrow( cmp ) == 3 &&
                                   ( ( cmp$mean[1] + cmp$mean[3] ) / 2 == cmp$mean[2] &&
                                     cmp$sd[1] == cmp$sd[3] ) ) ||
                ( isTRUE( mix.type == 4 ) && nrow( cmp ) == 4 &&
                                   ( cmp$mean[1] == cmp$mean[2] + cmp$mean[4] - cmp$mean[3] &&
                                     cmp$sd[1] == cmp$sd[3] && cmp$sd[2] == cmp$sd[4] ) ) )
    }
)

################################################################################################
#' Probability density function
#'
#' Gets the values of the probability density function for the given x-coordinates.
#' This function works like \code{\link[stats]{dnorm}} for a normal distribution.
#' @name    d
#' @aliases d
#' @aliases \S4method{d}{GGD}
#' @usage   \S4method{d}{GGD}(x)
#' @param   x           A vector of x-coordinates.
#' @return  The values of the probability density function for the given x-coordinates.
#' @importFrom  stats   dnorm pnorm
#' @examples
#'  a <- GGD$new()
#'  a$trace.q(
#'      data.frame( x = c( -0.67, 0, 0.53 ), p = c( 0.25, 0.5, 0.75 ) ),
#'      this.mix.type = 2 )
#'  a$d( c( -0.67, 0, 0.53 ) )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
################################################################################################
NULL
GGD$methods(
    d = function( x )
    {
        results <- vapply( x, function( x )
        {
            if ( length( mix.type ) == 0 || is.na( mix.type ) )
            {
                result <- NaN
            }
            else if ( is.normal() )
            {
                result <- dnorm( x, cmp$mean[1], cmp$sd[1] )
            }
            else if ( mix.type == 4 )
            {
                p.1 <- f.t3.p[[1]]( x, cmp$mean[1], cmp$sd[1] ) +
                       f.t3.p[[2]]( x, cmp$mean[2], cmp$sd[2] )
                p.2 <- f.t3.p[[1]]( x, cmp$mean[3], cmp$sd[3] ) +
                       f.t3.p[[2]]( x, cmp$mean[4], cmp$sd[4] )

                result <- ( 1 - p.1 ) *
                          ( f.t3.d[[1]]( x, cmp$mean[1], cmp$sd[1] ) +
                            f.t3.d[[2]]( x, cmp$mean[2], cmp$sd[2] ) ) +
                          p.2 * ( f.t3.d[[1]]( x, cmp$mean[3], cmp$sd[3] ) +
                                  f.t3.d[[2]]( x, cmp$mean[4], cmp$sd[4] ) )
            }
            else if ( mix.type == 3 )
            {
                right.tail.index <- ifelse( is.v2( strict = FALSE ), 1, 3 )
                result <- dp.t3( x, c( cmp$mean[1], cmp$mean[2], cmp$mean[right.tail.index] ),
                                    c( cmp$sd[1], cmp$sd[2], cmp$sd[right.tail.index] ), f.t3.d )
            }
            else if ( mix.type == 2 )
            {
                result <- ( 1 - pnorm( x, cmp$mean[1], cmp$sd[1] ) ) *
                                dnorm( x, cmp$mean[1], cmp$sd[1] ) +
                          pnorm( x, cmp$mean[2], cmp$sd[2] ) *
                          dnorm( x, cmp$mean[2], cmp$sd[2] )
            }
            else if ( mix.type == 1 )
            {
                result <- ( dnorm( x, cmp$mean[1], cmp$sd[1] ) +
                            dnorm( x, cmp$mean[2], cmp$sd[2] ) ) / 2
            }
            else
            {
                stop( "Error: mix.type is invalid." )
            }

            return ( result )
        }, 0 )

        return ( results )
    }
)

################################################################################################
#' Cumulative distribution function
#'
#' Gets the probabilities of that a value of the random variable is less than or equal to
#' the given x-coordinates.
#' This function works like \code{\link[stats]{pnorm}} for a normal distribution.
#' @name    p
#' @aliases p
#' @aliases \S4method{p}{GGD}
#' @usage   \S4method{p}{GGD}(x)
#' @param   x               A vector of x-coordinates.
#' @return  The probabilities of that x is less than or equal to given x-coordinates.
#' @importFrom  stats       pnorm
#' @examples
#'  a <- GGD$new()
#'  a$trace.q(
#'      data.frame( x = c( -0.67, 0, 0.53 ), p = c( 0.25, 0.5, 0.75 ) ),
#'      this.mix.type = 2 )
#'  a$p( c( -0.67, 0, 0.53 ) )
#'  plot( seq( -3, 3, 0.01 ), a$p( seq( -3, 3, 0.01 ) ), type = "l" )
################################################################################################
NULL
GGD$methods(
    p = function( x )
    {
        results <- vapply( x, function( x )
        {
            if ( length( mix.type ) == 0 || is.na( mix.type ) )
            {
                result <- NaN
            }
            else if ( is.normal() )
            {
                result <- pnorm( x, cmp$mean[1], cmp$sd[1] )
            }
            else if ( mix.type == 4 )
            {
                p.1 <- f.t3.p[[1]]( x, cmp$mean[1], cmp$sd[1] ) +
                       f.t3.p[[2]]( x, cmp$mean[2], cmp$sd[2] )
                p.2 <- f.t3.p[[1]]( x, cmp$mean[3], cmp$sd[3] ) +
                       f.t3.p[[2]]( x, cmp$mean[4], cmp$sd[4] )

                result <- p.1 - p.1^2 / 2 + p.2^2 / 2
            }
            else if ( mix.type == 3 )
            {
                right.tail.index <- ifelse( is.v2( strict = FALSE ), 1, 3 )
                result <- dp.t3( x, c( cmp$mean[1], cmp$mean[2], cmp$mean[right.tail.index] ),
                                    c( cmp$sd[1], cmp$sd[2], cmp$sd[right.tail.index] ),
                                 f.t3.p )
            }
            else if ( mix.type == 2 )
            {
                p1 <- pnorm( x, cmp$mean[1], cmp$sd[1] )
                p2 <- pnorm( x, cmp$mean[2], cmp$sd[2] )
                result <- p1 - ( p1 * p1 - p2 * p2 ) / 2
            }
            else if ( mix.type == 1 )
            {
                result <- ( pnorm( x, cmp$mean[1], cmp$sd[1] ) +
                            pnorm( x, cmp$mean[2], cmp$sd[2] ) ) / 2
            }
            else
            {
                stop( "Error: mix.type is invalid." )
            }

            return ( result )
        }, 0 )

        return ( results )
    }
)

################################################################################################
#' Quantile function
#'
#' Gets the x-coordinates at which the value of the cumulative distribution function will be
#' equal to the given probabilities.
#' This function works like \code{\link[stats]{qnorm}} for a normal distribution.
#' @name    q
#' @aliases q
#' @aliases \S4method{q}{GGD}
#' @usage   \S4method{q}{GGD}(prob, tol = .Machine$double.eps * 16)
#' @param   prob            A vector of probabilities.
#' @param   tol             The tolerance level for the convergence criterion.
#' @return  The x-coordinates at which the value of the cumulative distribution function
#'          will be equal to the given probabilities.
#' @importFrom  stats       qnorm
#' @examples
#'  a <- GGD$new()
#'  a$trace.q(
#'      data.frame( x = c( -0.67, 0, 0.53 ), p = c( 0.25, 0.5, 0.75 ) ),
#'      this.mix.type = 2 )
#'  a$q( c( 0.25, 0.5, 0.75 ) )
#'  plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" )
################################################################################################
NULL
GGD$methods(
    q = function( prob, tol = .Machine$double.eps * 16 )
    {
        means <- cmp$mean
        sds <- cmp$sd

        # To prepare for the process,
        # get the maximum and minimum values of the mean values and standard deviations
        # of the normal distributions of the components are obtained, and
        # secure the vector to store the candidates of upper and lower bounds of the solution.
        if ( isTRUE( mix.type == 4 ) )
        {
            min.mean <- min( means )
            max.mean <- max( means )
            min.sd <- min( sds[1], sds[2] * sqrt( 2 ) / 2, sds[3], sds[4] * sqrt( 2 ) / 2 )
            max.sd <- max( sds[1], sds[2] * sqrt( 2 ) / 2, sds[3], sds[4] * sqrt( 2 ) / 2 )
            qs <- numeric( 4 )
        }
        else if ( isTRUE( mix.type == 3 ) )
        {
            if ( length( means ) == 2 )
            {
                # Treat as same as 3 components.
                means[3] <- means[1]
                sds[3] <- sds[1]
            }

            sds.star <- sds * sqrt( 2 ) / 2

            max.mean.a <- max( means[1:2] )
            min.mean.a <- min( means[1:2] )
            max.mean.b <- max( means[2:3] )
            min.mean.b <- min( means[2:3] )

            max.sd.a <- max( sds[1], sds.star[2] )
            #min.sd.a <- min( sds[1], sds.star[2] ) # no need.
            min.sd.a.2 <- min( sds.star[1], sds.star[2] )
            max.sd.b <- max( sds[3], sds.star[2] )
            min.sd.b <- min( sds[3], sds.star[2] )
            min.sd.b.2 <- min( sds.star[3], sds.star[2] )
            qs <- numeric( 6 )
        }
        else if ( isTRUE( any( mix.type == 1:2 ) ) )
        {
            qs <- numeric( 2 )
        }

        # Find the upper and lower bounds of the quantile function,
        # and then find the value of the function with the bisection method using the bounds.
        results <- vapply( prob, function( prob )
        {
            if ( length( mix.type ) == 0 || is.na( mix.type ) )
            {
                return ( NaN )
            }

            if ( prob < 0 || prob > 1 )
            {
                return ( NaN )
            }
            else if ( prob == 0 )
            {
                return ( -Inf )
            }
            else if ( prob == 1 )
            {
                return ( Inf )
            }
            else if ( prob == 0.5 )
            {
                result <- median
            }

            if ( is.normal() )
            {
                result <- qnorm( prob, means[1], sds[1] )
            }
            else
            {
                if ( mix.type == 4 )
                {
                    p.2.m.sqrt2 <- ( 2 - sqrt( 2 ) ) * prob
                    qs[1] <- qnorm( p.2.m.sqrt2, min.mean, min.sd )
                    qs[2] <- qnorm( p.2.m.sqrt2, min.mean, max.sd )
                    qs[3] <- qnorm( p.2.m.sqrt2 + sqrt( 2 ) - 1, max.mean, min.sd )
                    qs[4] <- qnorm( p.2.m.sqrt2 + sqrt( 2 ) - 1, max.mean, max.sd )

                    if ( prob < 0.5 )
                    {
                        result <- bisection( function( x ) { p( x ) - prob },
                                             c( min( qs[1:2] ), median ), tol )
                    }
                    else
                    {
                        result <- bisection( function( x ) { p( x ) - prob },
                                             c( median, max( qs[3:4] ) ), tol )
                    }
                }
                else if ( mix.type == 3 )
                {
                    if ( prob >= p( means[1] ) && prob <= p( means[3] ) )
                    {
                        result <- qnorm( sqrt( 2 ) * ( prob - 0.5 ) + 0.5,
                                         means[2], sds.star[2] )
                    }
                    else
                    {
                        if ( prob < 0.5 )
                        {
                            qs[1] <- median
                            qs[2] <- qnorm( ( 2 - sqrt( 2 ) ) * prob, min.mean.a, max.sd.a )
                            qs[3] <- qnorm( prob, min.mean.b, max.sd.b )
                            qs[4] <- median
                            qs[5] <- qnorm( prob, max.mean.a, min.sd.a.2 )
                            qs[6] <- qnorm( ( 2 - sqrt( 2 ) ) * prob + sqrt( 2 ) - 1,
                                            max.mean.b, min.sd.b )
                            result <- bisection( function( x ) { p( x ) - prob },
                                                 c( min( qs[1:3] ), max( qs[4:6] ) ), tol )
                        }
                        else
                        {
                            # Note the median position for speed.
                            qs[1] <- qnorm( ( 2 - sqrt( 2 ) ) * prob, min.mean.a, sds[1] )
                            qs[2] <- qnorm( ( 2 - sqrt( 2 ) ) * prob, min.mean.a, sds.star[2] )
                            qs[3] <- qnorm( prob, min.mean.b, min.sd.b.2 )
                            qs[4] <- median
                            qs[5] <- qnorm( prob, max.mean.a, max.sd.a )
                            qs[6] <- qnorm( ( 2 - sqrt( 2 ) ) * prob + sqrt( 2 ) - 1,
                                            max.mean.b, max.sd.b )
                            result <- bisection( function( x ) { p( x ) - prob },
                                                 c( min( qs[1:4] ), max( qs[4:6] ) ), tol )
                        }
                    }
                }
                else if ( any( mix.type == 1:2 ) )
                {
                    # mix.type = 1 or 2 =>
                    # The upper and lower bounds of the quantile function are
                    # held by the values of the two quantile functions
                    # of the normal distributions of the components.
                    qs[1] <- qnorm( prob, means[1], sds[1] )
                    qs[2] <- qnorm( prob, means[2], sds[2] )
                    if ( qs[1]  == qs[2] )
                    {
                        result <- qs[1]
                    }
                    else
                    {
                        result <- bisection( function( x ) { p( x ) - prob }, qs, tol )
                    }
                }
                else
                {
                    stop( "Error: mix.type is invalid." )
                }
            }

            return ( result )
        }, 0 )

        return ( results )
    }
)

################################################################################################
#' Random generation
#'
#' Generates random numbers which follow the distribution model.
#' This function works like \code{\link[stats]{rnorm}} for a normal distribution.
#' @name    r
#' @aliases r
#' @aliases \S4method{r}{GGD}
#' @usage   \S4method{r}{GGD}(n, tol = 2^(-17))
#' @param   n       Number of output values.
#'                  If \code{length( n ) > 1}, the length is taken to be the number required.
#' @param   tol     The tolerance level for the convergence criterion for \code{\link[ggd]{q}}.
#' @return  A vector of random numbers.
#' @examples
#'  a <- GGD$new()
#'  a$trace.q(
#'      data.frame( x = c( -0.67, 0, 0.53 ), p = c( 0.25, 0.5, 0.75 ) ),
#'      this.mix.type = 2 )
#'  hist( a$r( 400 ) )
################################################################################################
NULL
GGD$methods(
    r = function( n, tol = 2^(-17) )
    {
        return ( q( runif( n, 0, 1 ), tol ) )
    }
)

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

################################################################################################
#' Output TeX format
#'
#' Outputs the TeX-formatted texts of the formulas for the probability density function
#' \eqn{g(x)} and the cumulative distribution function \eqn{\Psi(x)}
#' with \code{\link[base]{writeLines}}.
#' @name    tex
#' @aliases tex
#' @aliases tex.d
#' @aliases tex.p
#' @aliases \S4method{tex}{GGD}
#' @aliases \S4method{tex.d}{GGD}
#' @aliases \S4method{tex.p}{GGD}
#' @usage   \S4method{tex}{GGD}(con = stdout(), sep = "\n", comma = TRUE,
#'          format.num = function(x) format(x),
#'          frac.env = c("array", "aligned", "gathered", "default"))
#' @usage   \S4method{tex.d}{GGD}(con = stdout(), sep = "\n", comma = TRUE,
#'          format.num = function(x) format(x),
#'          frac.env = c("array", "aligned", "gathered", "default"))
#' @usage   \S4method{tex.p}{GGD}(con = stdout(), sep = "\n", comma = TRUE,
#'          format.num = function(x) format(x),
#'          frac.env = c("array", "aligned", "gathered", "default"))
#' @param   con         A \code{\link[base]{connection}} object or a character to indicate
#'                      the output destination.
#'                      See "Details" at \code{\link[base]{writeLines}}
#'                      for more information.
#' @param   sep         A string to be written to the connection after each line of text.
#'                      See "Details" at \code{\link[base]{writeLines}}
#'                      for more information.
#' @param   comma       A logical. If \code{TRUE}, this method writes "," as a separator
#'                      between each expression and "." at the end of output.
#'                      If \code{FALSE}, "," for the separator of expressions
#'                      and the final "." are not written.
#' @param   format.num  A function to format each number of value of a parameter;
#'                      mean value and standard deviation.
#'                      It should be a function with one argument for the number
#'                      to be displayed and returning a character string.
#' @param   frac.env    The TeX environment for formatting fractions with different heights
#'                      of numerator and denominator.
#'                      If \code{"default"}, such fractions will be formatted as default of TeX
#'                      (usually not so beautiful). Which environment is the most beautiful
#'                      form will depend on the TeX execution environment.
#' @return  An invisible NULL.
#' @seealso \code{\link[ggd]{adjust.cmp}}, \code{\link[ggd]{tex.d}}, \code{\link[ggd]{tex.p}}
#' @details
#'  \subsection{Composition of outputs}{
#'      \code{tex} outputs the formulas of both probability density function
#'      and cumulative distribution function.
#'
#'      Where \code{mix.type} field is from \code{0} to \code{3},
#'      this function outputs 3 parts divided with one empty line as:
#'      \itemize{
#'          \item Fomulas of the probability density function \eqn{g(x)}
#'              and the cumulative distribution function \eqn{\Psi(x)} of the model.
#'          \item Fomulas of the probability density function \eqn{f_i(x)}
#'              and the cumulative distribution function \eqn{\Phi_i(x)} of the components.
#'          \item Values of parameters of the mean values \eqn{\mu_i} and
#'              the standard deviations \eqn{\sigma_i} of the components.
#'      }
#'
#'      Where \code{mix.type = 4},
#'      the formulas of \eqn{g(x)} and \eqn{\Psi(x)} are divided into 2 parts in addition,
#'      and subscripts for \eqn{f(x)}, \eqn{\Phi(x)} and parameters are 2 like \eqn{f_{i,j}(x)}.
#'
#'      \code{tex.d} outputs the formulas of probability density function only.
#'
#'      \code{tex.p} outputs the formulas of cumulative distribution function only.
#'
#'      In these mothods, the formulas are output according to \code{mix.type}
#'      and the number of components in \code{cmp} field, not \code{kind} or \code{kind.index}.
#'      That is, for example, if the \code{kind} is \code{"Normal Distribution"}
#'      and \code{mix.type = 2},
#'      \code{tex} outputs fomulas of a horizontal gradational distribution.
#'      In such a case, if you want to display a normal distribution formula,
#'      you should simplify \code{mix.type} with \code{\link[ggd]{adjust.cmp}} in advance.
#'
#'      If \code{cmp} field has no rows, nothing is output.
#'  }
#'
#'  \subsection{Equaled mean values or standard deviations}{
#'      For clarity, when all mean values or standard deviations of components are equal
#'      (i.e., when \code{\link[ggd]{is.eq.mean}} or \code{\link[ggd]{is.eq.sd}} method
#'      returns \code{TRUE}), they are displayed with \eqn{=} to the 1st component parameter,
#'      like as \eqn{\sigma_2 = \sigma_1}.
#'
#'      If only the values of some parameters are equal (e.g., only \eqn{\sigma_2} and
#'      \eqn{\sigma_3} are equal and \eqn{\sigma_1} is different),
#'      they are not displayed as \eqn{\sigma_3 = \sigma_2}, but displayed with values
#'      to avoid misreading the subscripts.
#'
#'      Note that if the difference between the values of parameters is smaller than
#'      the number of decimal places displayed, \eqn{\sigma_2 = \sigma_1} may not be displayed,
#'      but the same number may be displayed for each.
#'  }
#' @examples
#'  a <- GGD$new()
#'  a$set.cmp( data.frame( mean = c( 0.018205, -0.011362 ),
#'                           sd = c( 1.131168,  0.705948 ) ),
#'             this.mix.type = 2 )
#'  a$tex()
#'  a$tex.d()
#'  a$tex.p()
#'
#'  a$set.cmp( data.frame( mean = c( 1.2658, -0.5 ),
#'                           sd = c( 2.7,    1.206879 ) ),
#'             this.mix.type = 3 )
#'  a$tex( comma = FALSE, format.num = function(x) format(x, digits = 3) )
#'  a$tex.d( comma = FALSE, format.num = function(x) as.character(x) )
#'  a$tex.p( comma = FALSE, format.num = function(x) sprintf("%.3f", x), frac.env = "aligned" )
################################################################################################
NULL
GGD$methods(
    tex = function( con = stdout(), sep = "\n", comma = TRUE,
                    format.num = function( x ) format( x ),
                    frac.env = c( "array", "aligned", "gathered", "default" ) )
    {
        if ( nrow( cmp ) == 0 )
        {
            return ( invisible( NULL ) )
        }

        comma <- isTRUE( comma )
        frac.env <- match.arg( frac.env )

        tex.body <- ""
        if ( mix.type < 3 )
        {
            tex.body <- c( tex.d.main[[mix.type + 1]], tex.p.main[[mix.type + 1]], "\\\\",
                            tex.d.sub[[mix.type + 1]],  tex.p.sub[[mix.type + 1]] )
        }
        else if ( mix.type == 3 )
        {
            if ( nrow( cmp ) == 2 )
            {
                tex.body <- c( tex.d.main[[4]][[1]], tex.p.main[[4]][[1]], "\\\\",
                                tex.d.sub[[4]][[1]],  tex.p.sub[[4]][[1]] )
            }
            else
            {
                tex.body <- c( tex.d.main[[4]][[2]], tex.p.main[[4]][[2]], "\\\\",
                                tex.d.sub[[4]][[2]],  tex.p.sub[[4]][[2]], "\\\\",
                                tex.d.sub[[4]][[1]],  tex.p.sub[[4]][[1]] )
            }
        }
        else if ( mix.type == 4 )
        {
            tex.body <- c( tex.d.main[[5]], tex.p.main[[5]], "\\\\",
                            tex.p.sub[[5]], "\\\\",
                            tex.d.sub[[5]] )
        }

        tex.form <- gsub( "%begin-frac-env%", tex.begin.frac.env[frac.env],
                          gsub( "%end-frac-env%", tex.end.frac.env[frac.env],
                                c( tex.body, get.cmp.tex( .self, format.num ) ) ) )

        if ( comma )
        {
            tex.form <- c( tex.form.header,
                           gsub( ";", ",", gsub( ";;", ".", tex.form ) ),
                           tex.form.footer )
        }
        else
        {
            tex.form <- c( tex.form.header,
                           gsub( ";", "", tex.form ),
                           tex.form.footer )
        }

        writeLines( tex.form, con, sep )
    }
)

GGD$methods(
    tex.d = function( con = stdout(), sep = "\n", comma = TRUE,
                      format.num = function( x ) format( x ),
                      frac.env = c( "array", "aligned", "gathered", "default" ) )
    {
        if ( nrow( cmp ) == 0 )
        {
            return ( invisible( NULL ) )
        }

        comma <- isTRUE( comma )
        frac.env <- match.arg( frac.env )

        tex.body <- ""
        if ( mix.type < 2 )
        {
            tex.body <- c( tex.d.main[[mix.type + 1]], "\\\\",
                            tex.d.sub[[mix.type + 1]] )
        }
        else if ( mix.type == 2 )
        {
            # Where mix.type = 2, tex.p.sub is needed because g_i(x) uses \Psi_i(x).
            tex.body <- c( tex.d.main[[3]], "\\\\",
                            tex.p.sub[[3]], tex.d.sub[[3]] )
        }
        else if ( mix.type == 3 )
        {
            if ( nrow( cmp ) == 2 )
            {
                tex.body <- c( tex.d.main[[4]][[1]], "\\\\",
                                tex.d.sub[[4]][[1]] )
            }
            else
            {
                tex.body <- c( tex.d.main[[4]][[2]], "\\\\",
                                tex.d.sub[[4]][[2]], "\\\\",
                                tex.d.sub[[4]][[1]] )
            }
        }
        else if ( mix.type == 4 )
        {
            # Where mix.type = 4, tex.p.sub.4 is needed because g_i(x) uses \Psi_i(x).
            tex.body <- c( tex.d.main[[5]], "\\\\",
                            tex.p.sub[[5]], "\\\\",
                            tex.d.sub[[5]] )
        }

        tex.form <- gsub( "%begin-frac-env%", tex.begin.frac.env[frac.env],
                          gsub( "%end-frac-env%", tex.end.frac.env[frac.env],
                                c( tex.body, get.cmp.tex( .self, format.num ) ) ) )

        if ( comma )
        {
            tex.form <- c( tex.form.header,
                           gsub( ";", ",", gsub( ";;", ".", tex.form ) ),
                           tex.form.footer )
        }
        else
        {
            tex.form <- c( tex.form.header,
                           gsub( ";", "", tex.form ),
                           tex.form.footer )
        }

        writeLines( tex.form, con, sep )
    }
)

GGD$methods(
    tex.p = function( con = stdout(), sep = "\n", comma = TRUE,
                      format.num = function( x ) format( x ),
                      frac.env = c( "array", "aligned", "gathered", "default" ) )
    {
        if ( nrow( cmp ) == 0 )
        {
            return ( invisible( NULL ) )
        }

        comma <- isTRUE( comma )
        frac.env <- match.arg( frac.env )

        tex.body <- ""
        if ( mix.type < 3 )
        {
            tex.body <- c( tex.p.main[[mix.type + 1]], "\\\\",
                            tex.p.sub[[mix.type + 1]] )
        }
        else if ( mix.type == 3 )
        {
            if ( nrow( cmp ) == 2 )
            {
                tex.body <- c( tex.p.main[[4]][[1]], "\\\\",
                                tex.p.sub[[4]][[1]] )
            }
            else
            {
                tex.body <- c( tex.p.main[[4]][[2]], "\\\\",
                                tex.p.sub[[4]][[2]], "\\\\",
                                tex.p.sub[[4]][[1]] )
            }
        }
        else if ( mix.type == 4 )
        {
            tex.body <- c( tex.p.main[[5]], "\\\\",
                            tex.p.sub[[5]] )
        }

        tex.form <- gsub( "%begin-frac-env%", tex.begin.frac.env[frac.env],
                          gsub( "%end-frac-env%", tex.end.frac.env[frac.env],
                                c( tex.body, get.cmp.tex( .self, format.num ) ) ) )

        if ( comma )
        {
            tex.form <- c( tex.form.header,
                           gsub( ";", ",", gsub( ";;", ".", tex.form ) ),
                           tex.form.footer )
        }
        else
        {
            tex.form <- c( tex.form.header,
                           gsub( ";", "", tex.form ),
                           tex.form.footer )
        }

        writeLines( tex.form, con, sep )
    }
)

################################################################################################
#' [Non-exported] TeX format for mean and standard deviation
#'
#' Gets the TeX-formatted texts to represent the mean values and standard deviations of
#' the normal dintributions of the components.
#' @param   obj         A \code{\link[ggd]{GGD}} object.
#' @param   format.num  A function to format each number of value of a parameter;
#'                      mean value and standard deviation. See \code{\link[ggd]{tex}}.
#' @return  TeX-formatted texts to represent the mean values and standard deviations of
#'          the normal dintributions of the components.
################################################################################################
get.cmp.tex <- function( obj, format.num )
{
    if ( nrow( obj$cmp ) == 1 )
    {
        tex <- sub( "mean", format.num( obj$mean ), tex.val.sub[[1]] )
        tex <- sub(   "sd", format.num( obj$sd ), tex )
    }
    else if ( nrow( obj$cmp ) == 2 )
    {
        tex <- sub( "mean.1", format.num( obj$cmp$mean[1] ), tex.val.sub[[2]] )
        tex <- sub(   "sd.1", format.num( obj$cmp$sd[1] ), tex )

        if ( obj$is.eq.mean() )
        {
            tex <- sub( "mean.2", "\\\\mu_1", tex )
        }
        else
        {
            tex <- sub( "mean.2", format.num( obj$cmp$mean[2] ), tex )
        }

        if ( obj$is.eq.sd() )
        {
            tex <- sub( "sd.2", "\\\\sigma_1", tex )
        }
        else
        {
            tex <- sub( "sd.2", format.num( obj$cmp$sd[2] ), tex )
        }
    }
    else if ( nrow( obj$cmp ) == 3 )
    {
        tex <- sub( "mean.1", format.num( obj$cmp$mean[1] ), tex.val.sub[[3]] )
        tex <- sub(   "sd.1", format.num( obj$cmp$sd[1] ), tex )

        if ( obj$is.eq.mean() )
        {
            tex <- sub( "mean.2", "\\\\mu_1", tex )
            tex <- sub( "mean.3", "\\\\mu_1", tex )
        }
        else
        {
            tex <- sub( "mean.2", format.num( obj$cmp$mean[2] ), tex )
            tex <- sub( "mean.3", format.num( obj$cmp$mean[3] ), tex )
        }

        if ( obj$is.eq.sd() )
        {
            tex <- sub( "sd.2", "\\\\sigma_1", tex )
            tex <- sub( "sd.3", "\\\\sigma_1", tex )
        }
        else
        {
            tex <- sub( "sd.2", format.num( obj$cmp$sd[2] ), tex )
            tex <- sub( "sd.3", format.num( obj$cmp$sd[3] ), tex )
        }
    }
    else if ( nrow( obj$cmp ) == 4 )
    {
        tex <- sub( "mean.1.1", format.num( obj$cmp$mean[1] ), tex.val.sub[[4]] )
        tex <- sub(   "sd.1.1", format.num( obj$cmp$sd[1] ), tex )

        if ( obj$is.eq.mean() )
        {
            tex <- sub( "mean.1.2", "\\\\mu_{1,1}", tex )
            tex <- sub( "mean.2.1", "\\\\mu_{1,1}", tex )
            tex <- sub( "mean.2.2", "\\\\mu_{1,1}", tex )
        }
        else
        {
            tex <- sub( "mean.1.2", format.num( obj$cmp$mean[2] ), tex )
            tex <- sub( "mean.2.1", format.num( obj$cmp$mean[3] ), tex )
            tex <- sub( "mean.2.2", format.num( obj$cmp$mean[4] ), tex )
        }

        if ( obj$is.eq.sd() )
        {
            tex <- sub( "sd.1.2", "\\\\sigma_{1,1}", tex )
            tex <- sub( "sd.2.1", "\\\\sigma_{1,1}", tex )
            tex <- sub( "sd.2.2", "\\\\sigma_{1,1}", tex )
        }
        else
        {
            tex <- sub( "sd.1.2", format.num( obj$cmp$sd[2] ), tex )
            tex <- sub( "sd.2.1", format.num( obj$cmp$sd[3] ), tex )
            tex <- sub( "sd.2.2", format.num( obj$cmp$sd[4] ), tex )
        }
    }

    return ( tex )
}
