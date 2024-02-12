################################################################################################
# Functions related to TeX
# @file         ggd.tex.R
# @author       Kimitsuna Ura
# @copyright    Copyright (C) 2024 Kimitsuna Ura
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Constants

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
#  Functions

################################################################################################
#' Output TeX format
#'
#' Outputs the TeX-formatted texts of the formulas of the probability density function
#' \eqn{g(x)} and the cumulative distribution function \eqn{\Psi(x)}
#' using \code{\link[base]{writeLines}}.
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
#' @param   con         A \code{\link[base]{connection}} object or a character string
#'                      to indicate the output destination.
#'                      See \sQuote{Details} at \code{\link[base]{writeLines}}
#'                      for more information.
#' @param   sep         A character string to be written to the connection after each line
#'                      of text. See \sQuote{Details} at \code{\link[base]{writeLines}}
#'                      for more information.
#' @param   comma       A logical. If \code{TRUE}, this method writes a \sQuote{,} (comma)
#'                      as a separator between each expression and a \sQuote{.} (period)
#'                      at the end of the output.
#'                      If \code{FALSE}, those \sQuote{,} and \sQuote{.} will not be written.
#' @param   format.num  A function to format each numeric value of mean values and
#'                      standard deviations.
#'                      It should be a function with one argument for the value
#'                      to be displayed and returning a character string.
#' @param   frac.env    The TeX environment to format fractions with different heights
#'                      of numerator and denominator.
#'                      If \code{"default"}, such fractions will be formatted
#'                      in the default style of TeX, but their appearance is usually poor.
#'                      The environment which produces the most beautiful form
#'                      will depend on the TeX execution environment.
#' @return  An invisible \code{NULL}.
#' @seealso \code{\link[ggd]{adjust.cmp}}, \code{\link[ggd]{tex.d}}, \code{\link[ggd]{tex.p}}
#' @details
#'  \subsection{Composition of outputs}{
#'      \code{tex} method outputs the formulas of both probability density function
#'      and cumulative distribution function.
#'
#'      \code{tex.d} method outputs the formulas of probability density function only.
#'
#'      \code{tex.p} method outputs the formulas of cumulative distribution function only.
#'
#'      In these methods, the formulas are output according to \code{mix.type}
#'      and the number of components in \code{cmp} field, not \code{kind} or \code{kind.index}.
#'      That is, for example, if the \code{kind} is \code{"Normal Distribution"}
#'      and \code{mix.type = 2},
#'      \code{tex} outputs formulas of a horizontal gradational distribution.
#'
#'      If \code{cmp} field has no rows, nothing is output.
#'  }
#'
#'  \subsection{Equaled mean values or standard deviations}{
#'      For clarity, when all mean values or standard deviations of components are equal
#'      (i.e., when \code{\link[ggd]{is.eq.mean}} or \code{\link[ggd]{is.eq.sd}} method
#'      returns \code{TRUE}), they are displayed with \sQuote{\eqn{=}} to the 1st parameter,
#'      like as \sQuote{\eqn{\sigma_2 = \sigma_1}}.
#'
#'      If only the values of some parameters are equal (e.g., only \eqn{\sigma_2} and
#'      \eqn{\sigma_3} are equal and \eqn{\sigma_1} is different),
#'      each value is displayed as \sQuote{\eqn{\sigma_2 = x, \sigma_3 = x}}
#'      instead of \sQuote{\eqn{\sigma_3 = \sigma_2}} to avoid misreading.
#'
#'      Note that if the difference between the values of parameters is smaller than
#'      displayable number of decimal places, \sQuote{\eqn{\sigma_2 = \sigma_1}}
#'      will not be displayed, but the same number will be displayed for each.
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
#' [Non-exported] TeX format for mean values and standard deviations
#'
#' Gets the TeX-formatted texts representing the mean values and standard deviations of
#' the normal distributions of the components.
#' @param   obj         A \code{\link[ggd]{GGD}} object.
#' @param   format.num  A function to format each numeric value of mean values and
#'                      standard deviations. See \code{\link[ggd]{tex}}.
#' @return  TeX-formatted texts representing the mean values and standard deviations of
#'          the normal distributions of the components.
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
