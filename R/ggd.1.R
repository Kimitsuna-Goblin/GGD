################################################################################################
# Base of the Gradational Gaussian Distribution class
# @file         ggd.1.R
# @author       Kimitsuna-Goblin (Ura, Kimitsuna)
# @copyright    Copyright (C) 2024 Kimitsuna-Goblin (Ura, Kimitsuna)
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Constants

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

################################################################################################
#  Functions

################################################################################################
#' Gradational Gaussian Distribution class
#'
#' The class provides the Gradational Gaussian Distribution.
#' @export      GGD
#' @exportClass GGD
#' @field   kind.index      An integer; the index number of the kind of the distribution model.
#' @field   kind            A string; the name of the kind of the distribution model.
#' @field   mix.type        An integer which represents how to mix normal distributions
#'                          of the components.
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
#'            h_1(x) \ \mathcal{N}_1 + h_2(x) \ \mathcal{N}_2,}
#'      where \eqn{h_1} and \eqn{h_2} are the mixing ratio functions defined
#'      using \eqn{\Phi_1} and \eqn{\Phi_2}, the cumulative distribution functions of
#'      \eqn{\mathcal{N}_1} and \eqn{\mathcal{N}_2} as
#'      \deqn{h_1(x) = 1 - \Phi_1(x), \\
#'            h_2(x) = \Phi_2(x). \quad \ \ \ }
#'
#'      Because \eqn{\mathcal{N}_1} is dominant on the left (lower) side of x-axis,
#'      and \eqn{\mathcal{N}_2} is on the right (upper) side,
#'      we call \eqn{\mathcal{N}_1} the \bold{left- (lower-) side} distribution,
#'      and \eqn{\mathcal{N}_2} the \bold{right- (upper-) side} distribution.
#'
#'      About the \bold{vertical gradational Gaussian distribution}
#'      \eqn{\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2]},
#'      it is expressed as
#'      \deqn{\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2] =
#'            v_1(x) \ \mathcal{N}_1 + v_2(x) \ \mathcal{N}_2,}
#'
#'      where the mixing ratio functions \eqn{v_1} and \eqn{v_2} are defined
#'      using \eqn{f_1} and \eqn{f_2}, the probability density functions of
#'      \eqn{\mathcal{N}_1} and \eqn{\mathcal{N}_2} as
#'      \deqn{v_1(x) = 1 - \dfrac{f_1(x)}{f_1(\mu_1)}, \\
#'            v_2(x) = \dfrac{f_2(x)}{f_2(\mu_2)}. \quad \ \ \ }
#'
#'      Here, we call \eqn{\mathcal{N}_1} the \bold{tail-side} distribution,
#'      and \eqn{\mathcal{N}_2} the \bold{top-side} distribution.
#'
#'      In this package, we use \eqn{v_1(x) = 1 - f_1(x) / f_1(\mu_1)}
#'      and \eqn{v_2(x) = f_2(x) / f_2(\mu_1)},
#'      where \eqn{\mu_1} and \eqn{\mu_2} are the mean values of
#'      \eqn{\mathcal{N}_1} and  \eqn{\mathcal{N}_2}, respectively.
#'
#'      You can divide the tail-side distribution along x-axis into left (lower) side
#'      and right (upper) side.
#'
#'      In this case, the distribution
#'      \eqn{\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2 \downarrow \mathcal{N}_3]}
#'      is expressed as
#'      \deqn{\mathcal{G}[\mathcal{N}_1 \uparrow \mathcal{N}_2 \downarrow \mathcal{N}_3] =
#'            v_1(x) \ \mathcal{N}_1 + v_2(x) \ \mathcal{N}_2 + v_3(x) \ \mathcal{N}_3,}
#'      where the mixing ratio function \eqn{v_1} is defined as
#'      \deqn{v_1(x) =
#'            \begin{cases}
#'            1 - \dfrac{f_1(x)}{f_1(\mu_1)} & (x \leq \mu_1), \\
#'            0 & (x > \mu_1),
#'            \end{cases}}
#'
#'      \eqn{v_3} is defined as
#'      \deqn{v_3(x) =
#'            \begin{cases}
#'            0 & (x < \mu_3), \\
#'            1 - \dfrac{f_3(x)}{f_3(\mu_3)} & (x \geq \mu_3),
#'            \end{cases}}
#'      and \eqn{v_2(x)} for the top side is defined as same as with 2 components.
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
#'      \deqn{\ \ \ \ \mathcal{G}[\mathcal{G}_1 \rightarrow \mathcal{G}_2] =
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
#'                          \dfrac{\Phi_2(x)^2}{2} \qquad \ \ }}
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
#'                                    \dfrac{\Psi_2(x)^2}{2} \hspace{9em} \\
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
#' Clears all fields. The lengths of all vector fields and the number of rows
#' in the \code{cmp} field will be \code{0}.
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
#'  ## ggd.nls.freq.all returns a list of GGD objects to $obj.
#'  df <- data.frame( x = seq( -2, 2, 0.2 ),
#'                    freq = c(     57,    277,   1002,   3178,   9646,  22109, 42723,
#'                               80646, 117625, 139181, 162319, 150870, 109947, 78736,
#'                               46616,  21058,   9211,   3466,    976,    260,    61 ) )
#'  result <- ggd.nls.freq.all( df, not.use.nls = TRUE, start.level = 2 )
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
#'  ## ggd.nls.freq.all returns a list of GGD objects to $obj.
#'  df <- data.frame( x = seq( -2, 2, 0.2 ),
#'                    freq = c(     57,    277,   1002,   3178,   9646,  22109, 42723,
#'                               80646, 117625, 139181, 162319, 150870, 109947, 78736,
#'                               46616,  21058,   9211,   3466,    976,    260,    61 ) )
#'  result <- ggd.nls.freq.all( df, not.use.nls = TRUE, start.level = 2 )
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
#'                      This argument works when the length of \code{mix.type} argument
#'                      is \code{0}, and \code{grad} is \code{"default"}.
#'
#'                      The matching method of this argument follows that of elements of
#'                      the \code{objs} argument of the \code{\link[ggd]{ggd.kind.index}}.
#'
#'                      If \code{NA} is indicated, \code{NA_integer_} will be returned.
#'                      If indicated character string pattern or index number does not match
#'                      to any element of \code{ggd:::kind}, an error will occur.
#'
#' @param   mix.type    Default value which is returned if \code{grad} is \code{"default"}.
#'                      Although return value will become in integer class,
#'                      this function does not check if the value of this argument is valid.
#'
#' @return  An integer of \code{mix.type} value appropriate for a distribution model
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
#' Gets a vector of integers of \code{mix.type}s
#' which are appropriate to the indicated index numbers of \code{ggd:::kinds}.
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
#'
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
#' Referring only \code{cmp} field, checks if the distribution is a normal distribution.
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
#' Referring \code{mix.type} and \code{cmp} field, checks if the distribution is constructed as
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
#' Referring \code{mix.type} and \code{cmp} field, checks if the distribution is constructed as
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
#' Referring \code{mix.type} and \code{cmp} field, checks if the distribution is constructed as
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
#' Referring \code{mix.type} and \code{cmp} field, checks if the distribution is constructed as
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
#'          4 (2x2) normal distributions, otherwise \code{FALSE}.
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
#' Referring \code{mix.type} and \code{cmp} field,
#' checks if the probability density function is symmetric about the mean.
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
#'          symmetric about the mean, otherwise \code{FALSE}.
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
#'                  If \code{length(n) > 1}, the length is taken to be the number required.
#' @param   tol     The tolerance level for the convergence criterion for
#'                  \code{\link[ggd]{q}} method.
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
