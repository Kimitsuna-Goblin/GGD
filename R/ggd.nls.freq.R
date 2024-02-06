################################################################################################
# Approximating frequency distributions
# @file         ggd.nls.freq.R
# @author       Kimitsuna Ura
# @copyright    Copyright (C) 2024 Kimitsuna Ura
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Functions

################################################################################################
#' Approximating a frequency distribution
#'
#' With the non-linear least squares (\code{\link[stats]{nls}}),
#' constructs a \code{\link[ggd]{GGD}} object which (locally) most closely approximates
#' the given frequency distribution.
#' Then \code{ggd.nls.freq} function generates a \code{\link[ggd]{GGD}} object,
#' and \code{nls.freq} method sets the fields according to the result.
#' 'Locally' means that if the start value is modified, more closely approximating model
#' may be constructed.
#' The outliers of the frequency distribution will not be excluded in this function.
#' If necessary, outliers should be excluded by preprocessing.
#' @export
#' @name    nls.freq
#' @aliases ggd.nls.freq
#' @aliases nls.freq
#' @aliases \S4method{nls.freq}{GGD}
#' @usage   ggd.nls.freq(data, x = "x", freq = "freq", total = NULL,
#'          kind = NULL, mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"),
#'          custom.d = NULL, ncmp = 0,
#'          eq.mean = logical(), eq.sd = logical(),
#'          start.level = 100, start = NULL, control = list(),
#'          not.use.nls = FALSE, cor.method = NULL, ...)
#' @usage   \S4method{nls.freq}{GGD}(data, x = "x", freq = "freq", total = NULL,
#'          this.kind = NULL, this.mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"),
#'          this.custom.d = NULL, ncmp = nrow(cmp),
#'          eq.mean = logical(), eq.sd = logical(),
#'          start.level = 100, start = NULL, control = list(),
#'          not.use.nls = FALSE, cor.method = NULL, ...)
#'
#' @param   data        A data frame which represents the frequency distribution.
#'                      It must contain at least 2 numeric columns for \code{x} and \code{freq}.
#'
#'                      Column \bold{\code{x}} is for the x-coordinates.
#'                      Each value is expected to be a numeric value which represents a cell of
#'                      the frequency distribution
#'                      as the x-coordinate at the center of the cell.
#'                      The values must be arranged in ascending order, and not be duplicated.
#'
#'                      Column \bold{\code{freq}} is for the frequencies following \code{x}.
#'                      The values of frequencies should not be negative.
#'                      Both integers and real numbers are allowed for the values.
#'
#'                      Rows which contain \code{NA} or \code{NaN} for \code{x} or \code{freq}
#'                      are ignored. For fine approximation, the number of rows should be
#'                      large enough; it is recommended that there are more than 8 valid rows.
#'
#'                      Column names and column numbers for \code{x} and \code{freq}
#'                      are flexible. You can indicate them with next two arguments.
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
#'                      which indicates the kind of distribution model to be generated.
#'                      The matching method of \code{kind} follows that of
#'                      \code{objs} argument of \code{\link[ggd]{ggd.kind.index}}.
#'
#'                      This argument gives the conditions of
#'                      the value of \code{mix.type} field,
#'                      and of whether the mean values or standard deviations of the components
#'                      should be aligned to the same value or not.
#'
#'                      Indicating \code{mix.type} or \code{grad} other than \code{"default"},
#'                      or \code{eq.mean} and \code{eq.sd} other than \code{logical(0)}
#'                      can overwrite the condition of this argument.
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
#'                          \item 3: Vertical gradation of 2 (or 3) normal distributions.
#'                          \item 4: Horizontal-Vertical gradation
#'                                   with 4 (2x2) normal distributions.
#'                          \item 5: Custom distribution.
#'                      }
#'
#'                      Where \code{mix.type = 3} is indicated,
#'                      \code{ggd.nls.freq} function generates a 2-component model.
#'                      If you want to generate a 3-component model, use \code{grad = "v3"}.
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
#'                      \code{"normal"} is for a normal distribution.
#'                      \code{"default"} is for depending on values of other arguments.
#'
#'                      If other than \code{"default"} is indicated,
#'                      this function will create a \code{\link[ggd]{GGD}} object
#'                      according to this argument with ignoring \code{[this.]mix.type} argument
#'                      and overwriting the type indicated by \code{[this.]kind} argument.
#'
#' @param   custom.d    A function for the density function of the custom distribution.
#'
#'                      If \code{NULL}, the default function will be used with some warnings
#'                      when approximating with a custom distribution.
#'
#'                      See \code{\link[ggd]{set.cmp}} for more information.
#'
#' @param   ncmp        Number of components in \code{cmp} field.
#'
#'                      This argument works only when using a custom distribution.
#'                      For other distribution models, the number of components is determined
#'                      according to the model.
#'
#'                      For \code{ggd.nls.freq} function,
#'                      a positive integer must be indicated when using a custom distribution.
#'
#'                      For \code{nls.freq} method,
#'                      The number of rows in \code{cmp} field is used by default.
#'                      If another value is indicated, the value will be used instead.
#'
#' @param   eq.mean     A logical. If \code{TRUE}, all of the mean values of of the components
#'                      are forced to be equal.
#'
#'                      If \code{FALSE} or \code{logical(0)},
#'                      the mean values are not bound,
#'                      and mean-equaled components will be rarely constructed.
#'
#'                      \code{TRUE} and \code{FALSE} can overwrite the condition indicated by
#'                      \code{kind} or \code{this.kind} argument.
#'
#' @param   eq.sd       A logical. If \code{TRUE}, all of the standard deviations of
#'                      the components are forced to be equal.
#'
#'                      If \code{FALSE} or \code{logical(0)},
#'                      the standard deviations are not bound,
#'                      and sigma-equaled components will be rarely constructed.
#'
#'                      If both \code{eq.mean} and \code{eq.sd} are \code{TRUE},
#'                      a normal distribution will be constructed.
#'
#'                      \code{TRUE} and \code{FALSE} can overwrite the condition indicated by
#'                      \code{kind} or \code{this.kind} argument.
#'
#' @param   start.level A numeric value of integer in from \code{0} to \code{3} or \code{100}
#'                      with default \code{100}; the level of initial guessing
#'                      for \code{start} argument of \code{\link[stats]{nls}}.
#'
#'          The detail of each level is:
#'          \itemize{
#'              \item \code{0}:
#'                       The mean and the standard deviation of the frequency distribution
#'                       are used as the start values.
#'              \item \code{1}:
#'                       In addition to level \code{0}, it computes the local mean values
#'                       and local standard deviations of the frequency distribution
#'                       in ranges of x-coordinates where the effect of each of components is
#'                       likely to be heavy, and uses them for guessing the start values
#'                       if they are likely to be better guesses.
#'              \item \code{2}:
#'                       In addition to level \code{0}, at first it computes some quantiles
#'                       from the frequency distribution, then it finds normal distributions
#'                       tracing two of the quantiles, and uses the mean values and
#'                       standard deviations of them for guessing the start values
#'                       if they are likely to be better guesses.
#'              \item \code{3}:
#'                       It generates a \code{\link[ggd]{GGD}} object tracing
#'                       some (2, 3 or 5) quantiles computed from the frequency distribution,
#'                       and uses the mean values and standard deviations of the components
#'                       of the object as the start values.
#'                       If the generating fails, level \code{2} is used instead.
#'              \item \code{100}:
#'                       Try all of above levels and adopt the result of the highest
#'                       \code{\link[stats]{cor}} value.
#'          }
#'
#'          As the level increases within the range of \code{0} to \code{3},
#'          the initial model will tend to be closer to the frequency distribution.
#'          However, the accuracy of the result may not along with the level.
#'          It is possible that \code{\link[stats]{nls}} will succeed at level \code{1}
#'          and fail at level \code{3} for the same data.
#'
#' @param   start       A list for \code{start} argument of \code{\link[stats]{nls}}.
#'
#'                      You can provide your own \code{start} for \code{\link[stats]{nls}},
#'                      the mean values (parameters are like: \code{mean}, \code{mean.i},
#'                      or \code{mean.i.j})
#'                      and the \bold{square root} of the standard deviations
#'                      (parameters are like: \code{sqrt.sd}, \code{sqrt.sd.i},
#'                      or \code{sqrt.sd.i.j}) of the normal distributions of the components.
#'
#'                      Depending on the kind of the distribution model,
#'                      the names of the parameters are different.
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
#'                      If \code{TRUE}, this function does not use \code{\link[stats]{nls}} and
#'                      it outputs an object having the start values in \code{cmp} field
#'                      as the result.
#'                      If \code{FALSE}, this function uses \code{\link[stats]{nls}}.
#'
#'                      This argument works when \code{start.level} argument is
#'                      other than \code{100}. A warning will occur if \code{TRUE}
#'                      when \code{start.level} is \code{100}.
#'
#'                      You can use \code{not.use.nls = TRUE} to check
#'                      whether the start values are appropriate
#'                      when you have obtained an undesirable result from this function.
#'
#' @param   cor.method  The \code{method} argument for \code{\link[stats]{cor}}.
#'                      It is a character string indicating which correlation coefficient
#'                      (or covariance) is to be computed in order to compare the results
#'                      between levels when \code{start.level = 100}.
#'                      If \code{NULL}, it uses the default method of \code{\link[stats]{cor}}.
#'                      This argument works only if \code{start.level = 100}.
#'                      See \code{\link[stats]{cor}} for more information.
#'
#' @param   ...         Each argument for \code{\link[stats]{nls}} can be indicated.
#'                      See 'Arguments' of \code{\link[stats]{nls}} for more information.
#'
#' @param   this.kind   A character string or a numeric value or a \code{\link[ggd]{GGD}} object
#'                      which indicates the kind of distribution model to be constructed.
#'                      It is equivalent to \code{kind} argument of \code{ggd.nls.freq}.
#'
#'                      When this method is called without \code{this.kind} argument
#'                      or other conditions, it attempt to retain the value of
#'                      \code{mix.type} field as much as possible, but not \code{kind} field.
#'                      That is, the condition that the mean values or standard deviations
#'                      of the components are aligned to the same values or not may not
#'                      be retained. If you want to retain the condition as well,
#'                      indicate the object itself to \code{this.kind} argument like as
#'                      \code{obj$nls.freq(data, this.kind = obj)}.
#'
#' @param   this.mix.type   A numeric value to set into \code{mix.type} field as an integer.
#'                          It is equivalent to \code{mix.type} argument of
#'                          \code{ggd.nls.freq}.
#'
#'                      Where \code{this.mix.type = 3} is indicated,
#'                      \code{nls.freq} method constructs 2- or 3- component model
#'                      according to the number of current components.
#'
#'                      If both of \code{this.kind} and \code{this.mix.type} are not given
#'                      and \code{grad} argument is \code{"default"},
#'                      the current value of \code{mix.type} field will be retained,
#'                      and number of components will also.
#'                      However, if the object has been cleared when this method is called,
#'                      \code{mix.type} field will be \code{2}, the start value.
#'
#' @param   this.custom.d   A function for the density function of the custom distribution.
#'                          It is equivalent to \code{custom.d} argument of
#'                          \code{ggd.nls.freq}.
#'
#'                      If \code{NULL}, the current function in \code{custom.d} field
#'                      will be used when approximating with a custom distribution.
#'
#'                      See \code{\link[ggd]{set.cmp}} for more information.
#'
#' @return  A list containing components (invisible for \code{GGD} method)
#'          \item{obj}{
#'                  Generated \code{\link[ggd]{GGD}} object which most (at least locally)
#'                  closely approximates the given frequency distribution.
#'                  If \code{\link[stats]{nls}} has failed, it will be a cleared object.
#'                  For \code{\link[ggd]{GGD}} method,
#'                  the \code{\link[ggd]{GGD}} object itself.}
#'          \item{nls.out}{
#'                  The list of the output of \code{\link[stats]{nls}}.
#'                  If \code{\link[stats]{nls}} has not been used, \code{NULL} will be set.
#'                  See 'Value' of \code{\link[stats]{nls}} for more information.}
#'          \item{start.level}{
#'                  The initial guessing level which is used actually to obtain \code{obj}.
#'                  If \code{start.level = 100} is indicated,
#'                  the level for the best result will be set.
#'                  When \code{start.level = 3} is indicated
#'                  and if initial guessing has failed, \code{2} will be set.
#'                  If you indicate \code{start} argument a not-\code{NULL} list,
#'                  \code{start.level} will be \code{NA}.}
#'          \item{start}{
#'                  The used \code{start} argument of \code{\link[stats]{nls}}.}
#'          \item{start.obj}{
#'                  A \code{\link[ggd]{GGD}} object corresponding to the start values.
#'                  That is, a \code{\link[ggd]{GGD}} object in which
#'                  values of the above \code{start} are set directly to \code{cmp} field.}
#'          \item{cor}{
#'                  The vector of the correlation coefficient of between the result for
#'                  each initial guessing level in the range of \code{0} to \code{3}
#'                  and the frequency distribution.
#'                  This component is given only if \code{start.level = 100}.}
#'          \item{errors}{
#'                  A list of information about errors occurred in \code{\link[stats]{nls}}.
#'                  This component is given only if \code{start.level = 100}.
#'                  Each element in the list contains:
#'                  \itemize{
#'                      \item level: The initial guessing level when the error has occurred.
#'                      \item message: The error message.
#'                  }}
#'          \item{warnings}{
#'                  A list of information about warnings occurred in \code{\link[stats]{nls}}.
#'                  This component is given only if \code{start.level = 100}.
#'                  The composition of each element is same as \code{errors}.}
#'
#'          For \code{GGD} method: If an error occurs, all fields
#'                                 (except \code{custom.d} and \code{custom.p})
#'                                 of the object will be cleared in most cases.
#'
#' @importFrom  methods     new
#' @importFrom  stats       complete.cases
#' @seealso \code{\link[stats]{nls}}, \code{\link[stats]{nls.control}},
#'          \code{\link[ggd]{ggd.nls.freq.all}}, \code{\link[ggd]{ggd.start.template}}
#'
#' @details
#'  \subsection{Why the standard deviations for 'start' are square-rooted?}{
#'      You know a standard deviation must be a positive value.
#'      But if you use standard deviations directly in the formula for \code{\link[stats]{nls}},
#'      they sometimes drop into negative values while the Gauss-Newton algorithm is running
#'      and the algorithm will fail, even if it can reach convergence if done better.
#'
#'      Therefore, to avoid such failures, we use square roots of standard deviations and
#'      take squares of them in the formula for \code{\link[stats]{nls}}.
#'  }
#'
#' @examples
#'  ## Preparing:
#'  df <- data.frame(
#'              x      = seq( -2, 2, 0.2 ),
#'              freq   = c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'                          7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'                          3698,  2740,  2549,  2284,  1499,  1147,   918 ),
#'              x.2    = seq( -20, 20, 2 ),
#'              freq.2 = c( .000974, .003797, .008523, .023142, .045017, .081743, .120990,
#'                          .142527, .124627, .106294, .078625, .059378, .045690, .042958,
#'                          .035760, .030938, .015675, .012516, .008139, .005114, .003582 ) )
#'
#'  ## This function plots probability densities obtained from the frequency distribution
#'  ## and the probability density function of a GGD object.
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
#'  ## Examples:
#'  result <- ggd.nls.freq( df, mix.type = 0 )
#'  result
#'  plot.freq.and.d( result$obj, df$x, df$freq )
#'
#'  ## You should not indicate 'start.level' to obtain good quality results,
#'  ## but it is indicated here to make the process faster.
#'  a <- GGD$new()
#'  a$nls.freq( df, this.kind = "2.*Sigma-Equaled Vertical", start.level = 2 )
#'  a
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  ## Overwriting 'Sigma-Differed' with 'eq.sd = FALSE' after 'kind = a'.
#'  b <- ggd.nls.freq( df, kind = a, eq.sd = FALSE, start.level = 2 )$obj
#'  a$kind; b$kind      ## "... Sigma-Equaled ..."; "... Sigma-Differed ..."
#'  plot.freq.and.d( b, df$x, df$freq )
#'
#'  ## You can specify start values with 'start' argument.
#'  start.list <- ggd.start.template( 14 )
#'  start.list
#'
#'  start.list$mean.1.1 <- -0.671
#'  start.list$mean.1.2 <- -0.198
#'  start.list$mean.2.1 <- 0.293
#'  start.list$mean.2.2 <- -0.198
#'  start.list$sqrt.sd <- sqrt( 0.640 ) ## 'sqrt.sd' is the sqrt of the standard deviation.
#'
#'  ## You can check the start values with 'not.use.nls = TRUE' before approximating.
#'  a$nls.freq( df, this.kind = 14, start = start.list, not.use.nls = TRUE )
#'  a$cmp
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  ## When you indicate 'start' argument, 'start.level' argument is ignored.
#'  result <- a$nls.freq( df, this.kind = 14, start.level = 1, start = start.list )
#'  result$start.level      ## NA
#'  result$start.obj$cmp    ## Verify start values.
#'  a$cmp                   ## results
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  ## When you call nls.freq method of a GGD object consecutively,
#'  ## the conditions saved in the fields are retained (if no error has occurred).
#'  a$nls.freq( df, grad = "hv", eq.mean = TRUE, start.level = 2 )
#'  a$mix.type; a$is.eq.mean()  ## 4; TRUE
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  a$nls.freq( df, eq.mean = FALSE, start.level = 2 )   ## 'grad = "hv"' is retained.
#'  a$mix.type; a$is.eq.mean()  ## 4; FALSE
#'  plot.freq.and.d( a, df$x, df$freq )
#'
#'  ## Using "x.2" for x and "freq.2" for freq.
#'  b <- ggd.nls.freq( df, x = "x.2", freq = "freq.2", start.level = 2 )$obj
#'  b
#'  plot.freq.and.d( b, df$x.2, df$freq.2 )
#'
#'\dontrun{
#'  ## Using the default 'start.level' (100) is the best way to obtain a good quality result,
#'  ## but the process is slow to demonstrate as a running example. So, it is not run here.
#'  ##
#'  ## The following results written in comments are
#'  ## obtained on a Windows 11 64-bit environment using ggd v1.0.3 package.
#'  result <- a$nls.freq( df, grad = "v" )
#'  result$cor
#'  ##   level.0   level.1   level.2   level.3
#'  ## 0.9867103 0.9867102 0.9867103 0.9867102
#'  result$start.level
#'  ## 2
#'  a$cmp
#'  ##           mean       sd
#'  ## n.1 -0.1841741 1.039255
#'  ## n.2 -0.1973208 0.648003
#'
#'  ## 'cor.method' argument is available for 'start.level = 100'
#'  ## but the default value is recommended (see 'Details' at the manual of cor).
#'  result <- a$nls.freq( df, cor.method = "kendall" )
#'  result$cor
#'  ##   level.0   level.1   level.2   level.3
#'  ## 0.9428571 0.9428571 0.9428571 0.9428571  ## All values were equal.
#'  result$start.level
#'  ## 0
#'  a$cmp
#'  ##           mean       sd
#'  ## n.1 -0.1841703 1.039255
#'  ## n.2 -0.1973202 0.648003
#'
#'  ## 'start.level = 3' does not always generate good start values.
#'  ## This execution may cause an error in nls due to strange start values.
#'  a$nls.freq( df, grad = "hv", start.level = 3 )
#'  ## If run, an error may occur as: 'nls has failed. Message: Error in nls...'
#'
#'  ## To get a result even if an error occurs in nls,
#'  ## you can indicate 'warnOnly = TRUE' in 'control' option.
#'  a$nls.freq( df, grad = "hv", start.level = 3, control = list( warnOnly = TRUE ) )
#'  ## If run, a warning may occur (a singular gradient).}
################################################################################################
ggd.nls.freq <- function( data, x = "x", freq = "freq", total = NULL,
                          kind = NULL, mix.type = NULL,
                          grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ),
                          custom.d = NULL, ncmp = 0,
                          eq.mean = logical(), eq.sd = logical(),
                          start.level = 100, start = NULL, control = list(),
                          not.use.nls = FALSE, cor.method = NULL, ... )
{
    obj <- GGD$new()
    return ( withVisible( obj$nls.freq( data            = data,
                                        x               = x,
                                        freq            = freq,
                                        total           = total,
                                        this.kind       = kind,
                                        this.mix.type   = mix.type,
                                        grad            = grad,
                                        this.custom.d   = custom.d,
                                        ncmp            = ncmp,
                                        eq.mean         = eq.mean,
                                        eq.sd           = eq.sd,
                                        start.level     = start.level,
                                        start           = start,
                                        control         = control,
                                        not.use.nls     = not.use.nls,
                                        cor.method      = cor.method, ... ) )$value )
}

GGD$methods(
    nls.freq = function( data, x = "x", freq = "freq", total = NULL,
                         this.kind = NULL, this.mix.type = NULL,
                         grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ),
                         this.custom.d = NULL, ncmp = nrow( cmp ),
                         eq.mean = logical(), eq.sd = logical(),
                         start.level = 100, start = NULL, control = list(),
                         not.use.nls = FALSE, cor.method = NULL, ...)
    {
        # Note:
        # In this function, when an error occurs,
        # we clear all fields except custom.d and custom.p as much as possible.
        #
        # Because this function does not directly set specified values to the fields,
        # if the fields are not cleared and contain some normal values,
        # users may let the subsequent processes take place without noticing the error.
        # During the development phase, the developer actually experienced such mistakes.
        on.exit( clear( keep.custom.d = TRUE ) )

        result <- list( obj = NULL, nls.out = NULL,
                        start.level = NULL, start = NULL, start.obj = NULL )

        # Check errors of data frame and discard NA and NaN.
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

        ################################################################
        # Check options and get new mix.type according to the priority.
        grad <- match.arg( grad )
        if ( grad == "v" )
        {
            grad <- "v2"
        }

        if ( !is.null( this.kind ) )
        {
            this.kind.index <- ggd.kind.index( this.kind, undef.err = TRUE )
            if ( length( this.kind.index ) > 1 )
            {
                stop( "Error: kind should be valid single value or a GGD object." )
            }
            else if ( length( this.kind.index ) == 1 && !is.na( this.kind.index ) )
            {
                # Set "v2" or "v3" to grad by this.kind
                # (but if this.mix.type is indicated, it takes priority)
                if ( grad == "default" && is.null( this.mix.type ) )
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

        new.mix.type <- ggd.mix.type.for( grad, kind = this.kind, mix.type = this.mix.type )
        if ( length( new.mix.type ) != 1 || is.na( new.mix.type ) ||
             !any( new.mix.type == 0:5 ) )
        {
            if ( !is.null( this.kind ) )
            {
                # This code will run if this.kind = character( 0 ).
                stop( "Error: kind should be valid single value or a GGD object." )
            }
            else
            {
                stop( "Error: mix.type should be single integer from 0 to 5." )
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

        # If new mix.type is going to be 5, ncmp must be indicated.
        if ( new.mix.type == 5 && grad == "default" )
        {
            if ( !is.null( this.custom.d ) )
            {
                custom.d <<- this.custom.d
            }

            if ( length( ncmp ) != 1 || !is.numeric( ncmp ) || as.integer( ncmp ) <= 0 )
            {
                stop( paste( "Error: ncmp should be given the number of components." ) )
            }
            ncmp <- as.integer( ncmp )
        }

        ################################################################
        # Check start.level.
        if ( !is.null( start ) )
        {
            start.level <- NA_integer_
        }
        else
        {
            if ( length( start.level ) != 1 || !is.numeric( start.level ) ||
                 is.na( start.level ) || !any( start.level == c( 0:3, 100 ) ) )
            {
                stop( "Error: start.level should be single integer in 0:3 or 100." )
            }
        }

        if ( isTRUE( not.use.nls ) && !is.na( start.level ) && start.level == 100 )
        {
            warning( paste( "Warning: not.use.nls does not work with",
                                     "start.level is default 100." ) )
            not.use.nls <- FALSE
        }

        if ( isTRUE( start.level == 100 ) )
        {
            ################################################################
            # Loop with each level.
            result <- try( nls.freq.level.100( data.ext, total, this.kind, this.mix.type,
                                               grad, custom.d, ncmp, eq.mean, eq.sd, control,
                                               not.use.nls = FALSE, cor.method, ... ),
                           silent = TRUE )
            if ( inherits( result, "try-error" ) )
            {
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
            params <- get.nls.params( data.ext$x, data.ext$freq, total, new.mix.type,
                                      grad, ncmp, eq.mean, eq.sd, start.level )

            # Output start parameters.
            result$start.level <- as.integer( params$start.level )
            if ( is.null( start ) )
            {
                result$start <- params$start
            }
            else
            {
                result$start <- start
            }
            result$start.obj <- ggd.set.cmp( get.cmp.with.nls.coef( unlist( result$start ),
                                                                    new.mix.type, grad, ncmp,
                                                                    eq.mean, eq.sd ),
                                             mix.type = new.mix.type, grad = grad,
                                             custom.d = custom.d )

            ################################################################
            # Get the result components.
            result.cmp <- NULL
            if ( isTRUE( not.use.nls ) )
            {
                # Output using start parameters directly without nls.
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
                    stop( paste( "nls has failed. Message:", result$nls.out ) )
                }
                else
                {
                    result.cmp <- get.cmp.with.nls.coef( coef( result$nls.out ), new.mix.type,
                                                         grad, ncmp, eq.mean, eq.sd )
                }
            }

            ################################################################
            # Update fields with the result.
            set.cmp( result.cmp, this.mix.type = new.mix.type, grad = grad )

            result$obj <- .self
        }

        on.exit()
        return ( invisible( result ) )
    }
)

################################################################################################
#' [Non-exported] Loop nls for start.level = 100
#'
#' Executes a loop of \code{\link[stats]{nls}} with changing initial guessing levels.
#' This is the main process of \code{\link[ggd]{nls.freq}} with \code{start.level = 100}.
#' @param   data        A data frame which represents the frequency distribution.
#'                      It must contain 2 numeric columns named \code{"x"} and \code{"freq"}.
#' @param   total       Total value of the frequencies.
#' @param   kind        An object indicating the \code{kind} of \code{\link[ggd]{GGD}} object.
#' @param   mix.type    A numeric value to set into \code{mix.type} field of
#'                      the \code{\link[ggd]{GGD}} object as an integer.
#'                      It is an integer from \code{0} to \code{4} or \code{NULL}.
#' @param   grad        A character string indicating the method of gradation.
#' @param   custom.d    A function for the density function of the custom distribution.
#' @param   ncmp        Number of components for the custom distribution.
#' @param   eq.mean     A logical. If \code{TRUE}, all of the mean values of of the components
#'                      are forced to be equal.
#' @param   eq.sd       A logical. If \code{TRUE}, all of the standard deviations of
#'                      the components are forced to be equal.
#' @param   control     The list for \code{control} argument of \code{\link[stats]{nls}}.
#'                      See \code{\link[stats]{nls.control}} for more information.
#' @param   cor.method  The \code{method} argument for \code{\link[stats]{cor}}.
#' @param   ...         Each argument for \code{\link[stats]{nls}} can be indicated.
#'                      See 'Arguments' of \code{\link[stats]{nls}} for more information.
#' @return  A list conforming the return value of \code{\link[ggd]{nls.freq}}.
#' @seealso \code{\link[ggd]{nls.freq}}
################################################################################################
nls.freq.level.100 <- function( data, total, kind, mix.type, grad, custom.d, ncmp,
                                eq.mean, eq.sd, control, cor.method, ... )
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
                                      custom.d      = custom.d,
                                      ncmp          = ncmp,
                                      eq.mean       = eq.mean,
                                      eq.sd         = eq.sd,
                                      start.level   = level,
                                      control       = control, ... ),
                        warning = function( w )
                        {
                            wrnl <<- append( wrnl,
                                             list( list( level = level,
                                                         message = conditionMessage( w ) ) ) )
                        } ),
                    error = function( e )
                    {
                        errl <<- append( errl,
                                         list( list( level = level,
                                                     message = conditionMessage( e ) ) ) )
                    } ) ) )
    } )


    cors <- ggd.cor.vs.freq( lapply( outl, function( out ) out$obj ),
                             data$x, data$freq, total, cor.method )
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
#' Each index number of above list is
#' also set into \code{kind.index} field of each \code{\link[ggd]{GGD}} object.
#'
#' This function generates 16 \code{\link[ggd]{GGD}} objects and
#' calls \code{\link[ggd]{nls.freq}} method 16 times.
#' By default, \code{\link[ggd]{nls.freq}} is called with \code{warnOnly = TRUE},
#' so \code{\link[ggd]{nls.freq}} does not generate errors, but generates warnings often.
#' When a warning occur, this function generates another warning like
#' '\code{Warning for kind = xx :}' to inform which \code{kind.index} gets a poor result
#' (poor, but may be accurate enough).
#' So when one warning has occurred, two warnings will occur eventually.
#'
#' If you indicate \code{warnOnly = FALSE} in \code{control} argument
#' and overwrite \code{warnOnly} option, \code{\link[ggd]{nls.freq}} can generate errors.
#' If an error occurs in one of \code{\link[ggd]{nls.freq}} processes,
#' this function throws messages like '\code{Error for kind = xx :}' and '\code{Error in ...}'
#' instead of throwing an error and skips the process,
#' then tries other \code{\link[ggd]{nls.freq}} processes.
#' For the result of error-occurred \code{kind.index}, a cleared \code{\link[ggd]{GGD}} object
#' will be got as the element of \code{obj} (see 'Value').
#'
#' @export
#' @param   data    A data frame which represents the frequency distribution.
#'                  It must contain at least 2 numeric columns for \code{x} and \code{freq}.
#'                  See \code{\link[ggd]{nls.freq}} for more information.
#'
#'                  Column names and indexes for \code{x} and \code{freq} are flexible.
#'                  You can indicate them with next two arguments.
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
#' @param   start.level A numeric value of integer in from \code{0} to \code{3} or \code{100}
#'                      with default \code{100}; the level of guessing the start values
#'                      for \code{start} argument of \code{\link[stats]{nls}}.
#'                      See \code{\link[ggd]{nls.freq}} for more information.
#'
#' @param   start   A \bold{list of lists} with the length of 16
#'                  for each of \code{start} arguments of \code{\link[stats]{nls}}
#'                  as start values.
#'                  Each element (a list) will give to \code{start} argument of
#'                  \code{\link[stats]{nls}} one by one.
#'
#'                  For cases where \code{NULL} is indicated in this argument,
#'                  internally computed start values depending on \code{start.level} are used.
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
#'                  See 'Examples' for usages of these tools.
#'
#' @param   control The \code{control} argument for \code{\link[stats]{nls}}.
#'                  See \code{\link[stats]{nls.control}} for more information.
#'
#' @param   not.use.nls A logical.
#'                  If \code{TRUE}, this function does not use \code{\link[stats]{nls}}
#'                  and it outputs objects having the start values in \code{cmp} field
#'                  as the results.
#'                  If \code{FALSE}, this function uses \code{\link[stats]{nls}}.
#'
#'                  This argument works when \code{start.level} argument is
#'                  other than \code{100}.
#'                  A warning will occur if \code{TRUE} when \code{start.level} is \code{100}.
#'
#'                  You can use \code{not.use.nls = TRUE} to check whether the start values
#'                  are appropriate when obtained an undesirable result from this function.
#'
#' @param   cor.method  The \code{method} argument for \code{\link[stats]{cor}}.
#'                      It is a character string indicating which correlation coefficient
#'                      (or covariance) is to be computed.
#'                      If \code{NULL}, it uses the default method of \code{\link[stats]{cor}}.
#'                      See \code{\link[stats]{cor}} for more information.
#'
#' @param   ...     Each argument for \code{\link[stats]{nls}} can be indicated.
#'                  See 'Arguments' of \code{\link[stats]{nls}} for more information.
#'
#' @return  A list containing components
#'          \item{best}{
#'                  The \code{\link[ggd]{GGD}} object which has got
#'                  the highest correlation coefficient.
#'                  That is, it would be the most approximate to
#'                  the given frequency distribution in all of the supported models.
#'                  If there are some models which have got the same highest correlation
#'                  coefficient, the object with the earlier \code{kind.index} is
#'                  given priority.}
#'
#'          \item{best.cor}{
#'                  The correlation coefficient of \code{best} for the given
#'                  frequency distribution.}
#'
#'          \item{obj}{
#'                  The list of 16 \code{\link[ggd]{GGD}} objects
#'                  ordered along with \code{ggd:::kinds}.
#'                  If an error has occurred, the element will be a cleared object.}
#'
#'          \item{cor}{
#'                  The vector of the correlation coefficients
#'                  between the result of each model and the frequency distribution.
#'                  \code{NA} will be given for an error case or an extremely bad result.}
#'
#'          \item{detail}{
#'                  The list of 16 results of \code{\link[ggd]{nls.freq}}.
#'                  Normally, each element is a list of the output of
#'                  \code{\link[ggd]{nls.freq}}.
#'                  But if an error has occurred, the element will be an error condition.
#'                  See 'Value' of \code{\link[ggd]{nls.freq}} for more information.}
#'
#' @importFrom  methods     new
#' @seealso \code{\link[ggd]{nls.freq}}, \code{\link[stats]{cor}},
#'          \code{\link[ggd]{ggd.init.start}}, \code{\link[ggd]{ggd.start.template}}
#' @examples
#'  ## Preparing:
#'  df <- data.frame(
#'              x = seq( -2, 2, 0.2 ),
#'              freq = c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'                        7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'                        3698,  2740,  2549,  2284,  1499,  1147,   918 ) )
#'
#'  ## This function plots probability densities obtained from the frequency distribution
#'  ## and the probability density function of a GGD object.
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
#'  ## Examples:
#'  ## We specify 'start.level = 1' here in order to obtain less-than-ideal results purposely
#'  ## to explain how to use 'start' argument using the results.
#'  result <- ggd.nls.freq.all( df, start.level = 1 )
#'
#'  ## Show the results.
#'  result$cor
#'  result$best.cor; result$best$kind.index
#'
#'  ## Check that the value of 'cor' for kind = 14 is very low.
#'  result$cor[[14]]
#'
#'\dontrun{
#'  ## If you want to ignore warning cases at ggd.nls.freq.all,
#'  ## indicate 'warnOnly = FALSE' in 'control' option.
#'  ## This option turns warnings into errors, and cases which cause errors are skipped.
#'  result <- ggd.nls.freq.all( df, start.level = 1, control = list( warnOnly = FALSE ) )}
#'
#'  ## Let's try to increase the value of 'cor' by changing the start values.
#'  ##
#'  ## There is an easy and good way to increase the value of 'cor',
#'  ## that is to remove 'start.level = 1'.
#'  ## But here, we keep 'start.level = 1' and use 'start' argument for explaining.
#'  ##
#'  ## First, to see what 'kind = 14' is, display the kind.
#'  result$obj[[14]]$kind
#'
#'  ## Also, using ggd.kind, you can get the character string for the index.
#'  ggd.kind( 14 )
#'
#'  ## Inversely, using ggd.kind.index, you can get index for each kind.
#'  ggd.kind.index( "Mean-Differed Sigma-Equaled Horizontal-Vertical" )
#'
#'  ## Show the 'cmp' field and plot the probability density function.
#'  result$obj[[14]]$cmp
#'  plot.freq.and.d( result$obj[[14]], df$x, df$freq )
#'
#'  ## Now, for the start values, we are going to use the result of
#'  ## '2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution'.
#'  ggd.kind.index( "2-Mean-Differed Sigma-Equaled Vertical" )  ## 8
#'  result$cor[[8]]
#'  result$obj[[8]]$cmp
#'
#'  ## Display the parameters for the start values for kind = 14.
#'  ggd.start.template( 14 )
#'
#'  ## Specify the start values for kind = 14.
#'  start.list <- ggd.init.start()
#'  start.list[[14]] <- ggd.start.template( 14 )
#'  start.list[[14]]$mean.1.1 <- result$obj[[8]]$cmp$mean[1]
#'  start.list[[14]]$mean.1.2 <- result$obj[[8]]$cmp$mean[2]
#'  start.list[[14]]$mean.2.1 <- result$obj[[8]]$cmp$mean[1]
#'  start.list[[14]]$mean.2.2 <- result$obj[[8]]$cmp$mean[2]
#'  start.list[[14]]$sqrt.sd <- sqrt( result$obj[[8]]$cmp$sd[1] )   ## Take sqrt for SD.
#'
#'  ## Retry ggd.nls.freq.all.
#'  result <- ggd.nls.freq.all( df, start.level = 1, start = start.list )
#'  result$cor[[14]]
#'  result$obj[[14]]$cmp
#'  plot.freq.and.d( result$obj[[14]], df$x, df$freq )
#'
#'  ## On the other hand, you can check the start values with 'not.use.nls = TRUE'.
#'  start <- ggd.nls.freq.all( df, start.level = 1, not.use.nls = TRUE )
#'  start$cor
#'  start$best.cor; start$best$kind.index
#'  start$obj[[14]]$cmp
#'  plot.freq.and.d( start$obj[[14]], df$x, df$freq )
#'
#'  ## You will find that 'start.level = 2' can generate reasonable start values basically.
#'  start <- ggd.nls.freq.all( df, start.level = 2, not.use.nls = TRUE )
#'  start$cor
#'  start$best.cor; start$best$kind.index   ## 14
#'  start$obj[[14]]$cmp
#'  plot.freq.and.d( start$obj[[14]], df$x, df$freq )
#'
#'  ## Using 'cor.method' argument,
#'  ## you can evaluate the correlations with nonparametric methods.
#'  start <- ggd.nls.freq.all( df, start.level = 2, not.use.nls = TRUE,
#'                             cor.method = "kendall" )
#'  start$cor
#'  start$best.cor; start$best$kind.index   ## 2
#'  start$obj[[2]]$cmp
#'  plot.freq.and.d( start$obj[[2]], df$x, df$freq )
################################################################################################
ggd.nls.freq.all <- function( data, x = "x", freq = "freq", total = NULL,
                              start.level = 100, start = NULL,
                              control = list( maxiter = 300, warnOnly = TRUE ),
                              not.use.nls = FALSE, cor.method = NULL, ... )
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

    if ( isTRUE( not.use.nls ) && start.level == 100 )
    {
        warning( paste( "Warning: not.use.nls does not work",
                                 "with start.level is default 100." ) )
        not.use.nls <- FALSE
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

    # Execute nls.
    # If an error occurs, the error message is displayed but other processes continue.
    results <- lapply( 1:16,
    function( i )
    {
        # Remark if an error occurs at nls.freq, obj will be a cleared object (not NULL).
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
                                       cor.method       = cor.method, ... ),
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

    cors <- ggd.cor.vs.freq( objs, data.ext$x, data.ext$freq, total, cor.method )

    best.cor <- max( cors, na.rm = TRUE )

    return ( list( best = objs[( !is.na( cors ) ) & ( cors == best.cor )][[1]],
                   best.cor = best.cor, obj = objs, cor = cors, detail = details ) )
}

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
        stop( "Error: Too few rows for data." )
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
#'                  Both integers and real numbers are allowed for the values.
#'
#' @param   total   Total value of the frequencies.
#'
#'                  If \code{NULL} (the default),
#'                  the total of \code{freq}, i.e., \code{\link[base]{sum}(freq)}
#'                  is used for it.
#'
#' @return  The vector of expected probability density values following the x-coordinates.
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
#' the x-coordinate of the frequency distribution.
#' This function assumes that each x-coordinate of the frequency distribution is at the center
#' of a cell and that an equal number of samples of both upper and lower outside of the range
#' have been excluded.
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
#'
#'                      In this function, \code{grad} argument have no priority
#'                      over \code{mix.type} argument.
#'                      Therefore, \code{grad = "v2"} or \code{"v3"} works only if
#'                      \code{mix.type = 3} is indicated.
#'                      And if \code{mix.type = 3} is indicated,
#'                      either \code{grad = "v2"} or \code{"v3"} must be indicated.
#'                      If \code{"v3"}, the number of components will forced to be 3.
#'
#' @param   ncmp        Number of components for the custom distribution.
#' @param   eq.mean     A logical; the flag whether to make all of the mean values of
#'                      the normal distributions of the components to be equal.
#' @param   eq.sd       A logical; the flag whether to make all of the standard deviations of
#'                      the normal distributions of the components to be equal.
#' @param   start.level An integer from \code{0} to \code{3} or \code{NA};
#'                      the desired level at which to guess
#'                      the initial \code{start} parameters for \code{\link[stats]{nls}}.
#'
#'          The detail of each level is:
#'          \itemize{
#'              \item from \code{0} to \code{3}:
#'                      Same as \code{start.level} in arguments of \code{\link[ggd]{nls.freq}}.
#'              \item \code{NA}: All elements in \code{start} will be \code{NA}.
#'          }
#' @return  A list containing \code{formula} and \code{start} for \code{\link[stats]{nls}}
#'          and adopted \code{start.level}.
#'          The value of \code{start.level} may be different from the argument value
#'          if initial guessing has failed at the desired level.
#' @importFrom  stats   dnorm pnorm as.formula
################################################################################################
get.nls.params <- function( x, freq, total, mix.type, grad, ncmp,
                            eq.mean, eq.sd, start.level )
{
    fm <- NULL          # formula for return value
    start <- list()     # start for return value

    # Mean and standard deviation of the data
    data.mean <- sum( x * freq ) / total
    data.sd   <- sqrt( sum( ( data.mean - x )^2 * freq ) / total )

    # Exclude data with 0 or extremely small frequencies on both edges of
    # the range of the x-coordinates.
    x.freq <- exclude.freq.edge( x, freq )

    # Quarter the vectors of the frequency distribution and get start values.
    #
    # The start values are as:
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
    #   sqrt.sds.mid:
    #       Global or local standard deviation corresponding to around center of the range of
    #       the frequency distribution.
    if ( is.na( start.level ) )
    {
        # Level NA: all parameters are NA.
        means.mid <- means <- rep( NA_real_, 4 )
        sqrt.sds.mid <- sqrt.sds <- rep( NA_real_, 4 )

        mean.lower <- mean.upper <- mean.inner <- mean.outer <- NA_real_
        sqrt.sd.lower <- sqrt.sd.upper <- sqrt.sd.inner <- sqrt.sd.outer <- NA_real_
    }
    else if ( start.level == 0 )
    {
        # Level 0: use global mean value and standard deviation for all start values.
        means <- rep( data.mean, 4 )
        sqrt.sds <- rep( sqrt( data.sd ), 4 )

        mean.lower <- mean.upper <- mean.inner <- mean.outer <- data.mean
        sqrt.sd.lower <- sqrt.sd.upper <- sqrt.sd.inner <- sqrt.sd.outer <- sqrt( data.sd )

        means.mid <- rep( data.mean, 4 )
        sqrt.sds.mid <- sqrt( data.sd )
    }
    else if ( start.level == 1 )
    {
        # Level 1: compute local mean values and standard deviations.
        sep <- divide.data.quarter( x.freq$x, x.freq$freq, data.mean )

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
        sqrt.sds.mid <- sqrt( data.sd )
    }
    else if ( start.level >= 2 )
    {
        # Level 2: pick up 2 quantiles for each component.
        freq.ps <- get.p.freq( x.freq$freq, total )
        sep <- divide.data.quarter( x.freq$x, freq.ps, data.mean )
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
        sqrt.sds.mid <- sqrt( data.sd )

        if ( start.level == 3 )
        {
            # Level 3: tracing quantiles.
            # Overwriting the guessing to level 2 guessing.
            if ( mix.type == 0 )
            {
                ms <- ms.norm.xp( c( sep$x[[1]][lengths[1]], sep$x[[4]][1] ),
                                  c( sep$data[[1]][lengths[1]], sep$data[[4]][1] ) )

                means.mid <- rep( ms$mean, 4 )
                sqrt.sds.mid <- sqrt( ms$sd )
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
                    start.level <- 2
                }
                else
                {
                    if ( mix.type == 4 )
                    {
                        means.mid <- means <- obj$cmp$mean[c( 1, 2, 4, 3 )]
                        sqrt.sds <- sqrt( obj$cmp$sd[c( 1, 2, 4, 3 )] )
                        sqrt.sds.mid <- ( sqrt.sds[2] + sqrt.sds[3] ) / 2
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

                        sqrt.sds.mid <- sqrt.sds[2]
                    }
                }
            }
        }
    }

    if ( mix.type == 0 )
    {
        # via Normal Distribution
        fm <- d ~ dnorm( x, mean, sqrt.sd^2 )

        start <- list( mean = means.mid[1], sqrt.sd = sqrt.sds.mid )
    }
    else
    {
        if ( mix.type == 1 )
        {
            # via Mean of 2 Normal Distributions
            if ( eq.sd )
            {
                fm <- d ~ ( dnorm( x, mean.1, sqrt.sd^2 ) +
                            dnorm( x, mean.2, sqrt.sd^2 ) ) / 2
                start <- list( mean.1 = mean.lower,
                               mean.2 = mean.upper,
                               sqrt.sd = sqrt.sds.mid )
            }
            else if ( eq.mean )
            {
                fm <- d ~ ( dnorm( x, mean, sqrt.sd.1^2 ) +
                            dnorm( x, mean, sqrt.sd.2^2 ) ) / 2
                start <- list( mean = means.mid[1],
                               sqrt.sd.1 = sqrt.sd.outer,
                               sqrt.sd.2 = sqrt.sd.inner )
            }
            else
            {
                fm <- d ~ ( dnorm( x, mean.1, sqrt.sd.1^2 ) +
                            dnorm( x, mean.2, sqrt.sd.2^2 ) ) / 2
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
                               sqrt.sd  = sqrt.sds.mid )
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
                                   sqrt.sd = sqrt.sds.mid )
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
                                   sqrt.sd = sqrt.sds.mid )
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
        else if ( mix.type == 4 )
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
                               sqrt.sd = sqrt.sds.mid )
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
        else # if ( mix.type == 5 )
        {
            # This loop must be done in sequential.
            mean.string <- sd.string <- ""
            for ( i in 1:ncmp )
            {
                mean.string <- paste0( mean.string, "mean.", i )
                sd.string <- paste0( sd.string, "sqrt.sd.", i, "^2" )

                if ( i < ncmp )
                {
                    mean.string <- paste0( mean.string, ", " )
                    sd.string <- paste0( sd.string, ", " )
                }
            }

            # env needs to be specified in order to access class field.
            fm <- as.formula( paste( "d ~ custom.d(x, data.frame(mean = c(",
                                     mean.string, "), sd = c(", sd.string, ")))" ),
                              env = parent.frame() )

            # Assign start values as evenly as possible.
            if ( ncmp == 1 )
            {
                start <- list( mean.1    = ( means[2] + means[3] ) / 2,
                               sqrt.sd.1 = ( sqrt.sds[2] + sqrt.sds[3] ) / 2 )
            }
            else if ( ncmp == 2 )
            {
                start <- list( mean.1    = ( means[1] + means[3] ) / 2,
                               mean.2    = ( means[2] + means[4] ) / 2,
                               sqrt.sd.1 = ( sqrt.sds[1] + sqrt.sds[3] ) / 2,
                               sqrt.sd.2 = ( sqrt.sds[2] + sqrt.sds[4] ) / 2 )
            }
            else if ( ncmp == 3 )
            {
                start <- list( mean.1    = ( means[1] + means[2] ) / 2,
                               mean.2    = ( means[2] + means[3] ) / 2,
                               mean.3    = ( means[3] + means[4] ) / 2,
                               sqrt.sd.1 = ( sqrt.sds[1] + sqrt.sds[2] ) / 2,
                               sqrt.sd.2 = ( sqrt.sds[2] + sqrt.sds[3] ) / 2,
                               sqrt.sd.2 = ( sqrt.sds[3] + sqrt.sds[4] ) / 2 )
            }
            else
            {
                # This loop must be done in sequential.
                start.string <- "list("
                for ( i in 1:ncmp )
                {
                    start.string <- paste0( start.string,
                                            "mean.", i, " = means[",
                                            1 + floor( ( i - 1 ) * 4 / ncmp ), "], ",
                                            "sqrt.sd.", i, " = sqrt.sds[",
                                            1 + floor( ( i - 1 ) * 4 / ncmp ), "]" )
                    if ( i < ncmp )
                    {
                        start.string <- paste0( start.string, ", " )
                    }
                    else
                    {
                        start.string <- paste0( start.string, ")" )
                    }
                }

                start <- eval( parse( text = start.string ) )
            }

        }
    }

    return ( list( formula = fm, start = start, start.level = start.level ) )
}

################################################################################################
#' [Non-exported] Exclude small frequencies on edges
#'
#' Excludes data with \code{0} or extremely small frequencies on both edges of the range of
#' the x-coordinates from data of a frequency distribution.
#' @usage   exclude.freq.edge(x, freq)
#' @param   x               A vector of x-coordinates of a frequency distribution.
#' @param   freq            A vector of frequencies of a frequency distribution.
#' @return  A list containing components
#'          \item{x}{
#'                  The vectors of x-coordinates after excluding.}
#'          \item{freq}{
#'                  The vectors of frequencies after excluding.}
################################################################################################
exclude.freq.edge <- function( x, freq )
{
    min.i <- min( ( 1:length( freq ) )[freq > .Machine$double.eps * max( freq )] )
    max.i <- max( ( 1:length( freq ) )[freq > .Machine$double.eps * max( freq )] )

    return ( list( x = x[min.i:max.i], freq = freq[min.i:max.i] ) )
}

################################################################################################
#' [Non-exported] Quartering data
#'
#' Divides numeric data into 4 groups as:
#' at first, it divides the data into two groups by the mean of the data,
#' and next, divides each data into two groups so that the number of elements are equal to
#' each other.
#' If there is an x-coordinate equal to the mean,
#' the element is included in both of the 2nd and 3rd groups.
#' In the second division, if the original group has an odd number of elements,
#' the element at the dividing point is included in both divided groups.
#' @param   x           A vector of x-coordinates arranged in ascending order
#'                      and non-duplicated.
#' @param   data        A vector of numeric values of the data with respect to x-coordinates.
#' @param   data.mean   Mean value of \code{data}.
#' @return  A list containing components
#'          \item{x}{
#'                  The vectors of x-coordinates of the 4 groups.}
#'          \item{data}{
#'                  The vectors of data of the 4 groups.}
#'          \item{(x|data).lower}{
#'                  The vector of x-coordinates or data integrated the lower-side 2 groups.}
#'          \item{(x|data).upper}{
#'                  The vector of x-coordinates or data integrated the upper-side 2 groups.}
#'          \item{(x|data).outer}{
#'                  The vector of x-coordinates or data integrated the outer-side 2 groups.}
#'          \item{(x|data).inner}{
#'                  The vector of x-coordinates or data integrated the inner-side 2 groups.}
#' @importFrom  utils       head tail
################################################################################################
divide.data.quarter <- function( x, data, data.mean )
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
#'
#'                      In this function, \code{grad} argument has no priority over
#'                      \code{mix.type} argument.
#'                      Therefore, \code{grad = "v2"} or \code{"v3"} works
#'                      only if \code{mix.type = 3} is indicated.
#'                      And if \code{mix.type = 3} is indicated,
#'                      either \code{grad = "v2"} or \code{"v3"} must be indicated.
#'                      If \code{"v3"}, the number of components will forced to be 3,
#'                      even if the values in \code{coefs} represent a 2-component model.
#'
#' @param   ncmp        Number of components for the custom distribution.
#' @param   eq.mean     A logical; the flag whether to make all of the mean values of
#'                      the normal distributions of the components to be equal.
#' @param   eq.sd       A logical; the flag whether to make all of the standard deviations of
#'                      the normal distributions of the components to be equal.
#' @return  A data frame for \code{cmp} field
#'          according to the result of \code{\link[stats]{nls}}.
################################################################################################
get.cmp.with.nls.coef <- function( coefs, mix.type, grad, ncmp, eq.mean, eq.sd )
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
    else if ( mix.type == 4 )
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
    else # if ( mix.type == 5 )
    {
        # Custom Distribution: number of components is ncmp
        mean.names  <- vapply( 1:ncmp, function( i ) paste0( "mean.", i ), "" )
        sd.names    <- vapply( 1:ncmp, function( i ) paste0( "sqrt.sd.", i ), "" )

        cmp <- data.frame( mean = c( unname( coefs[mean.names] ) ),
                           sd   = c( unname( coefs[sd.names] )^2 ) )
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
#' @param   objs        A \code{\link[ggd]{GGD}} object or a list of
#'                      \code{\link[ggd]{GGD}} objects.
#' @param   x           The vector of the x-coordinates of the frequency distribution.
#' @param   freq        The vector of the frequencies of the frequency distribution.
#' @param   total       Total value of the frequencies.
#' @param   cor.method  The \code{method} argument for \code{\link[stats]{cor}}.
#'                      It is a character string indicating which correlation coefficient
#'                      (or covariance) is to be computed.
#'                      If \code{NULL}, it uses the default \code{method}.
#'                      See \code{\link[stats]{cor}} for more information.
#' @return  A vector of correlation coefficients.
#'          Its order follows the order of the elements in \code{objs} argument.
#'          If \code{cmp} field of an \code{\link[ggd]{GGD}} object in \code{objs} has no rows,
#'          \code{NA} will be set to the element.
#' @importFrom  stats   cor
#' @examples
#'  df <- data.frame( x     = seq( -2, 2, 0.2 ),
#'                    freq  = c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'                               7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'                               3698,  2740,  2549,  2284,  1499,  1147,   918 ) )
#'  objs <- list( ggd.nls.freq( df, kind = "Mean-Eq.*Vertical" )$obj,
#'                ggd.nls.freq( df, kind = "Sigma-Eq.*Vertical" )$obj,
#'                ggd.nls.freq( df, kind = "Vertical" )$obj )
#'
#'  ggd.cor.vs.freq( objs[[1]], df$x, df$freq )
#'  ggd.cor.vs.freq( objs, df$x, df$freq )
################################################################################################
ggd.cor.vs.freq <- function( objs, x, freq, total = sum( freq ), cor.method = NULL )
{
    if ( inherits( objs, "GGD" ) )
    {
        objs <- list( objs )
    }

    if ( is.null( cor.method ) )
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

                                cor.f( obj$d( x ), get.d.freq( x, freq, total ), cor.method )
                            }, 0 ) ) )

    return ( cors )
}

################################################################################################
#' Initialize start for ggd.nls.freq.all
#'
#' Generates a list of 16 \code{NULL} lists.
#' If you want to indicate some start values for \code{\link[ggd]{ggd.nls.freq.all}},
#' insert a list of the start values instead of \code{NULL} at some elements of
#' the 16-\code{NULL} list and use it as \code{start} argument.
#' For more information about the format of each element and other details,
#' see \code{\link[ggd]{ggd.start.template}} and \code{\link[ggd]{ggd.nls.freq.all}}.
#' @export
#' @return  An all-\code{NULL} list with the length of 16 (= \code{length(ggd:::kinds)}).
#' @seealso \code{\link[ggd]{ggd.start.template}}, \code{\link[ggd]{ggd.nls.freq.all}}
#' @examples
#'  ## Let's approximate this frequency distribution.
#'  df <- data.frame(
#'              x = seq( -2, 2, 0.2 ),
#'              freq = c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'                        7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'                        3698,  2740,  2549,  2284,  1499,  1147,   918 ) )
#'
#'  ## Get the initial list.
#'  start.list <- ggd.init.start()
#'  start.list
#'
#'  ## Check the parameters for ggd:::kinds[14].
#'  ggd.start.template( 14 )
#'
#'  ## Set the start values of ggd:::kinds[14] to get better result.
#'  start.list[[14]]$mean.1.1 <-  0.426831
#'  start.list[[14]]$mean.1.2 <- -0.130757
#'  start.list[[14]]$mean.2.1 <-  0.426831
#'  start.list[[14]]$mean.2.2 <- -0.130757
#'  start.list[[14]]$sqrt.sd <- sqrt( 0.812744 )
#'
#'  ## Run ggd.nls.freq.all (the result of ggd:::kinds[14] will be better than ggd:::kinds[8]).
#'  result <- ggd.nls.freq.all( df, start.level = 1, start = start.list )
#'  result$cor[8]
#'  result$cor[14]
################################################################################################
ggd.init.start <- function()
{
    return ( lapply( 1:16, function( x ) NULL ) )
}

################################################################################################
#' Template for start of nls
#'
#' Gets a template list for \code{start} argument of \code{\link[ggd]{nls.freq}}
#' or for an element of \code{start} argument of \code{\link[ggd]{ggd.nls.freq.all}}.
#' This function can output only one template at a time.
#' So if you want to obtain two or more templates,
#' you should call this function one by one for each one.
#' @export
#' @param   kind    A character string or a numeric value or a \code{\link[ggd]{GGD}} object
#'                  which indicates the kind of distribution model for which the template will
#'                  be output. The length should be 1.
#'
#'                  The matching method of \code{kind} follows that of
#'                  \code{objs} argument of \code{\link[ggd]{ggd.kind.index}}.
#'                  If it matches to two or more distribution models,
#'                  this function returns only one template for the first matched model.
#'
#' @param   ncmp    Number of components for the custom distribution.
#'
#'                  If \code{kind} indicates \code{"Custom Distribution"} or \code{17},
#'                  a positive integer must be indicated for this argument.
#'
#'                  When \code{ncmp} is \code{0} (the default),
#'                  if a \code{\link[ggd]{GGD}} object is indicated for \code{kind},
#'                  the number of rows in \code{cmp} field of the object is used.
#'                  When \code{ncmp} is a positive integer, the value of \code{ncmp}
#'                  is used.
#' @return  A list containing components any of
#'          \item{mean}{
#'                  The start value for the mean value shared by all components. 0 is preset.}
#'          \item{mean.i}{
#'                  The start value for the mean value of the i-th component.
#'                  0 is preset.}
#'          \item{mean.i.j}{
#'                  The start value for the mean value of the i,j-th component
#'                  in the 2x2 components. 0 is preset.}
#'          \item{sqrt.sd}{
#'                  The start value for the square root of the standard deviation shared
#'                  by all components. 1 is preset.}
#'          \item{sqrt.sd.i}{
#'                  The start value for the square root of the standard deviation of
#'                  the i-th component. 1 is preset.}
#'          \item{sqrt.sd.i.j}{
#'                  The start value for the square root of the standard deviation of
#'                  the i,j-th component in the 2x2 components. 1 is preset.}
#'
#'          If \code{kind} represents an unsupported distribution model,
#'          \code{NULL} will be returned.
#' @seealso \code{\link[ggd]{nls.freq}}, \code{\link[ggd]{ggd.nls.freq.all}},
#'          \code{\link[ggd]{ggd.init.start}}
#' @examples
#'  ## Preparing:
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 1517,  2292,  2513,  2763,  3724,  4046,  4713,
#'             7947, 10997, 11824, 11133,  7868,  4692,  4103,
#'             3698,  2740,  2549,  2284,  1499,  1147,   918 )
#'
#'  ## Set the start values.
#'  start <- ggd.start.template( 14 )
#'  start   ## Check the parameters for the 'start' of ggd:::kinds[14].
#'
#'  start$mean.1.1 <- -0.671
#'  start$mean.1.2 <- -0.198
#'  start$mean.2.1 <-  0.293
#'  start$mean.2.2 <- -0.198
#'  start$sqrt.sd <- sqrt( 0.640 )  ## 'sqrt.sd' is the square root of the standard deviation.
#'
#'  ## Run ggd.nls.freq.
#'  ggd.nls.freq( data.frame( x, freq ), start = start, kind = 14 )$obj
################################################################################################
ggd.start.template <- function( kind, ncmp = 0 )
{
    kind.index <- ggd.kind.index( kind, undef.err = FALSE )[1]
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
    else if ( kind.index == 17 )
    {
        ncmp <- as.integer( ncmp )
        if ( ncmp == 0 )
        {
            if ( inherits( kind, "GGD" ) )
            {
                ncmp <- nrow( kind$cmp )
            }
            else
            {
                stop( paste( "Error: ncmp should be given the number of components." ) )
            }
        }
        else if ( ncmp < 0 )
        {
            stop( paste( "Error: ncmp should be positive." ) )
        }

        start.string <- "list("
        for ( i in 1:ncmp )
        {
            start.string <- paste0( start.string, "mean.", i, " = 0, ", "sqrt.sd.", i, " = 1" )
            if ( i < ncmp )
            {
                start.string <- paste0( start.string, ", " )
            }
            else
            {
                start.string <- paste0( start.string, ")" )
            }
        }

        temp <- eval( parse( text = start.string ) )
    }

    return ( temp )
}
