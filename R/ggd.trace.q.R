################################################################################################
# Quantile tracing
# @file         ggd.trace.q.R
# @author       Kimitsuna Ura
# @copyright    Copyright (C) 2024 Kimitsuna Ura
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Functions

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
#'                      the \code{objs} argument of the \code{\link[ggd]{ggd.kind.index}}.
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
#'                  which has succeeded to solve tracing quantiles.}
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
#'      and also determines the degrees of freedom of the distribution model.
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

                                    # First, get 2 SDs of the normal distributions which
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
#' [Non-exported] PDF/CDF for mix.type = 3
#'
#' Calculates the values of the probability density function or
#' the cumulative distribution function of the GGD model with \code{mix.type = 3}.
#' Both \code{means} and \code{sds} vectors need 3 elements for this function.
#' Where with two components, you must set \code{means[3]} and \code{sds[3]}
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
#' with mean-differed 2 normal distributions of the components by crossover-tracing.
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

    succeeded <- FALSE
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
            succeeded <- TRUE
            break
        }

        message( paste( "nleqslv has once failed. Message:",
                        result$message ) )
        message( paste0( "Message: Tracing with crossing over #", i,
                        " quantile has failed.",
                        " The result may distort heavily." ) )
    }

    if ( !succeeded )
    {
        stop( "Error: Failed to construct components." )
    }

    return ( list( cmp = cmp, result = result ) )
}

################################################################################################
#' [Non-exported] Gets cmp field (4 quantiles, vertical gradation of 2 normal distributions)
#'
#' Gets the data frame for \code{cmp} field where \code{mix.type = 3} and tracing 4 quantiles
#' with 2 normal distributions of the components.
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
#' with 3 normal distributions of the components.
#' @param   qt          A data frame; the quantiles to be traced.
#' @param   eq.mean     A logical; the flag to be equal all of the mean values.
#' @param   eq.sd       A logical; the flag to be equal all of the standard deviations.
#' @param   grad        A character string indicating the method of gradation.
#'                      If \code{"v3"}, constructing with 3 components is enforcedly,
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
#' \code{[1]} left-tail side, \code{[2]} top side and \code{[3]} right-tail side.
#' @export
#' @param   means       A vector of mean values of the 3 normal distributions of the components.
#' @param   sds         A vector of standard deviations of the 3 normal distributions of
#'                      the components.
#' @param   grad        A character string indicating the method of gradation.
#'                      If \code{"v3"}, the number of components is forced to be 3.
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
