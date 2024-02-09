################################################################################################
# Setting or adjusting cmp field
# @file         ggd.set.cmp.R
# @author       Kimitsuna Ura
# @copyright    Copyright (C) 2024 Kimitsuna Ura
# @license      Released under the MIT license. See https://opensource.org/licenses/MIT/
################################################################################################

################################################################################################
#  Functions

################################################################################################
#' Setting components
#'
#' Generates a \code{\link[ggd]{GGD}} object according to indicated values of \code{cmp} field
#' (\code{ggd.set.cmp}), or sets the values in \code{cmp} field and all of other fields
#' accordingly (\code{set.cmp}).
#' Whenever you want to set values in \code{cmp} field, it is strongly recommended to use
#' this method.
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
#' @param   cmp         A data frame for \code{cmp} field.
#'
#'                      It must have just 2 columns named \code{"mean"} and \code{"sd"},
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
#'                      which indicates the kind of distribution model to be generated.
#'
#'                      The matching method of \code{kind} follows that of
#'                      \code{objs} argument of \code{\link[ggd]{ggd.kind.index}}.
#'                      \code{NA} is allowed when \code{cmp} has no rows.
#'
#'                      If \code{mix.type} argument or other than \code{"default"} for
#'                      \code{grad} argument is indicated, this argument will be ignored.
#'
#' @param   mix.type    A numeric value represents how to mix the normal distributions.
#'
#'                      The type of the distribution model and the number of rows in \code{cmp}
#'                      follow \code{mix.type} as:
#'                      \tabular{clc}{
#'                          \code{mix.type} \tab Distribution model     \tab Number of rows \cr
#'                          \code{0} \tab Normal distribution                   \tab 1 \cr
#'                          \code{1} \tab Mean of 2 normal distributions        \tab 2 \cr
#'                          \code{2} \tab Horizontal gradational distribution   \tab 2 \cr
#'                          \code{3} \tab Vertical gradational distribution     \tab 2 or 3 \cr
#'                          \code{4} \tab Horizontal-vertical gradational distribution  \tab 4
#'                      }
#'
#'                      If the number of rows in \code{cmp} argument is different from
#'                      the number shown above, \code{cmp} field will be
#'                      redundant/simplified to have the number of rows as above,
#'                      if possible. If not possible, an error will occur.
#'
#'                      You can indicate \code{mix.type = NA} only if \code{cmp} has no rows.
#'
#'                      If \code{grad} argument other than \code{"default"} is indicated,
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
#'                      then also, \code{'grad = "no"'} can be read as 'no gradation'.
#'
#'                      \code{"default"} is, if \code{kind} or \code{mix.type} argument
#'                      is given, follows it, otherwise it depends on the number of columns
#'                      in \code{cmp} argument. If the number of columns in cmp is \code{2},
#'                      the current \code{mix.type} is retained or horizontal (default) is used.
#'
#' @param   this.cmp    A data frame for setting into \code{cmp} field.
#'                      It is equivalent to \code{cmp} argument of \code{ggd.set.cmp}.
#'
#' @param   this.kind   A string or a numeric value or a \code{\link[ggd]{GGD}} object
#'                      which indicates the kind of distribution model to be constructed.
#'                      It is equivalent to \code{kind} argument of \code{ggd.set.cmp}.
#'
#' @param   this.mix.type   A numeric value represents how to mix the normal distributions.
#'                          It is equivalent to \code{mix.type} argument of \code{ggd.set.cmp}.
#'
#' @return  The \code{\link[ggd]{GGD}} object itself (invisible for \code{GGD} method).
#'
#'          For \code{GGD} method: If an error occurs, each value of field will not be changed.
#'
#' @importFrom  methods     new
#'
#' @details
#'  \subsection{About 'kind' and 'mix.type'}{
#'      In this function,
#'      unlike \code{\link[ggd]{trace.q}} and \code{\link[ggd]{nls.freq}} methods,
#'      \code{[this.]kind} argument is only used to determine \code{mix.type} value,
#'      which represents how to mix the normal distributions of the components.
#'      That is, \code{[this.]kind} argument has no effect to align the mean values or
#'      standard deviations of the components to be equal.
#'
#'      So, a regular expression indicated as \code{[this.]kind} argument may not match
#'      the new value of \code{[this.]kind} field.
#'      For example, if you indicate \code{[this.]kind = "Mean-Eq.*Horizontal"} and
#'      \code{[this.]cmp = data.frame(mean = c(0, 1), sd = c(0.8, 1.2))},
#'      the new \code{kind} field will be
#'      \code{"Mean-Differed Sigma-Differed Horizontal Gradational Distribution"},
#'      which is not matched with indicated regular expression.
#'      In such a case, a warning will occur.
#'
#'      On the other hand, when a \code{\link[ggd]{GGD}} object is
#'      indicated as \code{[this.]kind}, no warning will occur if the new \code{kind} field
#'      is different from that of the indicated object.
#'      Indicated object is regarded as just for specifying \code{mix.type} value.
#'
#'      It is not recommended but if you indicate not-\code{NULL} values for both of
#'      \code{[this.]kind} and \code{[this.]mix.type} at once,
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
#'                                              Otherwise, \code{mix.type} will be \code{2}.
#'          \item \code{nrow([this.]cmp) = 3} : \code{mix.type} will be \code{3}.
#'          \item \code{nrow([this.]cmp) = 4} : \code{mix.type} will be \code{4}.
#'      }
#'  }
#'
#' @examples
#'  ## Normal Distribution
#'  a <- ggd.set.cmp( data.frame( mean = 0, sd = 1.5 ) )
#'  a$kind; a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Indicating "normal" for 'grad' argument is allowed. However, in this function,
#'  ## it works only to assert that 'cmp' represents a normal distribution.
#'  a <- ggd.set.cmp( data.frame( mean = 1, sd = 2 ), grad = "normal" )
#'  a$kind; a$mix.type; a$cmp
#'
#'  ## Where the number of rows in the 'cmp' argument is 2,
#'  ## it is recommended to indicate 'grad' or 'mix.type' or 'kind' to avoid confusing.
#'
#'  ## Mean of 2 normal distributions (mix.type = 1)
#'  ## is not a gradational Gaussian distribution (GDD),
#'  ## but a kind of Gaussian mixture model (GMM).
#'  rm( a )
#'  a <- ggd.set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1.2, 0.8 ) ),
#'                    kind = "Mean of.*2 Normal Distributions" )
#'  a$kind; a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Changing to mix.type = 2 : Horizontal Gradational Distribution
#'  ## while retaining 'cmp' field.
#'  a$set.cmp( this.mix.type = 2 )
#'  a$kind; a$mix.type; a$cmp
#'
#'  ## You can also write as:
#'  a$set.cmp( grad = "h" )
#'  a$kind; a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## kind = 9 : '2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution'
#'  ##
#'  ## You can also write as 'this.kind = "2.*Vertical"' or 'this.mix.type = 3' or 'grad = "v2"'
#'  ## instead of 'this.kind = 9'.
#'  a$set.cmp( this.kind = 9 )
#'  a$kind; a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## You can generate a same kind object with indicating a GGD object for 'kind' argument.
#'  b <- ggd.set.cmp( data.frame( mean = c( -1, -1 ), sd = c( 1.2, 0.4 ) ), kind = a )
#'  b$kind; b$mix.type; b$cmp
#'  plot( seq( -3, 3, 0.01 ), b$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Indicating 3 components generates a vertical gradation normally.
#'  a$clear()
#'  a$set.cmp( data.frame( mean = c( -0.5, 0, 0.5 ), sd = c( 1.2, 0.8, 1.2 ) ) )
#'  a$kind; a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Indicating 4 components generates a horizontal-vertical gradation normally.
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ), sd = c( 0.7, 0.5, 2.0, 1.5 ) ) )
#'  a$kind; a$mix.type; a$cmp
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## If 'cmp' field can be simplified, the number of components is automatically reduced,
#'  ## and 'mix.type' field follows the reduction.
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ), sd = c( 1, 0.7, 1, 0.7 ) ) )
#'  a$kind      ## '2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution'
#'  a$mix.type  ## 3 (not 4)
#'  a$cmp       ## with 2 rows
#'
#'  ## If you want not to simplify 'cmp' field,
#'  ## indicate '[this.]kind' or '[this.]mix.type' or 'grad' argument for the condition.
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ), sd = c( 1, 0.7, 1, 0.7 ) ),
#'             this.mix.type = 4 )
#'  a$kind      ## '2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution'
#'  a$mix.type  ## 4
#'  a$cmp       ## with 4 rows
#'
#'  ## You can also write as:
#'  a$set.cmp( data.frame( mean = c( 0, 0, 0, 0 ), sd = c( 1, 0.7, 1, 0.7 ) ),
#'             grad = "hv" )
#'  a$kind; a$mix.type; a$cmp
#'
#'  ## You can also use '[this.]kind' argument to avoid simplifying.
#'  ## However, if the indicated regular expression for '[this.]kind' argument does not match
#'  ## the resulting 'kind' field, a warning will occur.
#'  ## For avoiding confusing, using '[this.]mix.type' or 'grad' argument is recommended.
#'  ##
#'  ## This sample will work with a warning;
#'  ## because 'this.kind' argument does not match the resulting 'kind' field.
#'  a$set.cmp(
#'      this.cmp = data.frame( mean = c( 0, 0, 0, 0 ), sd = c( 1, 0.7, 1, 0.7 ) ),
#'      this.kind = "Horizontal-Vertical" )
#'  a$kind; a$mix.type; a$cmp
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
        #   for safety of the data copy process
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

        # Fix current 'mix.type' field and set indicated 'cmp' field.
        mix.type <<- tent.mix.type
        cmp <<- this.cmp

        # Adjust 'mix.type' and 'cmp' fields.
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
            warning( paste( "Warning: Indicated kind does not match the result."  ) )
        }

        # Set median, mean, sd and its family.
        return ( adjust.median.mean.sd() )
    }
)

################################################################################################
#' Adjust cmp field
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
#'  ## Usually 'cmp' field is simplified automatically,
#'  ## so you do not have to call adjust.cmp by your own.
#'  ## For example, in this case,
#'  ## the number of components is simplified to 1 automatically.
#'  a <- ggd.set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1.1, 1.1 ) ) )
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 0: a Normal Distribution
#'
#'  ## GGD$new() makes 2 components of normal distributions for convenience.
#'  ## You can use adjust.cmp in order to adjust 'cmp' field of a new object to 1 component.
#'  a <- GGD$new()
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 2: Horizontal Gradational Distribution
#'
#'  a$adjust.cmp()
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 0: Normal Distribution
#'
#'  ## You can also write as:
#'  a <- GGD$new()$adjust.cmp()
#'  a$kind; a$mix.type; a$cmp
#'
#'  ## If you want to give redundancy to the components, you can also use adjust.cmp.
#'  ## Normal Distribution with 2 components.
#'  a$adjust.cmp( this.mix.type = 1 )
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 1: Mean of 2 Normal Distributions
#'
#'  ## Normal Distribution with 3 components.
#'  a$adjust.cmp( grad = "v3" )
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 3: Vertical Gradational Distribution
#'
#'  ## Normal Distribution with 4 components.
#'  a$adjust.cmp( grad = "hv" )
#'  a$kind; a$mix.type; a$cmp   ## mix.type = 4: H-V Gradational Distribution
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

            # First, optimize the components of cmp field.
            current.mix.type <- integer()   # current essential mix type; the distribution model
            cmp.rows <- nrow( cmp )
            means <- cmp$mean
            sds <- cmp$sd

            if ( cmp.rows > 4 )
            {
                # 5 and more rows in cmp field are not allowed.
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
                    # if the current mix.type is strange, an error occurs
                    # even if a valid this.mix.type is indicated.
                    #
                    # Because setting a valid value to mix.type field
                    # may affect the substance of the distribution.
                    if ( length( mix.type ) == 0 || is.na( mix.type ) ||
                         !any( mix.type == 1:3 ) )
                    {
                        stop( "Error: Cannot identify current mix.type." )
                    }

                    current.mix.type <- mix.type
                }
            }

            new.mix.type <- current.mix.type    # the new value for mix.type

            # Second, give redundancy to cmp if this.mix.type or grad argument is given.
            if ( length( this.mix.type ) > 0 )
            {
                # new.ncmp: number of cmp rows to have
                new.ncmp <- ggd.ncmp.for( grad, mix.type = this.mix.type )

                # The normal distribution can enhance to any other type.
                # On the other hand, that of mix.type = 1 cannot enhance to any other type.
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
