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
#' Sets the normal distributions of the components to the \code{cmp} field.
#' All fields of the \code{\link[ggd]{GGD}} object will be set as appropriate according to
#' given components and other arguments, respectively.
#' You should never set any values into the \code{cmp} field directly without using this method.
#' @export
#' @name    set.cmp
#' @aliases ggd.set.cmp
#' @aliases set.cmp
#' @aliases \S4method{set.cmp}{GGD}
#' @usage   ggd.set.cmp(cmp, kind = NULL, mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"),
#'          custom.d = NULL)
#' @usage   \S4method{set.cmp}{GGD}(this.cmp = .self$cmp,
#'          this.kind = NULL, this.mix.type = NULL,
#'          grad = c("default", "normal", "h", "v", "v2", "v3", "hv"),
#'          this.custom.d = NULL)
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
#'                      the \code{objs} argument of the \code{\link[ggd]{ggd.kind.index}}.
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
#'                          \item \code{5} : Customized distribution.
#'                                           The number of rows in \code{cmp} is free.
#'                                           The customized probability density function
#'                                           muse be given by user.
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
#' @param   custom.d    A function for the probability density function
#'                      of the custom distribution.
#'
#'                      If \code{kind} indicates \code{"Custom Distribution"} or
#'                      \code{mix.type} is \code{5}, this argument must be indicated.
#'
#'                      It must be a function which receives 2 arguments and
#'                      returns a positive numeric value (or \code{0}) as the density.
#'
#'                      The 2 arguments for the function are
#'                      \itemize{
#'                          \item   A numeric value (length = \code{1}) for the x-coordinate.
#'                          \item   A data frame of the mean values and standard deviations
#'                                  of the components. That is, the \code{cmp} field.
#'                      }
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
#' @param   this.custom.d   A function for the probability density function of
#'                          the custom distribution.
#'                          It is equivalent to \code{custom.d} argument for \code{ggd.set.cmp}.
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
#'                                              Otherwise, \code{mix.type} will be \code{2}.
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
                         grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ),
                         custom.d = NULL )
{
    obj <- GGD$new()
    return ( withVisible( obj$set.cmp( this.cmp      = cmp,
                                       this.kind     = kind,
                                       this.mix.type = mix.type,
                                       grad          = grad,
                                       this.custom.d = custom.d ) )$value )
}

GGD$methods(
    set.cmp = function( this.cmp = .self$cmp,
                        this.kind = NULL, this.mix.type = NULL,
                        grad = c( "default", "normal", "h", "v", "v2", "v3", "hv" ),
                        this.custom.d = NULL )
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

        # Checking number of rows in this.cmp is moved from here to after fixing new mix.type
        # because of "Custom Distribution" implementation, which allows free number of rows.

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
            else if ( length( new.mix.type ) > 1 || !any( new.mix.type == 0:5 ) )
            {
                stop( paste( "Error: mix.type should be single integer from 0 to 5." ) )
            }
        }

        # Check if the indicated new mix.type is suitable for nrow(this.cmp)
        tent.mix.type <- NA_integer_   # tentative (current) mix.type
        if ( isTRUE( new.mix.type == 5 ) || isTRUE( mix.type == 5 ) )
        {
            tent.mix.type <- 5L
        }
        else
        {
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
                # mix.type = 5 is allowed only when explicitly specified with
                # this.kind, this.mix.type or mix.type field.
                # Therefore, the maximum of mix.type if not explicitly specified is 4.
                tent.mix.type <- as.integer( min( nrow( this.cmp ), 4 ) )
            }
        }

        # Checking number of rows in this.cmp
        if ( nrow( this.cmp ) > 4 && tent.mix.type != 5 )
        {
            stop( "Error: The number of cmp rows is too large." )
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

        # Set custom.d
        if ( isTRUE( mix.type == 5 ) )
        {
            if ( is.null( this.custom.d ) )
            {
                if ( identical( custom.d, default.custom.d ) )
                {
                    # rollback
                    mix.type    <<- backup$mix.type
                    cmp         <<- backup$cmp

                    stop( paste( "Error: Your own function must be given to custom.d",
                                        "for a customized distribution." ) )
                }
            }
            else
            {
                custom.d <<- this.custom.d
            }
        }
        else
        {
            custom.d <<- default.custom.d
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
               !is.na( this.mix.type ) && !any( this.mix.type == 0:5 ) ) )
        {
            stop( paste( "Error: mix.type should be single integer from 0 to 5." ) )
        }

        # Where mix.type = 5, contents of cmp field is completely left to user,
        # so they are retained.
        if ( isTRUE( this.mix.type == 5 ) ||
             ( length( this.mix.type ) == 0 && isTRUE( mix.type == 5 ) ) )
        {
            return ( adjust.cmp.rownames() )
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
