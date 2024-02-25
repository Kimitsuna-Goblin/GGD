################################################################################################
## Expectations for GGD tests.
################################################################################################

library( "ggd" )
library( "testthat" )

################################################################################################
#' Check if a GGD object is cleared
#' @param   obj                 A \code{GGD} class object.
#' @param   expect.custom.d     The function which is expected to be in \code{custom.d} field.
#' @param   expect.custom.p     The function which is expected to be in \code{custom.p} field.
#'                              If \code{NULL}, the default function is assumed to be expected.
#' @importFrom  rlang   enquo
################################################################################################
expect_cleared <- function( obj,
                            expect.custom.d = ggd:::default.custom.d, expect.custom.p = NULL )
{
    act <- quasi_label( rlang::enquo( obj ), arg = "object" )
    c.d <- quasi_label( rlang::enquo( expect.custom.d ), arg = "expect.custom.d" )
    c.p <- quasi_label( rlang::enquo( expect.custom.p ), arg = "expect.custom.p" )

    c.p.val <- NULL
    if ( is.null( c.p$val ) )
    {
        c.p.val <- ggd:::default.custom.p.text
    }
    else
    {
        c.p.val <- paste( as.character( attributes( c.p$val )$srcref ),
                          collapse = "\\n" )[1]
    }

    act$val$custom.p <- paste( as.character( attributes( act$val$custom.p )$srcref ),
                               collapse = "\\n" )[1]

    expect( identical( act$val$kind.index,      integer() ) &&
            identical( act$val$kind,            character() ) &&
            identical( act$val$mix.type,        integer() ) &&
            nrow( act$val$cmp ) == 0 &&
            identical( act$val$custom.d,        c.d$val ) &&
            act$val$custom.p == c.p.val &&
            identical( act$val$median,          numeric() ) &&
            identical( act$val$mean,            numeric() ) &&
            identical( act$val$sd,              numeric() ) &&
            identical( act$val$lsd,             numeric() ) &&
            identical( act$val$usd,             numeric() ) &&
            identical( act$val$lsd.abs.error,   numeric() ) &&
            identical( act$val$usd.abs.error,   numeric() ),
            {
                if ( !identical( act$val$custom.d, c.d$val ) )
                {
                    sprintf( "custom.d of %s is different from %s", act$lab, c.d$lab )
                }
                else if ( act$val$custom.p != c.p$val )
                {
                    if ( is.null( c.p$val ) )
                    {
                        sprintf( "custom.p of %s is not the default function", act$lab )
                    }
                    else
                    {
                        sprintf( "custom.p of %s is different from %s", act$lab, c.p$lab )
                    }
                }
                else
                {
                    sprintf( "%s is not cleared.", act$lab )
                }
            } )

    invisible( act$val )
}

################################################################################################
#' Check if a GGD object is a NA-object
#' @param   obj             A \code{GGD} class object.
#' @param   kind.length     A numeric value of expected length of \code{kind} field.
#'                          1 or 0 is allowed.
#' @importFrom  rlang   enquo
################################################################################################
expect_na_ggd <- function( obj, kind.length = 1 )
{
    act <- quasi_label( rlang::enquo( obj ), arg = "object" )
    len <- quasi_label( rlang::enquo( kind.length ) )

    expect( ( if ( len$val == 0 )
            {
                identical( act$val$kind.index,  integer() ) &&
                identical( act$val$kind,        character() ) &&
                identical( act$val$mix.type,    integer() )
            }
            else if ( len$val == 1 )
            {
                identical( act$val$kind.index,  NA_integer_ ) &&
                identical( act$val$kind,        NA_character_ ) &&
                identical( act$val$mix.type,    NA_integer_ )
            } ) &&
            nrow( act$val$cmp ) == 0 &&
            identical( act$val$median,          NaN ) &&
            identical( act$val$mean,            NaN ) &&
            identical( act$val$sd,              NaN ) &&
            identical( act$val$lsd,             NaN ) &&
            identical( act$val$usd,             NaN ) &&
            identical( act$val$lsd.abs.error,   NaN ) &&
            identical( act$val$usd.abs.error,   NaN ),
            sprintf( "%s is not a NA object or length of kind is not %d.", act$lab, len$val ) )

    invisible( act$val )
}

################################################################################################
#' Check whether two functions equal to each other in the defining character string level.
#' @param   f.1     A function.
#' @param   f.3     The other function.
#' @importFrom  rlang   enquo
################################################################################################
expect_equal_function <- function( f.1, f.2 )
{
    act <- quasi_label( rlang::enquo( f.1 ), arg = "object" )
    exp <- quasi_label( rlang::enquo( f.2 ), arg = "expected" )

    expect( is.function( act$val ) && is.function( exp$val ) &&
            paste( as.character( attributes( act$val )$srcref ),
                                 collapse = "\\n" ) ==
            paste( as.character( attributes( exp$val )$srcref ),
                                 collapse = "\\n" ),
            if ( !is.function( act$val ) )
            {
                sprintf( "%s is not a function.", act$lab )
            }
            else if ( !is.function( exp$val ) )
            {
                sprintf( "%s is not a function.", exp$lab )
            }
            else
            {
                sprintf( "%s not equal to %s.", act$lab, exp$lab )
            } )

    invisible( act$val )
}

################################################################################################
#' Check whether GGD object a equals to b except for custom.* fields
#' @param   a       A GGD object. It must not be a cleared object.
#' @param   b       A GGD object which is expected to be equal to a.
#' @importFrom  rlang   enquo
################################################################################################
expect_equal_ggd <- function( a, b )
{
    act <- quasi_label( rlang::enquo( a ), arg = "object" )
    exp <- quasi_label( rlang::enquo( b ), arg = "expected" )

    expect( act$val$kind.index  == exp$val$kind.index &&
            act$val$mix.type    == exp$val$mix.type &&
            nrow( act$val$cmp ) == nrow( exp$val$cmp ) &&
            all( vapply( 1:nrow( act$val$cmp ),
                            function( i )
                            {
                                act$val$cmp$mean[i] == exp$val$cmp$mean[i] &&
                                act$val$cmp$sd[i]   == exp$val$cmp$sd[i]
                            }, TRUE ) ) &&
            act$val$median  == exp$val$median &&
            act$val$mean    == exp$val$mean &&
            act$val$sd      == exp$val$sd &&
            act$val$lsd     == exp$val$lsd &&
            act$val$usd     == exp$val$usd &&
            act$val$mean.abs.error == exp$val$mean.abs.error &&
            act$val$sd.abs.error   == exp$val$sd.abs.error &&
            act$val$lsd.abs.error  == exp$val$lsd.abs.error &&
            act$val$usd.abs.error  == exp$val$usd.abs.error,
            sprintf( "%s not equal to %s.", act$lab, exp$lab ) )

    invisible( act$val )
}
