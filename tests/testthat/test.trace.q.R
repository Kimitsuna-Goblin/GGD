################################################################################################
## This file is to test trace.q mainly.
################################################################################################

# For interactive test, load setup.R expressly.
if ( file.exists( "tests/testthat" ) ) source( "tests/testthat/setup.R" )

#### Preparing

a <- GGD$new()
if ( dev.cur() == 1 ) { dev.new(); plot.new() }

## For manual testing,
## if you want to wait a moment before drawing a new graph
## to watch the previous graph, set TRUE this option.
wait.before.new.graph <- FALSE
#wait.before.new.graph <- TRUE

## for control option test
control <- list( ftol = 1e-4 )


#### Functions

################################################################################################
#' Expect near but not equal
#'
#' Check if the values of two vectors are near but never eqaul to each other.
#' @param   v1          A vector to be checked.
#' @param   v2          The other vector to be checked.
#' @param   tol.equal   Tolerance to consider that values of two vectors are equal.
#' @param   tol.near    Tolerance to consider that values of two vectors are near.
################################################################################################
expect_near <- function( v1, v2, tol.equal = 5e-9, tol.near = 1e-3 )
{
    v.diff <- ifelse( v2 == 0, abs( v1 - v2 ), abs( ( v1 - v2 ) / v2 ) )

    expect_equal( v.diff > rep( tol.equal, length( v.diff ) ), rep( TRUE, length( v.diff ) ) )
    expect_equal( v.diff < rep( tol.near, length( v.diff ) ),  rep( TRUE, length( v.diff ) ) )
}

################################################################################################
#' Plots quantiles and a$p()
#'
#' @param   a       A GGD object.
#' @param   p       A vector of probabilities of quantiles.
#' @param   x       A vector of x-coodinates of quantiles.
#' @param   xlim    A vector of the x-range for plottings.
################################################################################################
plot.quantiles.and.p <- function( a, p, x, xlim = NULL )
{
    if ( is.null( xlim ) )
    {
        xlim <- c( min( x ) - abs( x[2] - x[1] ), max( x ) + abs( x[length( x )] - x[length( x - 1 )] ) )
    }
    ylim <- c( 0, 1 )
    plot( x, p, xlim = xlim, ylim = ylim, xlab = "", ylab = "" )
    par( new = TRUE )
    plot( seq( xlim[1], xlim[2], 0.01 ), a$p( seq( xlim[1], xlim[2], 0.01 ) ), type = "l", xlim = xlim, ylim = ylim )
}

################################################################################################
#' Computes mean value with integrate function
#'
#' @param   a       A GGD object.
#' @param   lower   The lower limit of integral.
#' @param   upper   The upper limit of integral.
#' @return  The mean value within the specified x-range.
################################################################################################
mean.via.integrate <- function( a, lower = -Inf, upper = Inf )
{
    integrate( f <- function( t ) { t * a$d( t ) }, lower, upper )$value
}

################################################################################################
#' Computes variance with integrate function
#'
#' @param   a       A GGD object.
#' @param   lower   The lower limit of integral.
#' @param   upper   The upper limit of integral.
#' @return  The variance within the specified x-range.
################################################################################################
v.via.integrate <- function( a, lower = -Inf, upper = Inf )
{
    integrate( f <- function( t ) { ( t - a$mean )^2 * a$d( t ) }, lower, upper )$value
}

################################################################################################
#' Computes standard deviation with integrate function
#'
#' @param   a       A GGD object.
#' @param   lower   The lower limit of integral.
#' @param   upper   The upper limit of integral.
#' @return  The standard deviation within the specified x-range.
################################################################################################
sd.via.integrate <- function( a, lower = -Inf, upper = Inf )
{
    sqrt( v.via.integrate( a, lower, upper ) )
}

################################################################################################
#' Shows the result of tracing
#'
#' This function is for visual check for the result of tracing.
#' It draws graphs of probability density function, cumulative distribution function,
#' quantile function and histogram of random samples, then print the fields of the GGD object.
#' In addition, it checks the difference of the mean value and standard deviations
#' with values computed by \code{\link[stats]{integrate}} function.
#'
#' This function should be run after each non-error test to confirm the result.
#' @param   obj             A GGD object.
#' @param   plot.range      A numeric value which stands for the x-range.
#'                          The x-range of the graphs will be set to
#'                          \code{[-plot.range, plot.range]}
#' @param   sample.num      A number of random samples for the histogram.
#' @param   is.extreme.case If \code{TRUE}, whether the differences of the mean value and
#'                          standard deviations are too large, no error will occur
#'                          because errors of \code{\link[stats]{integrate}} function
#'                          are expected to be too large.
#'                          If \code{FALSE}, if one of those differences is too large,
#'                          an error will occur.
################################################################################################
show.results <- function( obj = a, plot.range = 3, sample.num = 400, is.extreme.case = FALSE )
{
    if ( wait.before.new.graph )
    {
        # Waiting a moment to check the last-drawn graph on a manual test.
        Sys.sleep( 3 )
    }
    cat( "d:" )
    print( system.time( plot( seq( -plot.range, plot.range, 0.01 ),
                              obj$d( seq( -plot.range, plot.range, 0.01 ) ), type = "l" ) ) )

    if ( wait.before.new.graph )
    {
        # Waiting a moment to check the last-drawn graph on a manual test.
        Sys.sleep( 0.3 )
    }
    cat( "p:" )
    print( system.time( plot( seq( -plot.range, plot.range, 0.01 ),
                              obj$p( seq( -plot.range, plot.range, 0.01 ) ), type = "l" ) ) )

    if ( wait.before.new.graph )
    {
        # Waiting a moment to check the last-drawn graph on a manual test.
        Sys.sleep( 0.3 )
    }
    cat( "q:" )
    print( system.time( plot( seq( 0, 1, 0.01 ), obj$q( seq( 0, 1, 0.01 ) ), type = "l" ) ) )

    if ( wait.before.new.graph )
    {
        # Waiting a moment to check the last-drawn graph on a manual test.
        Sys.sleep( 0.3 )
    }
    cat( "r:" )
    print( system.time( sample <- obj$r( sample.num ) ) ); hist( sample )

    print( obj )

    ## These values are just for reference
    ## because the accuracy guarantees of mean and sd
    ## using the integrate function have been performed in tests/test.integ.R.
    ## So in most cases the cause of a high difference value will be in the integrate function.
    ## Note, however, if there is grater than 0.1 of difference in a less extreme case,
    ## you should doubt that something wrongs may be in GGD.R.
    cat ( "\nDifference vs integrate (for reference):\n" )

    mean.diff <- abs( obj$mean - mean.via.integrate( obj ) )
    sd.diff <- abs( ( obj$sd - sd.via.integrate( obj ) ) / obj$sd )
    lsd.diff <- abs( ( obj$lsd - sd.via.integrate( obj, -Inf, obj$mean ) * sqrt( 2 ) ) / obj$lsd )
    usd.diff <- abs( ( obj$usd - sd.via.integrate( obj, obj$mean, Inf ) * sqrt( 2 ) ) / obj$usd )

    cat( paste( "diff of mean:", mean.diff, "\n" ) )
    cat( paste( "diff of sd:  ", sd.diff, "\n" ) )
    cat( paste( "diff of lsd: ", lsd.diff, "\n" ) )
    cat( paste( "diff of usd: ", usd.diff, "\n" ) )

    if ( !is.extreme.case )
    {
        expect_equal( mean.diff < 0.1, TRUE )
        expect_equal( sd.diff < 0.1, TRUE )
        expect_equal( lsd.diff < 0.1, TRUE )
        expect_equal( usd.diff < 0.1, TRUE )
    }
    else
    {
        cat( "** Differences have not been checked because is.extreme.case = TRUE. **\n" )
    }
}

## In manual testing, you can use each of these commands instead of the above function.
system.time( plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" ) )
system.time( plot( seq( -3, 3, 0.01 ), a$p( seq( -3, 3, 0.01 ) ), type = "l" ) )
system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
system.time( sample <- a$r( 1000 ) ); hist( sample )
a
print( paste( "diff of mean:", abs( a$mean - mean.via.integrate( a ) ) ) )
print( paste( "diff of sd:  ", abs( ( a$sd - sd.via.integrate( a ) ) / a$sd ) ) )
print( paste( "diff of lsd: ", abs( ( a$lsd - sd.via.integrate( a, -Inf, a$mean ) * sqrt( 2 ) ) / a$lsd ) ) )
print( paste( "diff of usd: ", abs( ( a$usd - sd.via.integrate( a, a$mean, Inf ) * sqrt( 2 ) ) / a$usd ) ) )
expect_equal( abs( a$mean - mean.via.integrate( a ) ) < 0.1, TRUE )
expect_equal( abs( ( a$sd - sd.via.integrate( a ) ) / a$sd ) < 0.1, TRUE )
expect_equal( abs( ( a$lsd - sd.via.integrate( a, -Inf, a$mean ) * sqrt( 2 ) ) / a$lsd ) < 0.1, TRUE )
expect_equal( abs( ( a$usd - sd.via.integrate( a, a$mean, Inf ) * sqrt( 2 ) ) / a$usd ) < 0.1, TRUE )

#### Tests

#### Basic tests
# Error case
expect_error( a$trace.q(
    list( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ) ),
    "quantiles must be a data frame" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( y = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ) ),
    "Column 'x' is undefined" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), q = c( 0.3, 0.5, 0.6 ) ) ),
    "Column 'p' is undefined" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), x = NULL ),
    "Argument x must be a column name or an index number" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), x = integer() ),
    "Argument x must be a column name or an index number" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), x = NA_integer_ ),
    "Argument x must be a column name or an index number" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), x = a ),
    "Argument x must be a column name or an index number" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), x = "x.2" ),
    "Column 'x.2' is undefined" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), x = -1 ),
    "Illegal column number for x" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), x = 0 ),
    "Illegal column number for x" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), x = 3 ),
    "Illegal column number for x" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), p = NULL ),
    "Argument p must be a column name or an index number" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), p = integer() ),
    "Argument p must be a column name or an index number" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), p = NA_integer_ ),
    "Argument p must be a column name or an index number" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), p = a ),
    "Argument p must be a column name or an index number" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), p = "p.2" ),
    "Column 'p.2' is undefined" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), p = -1 ),
    "Illegal column number for p" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), p = 0 ),
    "Illegal column number for p" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( x = c( -1, 0, 1 ), p = c( 0.3, 0.5, 0.6 ) ), p = 3 ),
    "Illegal column number for p" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame(
    p = numeric(),
    x = numeric() ) ),
    "No valid rows in quantiles" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame(
    p = c( NaN, 0, NA, NaN ),
    x = c( -Inf, NA, 1e+10, Inf ) ) ),
    "No valid rows in quantiles" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    mix.type = -1 ),
    "mix.type should be single integer from 0 to 4" )

# Error case
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    this.mix.type = 5 ),
    "mix.type should be single integer from 0 to 4" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    mix.type = NA ),
    "mix.type should be single integer from 0 to 4" )

# Error case
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    this.mix.type = numeric() ),
    "mix.type should be single integer from 0 to 4" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    this.mix.type = 1:4 ),
    "mix.type should be single integer from 0 to 4" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
    x = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.mix.type = 1:3 ),
    "mix.type should be single integer from 0 to 4" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    kind = NA ),
    "kind should be valid single value or a GGD object" )
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    this.kind = NA ),
    "kind should be valid single value or a GGD object" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    kind = character() ),
    "kind should be valid single value or a GGD object" )
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    this.kind = character() ),
    "kind should be valid single value or a GGD object" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    kind = 2:3 ),
    "kind should be valid single value or a GGD object" )
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    this.kind = 2:3 ),
    "kind should be valid single value or a GGD object" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    kind = c( "Horizontal", NA ) ),
    "kind should be valid single value or a GGD object" )
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    this.kind = c( "Horizontal", NA ) ),
    "kind should be valid single value or a GGD object" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    kind = "Random Distribution" ),
    "'Random Distribution' does not match any character strings of kinds" )

# Error case
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    this.kind = 0 ),
    "kind for index 0 is undefined" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    kind = 17 ),
    "kind for index 17 is undefined" )

# Error case
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    this.kind = iris ) )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame(
    p = c( 0.5, 0.1, 0.3, 0.6, 0.7 ),
    x = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ) ),
    "Order of x and p must be along" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    grad = "zzz" ), "should be one of" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    grad = "yyy" ), "should be one of" )

# Error case
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    grad = c( "h", "v2" ) ), "1" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    grad = c( "v3", "hv" ) ), "1" )

# Error case
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    grad = NA ) )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    grad = NA ) )

# Error case
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    grad = character() ) )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    grad = character() ) )

# Error case
expect_error( a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    grad = a ) )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( ggd.race.q(
    data.frame( p = c( 0.3, 0.5, 0.6 ), x = qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ) ),
    grad = a ) )

# Error case
expect_error( a$trace.q(
    data.frame(
    p.2 = c( 0.1, 0.3, 0.5, 0.7, 0.6 ),
    x.2 = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
    x = "x.2", p = "p.2" ),
    "Order of x.2 and p.2 must be along" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5, 0.6, 0.6 ),
    x = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ) ),
    "x and p must not duplicated" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
        data.frame(
        p = c( 0.5 ),
        x = c( 0 ) ) ),
        "Illegal number of quantiles" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( expect_warning( a$trace.q(
        data.frame(
        p.2 = c( 0, 0.5, 1 ),
        x.2 = c( -Inf, 10, Inf ) ),
        x = "x.2", p = "p.2" ),
        "No quantiles other than p.2 = 0, 0.5, 1" ),
        "Illegal number of valid quantiles" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
    data.frame(
    p.2 = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
    x.2 = c( qnorm( c( 0.1, 0.3, 0.5 ), 0, 1 ), 0.5, 0.5 ) ),
    x = "x.2", p = "p.2" ),
    "x.2 and p.2 must not duplicated" )
expect_cleared( a ); a <- GGD$new()

# normal test
a <- ggd.trace.q(
        data.frame(
        p = c( 0.25, 0.75 ),
        x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), mix.type = 0 )$obj
expect_equal( a$is.normal(),    TRUE )
expect_equal( a$is.h(),         TRUE )
expect_equal( a$is.h( TRUE ),   FALSE )
expect_equal( a$is.v2(),        TRUE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        TRUE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.741301, 1.741301, 1.741301 ), tolerance = 5e-7 )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a, c( 0.25, 0.75 ), c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ), c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

expect_true( withVisible(
    ggd.trace.q(
        data.frame(
        p = c( 0.25, 0.75 ),
        x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), mix.type = 0 ) )$visible )

#### Normal Distribution
# normal test
a <- GGD$new()
expect_identical( a$mix.type, 2L )
expect_equal( a$kind, "Normal Distribution" )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (no options any other quantiles)
expect_false( withVisible(
    a$trace.q(
        data.frame(
        p = c( 0.25, 0.75 ),
        x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ) ) )$visible )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a, c( 0.25, 0.75 ), c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ), c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (no options any other quantiles)
expect_false( withVisible( a$clear() )$visible )
expect_equal( a$mix.type, integer() )
a$trace.q(
    data.frame(
    p = c( 0.25, 0.75 ),
    x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ) )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a, c( 0.25, 0.75 ), c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ), c( -3, 3 ) )

# normal test (no options any other quantiles)
a <- ggd.trace.q(
        data.frame(
        p = c( 0.25, 0.75 ),
        x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ) )$obj
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a, c( 0.25, 0.75 ), c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ), c( -3, 3 ) )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.25, 0.75 ),
    x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
    this.mix.type = 3, eq.mean = TRUE, eq.sd = TRUE )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.25, 0.75 ),
    x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
    this.kind = 10, eq.mean = TRUE, eq.sd = TRUE )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.25, 0.75 ),
    x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
    grad = "h", eq.mean = TRUE, eq.sd = TRUE )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.25, 0.75 ),
    x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
    grad = "v2", eq.mean = TRUE, eq.sd = TRUE )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.25, 0.75 ),
    x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
    grad = "v3", eq.mean = TRUE, eq.sd = TRUE )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.25, 0.75 ),
    x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
    grad = "hv", eq.mean = TRUE, eq.sd = TRUE )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )

# Error case
a$clear()
expect_error(
    a$trace.q(
        data.frame(
        p = c( 0.25, 0.5, 0.75 ),
        x = c( qnorm( 0.25, 0, 1 ), 0.2, qnorm( 0.75, 1, 1 ) ) ),
        grad = "normal" ),
    "Illegal number of quantiles for mix.type = 0" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 1, 3 quantiles
# normal test
xs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.75 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 1 )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.normal(),    FALSE )
expect_equal( a$is.h(),         FALSE )
expect_equal( a$is.h( TRUE ),   FALSE )
expect_equal( a$is.v2(),        FALSE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        FALSE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        FALSE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_equal( a$is.eq.mean(),   FALSE )
expect_equal( a$is.eq.sd(),     FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 1, control = control )
expect_near( a$p( xs ), c( 0.1, 0.5, 0.6 ) )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control is NULL)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 1, control = NULL )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (eq.mean = TRUE, it will be symmetric)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 1, eq.mean = TRUE )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.normal(),    FALSE )
expect_equal( a$is.h(),         FALSE )
expect_equal( a$is.v2(),        FALSE )
expect_equal( a$is.v3(),        FALSE )
expect_equal( a$is.hv(),        FALSE )
expect_equal( a$is.eq.mean(),   TRUE )
expect_equal( a$is.eq.sd(),     FALSE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( a$median, 0 )
expect_equal( a$mean, 0 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

## Where symmetric distribution,
## ggd:::calc.v value with get.lv/get.uv = TRUE is "worse" than the lsd/usd field value.
## The theoretical maximum error of ggd:::calc.v (vs lsd/usd) is sqrt( .Machine$double.eps ).
## However, almost always, all values of them are equal.
expect_equal( abs( a$lsd - sqrt( ggd:::calc.v( 1, a$cmp$mean, a$cmp$sd, get.lv = TRUE ) * 2 ) ) <=
                sqrt( .Machine$double.eps ), TRUE )
expect_equal( abs( a$usd - sqrt( ggd:::calc.v( 1, a$cmp$mean, a$cmp$sd, get.uv = TRUE ) * 2 ) ) <=
                sqrt( .Machine$double.eps ), TRUE )

# normal test (keep mix.type = 1)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.61 ),
    x = xs ) )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.61 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.61 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 1, eq.mean = FALSE )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(),   FALSE )
expect_equal( a$is.eq.sd(),     FALSE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error( a$trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.6 ),
        x = xs ),
        this.mix.type = 0 ),
        "Illegal number of quantiles for mix.type = 0" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( qnorm( 0.1, -0.5, 1.9 ), -0.5, qnorm( 0.7, -0.5, 1.8 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 1, eq.mean = TRUE )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median, -0.5 )
expect_equal( a$mean, -0.5 )
expect_equal( a$cmp$mean[1], a$cmp$mean[2] )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( -0.5, qnorm( 0.1, -0.5, 1.9 ), qnorm( 0.7, -0.5, 1.8 ) )
a$trace.q(
    data.frame(
    p = c( 0.5, 0.1, 0.7 ),
    x = xs ),
    this.mix.type = 1, eq.mean = TRUE )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median, -0.5 )
expect_equal( a$mean, -0.5 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.5, 0.1, 0.7 ), tolerance = 5e-7 )

# Warning test
xs <- c( qnorm( 0.1, -0.5, 1.9 ), -0.5, qnorm( 0.7, -0.5, 1.8 ) )
df <- data.frame(
        p = c( -0.1, 0.1, 0.5, 0.7 ),
        x = c( -10, xs ),
        p.2 = c( 0.1, 0.5, 0.7, 1.2 ),
        x.2 = c( xs, 100 ) )

expect_warning( a$trace.q( df, this.mix.type = 1 ),
    "There is an out-of-range p value" )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )

expect_warning( a$trace.q( df, x = "x.2", p = "p.2", this.mix.type = 1 ),
    "There is an out-of-range p.2 value" )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )

# Warning test
df <- data.frame(
        p = c( 0, 0.1, 0.5, 0.7 ),
        x = c( -10, xs ),
        p.2 = c( 0.1, 0.5, 0.7, 1 ),
        x.2 = c( xs, 100 ) )
expect_warning( a$trace.q( df, this.mix.type = 1 ),
    "Wrong position for upper or lower limit of x or p value" )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )

# Warning test
expect_warning( a$trace.q( df, x = "x.2", p = "p.2", this.mix.type = 1 ),
    "Wrong position for upper or lower limit of x.2 or p.2 value" )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )

# Error case (sorry, this is mix.type = 0)
expect_error( a$trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.7 ),
        x = xs ),
        this.mix.type = 0 ),
        "Illegal number of quantiles for mix.type = 0" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), qnorm( 0.6, 0, 0.8 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6 ), x = xs ),
    this.mix.type = 1, eq.mean = TRUE )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$median, a$mean )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 1.9 ), 0, qnorm( 0.7, 0, 0.4 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = c( qnorm( 0.1, 0, 1.9 ), 0, qnorm( 0.7, 0, 0.4 ) ) ),
    this.mix.type = 1 )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 0.9 ), 0, qnorm( 0.6, 0, 1 ) )
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.6 ),
        x = xs ),
        this.mix.type = 1 )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 1, eq.mean = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
xs<-c( -1e-8, 0, 10 )
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 1 ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.7 ),
            x = xs ),
            this.mix.type = 1 ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 1, 4 quantiles
# normal test
xs <- c( qnorm( 0.1, 0, 1.1 ), qnorm( 0.25, 0, 1 ), qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 1 )
expect_identical( a$mix.type, 1L )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 1, control = control )
expect_identical( a$mix.type, 1L )
expect_near( a$p( xs ), c( 0.1, 0.25, 0.75, 0.9 ) )
show.results()

# Error case
expect_error( a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 1, eq.mean = TRUE ),
    "Illegal number of quantiles for mix.type = 1 and either eq.mean or eq.sd is TRUE" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( -1.7, -0.2, 0.3, 1.9 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 1 )
expect_identical( a$mix.type, 1L )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
xs <- c( -2, -0.1, 0, 9 )
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.3, 0.4, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 1 ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.3, 0.4, 0.6, 0.7 ),
            x = xs ),
            this.mix.type = 1 ),
        "nleqslv has failed. Message: Jacobian is too ill-conditioned" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 1/6: with median, without eq.mean)
xs <- c( -1e-8, 0, 7 )
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 1 ),
        "nleqslv has once failed. Message: x-values within tolerance 'xtol'" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/6: with median, eq.mean = TRUE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 1, eq.mean = TRUE ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 3/6: with median, eq.mean = FALSE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 1, eq.mean = FALSE ),
        "nleqslv has failed. Message: x-values within tolerance 'xtol'" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 4/6: without median, without eq.mean)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.51, 0.6 ),
            x = xs ),
            this.mix.type = 1 ),
        "nleqslv has once failed. Message: x-values within tolerance 'xtol'" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 5/6: without median, eq.mean = TRUE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.51, 0.6 ),
            x = xs ),
            this.mix.type = 1, eq.mean = TRUE ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 6/6: without median, eq.mean = FALSE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.51, 0.6 ),
            x = xs ),
            this.mix.type = 1, eq.mean = FALSE ),
        "nleqslv has failed. Message: x-values within tolerance 'xtol'" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 2 (hgrad), 3 quantiles
# normal test
xs <- c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) )
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 2 )
expect_equal( a$is.normal(),    FALSE )
expect_equal( a$is.h(),         TRUE )
expect_equal( a$is.h( TRUE ),   TRUE )
expect_equal( a$is.v2(),        FALSE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        FALSE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 2, control = control )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_near( a$p( xs ), c( 0.3, 0.5, 0.7 ), )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (eq.mean = FALSE)
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 2, eq.mean = FALSE )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (eq.mean = TRUE)
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 2, eq.mean = TRUE )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$median, 0 )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (eq.mean = FALSE, control option)
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 2, eq.mean = TRUE, control = control )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median, 0 )
expect_equal( a$p( xs[2] ), 0.5 )
expect_near( a$p( xs[c( 1, 3 )] ), c( 0.3, 0.7 ) )
show.results()

# normal test (overwrite grad = "h" to mix.type = 1)
xs <- c( qnorm( 0.3, 0.2, 0.7 ), 0.2, qnorm( 0.9, 0.2, 1.1 ) )
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.9 ),
    x = xs ),
    this.mix.type = 1, grad = "h", eq.mean = TRUE )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median, 0.2 )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (grad = "default" does not overwrite to mix.type)
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.9 ),
    x = xs ),
    this.mix.type = 1, grad = "default", eq.mean = TRUE )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.h( TRUE ), FALSE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median, 0.2 )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (grad = NULL does not overwrite to mix.type)
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.9 ),
    x = xs ),
    this.mix.type = 2, grad = NULL )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (no options any other quantiles with cleared object)
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.9 ),
    x = xs ) )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (no options any other quantiles with mix.type = NA)
a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.mix.type = NA )
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.9 ),
    x = xs ) )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a <- ggd.trace.q(
        data.frame(
        p = c( 0.3, 0.6, 0.9 ),
        x = xs ),
        mix.type = 2, eq.mean = TRUE )$obj
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.3, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a <- ggd.trace.q(
        data.frame(
        p = c( 0.52, 0.6, 0.9 ),
        x = xs ),
        grad = "h" )$obj
expect_identical( a$mix.type, 2L )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
plot.quantiles.and.p( a,
    c( 0.52, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.52, 0.6, 0.9 ),
            x = xs ),
            grad = "h", eq.mean = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( qnorm( 0.3, -0.2, 0.7 ), -0.2, qnorm( 0.9, -0.2, 1 ) )
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.9 ),
    x = xs ),
    grad = "h" )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

#normal test
xs.1 <- c( qnorm( 0.3,  0.2, 0.7 ),   NA,   0.2, qnorm( 0.9,  0.2, 1.1 ) )
xs.2 <- c( qnorm( 0.3, -0.2, 0.7 ), -0.2,    NA, qnorm( 0.9, -0.2, 1 ) )
xs.3 <- c( qnorm( 0.3, -0.2, 0.7 ),   NA,  -0.2, qnorm( 0.9,  0.2, 1.2 ) )
df <- data.frame(
        x.1 = xs.1,
        x.2 = xs.2,
        x.3 = xs.3,
        p.1 = c( NA, NA, 0.5, 0.9 ),
        p.2 = c( 0.3, NA, 0.5, 0.9 ),
        p.3 = c( 0.3, 0.5, NA, 0.9 ) )
a <- ggd.trace.q( df, x = "x.1", p = "p.1" )$obj
expect_identical( a$mix.type, 0L )
expect_equal( a$p( xs.1[3:4] ), c( 0.5, 0.9 ), tolerance = 5e-7 )

a$trace.q( df, x = "x.1", p = "p.2", this.mix.type = 2, eq.mean = TRUE )
expect_identical( a$mix.type, 2L )
expect_equal( a$median, 0.2 )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8858638, 0.7846082, 0.9766777 ), tolerance = 5e-7 )
expect_equal( a$p( xs.1 ), c( 0.3, NA, 0.5, 0.9 ), tolerance = 5e-7 )

a$trace.q( df, x = "x.1", p = "p.3" )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( xs.1[c( 1, 4 )] ), c( 0.3, 0.9 ), tolerance = 5e-7 )

a$trace.q( df, x = "x.2", p = "p.3", this.mix.type = 2, eq.mean = TRUE )
expect_identical( a$mix.type, 2L )
expect_equal( a$median, -0.2 )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.833052, 0.7543257, 0.9049563 ), tolerance = 5e-7 )
expect_equal( a$p( xs.2 ), c( 0.3, 0.5, NA, 0.9 ), tolerance = 5e-7 )

a$clear()
a$trace.q( df, x = "x.3", p = "p.2", this.mix.type = 2, eq.mean = TRUE )
expect_identical( a$mix.type, 2L )
expect_equal( a$median, -0.2 )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.1249, 0.9402237, 1.283269 ), tolerance = 5e-7 )
expect_equal( a$p( xs.3 ), c( 0.3, NA, 0.5, 0.9 ), tolerance = 5e-7 )

rm( xs.1, xs.2, df )

# Error case (process path test 1/6: with median, without eq.mean)
xs <- c( -1e-8, 0, 7 )
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 2 ),
        "nleqslv has once failed. Message: Jacobian is completely unusable" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message: Jacobian is singular" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/6: with median, eq.mean = TRUE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 2, eq.mean = TRUE ),
        "nleqslv has failed. Message: Jacobian is singular" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 3/6: with median, eq.mean = FALSE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 2, eq.mean = FALSE ),
        "nleqslv has failed. Message: Jacobian is completely unusable" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 4/6: without median, without eq.mean)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.51, 0.6 ),
            x = xs ),
            this.mix.type = 2 ),
        "nleqslv has once failed. Message: Jacobian is completely unusable" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 5/6: without median, eq.mean = TRUE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.51, 0.6 ),
            x = xs ),
            this.mix.type = 2, eq.mean = TRUE ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 6/6: without median, eq.mean = FALSE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.51, 0.6 ),
            x = xs ),
            this.mix.type = 2, eq.mean = FALSE ),
        "nleqslv has failed. Message: Jacobian is completely unusable" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 2 (hgrad), 4 quantiles
# normal test
xs <- c( qnorm( 0.1, 0, 1.1 ), qnorm( 0.25, 0, 1 ), qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 2 )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.normal(),    FALSE )
expect_equal( a$is.h(),         TRUE )
expect_equal( a$is.h( TRUE ),   TRUE )
expect_equal( a$is.v2(),        FALSE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        FALSE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 2, control = control )
expect_equal( a$is.h( TRUE ), TRUE )
expect_near( a$p( xs ), c( 0.1, 0.25, 0.75, 0.9 ) )
show.results()

# Error case
expect_error( a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 2, eq.mean = TRUE ),
    "Illegal number of quantiles for mix.type = 2 and either eq.mean or eq.sd is TRUE" )
expect_cleared( a ); a <- GGD$new()

# normal test (no options any other quantiles)
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.75, 0.9 ),
        x = xs ) )$obj
expect_identical( a$mix.type, 2L )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( -1.7, -0.2, 0.3, 1.9 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 2 )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

plot( seq( 0.4, 0.6, 0.001 ), a$q( seq( 0.4, 0.6, 0.001 ) ), type = "l" )
plot( seq( 0.01, 0.06, 0.001 ), a$q( seq( 0.01, 0.06, 0.001 ) ), type = "l" )

# normal test
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 2 )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ),  c( 0.1, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
xs <- c( -3, -1e-8, 0, 6 )
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.12, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 2, control = list( allowSingular = TRUE, maxit = 1000 ) ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 3, vgrad.2, basic tests
# Error case
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ), 0, qnorm( 0.6, 0, 0.85 )  )
expect_error( a$trace.q(
        data.frame(
        p = c( 0.1, 0.3, 0.4, 0.5, 0.6 ),
        x = xs ), grad = "v2" ),
        "Illegal number of quantiles for grad = \"v2\"" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 3, vgrad.2, 3 quantiles
# normal test
xs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median, 0 )
expect_equal( a$median == a$mean, TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.022428, 1.022428, 1.022428 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = TRUE, control = control )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median, 0 )
expect_equal( a$median == a$mean, TRUE )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_near( a$p( xs[c( 1, 3 )] ), c( 0.1, 0.7 ) )
show.results()

# normal test (keep vgrad.2)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.69 ),
    x = xs ) )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.69 ), tolerance = 5e-7 )
show.results()

# normal test ("v" is an alias of "v2")
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    grad = "v" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test ("v" is an alias of "v2")
a$clear()
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.8 ),
        x = xs ),
        grad = "v" )$obj
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (overwrite vgrad.2 to mix.type = 2)
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.4 ),
    x = xs ),
    this.mix.type = 2, grad = "v2" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.4 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.4 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.4 ),
    x = xs ),
    this.mix.type = 4, grad = "v2", control = control )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_near( a$p( xs ), c( 0.1, 0.3, 0.4 ) )
show.results()

# normal test (expected to be a normal distribution)
xs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.9, 0, 1 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.9 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$kind, "Normal Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2(), TRUE )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1, 1, 1 ) )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.8 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = FALSE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( 0, qnorm( 0.7, 0, 0.8 ), qnorm( 0.9, 0, 1 ) )
a$trace.q(
    data.frame(
    p = c( 0.5, 0.7, 0.9 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = FALSE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.5, 0.7, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.7, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
# This is an extreme case that
# large differences of mean and standard deviations vs integrate may occur.
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.5, 0.7, 0.9 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.5, 0.7, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.7, 0.9 ), xs, c( -3, 3 ) )
show.results( is.extreme.case = TRUE )


# Error case
xs <- c( 0, 0.7, 0.9 )
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.5, 0.7, 0.9 ),
            x = xs ),
            this.mix.type = 3, grad = "v2", eq.mean = TRUE ),
    "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test
# This is an extreme case that
# large differences of mean and standard deviations vs integrate may occur.
a$trace.q(
    data.frame(
    p = c( 0.5, 0.7, 0.9 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = FALSE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.5, 0.7, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.7, 0.9 ), xs, c( -3, 3 ) )
show.results( is.extreme.case = TRUE )

# normal test
# This is an extreme case that
# large differences of mean and standard deviations vs integrate may occur.
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.5, 0.7, 0.9 ),
    x = xs ),
    grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.5, 0.7, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.7, 0.9 ), xs, c( -3, 3 ) )
show.results( is.extreme.case = TRUE )


# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = FALSE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5 ), xs, c( -3, 3 ) )
show.results()
plot( seq( -8, 8, 0.01 ), a$d( seq( -8, 8, 0.01 ) ), type = "l" )

# normal test
xs <- c( -0.7, -0.5, 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = FALSE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
a$clear()
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.5 ),
            x = xs ),
            this.mix.type = 3, grad = "v2", eq.mean = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( -1.7, -0.5, 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = FALSE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.85 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median == a$mean, TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.029705, 1.029705, 1.029705 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

## Where symmetric distribution
## (e.g., mix.type = 3, is.v2() = TRUE and is.eq.mean() = TRUE is symmetric),
## ggd:::calc.v value with get.lv/get.uv = TRUE is "worse" than the lsd/usd field value.
## The theoretical maximum error of ggd:::calc.v (vs lsd/usd) is sqrt( .Machine$double.eps ).
## However, almost always, all values of them are equal.
expect_equal( abs( a$lsd - sqrt( ggd:::calc.v( 3, a$cmp$mean, a$cmp$sd,
                                 symmetric = TRUE, get.lv = TRUE ) * 2 ) ) <= sqrt( .Machine$double.eps ), TRUE )
expect_equal( abs( a$usd - sqrt( ggd:::calc.v( 3, a$cmp$mean, a$cmp$sd,
                                 symmetric = TRUE, get.uv = TRUE ) * 2 ) ) <= sqrt( .Machine$double.eps ), TRUE )

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.9 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = FALSE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test
xs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.85 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = FALSE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test
xs <- c( -0.479, -0.472, -0.215 )
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.3, 0.7 ),
        x = xs ),
        grad = "v2" )$obj
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.7 ), xs, c( -3, 3 ) )
show.results( is.extreme.case = TRUE )

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.7 ),
            x = xs ),
            grad = "v2", eq.mean = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test
# This is an extreme case that
# large differences of mean and standard deviations vs integrate may occur.
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.32 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.32 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.32 ), xs, c( -3, 3 ) )
show.results( is.extreme.case = TRUE )

# normal test
xs <- c( 0.215, 0.217, 0.472 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.12, 0.3 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.12, 0.3 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.12, 0.3 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.6, 0.62, 0.8 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.6, 0.62, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.6, 0.62, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case (process path test 1/6: with median, without eq.mean)
xs <- c( -0.479, -0.472, 0.472 )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
                data.frame(
                p = c( 0.1, 0.48, 0.5 ),
                x = xs ),
                this.mix.type = 3, grad = "v2" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #2 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed. Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/6: with median, eq.mean = TRUE)
expect_error(
    expect_message(
        a$trace.q(
                data.frame(
                p = c( 0.1, 0.48, 0.5 ),
                x = xs ),
                this.mix.type = 3, grad = "v2", eq.mean = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 3/6: with median, eq.mean = FALSE)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
                data.frame(
                p = c( 0.1, 0.48, 0.5 ),
                x = xs ),
                this.mix.type = 3, grad = "v2", eq.mean = FALSE ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #2 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed.[^a-z]*$" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 4/6: without median, without eq.mean)
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
                data.frame(
                p = c( 0.1, 0.12, 0.2 ),
                x = xs ),
                this.mix.type = 3, grad = "v2",
                control = list( maxit = 2000, xtol = 1e-11 ) ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with crossing over #2 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #3 quantile has failed" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed. Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 5/6: without median, eq.mean = TRUE)
expect_error(
    expect_message(
        a$trace.q(
                data.frame(
                p = c( 0.1, 0.12, 0.2 ),
                x = xs ),
                this.mix.type = 3, grad = "v2", eq.mean = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 6/6: without median, eq.mean = FALSE)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
                data.frame(
                p = c( 0.1, 0.12, 0.2 ),
                x = xs ),
                this.mix.type = 3, grad = "v2",
                control = list( maxit = 2000, xtol = 1e-11 ), eq.mean = FALSE ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with crossing over #2 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #3 quantile has failed" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed.[^a-z]*$" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 3, vgrad.2, 4 quantiles
# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), 0, qnorm( 0.6, 0, 0.85 )  )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.3, 0.5, 0.6 ),
        x = xs ),
        grad = "v2", eq.mean = TRUE ),
    "Illegal number of quantiles for grad = \"v2\" and either eq.mean or eq.sd is TRUE" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ), qnorm( 0.6, 0, 0.85 )  )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.4, 0.6 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.4, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.4, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.4, 0.6 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", control = control )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_near( a$p( xs ), c( 0.1, 0.3, 0.4, 0.6 ) )
show.results()

# normal test
xs <- c( qnorm( 0.6, 0, 1 ), qnorm( 0.7, 0, 1.01 ), qnorm( 0.8, 0, 1.1 ), qnorm( 0.9, 0, 1.12 )  )
a$trace.q(
    data.frame(
    p = c( 0.6, 0.7, 0.8, 0.9 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.6, 0.7, 0.8, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.6, 0.7, 0.8, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case (process path test 1/2: with median)
xs <- c( -1, 0, 0.475, 2 )
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.5, 0.6, 0.7 ),
            x = xs ),
            grad = "v2" ),
        "nleqslv has failed. Message:" ),  # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/2: without median)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.51, 0.6, 0.7 ),
            x = xs ),
            grad = "v2" ),
        "nleqslv has failed. Message:" ),  # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 3, without vgrad.*, basic tests
# Error case
expect_error( a$trace.q(
        data.frame(
        p = c( 0.5, 0.6 ),
        x = c( 0, 0.9 ) ),
        this.mix.type = 3 ),
        "Illegal number of quantiles for mix.type = 3" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
        data.frame(
        p = c( 0.1, 0.2, 0.4, 0.5, 0.6, 0.8, 0.9 ),
        x = c( -1.2, -1, -0.35, 0, 0.34, 0.979, 1.14 ) ),
        this.mix.type = 3 ),
        "Illegal number of quantiles for mix.type = 3" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
        x = c( qnorm( 0.1, 0, 1.12 ), qnorm( 0.25, 0, 1.11 ),
               0,
               qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) ) ),
        this.mix.type = 3, eq.mean = TRUE ),
    "Illegal number of quantiles for mix.type = 3 and either eq.mean or eq.sd is TRUE" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 3, without vgrad.* / grad = "v3", 3 quantiles
# normal test (median at an edge quantile)
xs <- c( qnorm( 0.1, 0, 1.2 ), qnorm( 0.4, 0, 0.9 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$is.v2(), TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.v3( FALSE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3, control = control )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_near( a$p( xs ), c( 0.1, 0.4, 0.5 ) )
show.results()

# Error case for subfunction
expect_error( ggd:::v2.qt4.cmp(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ), control = control ),
    "nrow.qt. must be 4 for v2.qt4.cmp. nrow: 3" )

# normal test (eq.mean = TRUE)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3, eq.mean = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3( FALSE ), TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$median, 0 )
expect_equal( a$mean, 0 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (eq.mean = TRUE, control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3, eq.mean = TRUE, control = control )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$median, 0 )
expect_equal( a$mean, 0 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs[3] ), 0.5 )
expect_near( a$p( xs[1:2] ), c( 0.1, 0.4 ) )
show.results()

# normal test (vgrad.3)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    grad = "v3" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_false( a$lsd == a$usd )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (overwrite vgrad.3, eq.mean = TRUE)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 4, grad = "v3", eq.mean = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_false( a$lsd == a$usd )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case for subfunction
expect_error( ggd:::v2.qt4.cmp(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ), control = control ),
    "nrow.qt. must be 4 for v2.qt4.cmp. nrow: 3" )

# Error case for subfunction
expect_error( ggd:::v3.qt4.cmp(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ), control = control ),
    "nrow.qt. must be 4 for v3.qt4.cmp. nrow: 3" )

# normal test (keep vgrad.3)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ) )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )

# normal test
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.4, 0, 0.7 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.eq.mean(), TRUE )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.4, 0, 0.7 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.eq.mean(), TRUE )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test
xs <- c( 0, qnorm( 0.6, 0, 0.7 ), qnorm( 0.9, 0, 0.9 ) )
a$trace.q(
    data.frame(
    p = c( 0.5, 0.6, 0.9 ),
    x = xs ),
    this.mix.type = 3, grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median == a$mean, FALSE )
expect_equal( any( c( a$sd, a$sd ) == c( a$lsd, a$usd ) ), FALSE )
expect_equal( a$p( xs ), c( 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (keep vgrad.3)
a$trace.q(
    data.frame(
    p = c( 0.5, 0.6, 0.91 ),
    x = xs ) )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.5, 0.6, 0.91 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.6, 0.91 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( 0, qnorm( 0.6, 0, 0.9 ), qnorm( 0.9, 0, 1.2 ) )
a$trace.q(
    data.frame(
    p = c( 0.5, 0.6, 0.9 ),
    x = xs ),
    this.mix.type = 3, grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median == a$mean, FALSE )
expect_equal( any( c( a$sd, a$sd ) == c( a$lsd, a$usd ) ), FALSE )
expect_equal( a$p( xs ), c( 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test
xs <- c( qnorm( 0.4, 0, 0.7 ), qnorm( 0.6, 0, 0.7 ), qnorm( 0.9, 0, 0.9 ) )
a$trace.q(
    data.frame(
    p = c( 0.4, 0.6, 0.9 ),
    x = xs ),
    this.mix.type = 3, eq.mean = TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median == a$mean, TRUE )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.4, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.4, 0.6, 0.9 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$p( xs ), c( 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.4, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( 0, qnorm( 0.6, 0, 0.9 ), qnorm( 0.9, 0, 1.2 ) )
a$trace.q(
    data.frame(
    p = c( 0.4, 0.6, 0.9 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$p( xs ), c( 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.4, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 1.5 ), qnorm( 0.3, 0, 1 ), qnorm( 0.6, 0, 0.8 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.6 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (median at the middle quantile)
xs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.normal(),    FALSE )
expect_equal( a$is.h(),         FALSE )
expect_equal( a$is.h( TRUE ),   FALSE )
expect_equal( a$is.v2(),        FALSE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        TRUE )
expect_equal( a$is.v3( TRUE ),  TRUE )
expect_equal( a$is.hv(),        FALSE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE, control = control )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$p( xs[2] ), 0.5 )
expect_near( a$p( xs[c( 1, 3 )] ), c( 0.1, 0.7 ) )
show.results()

# normal test
xs <- c( qnorm( 0.3, 0, 0.2 ), 0, qnorm( 0.6, 0, 0.7 ) )
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.6 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( -1, 0, 0.5 )
a$trace.q(
    data.frame(
    p = c( 0.499, 0.5, 0.6 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 486.4020, 581.6453, 367.2364 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.499, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.499, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.0001, 0.5, 0.7 ),
            x = c( -0.01, 0, 0.25 ) ),
            this.mix.type = 3 ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #3 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( -2, -0.5, 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.311066, 1.506895, 1.080305 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5 ),
    x = xs ),
    grad = "v3" )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.1002, 0.5 ),
            x = c( -2, -0.001, 0 ) ),
            this.mix.type = 3 ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with crossing over #2 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried." ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( 0, 0.2, 1.9 )
a$trace.q(
    data.frame(
    p = c( 0.5, 0.6, 0.9 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.2085623, 0.9596986, 1.4142929 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( 1, 1.2, 2.9 )
a$trace.q(
    data.frame(
    p = c( 0.5, 0.6, 0.9 ),
    x = xs ),
    grad = "v3" )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
xs <- c( 0, 1.2, 2.9 )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.5, 0.6, 0.9 ),
            x = xs ),
            this.mix.type = 3 ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #2 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #3 quantile has failed" ),
        "Crossover-tracing has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.5, 0.6, 0.9 ),
            x = xs ),
            grad = "v3" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 1/12: mix.type = 3, with median, without eq.mean)
xs <- c( -2.9, -1.2, 0 )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 2000, xtol=1e-11 ) ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #2 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/12: mix.type = 3, with median, eq.mean = TRUE)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 2000, xtol=1e-11 ), eq.mean = TRUE ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 3/12: mix.type = 3, with median, eq.mean = FALSE)
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 2000, xtol=1e-11 ), eq.mean = FALSE ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #2 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed. Tracing with 3 components has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 4/12: mix.type = 3, without median, without eq.mean)
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.51 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 2000, xtol=1e-11 ) ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #2 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #3 quantile has failed." ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 5/12: mix.type = 3, without median, eq.mean = TRUE)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.51 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 2000, xtol=1e-11 ), eq.mean = TRUE ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 6/12: mix.type = 3, without median, eq.mean = FALSE)
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.51 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 2000, xtol=1e-11 ), eq.mean = FALSE ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #2 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #3 quantile has failed." ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed. Tracing with 3 components has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 7/12: grad = "v3", with median, without eq.mean)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5 ),
            x = xs ),
            grad = "v3", control = list( maxit = 2000, xtol=1e-11 ) ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 8/12: grad = "v3", with median, eq.mean = TRUE)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5 ),
            x = xs ),
            grad = "v3", control = list( maxit = 2000, xtol=1e-11 ), eq.mean = TRUE ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 9/12: grad = "v3", with median, eq.mean = FALSE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5 ),
            x = xs ),
            grad = "v3", control = list( maxit = 2000, xtol=1e-11 ), eq.mean = FALSE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 10/12: grad = "v3", without median, without eq.mean)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.51 ),
            x = xs ),
            grad = "v3", control = list( maxit = 1000, xtol=1e-11 ) ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: " ),  # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 11/12: grad = "v3", without median, eq.mean = TRUE)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.51 ),
            x = xs ),
            grad = "v3", control = list( maxit = 1000, xtol=1e-11 ), eq.mean = TRUE ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: " ),  # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 12/12: grad = "v3", without median, eq.mean = FALSE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.51 ),
            x = xs ),
            grad = "v3", control = list( maxit = 1000, xtol=1e-11 ), eq.mean = FALSE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


# Error case
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 4000, xtol = 1e-11 ) ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with allowSingular option has failed. Crossover-tracing has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #3 quantile has failed" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with crossing over #1 quantile has failed" ),
        "Crossover-tracing has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6 ),
            x = xs ),
            grad = "v3", control = list( maxit = 4000, xtol = 1e-11 ) ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 3, without vgrad.* / grad = "v3", 4 quantiles
# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.87 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3 )
expect_equal( a$is.v2( TRUE ),  TRUE )
expect_equal( a$is.v3(),        TRUE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.eq.mean(),   FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.4, 0.5 ),
            x = xs ),
            this.mix.type = 3, eq.mean = TRUE ),
        "nleqslv has failed. Message: Jacobian is singular" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( 0.5, qnorm( 0.6, 0.5, 0.9 ), qnorm( 0.75, 0.52, 1 ), qnorm( 0.9, 0.57, 1.1 ) )
a$trace.q(
    data.frame(
    p = c( 0.5, 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 3 )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.5, 0.6, 0.75, 0.9 ),
            x = xs ),
            grad = "v3", eq.mean = TRUE ),
        "nleqslv has failed. Message: Jacobian is singular" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


# normal test (same quantiles, different optioins 1/4)
xs <- c( -1, -0.1, 0.2, 1.3 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6, 0.9 ),
    x = xs ),
    this.mix.type = 3 )
expect_equal( a$is.v2(), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (same quantiles, different optioins 2/4)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6, 0.9 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (same quantiles, different optioins 3/4)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6, 0.9 ),
    x = xs ),
    grad = "v3", eq.mean = FALSE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (same quantiles, different optioins 4/4)
b <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.4, 0.6, 0.9 ),
        x = xs ),
        grad = "v3" )$obj
expect_equal( b$is.v2(), FALSE )
expect_equal( b$is.v3(), TRUE )
expect_equal( b$is.eq.mean(), FALSE )
expect_equal( c( b$median, b$mean, b$sd, b$lsd, b$usd ), c( a$median, a$mean, a$sd, a$lsd, a$usd ) )
expect_equal( b$p( xs ), c( 0.1, 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
rm( b )


# normal test
xs <- c( -2, -0.5, 0, 0.7 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5, 0.8 ),
    x = xs ),
    this.mix.type = 3, eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.2595387, 1.4798423, 0.9914344 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( -2, -0.5, 0, 0.5 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 3, eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.640214, 1.640214, 1.640213 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( -4, -2.5, -2, -1.5 )
    a$trace.q(
    data.frame(
    p = c( 0.1, 0.3, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 3, eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.640214, 1.640214, 1.640213 ), tolerance = 5e-7 )
expect_equal( a$cmp$sd[1] == a$cmp$sd[3], FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.3, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.3, 0.5, 0.7 ), xs, c( -8, 4 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( -1.5, -0.4, 0, 0.5 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 3, eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.119693, 1.120214, 1.119173 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# This test should be performed after the previous test immediately (to use the previous result).
sd.2 <- a$cmp$sd[2]
xs <- c( -1.5, -0.4, 0, qnorm( 0.7, 0, sd.2 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 3, eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.458235, 1.348544, 1.560232 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
xs <- c( -2, -0.5, 0, 0.7 )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.5, 0.7 ),
            x = xs ),
            this.mix.type = 3, eq.mean = TRUE ),
        "The probability of the highest quantile is too near to 0.5" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: x-values within tolerance 'xtol'" ),
        "Message: Tracing with 2 components has been retried" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.5, 0.7 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 1000 ) ),
        "nleqslv has once failed. Message: " ), # This message may be different depending on the environment.
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with mean-equaled components has been retried" ),
        "The probability of the highest quantile is too near to 0.5" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.5, 0.7 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 1000 ), eq.mean = FALSE ),
        "nleqslv has once failed. Message: " ), # This message may be different depending on the environment.
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
xs <- c( -2, -0.5, 0, 0.1 )
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.5, 0.7 ),
            x = xs ),
            this.mix.type = 3 ),
        "nleqslv has once failed. Message: Jacobian is singular" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "The probability of the highest quantile is too large" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (messages will change when with allowSingular = TRUE)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.5, 0.7 ),
            x = xs ),
            this.mix.type = 3, control = list( allowSingular = TRUE ) ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "The probability of the highest quantile is too large" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (allowSingular = FALSE; messages will change than allowSingular = TRUE)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.5, 0.7 ),
            x = xs ),
            this.mix.type = 3, control = list( allowSingular = FALSE ) ),
        "nleqslv has once failed. Message: Jacobian is singular" ),
        "Tracing with 2 components has failed. Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: Jacobian is singular" ),
        "Tracing with mean-equaled components has been retried" ),
        "The probability of the highest quantile is too large" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: Jacobian is singular" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test (rare case 1/5)
#   This case of xs and p is:
#       if grad = "v2", it will succeed;
#       if grad = "v3" and eather eq.mean or eq.sd = TRUE, it will succeeed;
#       if grad = "v3" and both eq.mean and eq.sd = FALSE, it will fail.
xs <- c( -2, 0, 0.1, 2.3 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6, 0.9 ),
    x = xs ),
    this.mix.type = 3 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (rare case 2/5)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6, 0.9 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.694588, 1.610757, 1.7744622 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case (rare case 3/5)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6, 0.9 ),
            x = xs ),
            grad = "v3", eq.mean = FALSE, control = list( maxit = 1000 ) ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test (rare case 4/5)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6, 0.9 ),
    x = xs ),
    grad = "v3", eq.sd = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.263114, 1.211446, 1.312750 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (rare case 5/5)
expect_message( expect_message(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.6, 0.9 ),
        x = xs ),
        grad = "v3", control = list( maxit = 1000 ) ),
    "nleqslv has once failed. Message: No better point found" ),
    "Tracing with mean-equaled components has been retried" )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.694588, 1.610757, 1.7744622 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# This test should be performed after the previous test immediately (to use the previous result).
sd.2 <- a$cmp$sd[2]
xs <- c( qnorm( 0.1, 0, sd.2 ), 0, 0.1, 2.3 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6, 0.9 ),
    x = xs ),
    grad = "v3" )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test
xs <- c( 2.3, -1.5, 0, 0.3 )
a$trace.q(
    data.frame(
    p = c( 0.9, 0.1, 0.5, 0.6 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.559393, 1.338481, 1.752678 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.9, 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.9, 0.1, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (similar condition with 3 quantiles)
xs <- c( 0, 0.3, 2.3 )
a$trace.q(
    data.frame(
    p = c( 0.5, 0.6, 0.9 ),
    x = xs ),
    grad = "v3", eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.560729, 1.340740, 1.753330 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.5, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# Error case
xs <- c( -0.2, 0, 0.1, 2.3 )
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.5, 0.6, 0.9 ),
            x = xs ),
            grad = "v3" ),
        "nleqslv has once failed. Message:" ),      # This message may be different depending on the environment.
        "Tracing with mean-equaled components has been retried" ),
        "The probability of the lowest quantile is too near to 0.5" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 1/12: mix.type = 3, with median, with eq.mean)
xs <- c( -0.2, 0, 0.1, 2.3 )
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.5, 0.6, 0.9 ),
            x = xs ),
            this.mix.type = 3 ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with mean-equaled components has been retried" ),
        "The probability of the lowest quantile is too near to 0.5" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/12: mix.type = 3, with median, eq.mean = TRUE)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.5, 0.6, 0.9 ),
            x = xs ),
            this.mix.type = 3, eq.mean = TRUE ),
        "The probability of the lowest quantile is too near to 0.5" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 3/12: mix.type = 3, with median, eq.mean = FALSE)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.5, 0.6, 0.9 ),
            x = xs ),
            this.mix.type = 3, eq.mean = FALSE ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with 3 components has been retried" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 4/12: mix.type = 3, without median, without eq.mean)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.51, 0.6, 0.9 ),
            x = xs ),
            this.mix.type = 3, control = list( allowSingular = TRUE, xtol = 1e-11, maxit = 80 ) ),
        "nleqslv has once failed. Message: Iteration limit exceeded" ),
        "Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 5/12: mix.type = 3, without median, eq.mean = TRUE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.51, 0.6, 0.9 ),
            x = xs ),
            this.mix.type = 3, control = list( allowSingular = TRUE, xtol = 1e-11, maxit = 80 ), eq.mean = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 6/12: mix.type = 3, without median, eq.mean = FALSE)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.51, 0.6, 0.9 ),
            x = xs ),
            this.mix.type = 3, control = list( allowSingular = TRUE, xtol = 1e-11, maxit = 80 ), eq.mean = FALSE ),
        "nleqslv has once failed. Message: Iteration limit exceeded" ),
        "Tracing with 3 components has been retried" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 7/12: grad = "v3", with median, without eq.mean)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.5, 0.6, 0.9 ),
            x = xs ),
            grad = "v3" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with mean-equaled components has been retried" ),
        "The probability of the lowest quantile is too near to 0.5" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 8/12: grad = "v3", with median, eq.mean = TRUE)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.5, 0.6, 0.9 ),
            x = xs ),
            grad = "v3", eq.mean = TRUE ),
        "The probability of the lowest quantile is too near to 0.5" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 9/12: grad = "v3", with median, eq.mean = FALSE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.5, 0.6, 0.9 ),
            x = xs ),
            grad = "v3", eq.mean = FALSE ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 10/12: grad = "v3", without median, without eq.mean)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.51, 0.6, 0.9 ),
            x = xs ),
            grad = "v3", control = list( allowSingular = TRUE, xtol = 1e-11, maxit = 80 ) ),
        "nleqslv has once failed. Message: Iteration limit exceeded" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 11/12: grad = "v3", without median, eq.mean = TRUE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.51, 0.6, 0.9 ),
            x = xs ),
            grad = "v3", control = list( allowSingular = TRUE, xtol = 1e-11, maxit = 80 ), eq.mean = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 12/12: grad = "v3", without median, eq.mean = FALSE)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.4, 0.51, 0.6, 0.9 ),
            x = xs ),
            grad = "v3", control = list( allowSingular = TRUE, xtol = 1e-11, maxit = 80 ), eq.mean = FALSE ),
        "nleqslv has failed. Message: Iteration limit exceeded" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.04, 0.5, 0.6, 0.9 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 2000, xtol = 1e-11 ) ),
        "nleqslv has once failed. Message: Jacobian is singular" ),
        "Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "The probability of the lowest quantile is too small" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
xs <- c( -2.3, -0.1, 0, 0.2 )
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5, 0.9 ),
            x = xs ),
            this.mix.type = 3, control = list( maxit = 3000 ) ),
        "nleqslv has once failed. Message: Jacobian is too ill-conditioned" ),
        "Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "The probability of the highest quantile is too large" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5, 0.51 ),
            x = xs ),
            this.mix.type = 3 ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "The probability of the highest quantile is too near to 0.5" ),
        "Logical stepwise tracing has failed. Tracing with allowSingular option has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( -2.3, -0.2, 0, 0.2 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 3, eq.mean = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.851449, 1.851450, 1.851448 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.2, 0.4, 0.6, 0.9 ),
            x = c( -2.8, -0.1, 0.1, 2.3 ) ),
            this.mix.type = 3, control = list( maxit = 6000 ) ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 3 components has been retried" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "nleqslv has failed. Message: Jacobian is singular" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 3, 5 or 6 quantiles
# normal test
xs <- c( qnorm( 0.1, 0, 1.12 ), qnorm( 0.25, 0, 1.11 ),
         0,
         qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 3 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 3, control = control )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_near( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ) )
show.results()

# normal test (keep mix.type = 3)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ) )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
show.results()

# normal test (no options any other quantiles)
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
        x = xs ) )$obj
expect_identical( a$mix.type, 4L )
expect_equal( a$is.normal(),    FALSE )
expect_equal( a$is.h(),         FALSE )
expect_equal( a$is.h( TRUE ),   FALSE )
expect_equal( a$is.v2(),        FALSE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        FALSE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  TRUE )
expect_false( a$is.eq.mean() )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
show.results()

# normal test
xs <- c( -1.4, -0.7, 0, 0.75, 1.42 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 3 )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( -0.4, 0.3, 1, 1.75, 2.42 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 3 )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.75, 0.9 ), xs, c( -2, 4 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( -1.412, -0.262, 0.272, 0.756, 1.427 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 3 )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( -1.4, -0.1, 0.1, 0.75, 1.42 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.45, 0.62, 0.84, 0.92 ),
    x = xs ),
    this.mix.type = 3 )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.45, 0.62, 0.84, 0.92 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.45, 0.62, 0.84, 0.92 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
            x = xs ),
            this.mix.type = 3 ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.51, 0.75, 0.9 ),
            x = xs ),
            this.mix.type = 3 ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
            x = xs ),
            grad = "v3" ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.51, 0.75, 0.9 ),
            x = xs ),
            grad = "v3" ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


# normal test
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
         qnorm( 0.4, 0, 0.98 ), qnorm( 0.6, 0, 1.03 ),
         qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 3 )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
    x = xs ),
    grad = "v3", control = control )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_near( a$p( xs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ) )
show.results()

# normal test (keep mix.type = 3)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
    x = xs ) )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
xs <- c( -2, -1, -0.01, 0.01, 1, 2 )
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.4, 0.5, 0.6, 0.7, 0.8),
            x = xs ),
            this.mix.type = 3, control = list( allowSingular = TRUE ) ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.4, 0.45, 0.55, 0.6, 0.75),
            x = xs ),
            this.mix.type = 3, control = list( allowSingular = TRUE ) ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.4, 0.5, 0.6, 0.7, 0.8),
            x = xs ),
            grad = "v3", control = list( allowSingular = TRUE ) ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.4, 0.45, 0.55, 0.6, 0.75),
            x = xs ),
            grad = "v3", control = list( allowSingular = TRUE ) ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type == 4, basic tests
# Error case
xs <- c( qnorm( 0.25, 0, 0.9 ), qnorm( 0.5, 0, 0.95 ), qnorm( 0.75, 0, 1.1 ) )
expect_error( a$trace.q(
        data.frame(
        p = c( 0.25, 0.5, 0.75 ),
        x = xs ),
        this.mix.type = 4 ),
        "Illegal number of quantiles for mix.type = 4" )
expect_cleared( a ); a <- GGD$new()

# Error case
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
         qnorm( 0.6, 0, 1.03 ), qnorm( 0.75, 0, 1.1 ) )
expect_error( a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.75, 0.9 ),
        x = xs ),
        this.mix.type = 4 ),
        "Illegal number of quantiles for mix.type = 4" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
        data.frame(
        p = seq( 0.1, 0.9, 0.1 ),
        x = 1:9 ),
        this.mix.type = 4 ),
        "Illegal number of valid quantiles" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
        x = c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.97 ),
               qnorm( 0.6, 0, 1.1 ), qnorm( 0.75, 0, 1.12 ), qnorm( 0.9, 0, 1.2 ) ) ),
        this.mix.type = 4, eq.mean = TRUE ),
    "Illegal number of quantiles for mix.type = 4 and either eq.mean or eq.sd is TRUE" )
expect_cleared( a ); a <- GGD$new()


#### mix.type == 4, 5 quantiles
# normal test
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
         0,
         qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4 )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4, control = control )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_near( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ) )
show.results()

# normal test (keep mix.type = 4)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ) )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
show.results()


# normal test
expect_message(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.6, 0.75, 0.9 ),
        x = xs ),
        this.mix.type = 4 ),
    paste( "Initial guessing with 3-quantile-tracing for left-side mean-differed components has failed.",
           "2-quantile-tracing has used instead" ) )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.5, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
expect_message(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.4, 0.5, 0.9 ),
        x = xs ),
        this.mix.type = 4 ),
    paste( "Initial guessing with 3-quantile-tracing for right-side mean-differed components has failed.",
           "2-quantile-tracing has used instead" ) )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.5, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.5, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
expect_message(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.4, 0.6, 0.9 ),
        x = xs ),
        this.mix.type = 4 ),
    paste( "Initial guessing with 3-quantile-tracing for right-side mean-differed components has failed.",
           "2-quantile-tracing has used instead" ) )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.6, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test (eq.mean = TRUE)
xs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
         0,
         qnorm( 0.75, 0, 0.9 ), qnorm( 0.9, 0, 0.8 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4, eq.mean = TRUE )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
expect_equal( a$is.eq.mean(), TRUE )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (eq.mean = FALSE)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4, eq.mean = FALSE )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
expect_equal( a$is.eq.mean(), FALSE )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test
xs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
         0,
         qnorm( 0.75, 0, 0.9 ), qnorm( 0.9, 0, 0.792 ) )
expect_message(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
        x = xs ),
        this.mix.type = 3, grad = "hv", eq.mean = TRUE ),
    "Initial guessing with 3-quantile-tracing for right-side mean-equaled components" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 0.792 ), qnorm( 0.25, 0, 0.9 ),
         0,
         qnorm( 0.75, 0, 1.5 ), qnorm( 0.9, 0, 2 ) )
expect_message(
    a <- ggd.trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
            x = xs ),
            grad = "hv", eq.mean = TRUE )$obj,
        paste( "Initial guessing with 3-quantile-tracing for left-side mean-equaled components has failed.",
               "2-quantile-tracing has used instead" ) )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
         0,
         qnorm( 0.75, 0, 0.95 ), qnorm( 0.9, 0, 0.792 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    grad = "hv" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 0.792 ), qnorm( 0.25, 0, 0.95 ),
         0,
         qnorm( 0.75, 0, 1.5 ), qnorm( 0.9, 0, 2 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4 )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# Error case (process path test 1/6: with median, without eq.mean)
xs <- c( -1, -0.01, 0, 0.05, 2 )
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.4, 0.5, 0.6, 0.75 ),
            x = xs ),
            grad = "hv" ),
        "Initial guessing with 3-quantile-tracing for left-side mean-differed components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side mean-differed components has failed" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with 6 degrees of freedom has failed. Tracing with 8 degrees of freedom has been retried" ),
        "Initial guessing with 3-quantile-tracing for left-side components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side components has failed" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with mean-equaled components has been retried" ),
        "Initial guessing with 3-quantile-tracing for left-side mean-equaled components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side mean-equaled components has failed" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/6: with median, eq.mean = TRUE)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.4, 0.5, 0.6, 0.75 ),
            x = xs ),
            grad = "hv", eq.mean = TRUE ),
        "Initial guessing with 3-quantile-tracing for left-side mean-equaled components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side mean-equaled components has failed" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 3/6: with median, eq.mean = FALSE)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.4, 0.5, 0.6, 0.75 ),
            x = xs ),
            grad = "hv", eq.mean = FALSE ),
        "Initial guessing with 3-quantile-tracing for left-side mean-differed components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side mean-differed components has failed" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with 6 degrees of freedom has failed. Tracing with 8 degrees of freedom has been retried" ),
        "Initial guessing with 3-quantile-tracing for left-side components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side components has failed" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 4/6: without median, without eq.mean #1)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.4, 0.51, 0.6, 0.75 ),
            x = xs ),
            grad = "hv", control = list( allowSingular = TRUE ) ),
        "Initial guessing with 3-quantile-tracing for left-side mean-differed components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side mean-differed components has failed" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with 6 degrees of freedom has failed. Tracing with 8 degrees of freedom has been retried" ),
        "Initial guessing with 3-quantile-tracing for left-side components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side components has failed" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with mean-equaled components has been retried" ),
        "Initial guessing with 3-quantile-tracing for left-side mean-equaled components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side mean-equaled components has failed" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 5/6: without median, eq.mean = TRUE)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.4, 0.5, 0.6, 0.75 ),
            x = xs ),
            grad = "hv", eq.mean = TRUE ),
        "Initial guessing with 3-quantile-tracing for left-side mean-equaled components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side mean-equaled components has failed" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 6/6: without median, eq.mean = FALSE)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.4, 0.5, 0.6, 0.75 ),
            x = xs ),
            grad = "hv", eq.mean = FALSE ),
        "Initial guessing with 3-quantile-tracing for left-side mean-differed components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side mean-differed components has failed" ),
        "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
        "Tracing with 6 degrees of freedom has failed. Tracing with 8 degrees of freedom has been retried" ),
        "Initial guessing with 3-quantile-tracing for left-side components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side components has failed" ),
        "nleqslv has failed. Message:" ),       # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normat test
expect_message( expect_message(
    a$trace.q(
        data.frame(
        p = c( 0.25, 0.48, 0.5, 0.52, 0.75 ),
        x = xs ),
        grad = "hv" ),
    "nleqslv has once failed. Message:" ),  # This message may be different depending on the environment.
    "Tracing with 6 degrees of freedom has failed" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.25, 0.48, 0.5, 0.52, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.25, 0.48, 0.5, 0.52, 0.75 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


#### mix.type = 4, 6 quantiles
# normal test
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.97 ),
         qnorm( 0.6, 0, 1.1 ), qnorm( 0.75, 0, 1.12 ), qnorm( 0.9, 0, 1.2 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4 )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
xs <- c( qnorm( 0.1, 0, 0.9  ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.98 ),
         qnorm( 0.6, 0, 1.03 ), qnorm( 0.75, 0, 1.1  ), qnorm( 0.9, 0, 1.13 ) )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.3, 0.4, 0.75, 0.9 ),
            x = xs ),
            this.mix.type = 4 ),
        "nleqslv has once failed. Message: " ), # This message may be different depending on the environment.
        "Tracing with 6 degrees of freedom has failed" ),
        "Initial guessing with 3-quantile-tracing for left-side components has failed" ),
        "nleqslv has failed. Message: " ),      # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.6, 0.7, 0.75, 0.9 ),
            x = xs ),
            this.mix.type = 4 ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 6 degrees of freedom has failed" ),
        "nleqslv has failed. Message: " ),      # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4 )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.1, 0, 0.6 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.97 ),
         qnorm( 0.6, 0, 1.1 ), qnorm( 0.75, 0, 1.12 ), qnorm( 0.9, 0, 1.3 ) )
expect_message( expect_message(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
        x = xs ),
        this.mix.type = 4 ),
    "nleqslv has once failed. Message: No better point found" ),
    "Tracing with 6 degrees of freedom has failed" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case (process path test 1/2: with median)
xs <- c( -1, -0.99, 0, 0.03, 0.2, 3 )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5, 0.6, 0.75, 0.9 ),
            x = xs ),
            this.mix.type = 4 ),
        "Initial guessing with 3-quantile-tracing for left-side mean-differed components has failed" ),
        "nleqslv has once failed. Message: " ), # This message may be different depending on the environment.
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 6 degrees of freedom has failed" ),
        "Initial guessing with 3-quantile-tracing for left-side components has failed" ),
        "nleqslv has failed. Message: " ),      # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/2: without median)
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.501, 0.6, 0.75, 0.9 ),
            x = xs ),
            this.mix.type = 4 ),
        "Initial guessing with 3-quantile-tracing for left-side mean-differed components has failed" ),
        "nleqslv has once failed. Message: " ), # This message may be different depending on the environment.
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 6 degrees of freedom has failed" ),
        "Initial guessing with 3-quantile-tracing for left-side components has failed" ),
        "nleqslv has failed. Message: " ),      # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 4, 7 quantiles
# normal test
# This xs is given by
#   xs <- c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) );
#   a$trace.q( data.frame( p = c( 0.3, 0.5, 0.7 ), x = xs ), this.mix.type = 2 );
#   xs <- a$q( c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ) )
xs <- c( -1.9535092, -0.9569967, -0.3331447, 0, 0.3085147, 0.7858078, 1.4341828 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
    x = xs ),
    grad = "hv", control = list( maxit = 1000 ) )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$median, 0, tolerance = 5e-8 )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
    x = xs ),
    grad = "hv", control = control )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_near( a$p( xs ), c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ) )
show.results()


# normal test
xs <- c( -1.4, -0.78, -0.3, 0, 0.32, 0.96, 2 )
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
        x = xs ),
        mix.type = 4 )$obj
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$median, 0, tolerance = 5e-8 )
expect_equal( a$p( xs ), c( 0.10, 0.25, 0.40, 0.50, 0.60, 0.75, 0.90 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (with mix.type = 2 to trace some of quantiles which are got in above test)
# This test should be performed after the previous test immediately (to use the previous result).
xs.2 <- a$q( c( 0.25, 0.5, 0.75 ) )
a$trace.q(
    data.frame(
    p = c( 0.25, 0.5, 0.75 ),
    x = xs.2 ),
    this.mix.type = 2 )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$p( xs.2 ), c( 0.25, 0.50, 0.75 ), tolerance = 5e-7 )
expect_near( a$p( xs[c( 1, 3, 5, 7 )] ), c( 0.10, 0.40, 0.60, 0.90 ), tol.near = 5e-2 )
show.results()


# Error case (If maxit is increased, this case succeeds.)
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.98 ),
         0,
         qnorm( 0.6, 0, 1.03 ), qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.5, 0.55, 0.6, 0.75, 0.9 ),
            x = xs ),
            this.mix.type = 4, control = list( maxit = 10 ) ),
        "nleqslv has failed. Message: Iteration limit exceeded" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test (median is not centered in the quantiles)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.45, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4 )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$median, xs[5], tolerance = 5e-8 )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.45, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.45, 0.5, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.4, 0.45, 0.55, 0.75, 0.9 ),
            x = xs ),
            this.mix.type = 4, control = list( maxit = 1000 ) ),
        "nleqslv has failed. Message:" ),   # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test (median is not centered in the quantiles)
a$trace.q(
    data.frame(
        p = c( 0.1, 0.25, 0.5, 0.62, 0.7, 0.8, 0.9 ),
        x = xs ),
    this.mix.type = 4 )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$median, xs[3], tolerance = 5e-8 )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.5, 0.62, 0.7, 0.8, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.5, 0.62, 0.7, 0.8, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test (with median)
xs <- c( -1, -0.7, -0.3, 0, 0.32, 0.9, 1.5 )
a$trace.q(
    data.frame(
        p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
        x = xs ),
    this.mix.type = 4, control = list( maxit = 1000 ) )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$median, 0, tolerance = 5e-8 )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (without median)
a$trace.q(
    data.frame(
        p = c( 0.1, 0.25, 0.4, 0.501, 0.6, 0.75, 0.9 ),
        x = xs ),
    this.mix.type = 4, control = list( maxit = 1000 ) )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.501, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.501, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test (Baltan)
xs <- c( -1, -0.8, -0.3, 0, 0.32, 1, 1.2 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4, control = list( maxit = 1000 ) )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case (This test should be performed after the previous test immediately)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
            x = xs ),
            this.mix.type = 4, control = list( allowSingular = FALSE ) ),
        "nleqslv has failed. Message: Jacobian is too ill-conditioned" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test (sub1 of Baltan)
xs <- c( -1, -0.8, -0.3 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (sub2 of Baltan)
xs <- c( 0.32, 1, 1.2 )
a$trace.q(
    data.frame(
    p = c( 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (Baltan Rev.)
xs <- c( -1, -0.8, -0.3, 0, 0.32, 1, 1.2 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4, control = list( maxit = 1000 ) )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

a$set.cmp( data.frame(
                mean = c( -a$cmp$mean[3], -a$cmp$mean[4], -a$cmp$mean[1], -a$cmp$mean[2] ),
                sd = c( a$cmp$sd[3], a$cmp$sd[4], a$cmp$sd[1], a$cmp$sd[2] ) ) )
a$p( c( -1.2, -1, -0.32, 0, 0.3, 0.8, 1 ) )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), -xs[order( -xs )], c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (Baltan Rev.) (for success, it needs about 1000 iterations)
xs <- c( -1.2, -1, -0.32, 0, 0.3, 0.8, 1 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4, control = list( maxit = 1000 ) )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test (very straight quantiles)
# Note, even if the quantiles are symmetric, a symmetrical distribution may not generated.
xs <- c( -1.2, -0.83, -0.31, 0, 0.31, 0.83, 1.2 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 4 )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test (linear conditions)
xs <- seq( -1.5, 1.5, 0.5 )
a$trace.q(
    data.frame(
        p = seq( 0.2, 0.8, 0.1 ),
        x = xs ),
        this.mix.type = 4 )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), seq( 0.2, 0.8, 0.1 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    seq( 0.2, 0.8, 0.1 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case (with median)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.15, 0.2, 0.3, 0.35, 0.4, 0.5 ),
            x = xs ),
            this.mix.type = 4 ),
        "nleqslv has failed. Message: " ),  # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (without median)
expect_error(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.15, 0.2, 0.3, 0.35, 0.4, 0.51 ),
            x = xs ),
            this.mix.type = 4 ),
        "Initial guessing with 3-quantile-tracing for right-side components has failed" ),
        "nleqslv has failed. Message: " ),  # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### mix.type = 4, 8 quantiles
# Error case (without median)
xs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.2, 0.3, 0.35, 0.4, 0.7, 0.8, 0.9 ),
            x = xs ),
            this.mix.type = 4 ),
        "nleqslv has failed. Message: " ),  # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (without median)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.2, 0.3, 0.55, 0.6, 0.7, 0.8, 0.9 ),
            x = xs ),
            this.mix.type = 4, control = list( maxit = 3000 ) ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (with median)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 0.8, 0.9 ),
            x = xs ),
            this.mix.type = 4, control = list( maxit = 1000 ) ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


# normal test
xs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ),
    x = xs ),
    this.mix.type = 4 )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (trace again with very very similar conditions)
xs <- c( a$q( 0.1 ), a$q( 0.25 ), a$q( 0.4 ), a$q( 0.50 ), a$q( 0.6 ), a$q( 0.7 ), a$q( 0.8 ), a$q( 0.9 ) )
b <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
        x = xs ),
        mix.type = 4 )$obj
expect_equal( b$is.hv( TRUE ), TRUE )
expect_equal( c( b$sd, b$lsd, b$usd ), c( a$sd, a$lsd, a$usd ) )
show.results()

# normal test (with mix.type = 2 to trace some of quantiles which are got in above test)
# This test should be performed after the previous test immediately (to use the previous result).
xs.2 <- a$q( c( 0.25, 0.5, 0.75 ) )
a$trace.q(
    data.frame(
    p = c( 0.25, 0.5, 0.75 ),
    x = xs.2 ),
    this.mix.type = 2 )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.hv( TRUE ), FALSE )
plot.quantiles.and.p( a,
    c( 0.25, 0.5, 0.75 ), xs.2, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


#### eq.sd, basic tests
# Error case
expect_error( a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.6, 0.9 ),
        x = c( -1, 0, 0.5, 1 ) ),
        mix.type = 1, eq.sd = TRUE ),
        "Illegal number of quantiles for mix.type = 1 and either eq.mean or eq.sd is TRUE" )

# Error case
a <- GGD$new()
xs <- c( -2, -0.31, 0.29, 1.9 )
expect_error( a$trace.q(
        data.frame(
        p = c( 0.1, 0.4, 0.6, 0.9 ),
        x = xs ),
        this.mix.type = 2, eq.sd = TRUE ),
        "Illegal number of quantiles for mix.type = 2 and either eq.mean or eq.sd is TRUE" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$trace.q(
        data.frame(
        p = c( 0.25, 0.5, 0.75 ),
        x = c( -2, 0, 2.1 ) ),
        this.mix.type = 0, eq.sd = TRUE ),
        "Illegal number of quantiles for mix.type = 0" )
expect_cleared( a ); a <- GGD$new()

# Error case
xs <- c( -2, -0.31, 0.29, 1.9 )
expect_error( a$trace.q(
        data.frame(
        p = c( 0.1, 0.4, 0.6, 0.9 ),
        x = xs ),
        this.mix.type = 3, grad = "v2", eq.sd = TRUE ),
        "Illegal number of quantiles for grad = \"v2\" and either eq.mean or eq.sd is TRUE" )
expect_cleared( a ); a <- GGD$new()

# Error case
xs <- c( -2, -0.31, 0, 0.29, 1.9 )
expect_error( a$trace.q(
        data.frame(
        p = c( 0.1, 0.4, 0.5, 0.6, 0.9 ),
        x = xs ),
        this.mix.type = 3, eq.sd = TRUE ),
        "Illegal number of quantiles for mix.type = 3 and either eq.mean or eq.sd is TRUE" )
expect_cleared( a ); a <- GGD$new()

# Error case
xs <- c( -2, -1, -0.31, 0.29, 0.95, 1.9 )
expect_error( a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
        x = xs ),
        this.mix.type = 4, eq.sd = TRUE ),
        "Illegal number of quantiles for mix.type = 4 and either eq.mean or eq.sd is TRUE" )
expect_cleared( a ); a <- GGD$new()


#### eq.sd, mix.type = 1
# normal test
xs <- c( -1, 0, 0.5 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 1, eq.sd = TRUE )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.eq.sigma(), TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8066788, 0.8066788, 0.8066788 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6 ),
    x = xs ),
    this.mix.type = 1, eq.sd = TRUE )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9697424, 0.9697424, 0.9697424 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.6 ), tolerance = 5e-7 )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# Error case (process path test 1/2: with median)
expect_error(
    expect_message(
    a$trace.q(
            data.frame(
            p = c( 0.25, 0.5, 0.75 ),
            x = c( -2, 0, 2.1 ) ),
            this.mix.type = 1, eq.sd = TRUE ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/2: without median)
expect_error(
    expect_message(
    a$trace.q(
            data.frame(
            p = c( 0.25, 0.51, 0.75 ),
            x = c( -2, 0, 2.1 ) ),
            this.mix.type = 1, eq.sd = TRUE ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### eq.sd, mix.type = 2
# normal test
xs <- c( -0.584, 0, 0.291 )
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 2, eq.sd = TRUE )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9982686, 0.9982686, 0.9982686 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (without eq.sd)
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.6 ),
    x = xs ),
    this.mix.type = 2 )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( a$p( xs ), c( 0.3, 0.5, 0.6 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.3, 0.5, 0.6 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# Error case (process path test 1/2: with median)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.5, 0.75 ),
            x = c( -2, 0, 2.1 ) ),
            this.mix.type = 2, eq.sd = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/2: without median)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.25, 0.51, 0.75 ),
            x = c( -2, 0, 2.1 ) ),
            this.mix.type = 2, eq.sd = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


# normal test
xs <- c( -2, 0, 1.8 )
a$trace.q(
    data.frame(
    p = c( 0.25, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 2, eq.sd = TRUE )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 2.171117, 2.171117, 2.171117 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.25, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.25, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

plot( seq( -8, 8, 0.01 ), a$d( seq( -8, 8, 0.01 ) ), type = "l" )
plot( seq( -8, 8, 0.01 ), a$p( seq( -8, 8, 0.01 ) ), type = "l" )
plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" )
sample <- a$r( 1000 ); hist( sample )

# normal test (control option)
a$trace.q(
    data.frame(
    p = c( 0.25, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 2, eq.sd = TRUE, control = control )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$mean == 0, TRUE )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_near( a$p( xs[c( 1, 3 )] ), c( 0.25, 0.7 ) )
show.results()

# normal test
xs <- c( qnorm( 0.25, 0, 1 ), 0, qnorm( 0.7, 0, 1.02 ) )
a$trace.q(
    data.frame(
    p = c( 0.25, 0.5, 0.7 ),
    x = xs ),
    this.mix.type = 2, eq.sd = TRUE )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9156116, 0.9156116, 0.9156116 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.25, 0.5, 0.7 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.25, 0.5, 0.7 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.2, 0, 1 ), qnorm( 0.4, 0, 1.01 ), qnorm( 0.8, 0, 1.05 ) )
a <- ggd.trace.q(
        data.frame(
        p = c( 0.2, 0.4, 0.8 ),
        x = xs ),
        mix.type = 2, eq.sd = TRUE )$obj
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9637276, 0.9637276, 0.9637276 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.2, 0.4, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.2, 0.4, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (with eq.mean)
a$trace.q(
    data.frame(
    p = c( 0.2, 0.4, 0.8 ),
    x = xs ),
    this.mix.type = 2, eq.mean = TRUE )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.025681, 1.011411, 1.039755 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.2, 0.4, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.2, 0.4, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


#### eq.sd, mix.type = 3, vgrad.2
# normal test
xs <- c( qnorm( 0.1, -0.2, 1 ), qnorm( 0.4, 0, 1 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.sd = TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0172012, 1.0672460, 0.9645634 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (without eq.sd)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (with eq.mean)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3, grad = "v2", eq.mean = TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$mean == 0, TRUE )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# Error case (process path test 1/2: with median)
xs <- c( -0.479, -0.472, 0.472 )
expect_error(
    expect_message(
        a$trace.q(
                data.frame(
                p = c( 0.1, 0.48, 0.5 ),
                x = xs ),
                grad = "v2", eq.sd = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/2: without median)
expect_error(
    expect_message(
        a$trace.q(
                data.frame(
                p = c( 0.1, 0.48, 0.52 ),
                x = xs ),
                grad = "v2", eq.sd = TRUE ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### eq.sd, mix.type = 3, without vgrad.* / grad = "v3", 3 quantiles
# normal test
xs <- c( qnorm( 0.1, -0.2, 1 ), qnorm( 0.4, 0, 1 ), 0 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    this.mix.type = 3, eq.sd = TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3( TRUE ), FALSE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( a$median, 0, tolerance = 5e-8 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0172012, 1.0672460, 0.9645634 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5 ),
    x = xs ),
    grad = "v3", eq.sd = TRUE )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( a$median, 0, tolerance = 5e-8 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.073747, 1.099917, 1.046922 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
# This is an extreme case that
# large differences of mean and standard deviations vs integrate may occur.
xs <- c( -0.5, 0, 4 )
expect_message( expect_message(
    a <- ggd.trace.q(
            data.frame(
            p = c( 0.25, 0.5, 0.75 ),
            x = xs ),
            mix.type = 3, eq.sd = TRUE )$obj,
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 3 components has been retried" )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( a$median, 0, tolerance = 5e-8 )
expect_equal( a$p( xs ), c( 0.25, 0.5, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.25, 0.5, 0.75 ), xs, c( -3, 5 ) )
plot( seq( -3, 5, 0.01 ), a$d( seq( -3, 5, 0.01 ) ), type = "l" )
show.results( is.extreme.case = TRUE )


# Error case (process path test 1/4: mix.type = 3, with median)
xs <- c( -2.9, -1.2, 0 )
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5 ),
            x = xs ),
            this.mix.type = 3, eq.sd = TRUE ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 3 components has been retried" ),
        "nleqslv has failed. Message: x-values within tolerance 'xtol'" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/4: mix.type = 3, without median)
expect_error(
    expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.51 ),
            x = xs ),
            this.mix.type = 3, eq.sd = TRUE ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 3 components has been retried" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 3/4: vgrad.3, with median)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5 ),
            x = xs ),
            grad = "v3", eq.sd = TRUE ),
        "nleqslv has failed. Message: x-values within tolerance 'xtol'" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 4/4: vgrad.3, without median)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.51 ),
            x = xs ),
            grad = "v3", eq.sd = TRUE ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### eq.sd, mix.type = 3, without vgrad.* / grad = "v3", 4 quantiles
# normal test
xs <- c( qnorm( 0.1, -0.2, 1 ), qnorm( 0.4, 0, 1 ), qnorm( 0.6, 0.1, 1 ), qnorm( 0.8, 0.4, 1 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6, 0.8 ),
    x = xs ),
    this.mix.type = 3, eq.sd = TRUE )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.380724, 1.292676, 1.463485 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.6, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.6, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (without eq.sd)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6, 0.8 ),
    x = xs ),
    this.mix.type = 3 )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3( TRUE ), FALSE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.6, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.6, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.1, 0.41, 0.6, 0.8 ),
    x = xs ),
    grad = "v3", eq.sd = TRUE )
expect_equal( a$is.v2(), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.343029, 1.273571, 1.409067 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.41, 0.6, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.41, 0.6, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# Error case (to show that eq.sd = TRUE option is very hard to convergent)
xs <- c( qnorm( 0.1, -0.2, 0.8 ), qnorm( 0.4, 0, 0.9 ), qnorm( 0.6, 0.1, 1 ), qnorm( 0.8, 0.4, 1.1 ) )
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.6, 0.8 ),
            x = xs ),
            this.mix.type = 3, eq.sd = TRUE ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test (without eq.sd)
expect_message( expect_message(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.4, 0.6, 0.8 ),
        x = xs ),
        this.mix.type = 3 ),
    "nleqslv has once failed. Message: No better point found" ),
    "Tracing with 2 components has failed. Tracing with 3 components has been retried" )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.6, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.6, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case (process path test 1/3: mix.type = 3, with median)
#   * A test without median has already done.
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6, 0.8 ),
            x = xs ),
            this.mix.type = 3, eq.sd = TRUE ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 2/3: with vgrad.3, with median)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.5, 0.6, 0.8 ),
            x = xs ),
            grad = "v3", eq.sd = TRUE ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (process path test 3/3: with vgrad.3, without median)
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.6, 0.8 ),
            x = xs ),
            grad = "v3", eq.sd = TRUE ),
        "nleqslv has failed. Message: Jacobian is" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### eq.sd, mix.type = 4
# normal test
xs <- c( -1.464921, -0.280095, 0.027170, 0.352307, 1.201652 )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
    x = xs ),
    this.mix.type = 4, eq.sd = TRUE )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.normal(),    FALSE )
expect_equal( a$is.h(),         FALSE )
expect_equal( a$is.h( TRUE ),   FALSE )
expect_equal( a$is.v2(),        FALSE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        FALSE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.symmetric(), FALSE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.312522, 1.255553, 1.367119 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5, 0.6, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5, 0.6, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (without eq.sd, with eq.mean)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
    x = xs ),
    this.mix.type = 4, eq.mean = TRUE )
expect_equal( a$is.hv( TRUE ),  TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$median == xs[3], TRUE )
expect_equal( all( a$median == a$cmp$mean ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5, 0.6, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5, 0.6, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (without eq.sd, with eq.mean)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.501, 0.6, 0.8 ),
    x = xs ),
    this.mix.type = 4, eq.mean = TRUE )
expect_equal( a$is.hv( TRUE ),  TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( all( a$median == a$cmp$mean ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.501, 0.6, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.501, 0.6, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case (to show that eq.sd = TRUE option is very hard to convergent)
xs <- round( xs, 3 )
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
            x = xs ),
            this.mix.type = 4, eq.sd = TRUE, control = list( maxit = 2000 ) ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test (without eq.sd, with eq.mean)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
    x = xs ),
    this.mix.type = 4, eq.mean = TRUE )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$median == xs[3], TRUE )
expect_equal( all( a$median == a$cmp$mean ), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.322447, 1.250436, 1.390735 ), tolerance = 5e-7 )
expect_equal( a$p( xs ), c( 0.1, 0.4, 0.5, 0.6, 0.8 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.4, 0.5, 0.6, 0.8 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case
expect_error(
    expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.4, 0.51, 0.6, 0.8 ),
            x = xs ),
            this.mix.type = 4, eq.sd = TRUE, control = list( maxit = 2000 ) ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


#### eq.sd, constructing a normal distribution
# normal test
xs <- c( -0.5, 0, 0.5 )
a$trace.q(
    data.frame(
    p = c( 0.25, 0.5, 0.75 ),
    x = xs ),
    this.mix.type = 1, eq.sd = TRUE )
expect_identical( a$mix.type, 1L )
expect_equal( nrow( a$cmp ), 2 )
expect_equal( a$is.normal(),    TRUE )
expect_equal( a$is.h(),         TRUE )
expect_equal( a$is.h( TRUE ),   FALSE )
expect_equal( a$is.v2(),        TRUE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        TRUE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$mean == 0, TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.7413011, 0.7413011, 0.7413011 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.25, 0.5, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.25, 0.5, 0.75 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.25, 0.5, 0.75 ),
    x = xs ),
    this.mix.type = 2, eq.sd = TRUE )
expect_identical( a$mix.type, 2L )
expect_equal( nrow( a$cmp ), 2 )
expect_equal( a$is.normal(),    FALSE )
expect_equal( a$is.h(),         TRUE )
expect_equal( a$is.h( TRUE ),   TRUE )
expect_equal( a$is.v2(),        FALSE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        FALSE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$mean == 0, TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.7390707, 0.7390707, 0.7390707 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.25, 0.5, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.25, 0.5, 0.75 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.25, 0.5, 0.75 ),
    x = xs ),
    grad = "v2", eq.sd = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( nrow( a$cmp ), 2 )
expect_equal( a$is.normal(),    TRUE )
expect_equal( a$is.h(),         TRUE )
expect_equal( a$is.h( TRUE ),   FALSE )
expect_equal( a$is.v2(),        TRUE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        TRUE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$mean == 0, TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.7413011, 0.7413011, 0.7413011 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.25, 0.5, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.25, 0.5, 0.75 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.25, 0.5, 0.75 ),
    x = xs ),
    this.mix.type = 3, eq.sd = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( nrow( a$cmp ), 2 )
expect_equal( a$is.normal(),    TRUE )
expect_equal( a$is.h(),         TRUE )
expect_equal( a$is.h( TRUE ),   FALSE )
expect_equal( a$is.v2(),        TRUE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        TRUE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$mean == 0, TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.7413011, 0.7413011, 0.7413011 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.25, 0.5, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.25, 0.5, 0.75 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
    data.frame(
    p = c( 0.25, 0.5, 0.75 ),
    x = xs ),
    grad = "v3", eq.sd = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( nrow( a$cmp ), 3 )
expect_equal( a$is.normal(),    TRUE )
expect_equal( a$is.h(),         TRUE )
expect_equal( a$is.h( TRUE ),   FALSE )
expect_equal( a$is.v2(),        TRUE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        TRUE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$mean == 0, TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.7413011, 0.7413011, 0.7413011 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.25, 0.5, 0.75 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.25, 0.5, 0.75 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
xs <- c( qnorm( 0.125, 0, 1 ), qnorm( 0.25, 0, 1 ), 0, qnorm( 0.75, 0, 1 ), qnorm( 0.875, 0, 1 ) )
a$trace.q(
    data.frame(
    p = c( 0.125, 0.25, 0.5, 0.75, 0.875 ),
    x = xs ),
    this.mix.type = 4, eq.sd = TRUE )
expect_identical( a$mix.type, 4L )
expect_equal( nrow( a$cmp ), 4 )
expect_equal( a$is.normal(),    TRUE )
expect_equal( a$is.h(),         TRUE )
expect_equal( a$is.h( TRUE ),   FALSE )
expect_equal( a$is.v2(),        TRUE )
expect_equal( a$is.v2( TRUE ),  FALSE )
expect_equal( a$is.v3(),        TRUE )
expect_equal( a$is.v3( TRUE ),  FALSE )
expect_equal( a$is.hv(),        TRUE )
expect_equal( a$is.hv( TRUE ),  FALSE )
expect_equal( a$median == 0, TRUE )
expect_equal( a$mean == 0, TRUE )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1, 1, 1 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( xs ), c( 0.125, 0.25, 0.5, 0.75, 0.875 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.125, 0.25, 0.5, 0.75, 0.875 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


#### Sequential tracing with changing mix.type
# Error case
xs <- c( -( 2 + 2.25e-16 ), -2, 2, 2 + 2.25e-16 )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.3, 0.4, 0.6, 0.7 ),
            x = xs ) ),
        "At mix.type = 2, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 3 components has been retried" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 1, an error has occurred:" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
a$set.cmp( data.frame( mean = 0, sd = 1 ), this.mix.type = 0 )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.3, 0.4, 0.6, 0.7 ),
            x = xs ) ),
        "At mix.type = 2, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 3 components has been retried" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 1, an error has occurred:" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (Remark the order of messages)
a$set.cmp( data.frame( mean = 0, sd = 1 ), this.mix.type = 1 )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.3, 0.4, 0.6, 0.7 ),
            x = xs ) ),
        "At mix.type = 1, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 2, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 3, an error has occurred:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 3 components has been retried" ),
        "At mix.type = 3, an error has occurred:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "At mix.type = 3, an error has occurred:" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (Remark the order of messages)
a$set.cmp( data.frame( mean = 0, sd = 1 ), this.mix.type = 3 )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.3, 0.4, 0.6, 0.7 ),
            x = xs ) ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 3 components has been retried" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 2, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 1, an error has occurred:" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case
a$set.cmp( data.frame( mean = 0, sd = 1 ), this.mix.type = 4 )
expect_error(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.3, 0.4, 0.6, 0.7 ),
            x = xs ) ),
        "At mix.type = 2, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 3 components has been retried" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with mean-equaled components has been retried" ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 1, an error has occurred:" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
         qnorm( 0.4, 0, 0.98 ), qnorm( 0.6, 0, 1.03 ),
         qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
expect_message(
expect_message( expect_message(
    a <- ggd.trace.q(
            data.frame(
            p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
            x = xs ) )$obj,
        "At mix.type = 4, an error has occurred .then retried.:" ),
        "nleqslv has once failed. Message: No better point found" ),
        "Tracing with 6 degrees of freedom has failed" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test
a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
        x = xs ),
        grad = "v3" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# Error case (This test is expected that the current a$mixtype is 3.)
xs <- c( -4, -( 2 + 2.25e-16 ), -2, 2, 2 + 2.25e-16, 4 )
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.4, 0.6, 0.7, 0.9 ),
            x = xs ) ),
        "At mix.type = 3, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: " ),      # This message may be different depending on the environment.
        "Initial guessing with 3-quantile-tracing for left-side mean-differed components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side mean-differed components has failed" ),
        "At mix.type = 4, an error has occurred:" ),
        "nleqslv has once failed. Message: " ), # This message may be different depending on the environment.
        "Tracing with 6 degrees of freedom has failed" ),
        "Initial guessing with 3-quantile-tracing for left-side components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side components has failed" ),
        "At mix.type = 4, an error has occurred:" ),
        "nleqslv has failed. Message: No better point found" ),
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()

# Error case (The order of messages are different from the previous test)
expect_error(
    expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
    expect_message( expect_message(
        a$trace.q(
            data.frame(
            p = c( 0.1, 0.3, 0.4, 0.6, 0.7, 0.9 ),
            x = xs ) ),
        "Initial guessing with 3-quantile-tracing for left-side mean-differed components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side mean-differed components has failed" ),
        "At mix.type = 4, an error has occurred .then retried.:" ),
        "nleqslv has once failed. Message: " ), # This message may be different depending on the environment.
        "Tracing with 6 degrees of freedom has failed" ),
        "Initial guessing with 3-quantile-tracing for left-side components has failed" ),
        "Initial guessing with 3-quantile-tracing for right-side components has failed" ),
        "At mix.type = 4, an error has occurred .then retried.:" ),
        "nleqslv has failed. Message: No better point found" ),
        "At mix.type = 3, an error has occurred:" ),
        "nleqslv has failed. Message: " ),      # This message may be different depending on the environment.
    "Failed to construct components" )
expect_cleared( a ); a <- GGD$new()


# normal test
xs <- c( qnorm( 0.1, 0, 0.8 ), qnorm( 0.25, 0, 0.85 ),
         qnorm( 0.4, 0, 0.9 ), qnorm( 0.6, 0, 1.2 ),
         qnorm( 0.75, 0, 1.21 ), qnorm( 0.9, 0, 1.23 ) )
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
        x = xs ) )$obj
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_false( a$is.eq.mean() )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (with mix.type = 3 in same consition as above)
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
    x = xs ),
    this.mix.type = 3 )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$p( xs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.quantiles.and.p( a,
    c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), xs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


#### kind and overwriting

# Error case
expect_error(
    a$trace.q(
        data.frame(
        p = c( 0.25, 0.5, 0.75 ),
        x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.5, 0, 0.8 ), qnorm( 0.75, 1, 1 ) ) ),
        this.kind = "Normal Distribution" ),
    "Illegal number of quantiles for mix.type = 0" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( -1, 0, 0.5 )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6 ),
    x = xs ),
    this.kind = "Mean of Mean-Differed Sigma-Equaled 2 Normal Distributions" )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), qnorm( 0.6, 0, 0.8 ) )
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.4, 0.6 ),
        x = xs ),
        kind = "Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions" )$obj
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6 ),
    x = xs ),
    this.kind = "Mean of Mean-Differed Sigma-Differed 2 Normal Distributions" )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
xs <- c( -1, 0, 0.5 )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6 ),
    x = xs ),
    this.kind = "Mean-Differed Sigma-Equaled Horizontal Gradational Distribution" )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
xs <- c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) )
a <- ggd.trace.q(
        data.frame(
        p = c( 0.3, 0.5, 0.7 ),
        x = xs ),
        kind = "Mean-Equaled Sigma-Differed Horizontal Gradational Distribution" )$obj
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.7 ),
    x = xs ),
    this.kind = "Mean-Differed Sigma-Differed Horizontal Gradational Distribution" )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "Vertical" )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "3-Mean-Differed Sigma-Equaled Vertical Gradational Distribution" )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "3-Mean-Equaled Sigma-Differed Vertical Gradational Distribution" )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "3-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
         0,
         qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$clear()
expect_message(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
        x = xs ),
        this.kind = "Mean-Differed Sigma-Equaled Horizontal-Vertical Gradational Distribution" ),
    "Initial guessing with 3-quantile-tracing for left-side sigma-equaled components has failed" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.kind = "Mean-Equaled Sigma-Differed Horizontal-Vertical Gradational Distribution" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.kind = "Mean-Differed Sigma-Differed Horizontal-Vertical Gradational Distribution" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )


# Error case
expect_error(
    a$trace.q(
        data.frame(
        p = c( 0.25, 0.5, 0.75 ),
        x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.5, 0, 0.8 ), qnorm( 0.75, 1, 1 ) ) ),
        this.kind = 1 ),
    "Illegal number of quantiles for mix.type = 0" )
expect_cleared( a ); a <- GGD$new()

# normal test
xs <- c( -1, 0, 0.5 )
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.6 ),
        x = xs ),
        kind = 2 )$obj
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), qnorm( 0.6, 0, 0.8 ) )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6 ),
    x = xs ),
    this.kind = 3 )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), qnorm( 0.6, 0, 0.8 ) )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.4, 0.6 ),
    x = xs ),
    this.kind = 4 )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
xs <- c( -1, 0, 0.5 )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.6 ),
    x = xs ),
    this.kind = 5 )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
xs <- c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.3, 0.5, 0.7 ),
    x = xs ),
    this.kind = 6 )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a <- ggd.trace.q(
        data.frame(
        p = c( 0.3, 0.5, 0.7 ),
        x = xs ),
        kind = 7 )$obj
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
xs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = 8 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = 9 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = 10 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = 11 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = 12 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = 13 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
         0,
         qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$clear()
expect_message(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
        x = xs ),
        this.kind = 14 ),
    "Initial guessing with 3-quantile-tracing for left-side sigma-equaled components has failed" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.kind = 15 )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.kind = 16 )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )


# normal test
xs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "Mean-Equaled Sigma-Differed Horizontal-Vertical",
    this.mix.type = 3 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "Mean-Differed Sigma-Equaled Horizontal-Vertical",
    grad = "v3" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test
b <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.7 ),
        x = xs ),
        kind = a, eq.mean = TRUE, eq.sd = FALSE )$obj
expect_equal( b$mix.type, 3 )
expect_equal( b$is.v3( TRUE ), TRUE )
expect_equal( b$is.eq.mean(), TRUE )
expect_equal( b$is.eq.sd(), FALSE )

# normal test
b$clear()
b$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = a, eq.mean = FALSE )
expect_equal( b$mix.type, 3 )
expect_equal( b$is.v3( TRUE ), TRUE )
expect_equal( b$is.eq.mean(), FALSE )
expect_equal( b$is.eq.sd(), TRUE )
rm( b )

# normal test
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = a, eq.mean = TRUE, eq.sd = FALSE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = a, grad = "v2" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )


# normal test (overwrite by flag)
a$trace.q(
    data.frame(
    p = c( 0.25, 0.75 ),
    x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
    this.kind = "Mean-Differed Sigma-Equaled Horizontal-Vertical",
    grad = "normal" )
expect_identical( a$mix.type, 0L )

# normal test (overwrite by flag)
xs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), qnorm( 0.6, 0, 0.8 ) )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "Mean-Equaled Sigma-Differed Horizontal-Vertical",
    grad = "h" )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test (overwrite by flag)
xs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "3-Mean-Equaled Sigma-Differed Vertical",
    grad = "v2" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3( TRUE ), FALSE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test (overwrite by flag)
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.7 ),
        x = xs ),
        kind = "2-Mean-Equaled Sigma-Differed Vertical",
        grad = "v3" )$obj
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test (overwrite by flag)
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "Normal Distribution",
    grad = "v3" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# Error case (overwrite by flag)
expect_error(
    a$trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.7 ),
        x = xs ),
        this.kind = "2-Mean-Equaled Sigma-Differed Vertical",
        grad = "hv" ),
    "Illegal number of quantiles for mix.type = 4" )
expect_cleared( a ); a <- GGD$new()

# normal test (overwrite by flag)
xs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
         0,
         qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$trace.q(
    data.frame(
    p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
    x = xs ),
    this.kind = "3-Mean-Differed Sigma-Differed Vertical",
    grad = "hv" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )


# normal test (change eq.mean from FALSE to TRUE)
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.25, 0.75 ),
    x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
    this.kind = "Mean-Differed Sigma-Equaled Horizontal",
    eq.mean = TRUE )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )

# normal test (change eq.sd from FALSE to TRUE)
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.25, 0.75 ),
    x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
    this.kind = "2-Mean-Equaled Sigma-Differed Vertical",
    eq.sd = TRUE )
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )

# normal test (change eq.mean from FALSE to TRUE)
a <- ggd.trace.q(
        data.frame(
        p = c( 0.25, 0.75 ),
        x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
        kind = "3-Mean-Differed Sigma-Equaled Vertical",
        eq.mean = TRUE )$obj
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )

# normal test (change eq.sd from FALSE to TRUE)
a <- ggd.trace.q(
        data.frame(
        p = c( 0.25, 0.75 ),
        x = c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ),
        kind = "Mean-Equaled Sigma-Differed Horizontal-Vertical",
        eq.sd = TRUE )$obj
expect_identical( a$mix.type, 0L )
expect_equal( a$p( c( qnorm( 0.25, 0, 1 ), qnorm( 0.75, 1, 1 ) ) ), c( 0.25, 0.75 ), tolerance = 5e-7 )


# normal test (change eq.mean from TRUE to FALSE)
xs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "Mean-Equaled Sigma-Differed Horizontal",
    eq.mean = FALSE )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.h(), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test (change eq.sd from TRUE to FALSE)
a$clear()
a$trace.q(
    data.frame(
    p = c( 0.1, 0.5, 0.7 ),
    x = xs ),
    this.kind = "2-Mean-Differed Sigma-Equaled Vertical",
    eq.sd = FALSE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test (change eq.mean from TRUE to FALSE)
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.7 ),
        x = xs ),
        kind = "2-Mean-Equaled Sigma-Differed Vertical",
        eq.mean = FALSE )$obj
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test (change eq.mean from TRUE to FALSE)
a <- ggd.trace.q(
        data.frame(
        p = c( 0.1, 0.5, 0.7 ),
        x = xs ),
        kind = "Mean-Differed Sigma-Equaled Horizontal",
        eq.sd = FALSE )$obj
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )


#### Error statements for the coverage
expect_error( ggd:::bisection( function( x ) { x^2 }, c( -2, -1 ) ), "not of opposite sign" )

qt  <- data.frame( mean = c( 0, 0 ), sd = c( 1, 1 ) )
expect_error( ggd:::v2.crossover( qt, NULL ),
              "nrow.qt. must be 3 for v2.crossover. nrow: 2" )
qt  <- data.frame( mean = 1:4, sd = 1:4 )
expect_error( ggd:::v2.crossover( qt, NULL ),
              "nrow.qt. must be 3 for v2.crossover. nrow: 4" )
rm( qt )


#### Appendix

## Plot of the burden ratio of the lower distribution for mix.type = 3, symmetric.
x <- seq( -5, 5, 0.01 )
plot( x, pnorm( x, 0, 1 ) - pnorm( x, 0, 1/sqrt(2) ) / sqrt(2), type="l" )


## Plots to show pnorm( x, 0, 1/sqrt(2) ) / pnorm( x, 0, 1 ) is bounded on [1, 1.107] where x > 0,
## and ( 1 - pnorm( x, 0, 1/sqrt(2) ) ) / ( 1 - pnorm( x, 0, 1 ) ) is also bounded on [1, 1.107] where x < 0.
x <- seq( 0, 4, 0.01 )
plot( x, pnorm( x, 0, 1/sqrt(2) ) / pnorm( x, 0, 1 ), type="l" )
x <- seq( -4, 0, 0.01 )
plot( x, ( 1 - pnorm( x, 0, 1/sqrt(2) ) ) / ( 1 - pnorm( x, 0, 1 ) ), type="l" )

max.pp <- ggd:::bisection( function(x)
                            dnorm( x, 0, 1/sqrt(2) ) / pnorm( x, 0, 1 ) -
                            pnorm( x, 0, 1/sqrt(2) ) * dnorm( x, 0, 1 ) /
                            pnorm( x, 0, 1 )^2, c( 0, 1 ))
max.pp
pnorm( max.pp, 0, 1/sqrt(2) ) / pnorm( max.pp, 0, 1 )
( 1 - pnorm( -max.pp, 0, 1/sqrt(2) ) ) / ( 1 - pnorm( -max.pp, 0, 1 ) )


## Plot of x which maximize pnorm( x, 0, sigma * 1/sqrt(2) ) / pnorm( x, 0, sigma )
## where sigma > 0 (it is linear).
sigma <- seq( 0.1, 2, 0.1 )
x <- vapply( sigma, function(s)
                    { ggd:::bisection( function(y)
                            dnorm( y, 0, s * 1/sqrt(2) ) / pnorm( y, 0, s ) -
                            pnorm( y, 0, s * 1/sqrt(2) ) * dnorm( y, 0, s ) / pnorm( y, 0, s )^2,
                            c( 0, s * 2 ) ) }, 0 )
plot( sigma, x, type="l" )

# Plot of the maximum values (it is a constant).
plot( sigma, pnorm( x, 0, sigma * 1/sqrt(2) ) / pnorm( x, 0, sigma ), type="l" )


## The following plots are of no use to this package, but may be interesting as mathematics.

## Plot of x.max which maximize pnorm( x, 0, sigma ) / pnorm( x, 0, 1 ) where 0 < sigma < 1.
sigma <- c( 0.001, seq( 0.01, 0.99, 0.01 ), 0.999 )
x.max <- vapply( sigma, function(s)
                        { ggd:::bisection( function(y)
                                            dnorm( y, 0, s ) / pnorm( y, 0, 1 ) -
                                            pnorm( y, 0, s ) * dnorm( y, 0, 1 ) /
                                            pnorm( y, 0, 1 )^2, c( 0, 1 ) ) }, 0 )
plot( sigma, x.max, type="l" )

## Plot of the maximum values.
plot( sigma, pnorm( x.max, 0, sigma ) / pnorm( x.max, 0, 1 ), type="l" )

