################################################################################################
## This file is to test nls.freq and nls.freq.all mainly. TeX output is tested also.
################################################################################################

################################################################################################
## ** Note **
##
## This test file outputs many graphs as results.
## However, each result is easily affected by the execution environment, even on the same OS.
## For this reason, we have determined that it is not suitable for automated snapshot testing,
## and have placed sample results as {test}/graphs.pdf in the "reference_materials" directory
## as references for test results instead.
##
## Developers and testers should refer to "reference_materials" as needed.
## The most important point to check may be whether each graph of
## probability distribution function passes near the points of the frequency distribution,
## except for from #21 to #33 (especially #25, #29 and #31) where not.use.nls = TRUE.
## Although #2 and #3 are also not.use.nls = TRUE,
## because original data follow a normal distribution, both graphs pass near the points.
##
## However, important tests are evaluated using expect_ functions.
## So, if all expect_ functions pass, the tests in this file are considered to have passed.
################################################################################################

# For interactive test, load setup.R expressly.
if ( file.exists( "tests/testthat" ) ) source( "tests/testthat/setup.R" )

################################
# Preparing
################################

a <- GGD$new()
if ( dev.cur() == 1 ) { dev.new(); plot.new() }

################################
# Functions for tests
################################

################################################################################################
#' Function for manually check difference from frequency distribution
#'
#' @param   a       A GGD object.
#' @param   x       A vector of x-coordinates.
#' @param   freq    A vector of frequencies.
#' @param   total   A total of frequencies.
#' @return  A list containing components
#'          \item{diff}{
#'                  A vector of distance between frequencies and
#'                  the values of probability density function.}
#'          \item{summary}{
#'                  Min, max, mean, standard deviation of the distance
#'                  and the correlation coefficient.}
################################################################################################
diff.check <- function( a, x, freq, total = sum( freq ) )
{
    diff <- sqrt( ( a$d( x ) - ggd:::get.d.freq( x, freq, total ) )^2 )
    summary <- c( min( diff ), max( diff ), mean( diff ), sd( diff ), cor( a$d( x ),
                  ggd:::get.d.freq( x, freq, total ) ) )
    names( summary ) <- c( "diff.min", "diff.max", "diff.mean", "diff.sd", "cor" )

    list( diff = diff, summary = summary )
}

################################################################################################
#' Function for visual check of frequency distribution and probability density function
#'
#' @param   a       A GGD object.
#' @param   x       A vector of x-coordinates.
#' @param   freq    A vector of frequencies.
#' @param   total   A total of frequencies.
################################################################################################
plot.freq.and.d <- function( a, x, freq, total = sum( freq ) )
{
    xlim <- c( min( x ), max( x ) )
    ylim <- c( 0, max( ggd:::get.d.freq( x, freq, total ) ) * 1.2 )
    diffs <- diff.check( a, x, freq, total )

    plot( x, ggd:::get.d.freq( x, freq, total ), xlim = xlim, ylim = ylim, ylab = "" )
    par( new = T )
    plot( seq( min( x ), max( x ), 0.01 ), a$d( seq( min( x ), max( x ), 0.01 ) ),
          type = "l", xlim = xlim, ylim = ylim, xlab = "",
          sub = sprintf( "diff.mean : %f    diff.sd : %f    cor : %f",
                         diffs$summary[3], diffs$summary[4], diffs$summary[5] ) )
}

################################
# Tests
################################

#### Basic tests
# normal test
x <- seq( -2, 2, 0.2 )
freq <- ( pnorm( x + 0.1, 0, 0.5 ) - pnorm( x - 0.1, 0, 0.5 ) ) * ( 1000 + sin( x * 10 + 0.5 ) * 50 )
df <- data.frame( x = x, freq = freq )
total <- sum( df$freq )

expect_false( withVisible(
    a$nls.freq( df, total = total, grad = "normal" ) )$visible )
expect_identical( a$mix.type, 0L )
diff.check( a, x, freq, total )
plot.freq.and.d( a, x, freq, total )

result <- a$nls.freq( df, total = total, grad = "normal" )
expect_identical( a, result$obj )
expect_type( result$nls.out, "list" )
expect_s3_class( result$nls.out, "nls" )
expect_equal( result$nls.out$convInfo$isConv, TRUE )
expect_true( is.integer( result$start.level ) )
expect_true( any( result$start.level == 0:3 ) )
expect_s4_class( result$start.obj, "GGD" )
expect_true( is.numeric( result$cor ) )
expect_length( result$cor, 4 )

expect_true( withVisible(
    ggd.nls.freq( df, total = total, grad = "no" ) )$visible )

b <- ggd.nls.freq( df, total = total, grad = "no" )$obj
expect_equal_ggd( a, b )
rm( b )

# Error cases
a <- GGD$new()
expect_error( ggd.nls.freq( x, freq, grad = "normal" ), "data must be a data frame" )
expect_error( a$nls.freq( x, freq, grad = "normal" ), "data must be a data frame" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( data.frame( y = x, freq = freq ) ),
              "Column 'x' is undefined" )
expect_error( a$nls.freq( data.frame( y = x, freq = freq ) ),
              "Column 'x' is undefined" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( data.frame( x = x, p = freq ) ),
              "Column 'freq' is undefined" )
expect_error( a$nls.freq( data.frame( x = x, p = freq ) ),
              "Column 'freq' is undefined" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, freq = NULL ),
              "Argument freq must be a column name or an index number" )
expect_error( ggd.nls.freq( df, freq = integer() ),
              "Argument freq must be a column name or an index number" )
expect_error( ggd.nls.freq( df, freq = NA_integer_ ),
              "Argument freq must be a column name or an index number" )
expect_error( a$nls.freq( df, freq = a ),
              "Argument freq must be a column name or an index number" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, freq = "freq.2" ),
              "Column 'freq.2' is undefined" )
expect_error( a$nls.freq( df, freq = "freq.2" ),
              "Column 'freq.2' is undefined" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, freq = 0 ),
              "Illegal column number for freq" )
expect_error( a$nls.freq( df, freq = 3 ),
              "Illegal column number for freq" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, x = NULL ),
              "Argument x must be a column name or an index number" )
expect_error( ggd.nls.freq( df, x = integer() ),
              "Argument x must be a column name or an index number" )
expect_error( ggd.nls.freq( df, x = NA_integer_ ),
              "Argument x must be a column name or an index number" )
expect_error( a$nls.freq( df, x = a ),
              "Argument x must be a column name or an index number" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, x = "x.2" ),
              "Column 'x.2' is undefined" )
expect_error( a$nls.freq( df, x = "x.2" ),
              "Column 'x.2' is undefined" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, x = 0 ),
              "Illegal column number for x" )
expect_error( a$nls.freq( df, x = 3 ),
              "Illegal column number for x" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( data.frame( x = x[1:2], freq = freq[1:2] ) ),
              "Too few rows for data" )
expect_error( a$nls.freq( data.frame( x = x[1:2], freq = freq[1:2] ) ),
              "Too few rows for data" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( data.frame( x = c( x[1:2], NA, x[3] ) , freq = c( freq[1:3], NaN ) ) ),
              "Too few rows for data" )
expect_error( a$nls.freq( data.frame( x = c( x[1:2], NA, x[3] ) , freq = c( freq[1:3], NaN ) ) ),
              "Too few rows for data" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( data.frame( x = -x, freq = -freq ), grad = "normal" ),
              "Rows of x must have been sorted" )
expect_error( a$nls.freq( data.frame( x = -x, freq = -freq ), grad = "normal" ),
              "Rows of x must have been sorted" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( data.frame( x = c( x[1:10], x[10:21] ), freq = c( freq[1:10], freq[10:21] ) ) ),
              "Rows of x must have been sorted" )
expect_error( a$nls.freq( data.frame( x = c( x[1:10], x[10:21] ), freq = c( freq[1:10], freq[10:21] ) ) ),
              "Rows of x must have been sorted" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( data.frame( x.err = -x, freq = -freq ), x = "x.err" ),
              "Rows of x.err must have been sorted" )
expect_error( a$nls.freq( data.frame( x.err = -x, freq = -freq ), x = "x.err" ),
              "Rows of x.err must have been sorted" )
expect_cleared( a ); a <- GGD$new()

expect_error( a$nls.freq( df, total = -sum( freq ), grad = "normal" ),
              "total should be positive finite single value" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, total = 0, grad = "normal" ),
              "total should be positive finite single value" )
expect_error( a$nls.freq( df, total = 0, grad = "normal" ),
              "total should be positive finite single value" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, total = -sum( freq ), grad = "normal" ),
              "total should be positive finite single value" )
expect_error( a$nls.freq( df, total = -sum( freq ), grad = "normal" ),
              "total should be positive finite single value" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, total = NaN, grad = "normal" ),
              "total should be positive finite single value" )
expect_error( a$nls.freq( df, total = NaN, grad = "normal" ),
              "total should be positive finite single value" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, total = Inf, grad = "normal" ),
              "total should be positive finite single value" )
expect_error( a$nls.freq( df, total = Inf, grad = "normal" ),
              "total should be positive finite single value" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, total = numeric(), grad = "normal" ),
              "total should be positive finite single value" )
expect_error( a$nls.freq( df, total = numeric(), grad = "normal" ),
              "total should be positive finite single value" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, total = 1:2, grad = "normal" ),
              "total should be positive finite single value" )
expect_error( a$nls.freq( df, total = 1:2, grad = "normal" ),
              "total should be positive finite single value" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, total = a, grad = "normal" ),
              "total should be positive finite single value" )
expect_error( a$nls.freq( df, total = a, grad = "normal" ),
              "total should be positive finite single value" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, grad = "zzz" ),
              "should be one of" )
expect_error( a$nls.freq( df, grad = "yyy" ),
              "should be one of" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, grad = c( "h", "v2" ) ),
              "1" )
expect_error( a$nls.freq( df, grad = c( "v3", "hv" ) ),
              "1" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, grad = NA ) )
expect_error( a$nls.freq( df, grad = NA ) )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, grad = character() ) )
expect_error( a$nls.freq( df, grad = character() ) )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, grad = a ) )
expect_error( a$nls.freq( df, grad = a ) )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, grad = "normal", start.level = -1 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( a$nls.freq( df, grad = "normal", start.level = -1 ),
              "start.level should be single integer in 0:3 or 100" )
expect_cleared( a ); a <- GGD$new()
expect_error( a$nls.freq( df, grad = "normal", start.level = 4 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq( df, grad = "normal", start.level = 4 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( a$nls.freq( df, grad = "normal", start.level = 4 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq( df, grad = "normal", start.level = 99 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( a$nls.freq( df, grad = "normal", start.level = 99 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq( df, grad = "normal", start.level = 101 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( a$nls.freq( df, grad = "normal", start.level = 101 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq( df, grad = "normal", start.level = numeric() ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( a$nls.freq( df, grad = "normal", start.level = numeric() ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq( df, grad = "normal", start.level = 1:2 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( a$nls.freq( df, grad = "normal", start.level = 1:2 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq( df, grad = "normal", start.level = NULL ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( a$nls.freq( df, grad = "normal", start.level = NULL ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq( df, grad = "normal", start.level = a ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( a$nls.freq( df, grad = "normal", start.level = a ),
              "start.level should be single integer in 0:3 or 100" )
expect_cleared( a ); a <- GGD$new()

expect_error( ggd.nls.freq( df, mix.type = -1 ),    "mix.type should be single integer from 0 to 5" )
expect_error( a$nls.freq( df, this.mix.type = -1 ), "mix.type should be single integer from 0 to 5" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.nls.freq( df, mix.type = 6 ),     "mix.type should be single integer from 0 to 5" )
expect_error( a$nls.freq( df, this.mix.type = 6 ),  "mix.type should be single integer from 0 to 5" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.nls.freq( df, mix.type = NA ),    "mix.type should be single integer from 0 to 5" )
expect_error( a$nls.freq( df, this.mix.type = NA ), "mix.type should be single integer from 0 to 5" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.nls.freq( df, mix.type = numeric() ),     "mix.type should be single integer from 0 to 5" )
expect_error( a$nls.freq( df, this.mix.type = numeric() ),  "mix.type should be single integer from 0 to 5" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.nls.freq( df, mix.type = 1:2 ),           "mix.type should be single integer from 0 to 5" )
expect_error( a$nls.freq( df, this.mix.type = 1:2 ),  "mix.type should be single integer from 0 to 5" )
expect_cleared( a ); a <- GGD$new()
expect_error( a$nls.freq( df, start = list( mean = -10, sqrt.sd = 10 ), grad = "normal" ), "nls has failed" )
expect_cleared( a ); a <- GGD$new()

# normal test
expect_output( a$nls.freq( df, grad = "normal", this.mix.type = 1, trace = TRUE ), "par =" )

#### Normal distributions
freq <- floor( dnorm( x, 0, 0.5 ) * 1000 )
freq.2 <- floor( dnorm( x, 0, 0.3 ) * 1000 )
df <- data.frame(
        x = x,
        x.2 = x + 10,
        freq = freq,
        freq.2 = freq.2 )

# Error case (start values will be almost equal to freq)
expect_error(
    a$nls.freq( df, grad = "normal" ),
    paste0( "All of nls functions have failed.*",
            "level: 0.*nls has failed.*",
            "level: 1.*nls has failed.*",
            "level: 2.*nls has failed.*",
            "level: 3.*nls has failed" ) )
expect_cleared( a )

# normal test
a$nls.freq( df, grad = "normal", start.level = 1, not.use.nls = TRUE )
expect_equal( a$kind, "Normal Distribution" )
expect_identical( a$mix.type, 0L )
expect_equal( a$mean, 0, tolerance = 5e-7 )
expect_equal( a$sd, 0.5, tolerance = 0.05 )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test
a$nls.freq( df, x = "x.2", freq = "freq.2", start.level = 1, grad = "normal", not.use.nls = TRUE )
expect_equal( a$kind, "Normal Distribution" )
expect_identical( a$mix.type, 0L )
expect_equal( a$mean, 10, tolerance = 5e-7 )
expect_equal( a$sd, 0.3, tolerance = 0.05 )
plot.freq.and.d( a, x + 10, freq.2 )
diff.check( a, x + 10, freq.2 )

# normal test
freq <- dnorm( x, 0, 0.5 ) * ( 1000 + sin( x + 0.05 ) * 10 )
total <- sum( freq ) * 1.1
df <- data.frame( x = x, freq = freq )
a$nls.freq( df, total = total, grad = "normal", this.mix.type = 3 )
expect_identical( a$mix.type, 0L )
expect_equal( a$kind, "Normal Distribution" )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test
a$trace.q(
    data.frame( p = c( 0.3, 0.5, 0.7 ), x = c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) ) ),
    this.mix.type = 2 )
a$nls.freq( df, grad = "normal", this.kind = a$kind )
expect_identical( a$mix.type, 0L )
expect_equal( a$kind, "Normal Distribution" )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test
a$trace.q(
    data.frame( p = c( 0.1, 0.4, 0.5 ), x = c( qnorm( 0.1, 0, 1.2 ), qnorm( 0.4, 0, 0.9 ), 0 ) ),
    this.mix.type = 3 )
a$nls.freq( df, grad = "normal" )
expect_identical( a$mix.type, 0L )
expect_equal( a$kind, "Normal Distribution" )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test
a$clear()
a$nls.freq( df, eq.mean = TRUE, eq.sd = TRUE )
expect_identical( a$mix.type, 0L )
expect_equal( a$kind, "Normal Distribution" )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )


#### Sharp-center data
freq <- c(  15.164,  22.923,  25.134,  27.631,
            37.239,  40.464,  47.126,  79.469,
           109.966, 118.241, 111.333,  78.674,
            46.921,  41.026,  36.976,  27.403,
            25.493,  22.838,  14.992,  11.468,
             9.174 )
df <- data.frame( x = x, freq = freq )

## ggd.nls.freq

# normal test
a$nls.freq( df, grad = "normal" )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test
a$nls.freq( df, this.mix.type = 1 )
expect_identical( a$mix.type, 1L )
expect_true( a$kind.index > 1 )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test
a$nls.freq( df, grad = "v2" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2(), TRUE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test (rows containing NaN and NA are skipped, "v" is an alias of "v2")
df.2 <- data.frame(
            freq = c( freq[1],
                      777.777, 999.999,
                      freq[2:8],
                      NA,
                      freq[9:20],
                      NaN, NA,
                      freq[21] ),
            x = c( x[1],
                   NaN, NA,
                   x[2:8],
                   ( x[8] + x[9] ) / 2,
                   x[9:20],
                   ( x[20] + x[21] ) / 2, NA,
                   x[21] ) )
b <- ggd.nls.freq( df.2, grad = "v" )$obj
expect_equal_ggd( a, b )

df.3 <- data.frame(
            x.1 = rep( NA, nrow( df.2 ) ),
            x.2 = df.2$x,
            freq.1 = rep( 0, nrow( df.2 ) ),
            freq.2 = df.2$freq )
b <- ggd.nls.freq( df.3, x = "x.2", freq = "freq.2", grad = "v2" )$obj
expect_equal_ggd( a, b )

rm( b, df.2, df.3 )

# normal test (previous mix.type is retained)
a$nls.freq( df, eq.sd = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2(), TRUE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3( TRUE ), FALSE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.eq.sigma(), TRUE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test
a <- ggd.nls.freq( df )$obj
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test
a$clear()
a$nls.freq( df, this.kind = "Mean-Equaled Sigma-Differed Horizontal Gradational Distribution" )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.h(), TRUE )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test
a <- ggd.nls.freq( df, kind = "Mean-Differed Sigma-Equaled Vertical Gradational Distribution" )$obj
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2(), TRUE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3( TRUE ), FALSE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test
result <- ggd.nls.freq( df, kind = a )
expect_true( any( result$start.level == 1:3 ) ) # Level 0 can be excluded because the initial conditions are too poor.
expect_type( result$nls.out, "list" )
expect_s3_class( result$nls.out, "nls" )
expect_equal( result$nls.out$convInfo$isConv, TRUE )
expect_equal( result$obj$mix.type, 3 )
expect_equal( result$obj$is.v2(), TRUE )
expect_equal( result$obj$is.v3(), TRUE )
expect_equal( result$obj$is.v2( TRUE ), TRUE )
expect_equal( result$obj$is.v3( TRUE ), FALSE )
expect_equal( result$obj$is.eq.mean(), FALSE )
expect_equal( result$obj$is.eq.sd(), TRUE )
plot.freq.and.d( result$obj, x, freq )
diff.check( result$obj, x, freq )

# normal test
a <- GGD$new()
a$nls.freq( df, this.kind = a )
expect_identical( a$mix.type, 0L )
expect_equal( a$is.normal(), TRUE )
expect_equal( nrow( a$cmp ), 1 )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# normal test (total is less than sum( freq ); it is an illegal case as statistic, but works)
a$nls.freq( df, grad = "v2", total = sum( freq ) * 0.75 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2(), TRUE )
expect_equal( a$is.v3(), TRUE )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3( TRUE ), FALSE )
plot.freq.and.d( a, x, freq, total = sum( freq ) * 0.75 )
diff.check( a, x, freq, total = sum( freq ) * 0.75 )

# normal test ("v3" after "v2"; same mix.type but different nrow( cmp ))
a$nls.freq( df, grad = "v3" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# Error case
expect_error( ggd.nls.freq( df, kind = NA ),
              "kind should be valid single value or a GGD object" )
expect_error( a$nls.freq( df, this.kind = NA ),
              "kind should be valid single value or a GGD object" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.nls.freq( df, kind = character() ),
              "kind should be valid single value or a GGD object" )
expect_error( a$nls.freq( df, this.kind = character() ),
              "kind should be valid single value or a GGD object" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.nls.freq( df, kind = 1:2 ),
              "kind should be valid single value or a GGD object" )
expect_error( a$nls.freq( df, this.kind = 1:2 ),
              "kind should be valid single value or a GGD object" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.nls.freq( df, kind = c( "Normal", "Horizontal" ) ),
              "kind should be valid single value or a GGD object" )
expect_error( a$nls.freq( df, this.kind = c( "Normal", "Horizontal" ) ),
              "kind should be valid single value or a GGD object" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.nls.freq( df, kind = c( NA, "Horizontal" ) ),
              "kind should be valid single value or a GGD object" )
expect_error( a$nls.freq( df, this.kind = c( NA, "Horizontal" ) ),
              "kind should be valid single value or a GGD object" )
expect_cleared( a ); a <- GGD$new()

# Error case
expect_error( a$nls.freq( df, this.mix.type = NA ),
              "mix.type should be single integer from 0 to 5" )
expect_cleared( a )
expect_equal( a$is.eq.sd(), FALSE )

# Error case
a <- GGD$new()
expect_error( a$nls.freq( df, this.kind = -1 ),
              "kind for index -1 is undefined" )
expect_cleared( a )

# Error case
a <- GGD$new()
expect_error( a$nls.freq( df, this.kind = "Random distribution" ),
              "'Random distribution' does not match any character strings of kinds" )
expect_cleared( a )


## ggd.nls.freq.all

# normal test
expect_identical( ggd.kind( 1:16 ), ggd:::kinds[1:16] )
expect_identical( ggd.kind.index( 1:16 ), 1:16 )
expect_identical( ggd.kind.index( c( -1, 0, 1, 2, NA, -Inf, Inf,
                                     NaN, 3, 4, 5, 15, 16, 18 ) ),
                  c( NA_integer_, NA_integer_, 1L, 2L, NA_integer_, NA_integer_, NA_integer_,
                     NA_integer_, 3L, 4L, 5L, 15L, 16L, NA_integer_ ) )
expect_identical( ggd.kind.index( c( NA, "Normal Distribution", "Null Distribution" ) ),
                  c( NA_integer_, 1L, NA_integer_ ) )

# Error cases
expect_error( ggd.nls.freq.all( x, freq ), "data must be a data frame" )
expect_error( ggd.nls.freq.all( data.frame( y = x, freq = freq ) ),
              "Column 'x' is undefined" )
expect_error( ggd.nls.freq.all( data.frame( x = x, p = freq ) ),
              "Column 'freq' is undefined" )
expect_error( ggd.nls.freq.all( df, x = "x.2" ),
              "Column 'x.2' is undefined" )
expect_error( ggd.nls.freq.all( data.frame( x = x[1:2], freq = freq[1:2] ), grad = "normal" ),
              "Too few rows for data" )
expect_error( ggd.nls.freq.all( data.frame( x = x[1:2], freq = freq[1:2] ) ),
              "Too few rows for data" )
expect_error( ggd.nls.freq.all( data.frame( x = c( x[1:2], NA, x[3] ) , freq = c( freq[1:3], NaN ) ) ),
              "Too few rows for data" )
expect_error( ggd.nls.freq.all( data.frame( x = -x, freq = -freq ), grad = "normal" ),
              "Rows of x must have been sorted" )
expect_error( ggd.nls.freq.all( data.frame( x = c( x[1:10], x[10:21] ), freq = c( freq[1:10], freq[10:21] ) ) ),
              "Rows of x must have been sorted" )
expect_error( ggd.nls.freq.all( data.frame( x.err = -x, freq = -freq ), x = "x.err" ),
              "Rows of x.err must have been sorted" )
expect_error( ggd.nls.freq.all( df, total = -sum( freq ), grad = "normal" ),
              "total should be positive finite single value" )
expect_error( ggd.nls.freq.all( df, total = 0, grad = "normal" ),
              "total should be positive finite single value" )
expect_error( ggd.nls.freq.all( df, total = -sum( freq ), grad = "normal" ),
              "total should be positive finite single value" )
expect_error( ggd.nls.freq.all( df, total = NaN, grad = "normal" ),
              "total should be positive finite single value" )
expect_error( ggd.nls.freq.all( df, total = Inf, grad = "normal" ),
              "total should be positive finite single value" )
expect_error( ggd.nls.freq.all( df, total = numeric(), grad = "normal" ),
              "total should be positive finite single value" )
expect_error( ggd.nls.freq.all( df, total = 1:2, grad = "normal" ),
              "total should be positive finite single value" )
expect_error( ggd.nls.freq.all( df, total = a, grad = "normal" ),
              "total should be positive finite single value" )
expect_error( ggd.nls.freq.all( df, grad = "normal", start.level = -1 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq.all( df, grad = "normal", start.level = 4 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq.all( df, grad = "normal", start.level = 99 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq.all( df, grad = "normal", start.level = 101 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq.all( df, grad = "normal", start.level = numeric() ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq.all( df, grad = "normal", start.level = 1:2 ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq.all( df, grad = "normal", start.level = NULL ),
              "start.level should be single integer in 0:3 or 100" )
expect_error( ggd.nls.freq.all( df, grad = "normal", start.level = a ),
              "start.level should be single integer in 0:3 or 100" )

# normal test
expect_warning( expect_warning(
expect_warning( expect_warning(
expect_warning( expect_warning(
    ggds <- ggd.nls.freq.all( df, start.level = 1 ),
    "Warning for kind = 2 :" ) ),
    "Warning for kind = 5 :" ) ),
    "Warning for kind = 14 :" ) )
expect_equal( ggds$best$kind.index, 16 )
expect_true( ggds$best.cor > 0.996 )
expect_true( ggd.cor.vs.freq( ggds$best, df$x, df$freq ) == ggds$best.cor )
expect_equal( vapply( 1:length( ggds$obj ),
                        function( i )
                        {
                            expect_s4_class( ggds$obj[[i]], "GGD" )
                            expect_identical( ggds$obj[[i]]$kind, ggd:::kinds[i] )
                            expect_identical( ggds$obj[[i]], ggds$detail[[i]]$obj )
                            i
                        }, 0 ), 1:16 )

expect_warning( expect_warning(
expect_warning( expect_warning(
expect_warning( expect_warning(
expect_true( withVisible(
                ggd.nls.freq.all( df, start.level = 1 ) )$visible ) ) ) ) ) ) )

# check the result with different total
expect_warning( expect_warning(
expect_warning( expect_warning(
expect_warning( expect_warning(
    ggds2 <- ggd.nls.freq.all( df, start.level = 1, total = sum( freq ) * 1.1 ),
    "Warning for kind = 2 :" ) ),
    "Warning for kind = 5 :" ) ),
    "Warning for kind = 16 :" ) )
expect_true( all( ifelse( is.na( ggds$cor ), TRUE, ggds$cor != ggds2$cor ) ) )
expect_true( ggds$best.cor != ggds2$best.cor )
expect_true( ggd.cor.vs.freq( ggds2$best, df$x, df$freq, total = sum( freq ) * 1.1 ) ==
             ggds2$best.cor )
rm( ggds2 )

plot.freq.and.d( ggds$best, x, freq, sum( freq ) * 1.1 )

# normal test
expect_equal( ggd.kind( ggds$obj ), ggd:::kinds[1:16] )
expect_equal( ggd.kind.index( ggds$obj ), 1:16 )
expect_equal( ggd.kind( ggds$obj[[1]] ), ggds$obj[[1]]$kind )
expect_equal( ggd.kind.index( ggds$obj[[1]] ), ggds$obj[[1]]$kind.index )

# normal test of ggd.start.template
expect_true( unname( diff.check( ggds$obj[[14]], x, freq )$summary["cor"] <
                     diff.check( ggds$obj[[8]], x, freq )$summary["cor"] ) )

expect_equal( ggds$obj[[8]]$kind, "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution" )
expect_equal( ggds$obj[[14]]$kind, "Mean-Differed Sigma-Equaled Horizontal-Vertical Gradational Distribution" )

start.list <- ggd.start.template( ggds$obj[[14]]$kind )
expect_equal( start.list, list( mean.1.1 = 0, mean.1.2 = 0, mean.2.1 = 0, mean.2.2 = 0, sqrt.sd = 1 ) )

start.list$mean.1.1 <- ggds$obj[[8]]$cmp$mean[1]
start.list$mean.1.2 <- ggds$obj[[8]]$cmp$mean[2]
start.list$mean.2.1 <- ggds$obj[[8]]$cmp$mean[1]
start.list$mean.2.2 <- ggds$obj[[8]]$cmp$mean[2]
start.list$sqrt.sd  <- sqrt( ggds$obj[[11]]$cmp$sd[1] )

result <- a$nls.freq( df, start = start.list, this.kind = ggds$obj[[14]] )
expect_identical( result$start.level, NA_integer_ )
expect_equal( a$kind, ggds$obj[[14]]$kind )
plot.freq.and.d( a, x, freq )
expect_true( unname( diff.check( a, x, freq )$summary[5]
                     > diff.check( ggds$obj[[11]], x, freq )$summary[5] ) )

# then retrying ggd.nls.freq.all
lists <- ggd.init.start()
expect_equal( length( lists ), 16 )
expect_true( all( vapply( 1:16, function( i ) { is.null( lists[[i]] ) }, TRUE ) ) )
lists[[5]] <- list( mean.1 = 0.203, mean.2 = -0.305, sqrt.sd = sqrt( 1.526 ) )

lists[[14]] <- ggd.start.template( ggds$obj[[14]]$kind )
lists[[14]]$mean.1.1 <- ggds$obj[[8]]$cmp$mean[1]
lists[[14]]$mean.1.2 <- ggds$obj[[8]]$cmp$mean[2]
lists[[14]]$mean.2.1 <- ggds$obj[[8]]$cmp$mean[1]
lists[[14]]$mean.2.2 <- ggds$obj[[8]]$cmp$mean[2]
lists[[14]]$sqrt.sd  <- sqrt( ggds$obj[[11]]$cmp$sd[1] )

# normal test
expect_warning( expect_warning(
expect_warning( expect_warning(
    ggds <- ggd.nls.freq.all( df, start.level = 1, start = lists ),
    "Warning for kind = 2 :" ) ),
    "Warning for kind = 5 :" ) )
expect_true( ggds$cor[8] < ggds$cor[14] )
expect_identical( ggds$detail[[5]]$start.level, NA_integer_ )
expect_identical( ggds$detail[[14]]$start.level, NA_integer_ )
expect_true( all( vapply( ( 1:16 )[-14][-5],
                          function( i ) identical( ggds$detail[[i]]$start.level, 1L ), TRUE ) ) )
expect_identical( ggds$best$kind.index, 16L )
expect_true( ggds$best.cor > 0.9964 )
# 'lists' will be used later at not.use.nls (ggd.nls.freq.all) test.

#### Dent-center data
freq <- c(  11.378, 19.732, 24.927, 31.538, 47.720,
            53.611, 55.346, 71.002, 70.616, 60.070,
            66.877, 70.719, 61.554, 67.750, 65.952,
            48.530, 43.588, 37.500, 23.641, 17.362,
            13.318 )
df <- data.frame( x = x, freq = freq )

## ggd.nls.freq

## not.use.nls
# start.level = 0
result <- a$nls.freq( df, start.level = 0, not.use.nls = TRUE, grad = "normal" )
expect_identical( result$obj, a )       # The output must be identical to the object a.
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 0L )
expect_equal_ggd( result$start.obj, a ) # The start condition equals to the result, but not necessarily identical.
expect_equal( a$kind, "Normal Distribution" )

data.mean <- sum( x * freq ) / sum( freq )
data.sd <- sqrt( sum( ( data.mean - x )^2 * freq ) / sum( freq ) )
expect_equal( a$mean, data.mean )
expect_equal( a$sd, data.sd )

plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# start.level = 1
result <- a$nls.freq( df, start.level = 1, not.use.nls = TRUE, this.mix.type = 1 )
expect_identical( result$obj, a )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 1L )
expect_equal_ggd( result$start.obj, a )
expect_identical( a$mix.type, 1L )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# start.level = 2
result <- a$nls.freq( df, start.level = 2, not.use.nls = TRUE, this.mix.type = 1, eq.sd = TRUE )
expect_identical( result$obj, a )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 2L )
expect_equal_ggd( result$start.obj, a )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.sd(), TRUE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# start.level = 3
result <- a$nls.freq( df, start.level = 3, not.use.nls = TRUE, this.mix.type = 2 )
expect_identical( result$obj, a )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 3L )
expect_equal_ggd( result$start.obj, a )
expect_identical( a$mix.type, 2L )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# eq.sd affects the starting value for start.level = 1.
result <- ggd.nls.freq( df, start.level = 1, not.use.nls = TRUE, eq.sd = TRUE )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 1L )
expect_equal_ggd( result$start.obj, result$obj )
expect_equal( result$obj$mix.type, 2 )
expect_equal( result$obj$is.eq.sd(), TRUE )
plot.freq.and.d( result$obj, x, freq )
diff.check( result$obj, x, freq )

# eq.mean does not affect the starting value for start.level = 2, mix.type = 2.
result <- ggd.nls.freq( df, start.level = 2, not.use.nls = TRUE, mix.type = 2, eq.mean = FALSE )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 2L )
expect_equal_ggd( result$start.obj, result$obj )
expect_equal( result$obj$mix.type, 2 )
expect_equal( result$obj$is.eq.mean(), TRUE )
plot.freq.and.d( result$obj, x, freq )
diff.check( result$obj, x, freq )

# both eq.mean and eq.sd affects the starting value for start.level = 3.
result <- ggd.nls.freq( df, start.level = 3, not.use.nls = TRUE, grad = "v2" )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 3L )
expect_equal_ggd( result$start.obj, result$obj )
expect_equal( result$obj$mix.type, 3 )
expect_equal( result$obj$is.v2(), TRUE )
expect_equal( result$obj$is.eq.mean(), FALSE )
expect_equal( result$obj$is.eq.sd(), FALSE )
plot.freq.and.d( result$obj, x, freq )
diff.check( result$obj, x, freq )

# start.level = 0
result <- ggd.nls.freq( df, total = sum( freq ) * 1.5, start.level = 0, not.use.nls = TRUE, mix.type = 3 )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 0L )
expect_equal_ggd( result$start.obj, result$obj )
expect_equal( result$obj$mix.type, 3 )
expect_equal( result$obj$is.eq.mean(), TRUE )
expect_equal( result$obj$is.eq.sd(), TRUE )

data.mean <- sum( x * freq ) / ( sum( freq ) * 1.5 )
data.sd <- sqrt( sum( ( data.mean - x )^2 * freq ) / ( sum( freq ) * 1.5 ) )
expect_equal( result$obj$mean, data.mean )
expect_equal( result$obj$sd, data.sd )

plot.freq.and.d( result$obj, x, freq )
diff.check( result$obj, x, freq )

# start.level = 1, mix.type = 3
result <- a$nls.freq( df, start.level = 1, not.use.nls = TRUE, this.mix.type = 3, eq.mean = TRUE )
expect_identical( result$obj, a )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 1L )
expect_equal_ggd( result$start.obj, a )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

result <- a$nls.freq( df, start.level = 1, not.use.nls = TRUE, this.mix.type = 3, eq.sd = TRUE )
expect_identical( result$obj, a )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 1L )
expect_equal_ggd( result$start.obj, a )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

result <- a$nls.freq( df, start.level = 1, not.use.nls = TRUE, this.mix.type = 3 )
expect_identical( result$obj, a )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 1L )
expect_equal_ggd( result$start.obj, a )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# mix.type = 4
result <- a$nls.freq( df, start.level = 2, not.use.nls = TRUE, this.mix.type = 4, eq.sd = TRUE )
expect_identical( result$obj, a )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 2L )
expect_equal_ggd( result$start.obj, a )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

result <- a$nls.freq( df, start.level = 1, not.use.nls = TRUE, this.mix.type = 4 )
expect_identical( result$obj, a )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, 1L )
expect_equal_ggd( result$start.obj, a )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

# start will be set to result directly and start.level of the result will be NA.
result <- a$nls.freq( df, start.level = 1,
                      start = list( mean.1 = 1.25, mean.2 = 2,
                                    sqrt.sd.1 = 1.2, sqrt.sd.2 = 1.5 ),
                      not.use.nls = TRUE, this.mix.type = 2 )
expect_identical( result$obj, a )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, NA_integer_ ) # start.level is ignored if start is indicated.
expect_equal_ggd( result$start.obj, a )
expect_identical( a$mix.type, 2L )
expect_true( all( a$cmp == data.frame( mean = c( 1.25, 2 ), sd = c( 1.44, 2.25 ) ) ) )

result <- a$nls.freq( df,
                      start = list( mean.1 = -0.5, mean.2 = 1,
                                    sqrt.sd.1 = 1.5, sqrt.sd.2 = 1.2 ),
                      not.use.nls = TRUE, grad = "v" )
expect_identical( result$obj, a )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, NA_integer_ )
expect_equal_ggd( result$start.obj, a )
expect_identical( a$mix.type, 3L )
expect_true( all( a$cmp == data.frame( mean = c( -0.5, 1 ), sd = c( 2.25, 1.44 ) ) ) )

result <- ggd.nls.freq( df, start.level = 1,
                        start = list( mean.1 = 1.25, mean.2 = 2,
                                      sqrt.sd.1 = 1.2, sqrt.sd.2 = 1.5 ),
                        not.use.nls = TRUE, mix.type = 1 )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, NA_integer_ ) # start.level is ignored if start is indicated.
expect_equal_ggd( result$start.obj, result$obj )
expect_equal( result$obj$mix.type, 1 )
expect_true( all( result$obj$cmp == data.frame( mean = c( 1.25, 2 ), sd = c( 1.44, 2.25 ) ) ) )

result <- ggd.nls.freq( df,
                        start = list( mean.1 = -0.5, mean.2 = 1,
                                      sqrt.sd.1 = 1.5, sqrt.sd.2 = 1.2 ),
                        not.use.nls = TRUE, grad = "h" )
expect_equal( result$nls.out, NULL )
expect_identical( result$start.level, NA_integer_ )
expect_equal_ggd( result$start.obj, result$obj )
expect_equal( result$obj$mix.type, 2 )
expect_true( all( result$obj$cmp == data.frame( mean = c( -0.5, 1 ), sd = c( 2.25, 1.44 ) ) ) )

# not.use.nls = TRUE is ignored where start.level = 100.
expect_warning(
    result <- a$nls.freq( df, start.level = 100, not.use.nls = TRUE, this.mix.type = 2 ),
    "not.use.nls does not work with start.level is default 100" )
expect_identical( result$obj, a )
expect_equal( is.null( result$nls.out ), FALSE )
expect_equal( all( result$start.obj$cmp == a$cmp ), FALSE )
expect_equal( length( result$cor ) >= 4, TRUE )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

expect_warning(
    result <- a$nls.freq( df, not.use.nls = TRUE, this.mix.type = 2, eq.sd = TRUE ),
    "not.use.nls does not work with start.level is default 100" )
expect_identical( result$obj, a )
expect_equal( is.null( result$nls.out ), FALSE )
expect_equal( all( result$start.obj$cmp == a$cmp ), FALSE )
expect_equal( length( result$cor ) >= 4, TRUE )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )

expect_warning(
    result <- ggd.nls.freq( df, start.level = 100, not.use.nls = TRUE, mix.type = 2, eq.sd = TRUE ),
    "not.use.nls does not work with start.level is default 100" )
expect_equal( length( result$cor ) >= 4, TRUE )
expect_equal( result$obj$mix.type, 2 )
expect_equal( result$obj$is.eq.mean(), FALSE )
expect_equal( result$obj$is.eq.sd(), TRUE )
plot.freq.and.d( result$obj, x, freq )
diff.check( result$obj, x, freq )

expect_warning(
    result <- ggd.nls.freq( df, start.level = 100, not.use.nls = TRUE, mix.type = 2 ),
    "not.use.nls does not work with start.level is default 100" )
expect_equal( length( result$cor ) >= 4, TRUE )
expect_equal( result$obj$mix.type, 2 )
expect_equal( result$obj$is.eq.mean(), FALSE )
expect_equal( result$obj$is.eq.sd(), FALSE )
plot.freq.and.d( result$obj, x, freq )
diff.check( result$obj, x, freq )

## not.use.nls (ggd.nls.freq.all)
ggds <- ggd.nls.freq.all( df, start.level = 0, not.use.nls = TRUE )
data.mean <- sum( x * freq ) / sum( freq )
data.sd <- sqrt( sum( ( data.mean - x )^2 * freq ) / sum( freq ) )
expect_true( all( vapply( 1:16, function( i )
    {
        expect_equal( ggds$obj[[i]]$kind, "Normal Distribution" )
        expect_equal( ggds$obj[[i]]$mean, data.mean )
        expect_equal( ggds$obj[[i]]$sd, data.sd )
        TRUE
    }, TRUE ) ) )

# using total
ggds <- ggd.nls.freq.all( df, total = sum( freq ) * 1.25, start.level = 0, not.use.nls = TRUE )
data.mean <- sum( x * freq ) / ( sum( freq ) * 1.25 )
data.sd <- sqrt( sum( ( data.mean - x )^2 * freq ) / ( sum( freq ) * 1.25 ) )
expect_true( all( vapply( 1:16, function( i )
    {
        expect_equal( ggds$obj[[i]]$kind, "Normal Distribution" )
        expect_equal( ggds$obj[[i]]$mean, data.mean )
        expect_equal( ggds$obj[[i]]$sd, data.sd )
        expect_equal_ggd( ggds$detail[[i]]$start.obj, ggds$obj[[i]] )

        identical( ggds$detail[[i]]$obj, ggds$obj[[i]] ) &&
        is.null( ggds$detail[[i]]$nls.out ) &&
        identical( ggds$detail[[i]]$start.level, 0L ) &&
        is.null( ggds$detail[[i]]$errors ) &&
        is.null( ggds$detail[[i]]$warnings )
    }, TRUE ) ) )

# using lists
expect_type( lists[[5]], "list" )
expect_type( lists[[14]], "list" )
ggds <- ggd.nls.freq.all( df, start.level = 0, not.use.nls = TRUE, start = lists )
data.mean <- sum( x * freq ) / sum( freq )
data.sd <- sqrt( sum( ( data.mean - x )^2 * freq ) / sum( freq ) )
expect_true( all( vapply( 1:16, function( i )
    {
        if ( any( i == c( 5, 14 ) ) )
        {
            expect_identical( ggds$detail[[i]]$start.level, NA_integer_ )
        }
        else
        {
            expect_equal( ggds$obj[[i]]$kind, "Normal Distribution" )
            expect_equal( ggds$obj[[i]]$mean, data.mean )
            expect_equal( ggds$obj[[i]]$sd, data.sd )
            expect_identical( ggds$detail[[i]]$start.level, 0L )
        }
        expect_equal_ggd( ggds$detail[[i]]$start.obj, ggds$obj[[i]] )

        identical( ggds$detail[[i]]$obj, ggds$obj[[i]] ) &&
        is.null( ggds$detail[[i]]$nls.out ) &&
        is.null( ggds$detail[[i]]$errors ) &&
        is.null( ggds$detail[[i]]$warnings )
    }, TRUE ) ) )

expect_true( all( ggds$obj[[5]]$cmp == data.frame( mean = c( lists[[5]]$mean.1, lists[[5]]$mean.2 ),
                                                     sd = rep( lists[[5]]$sqrt.sd^2, 2 ) ) ) )
expect_true( all( ggds$obj[[14]]$cmp == data.frame( mean = c( lists[[14]]$mean.1.1, lists[[14]]$mean.1.2,
                                                              lists[[14]]$mean.2.1, lists[[14]]$mean.2.2 ),
                                                     sd = rep( lists[[14]]$sqrt.sd^2, 4 ) ) ) )

# start.level = 1
ggds <- ggd.nls.freq.all( df, start.level = 1, not.use.nls = TRUE )
expect_true( all( vapply( 1:16, function( i )
    {
        expect_equal_ggd( ggds$detail[[i]]$start.obj, ggds$obj[[i]] )

        identical( ggds$obj[[i]]$kind.index,
                   c( 1L, 2L, 3L, 3L, 5L, 6L, 6L, 8L, 9L, 9L, 11L, 12L, 12L, 14L, 15L, 15L )[i] ) &&
        identical( ggds$detail[[i]]$obj, ggds$obj[[i]] ) &&
        is.null( ggds$detail[[i]]$nls.out ) &&
        identical( ggds$detail[[i]]$start.level, 1L ) &&
        is.null( ggds$detail[[i]]$errors ) &&
        is.null( ggds$detail[[i]]$warnings )
    }, TRUE ) ) )

# start.level = 2
ggds <- ggd.nls.freq.all( df, start.level = 2, not.use.nls = TRUE )
expect_true( all( vapply( 1:16, function( i )
    {
        expect_equal_ggd( ggds$detail[[i]]$start.obj, ggds$obj[[i]] )

        identical( ggds$obj[[i]]$kind.index,
                   c( 1L, 2L, 3L, 3L, 5L, 6L, 6L, 8L, 9L, 9L, 11L, 12L, 12L, 14L, 15L, 15L )[i] ) &&
        identical( ggds$detail[[i]]$obj, ggds$obj[[i]] ) &&
        is.null( ggds$detail[[i]]$nls.out ) &&
        identical( ggds$detail[[i]]$start.level, 2L ) &&
        is.null( ggds$detail[[i]]$errors ) &&
        is.null( ggds$detail[[i]]$warnings )
    }, TRUE ) ) )

# start.level = 3 (some warnings occur at initial guessing)
expect_warning( expect_warning(
expect_warning( expect_warning(
expect_warning( expect_warning(
expect_warning( expect_warning(
    ggds <- ggd.nls.freq.all( df, start.level = 3, not.use.nls = TRUE ),
    "Warning for kind = 2 :" ),
    "Level 3 initial guessing has failed" ),
    "Warning for kind = 3 :" ),
    "Level 3 initial guessing has failed" ),
    "Warning for kind = 14 :" ),
    "Level 3 initial guessing has failed" ),
    "Warning for kind = 15 :" ),
    "Level 3 initial guessing has failed" )

expect_true( all( vapply( 1:16, function( i )
    {
        expect_equal_ggd( ggds$detail[[i]]$start.obj, ggds$obj[[i]] )
        if ( any( i == c( 2, 3, 14, 15 ) ) )
        {
            expect_identical( ggds$detail[[i]]$start.level, 2L )
        }
        else
        {
            expect_identical( ggds$detail[[i]]$start.level, 3L )
        }

        identical( ggds$obj[[i]]$kind.index, ( 1:16 )[i] ) &&
        identical( ggds$detail[[i]]$obj, ggds$obj[[i]] ) &&
        is.null( ggds$detail[[i]]$nls.out ) &&
        is.null( ggds$detail[[i]]$errors ) &&
        is.null( ggds$detail[[i]]$warnings )
    }, TRUE ) ) )

# Since this df gives too many warnings,
# a test with start.level = 100 will be done later using a different frequency distribution.

rm( lists )
rm( data.mean )
rm( data.sd )

## run nls
# normal test
result <- a$nls.freq( df, this.mix.type = 2, eq.sd = TRUE, not.use.nls = FALSE )
expect_equal( is.null( result$nls.out ), FALSE )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.sd(), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.98 )

# normal test
a$clear()
result <- a$nls.freq( df, not.use.nls = logical() )
expect_equal( is.null( result$nls.out ), FALSE )
expect_identical( a$mix.type, 2L )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.98 )

# normal test
a$set.cmp( data.frame( mean = numeric(), sd = numeric() ) )
a$nls.freq( df )
expect_identical( a$mix.type, 2L )
expect_identical( a$mix.type, 2L )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.98 )

# Warning case
expect_warning(
    result <- a$nls.freq( df, control = list( warnOnly = TRUE ),
                          start.level = 1, this.mix.type = 1, eq.mean = TRUE ), "minFactor" )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), TRUE )
expect_identical( result$obj, a )
expect_identical( result$start.level, 1L )
expect_equal( result$cor, NULL )
expect_equal( result$errors, NULL )
expect_equal( result$warnings, NULL )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.96 )

expect_warning(
    result <- a$nls.freq( df, control = list( warnOnly = TRUE ),
                          this.mix.type = 1, eq.mean = TRUE ), "minFactor" )
expect_equal( result$errors[[1]]$level, 0 )
expect_match( result$errors[[1]]$message, "nls has failed" )
expect_equal( result$warnings[[1]]$level, 1 )
expect_match( result$warnings[[1]]$message, "minFactor" )
expect_equal( result$warnings[[2]]$level, 2 )
expect_match( result$warnings[[2]]$message, "minFactor" )
expect_identical( result$obj, a )
expect_true( result$start.level > 1 )
expect_true( is.na( result$cor["level.0"] ) )
expect_true( is.numeric( result$cor["level.0"] ) )
expect_false( is.integer( result$cor["level.0"] ) )
expect_true( result$cor["level.1"] > 0.95 )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > result$cor["level.1"] )

# sample for easier convergence
freq <- c( 15.6076, 23.7265, 26.7947, 30.9733, 43.8590, 47.3217, 48.2163,
           62.7630, 65.0366, 58.9660, 69.5975, 69.4195, 61.2552, 67.4896,
           65.8579, 48.6660, 43.9381, 35.1997, 22.1480, 16.9332, 13.6742 )
df <- data.frame( x = x, freq = freq )

# normal test
a$nls.freq( df, this.mix.type = 2, eq.mean = TRUE )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )
expect_equal( a$cmp$mean[1], a$cmp$mean[2] )
expect_true( a$cmp$sd[1] != a$cmp$sd[2] )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.97 )

# normal test
a$nls.freq( df, this.mix.type = 2, eq.mean = FALSE )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )
expect_true( a$cmp$mean[1] != a$cmp$mean[2] )
expect_true( a$cmp$sd[1] != a$cmp$sd[2] )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.97 )

# normal test (this.mix.type = 3 where mix.type = 2)
a$nls.freq( df, this.mix.type = 3, eq.mean = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3( TRUE ), FALSE )
expect_true( a$cmp$mean[1] == a$cmp$mean[2] )
expect_true( a$cmp$sd[1] != a$cmp$sd[2] )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.97 )

# normal test ("v" is an alias of "v2")
a$nls.freq( df, grad = "v", eq.mean = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_true( a$cmp$mean[1] == a$cmp$mean[2] )
expect_true( a$cmp$sd[1] != a$cmp$sd[2] )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.97 )

# normal test
a$nls.freq( df, grad = "v2" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$cmp$mean[1] != a$cmp$mean[2] )
expect_true( a$cmp$sd[1] != a$cmp$sd[2] )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.98 )

# normal test (this.mix.type = 3 where current number of components is 2)
a$nls.freq( df, this.mix.type = 3, eq.mean = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$cmp$mean[1] == a$cmp$mean[2] )
expect_true( a$cmp$sd[1] != a$cmp$sd[2] )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.97 )

# normal test (retaining "v2")
a$nls.freq( df )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$cmp$mean[1] != a$cmp$mean[2] )
expect_true( a$cmp$sd[1] != a$cmp$sd[2] )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.98 )

# normal test
a$nls.freq( df, this.mix.type = 3, grad = "v3", eq.mean = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( nrow( a$cmp ), 3 )
expect_true( all( a$cmp$mean == a$cmp$mean[1] ) )
expect_true( all( a$cmp$sd[1:3] != a$cmp$sd[c( 2, 3, 1 )] ) )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.98 )

# normal test (this.mix.type = 3 where current number of components is 3)
a$nls.freq( df, this.mix.type = 3 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( nrow( a$cmp ), 3 )
expect_true( all( a$cmp$mean[1:3] != a$cmp$mean[c( 2, 3, 1 )] ) )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.98 )

# normal test (retaining "v3")
a$nls.freq( df, eq.mean = TRUE )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( nrow( a$cmp ), 3 )
expect_true( all( a$cmp$mean == a$cmp$mean[1] ) )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.98 )

# normal test
a$nls.freq( df, grad = "v3",
            start = list( mean.1 = a$cmp$mean[1],
                          mean.2 = a$cmp$mean[1],
                          mean.3 = a$cmp$mean[1],
                          sqrt.sd.1 = sqrt( a$cmp$sd[1] ),
                          sqrt.sd.2 = sqrt( a$cmp$sd[2] ),
                          sqrt.sd.3 = sqrt( a$cmp$sd[1] ) ) )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.eq.mean(), FALSE )
expect_true( all( a$cmp$mean[1:3] != a$cmp$mean[c( 2, 3, 1 )] ) )
expect_true( all( a$cmp$sd[1:3] != a$cmp$sd[c( 2, 3, 1 )] ) )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.98 )

# normal test
a$nls.freq( df, this.mix.type = 4, eq.mean = TRUE )
expect_identical( a$mix.type, 4L )
expect_true( all( a$cmp$mean == a$cmp$mean[1] ) )
expect_true( all( a$cmp$sd[c( 1:4, 1:2 )] != a$cmp$sd[c( 2:4, 1, 3:4 )] ) )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > 0.98 )

# normal test
prev.cor <- cor( a$d( x ), ggd:::get.d.freq( x, freq ) )
a$nls.freq( df, this.mix.type = 4, control = list( maxiter = 250 ) )
expect_identical( a$mix.type, 4L )
expect_true( all( a$cmp$mean[c( 1:4, 1:2 )] != a$cmp$mean[c( 2:4, 1, 3:4 )] ) )
expect_true( all( a$cmp$sd[c( 1:4, 1:2 )] != a$cmp$sd[c( 2:4, 1, 3:4 )] ) )
plot.freq.and.d( a, x, freq )
diff.check( a, x, freq )
expect_true( cor( a$d( x ), ggd:::get.d.freq( x, freq ) ) > prev.cor )
rm( prev.cor )

# normal test
result <- ggd.nls.freq( df, kind = 1 )
expect_identical( result$obj$kind.index, 1L )
expect_true( result$nls.out$convInfo$isConv )
a$clear()
expect_true( all( vapply( 2:16,
    function( i )
    {
        if ( any( i == c( 3, 11 ) ) )
        {
            expect_warning(
                result <- a$nls.freq( df, this.kind = i, control = list( warnOnly = TRUE ) ) )
        }
        else
        {
            result <- a$nls.freq( df, this.kind = i )
        }
        expect_identical( result$obj$kind.index, as.integer( i ) )
        expect_output( print( result$nls.out ), "sqrt\\.sd[\\.123]*\\^2" )
        TRUE
    }, TRUE ) ) )

## kind and overwriting
# normal test (Indicated kind will be output as it is.)
expect_true( all(
    vapply( ggd:::kinds[1:16],
        function( kind )
        {
            if ( any( ggd.kind.index( kind ) == c( 3, 11 ) ) )
            {
                expect_warning(
                    a$nls.freq( df, this.kind = kind, control = list( warnOnly = TRUE ) ) )
                expect_warning(
                   obj.kind <- ggd.nls.freq( df, kind = kind,
                                         control = list( warnOnly = TRUE ) )$obj$kind )
            }
            else
            {
                a$nls.freq( df, this.kind = kind )
                obj.kind <- ggd.nls.freq( df, kind = kind )$obj$kind
            }
            a$kind == kind && obj.kind == kind
        }, TRUE ) ) )

# normal test (matching order)
a$clear()
a <- ggd.nls.freq( df, kind = "Distribution" )$obj
expect_equal( a$kind, "Normal Distribution" )
expect_identical( a$mix.type, 0L )
expect_equal( a$is.normal(), TRUE )

a$clear()
a <- ggd.nls.freq( df, kind = "Mean of.* 2 Normal" )$obj
expect_equal( a$kind, "Mean of Mean-Differed Sigma-Differed 2 Normal Distributions" )
expect_identical( a$mix.type, 1L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

a$clear()
a <- ggd.nls.freq( df, kind = "Horizontal" )$obj
expect_equal( a$kind, "Mean-Differed Sigma-Differed Horizontal Gradational Distribution" )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.h( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

a$clear()
a <- ggd.nls.freq( df, kind = "Vertical" )$obj
expect_equal( a$kind, "2-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

a$clear()
a <- ggd.nls.freq( df, kind = "3-.*Vertical" )$obj
expect_equal( a$kind, "3-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

a$clear()
a <- ggd.nls.freq( df, kind = "Horizontal-Vertical" )$obj
expect_equal( a$kind, "Mean-Differed Sigma-Differed Horizontal-Vertical Gradational Distribution" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test (overwriting kind with mix.type)
kind = "Mean-Equaled Sigma-Differed Horizontal-Vertical Gradational Distribution"
a$nls.freq( df, this.kind = kind, this.mix.type = 3 )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

a$clear()
a <- ggd.nls.freq( df, kind = kind, mix.type = 3 )$obj
expect_identical( a$mix.type, 3L )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

# normal test (overwrite by flag)
kind = "Mean-Differed Sigma-Equaled Horizontal-Vertical Gradational Distribution"
a$nls.freq( df, this.kind = kind, grad = "v2" )
expect_equal( a$kind, "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

a$clear()
a <- ggd.nls.freq( df, kind = kind, grad = "v2" )$obj
expect_equal( a$kind, "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

# normal test (overwrite eq.mean and eq.sd)
a$nls.freq( df, this.kind = a, eq.mean = TRUE, eq.sd = FALSE )
expect_equal( a$kind, "2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

a$nls.freq( df, this.kind = a, eq.mean = FALSE, eq.sd = TRUE )
expect_equal( a$kind, "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

a <- ggd.nls.freq( df, kind = a, eq.mean = TRUE, eq.sd = FALSE )$obj
expect_equal( a$kind, "2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

a <- ggd.nls.freq( df, kind = a, eq.mean = FALSE, eq.sd = TRUE )$obj
expect_equal( a$kind, "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

a$nls.freq( df, this.kind = "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution",
            eq.mean = TRUE )
expect_equal( a$kind, "Normal Distribution" )
expect_identical( a$mix.type, 0L )

a <- ggd.nls.freq( df, kind = "3-Mean-Equaled Sigma-Differed Vertical Gradational Distribution",
               eq.sd = TRUE )$obj
expect_equal( a$kind, "Normal Distribution" )
expect_identical( a$mix.type, 0L )

# normal test (overwrite by flags)
a$nls.freq( df, this.kind = "Mean-Equaled Sigma-Differed Horizontal-Vertical Gradational Distribution",
            grad = "normal" )
expect_equal( a$kind, "Normal Distribution" )
expect_identical( a$mix.type, 0L )

a$clear()
a$nls.freq( df, this.kind = "Mean-Differed Sigma-Equaled Vertical",
            grad = "h" )
expect_equal( a$kind, "Mean-Differed Sigma-Equaled Horizontal Gradational Distribution" )
expect_identical( a$mix.type, 2L )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

a$clear()
a$nls.freq( df, this.kind = "3-Mean-Equaled Sigma-Differed",
            grad = "v2" )
expect_equal( a$kind, "2-Mean-Equaled Sigma-Differed Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3( TRUE ), FALSE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

a <- ggd.nls.freq( df, kind = "Mean-Differed Sigma-Equaled Horizontal",
               grad = "v2" )$obj
expect_equal( a$kind, "2-Mean-Differed Sigma-Equaled Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), TRUE )
expect_equal( a$is.v3( TRUE ), FALSE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )

a$clear()
a$nls.freq( df, this.kind = "Normal Distribution",
            grad = "v3",
            start = list( mean.1 = 0.00418, mean.2 = 0.00418, mean.3 = 0.00418,
                          sqrt.sd.1 = 1.035, sqrt.sd.2 = 1.077, sqrt.sd.3 = 0.929 ) )
expect_equal( a$kind, "3-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), FALSE )

a <- ggd.nls.freq( df, kind = "2-Mean-Equaled Sigma-Differed Vertical",
               grad = "v3" )$obj
expect_equal( a$kind, "3-Mean-Equaled Sigma-Differed Vertical Gradational Distribution" )
expect_identical( a$mix.type, 3L )
expect_equal( a$is.v2( TRUE ), FALSE )
expect_equal( a$is.v3( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

a$clear()
a$nls.freq( df, this.kind = "2-Mean-Equaled Sigma-Differed Vertical",
            grad = "hv" )
expect_equal( a$kind, "Mean-Equaled Sigma-Differed Horizontal-Vertical Gradational Distribution" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), TRUE )
expect_equal( a$is.eq.sd(), FALSE )

a <- ggd.nls.freq( df, kind = "3-Mean-Differed Sigma-Equaled Vertical",
               grad = "hv" )$obj
expect_equal( a$kind, "Mean-Differed Sigma-Equaled Horizontal-Vertical Gradational Distribution" )
expect_identical( a$mix.type, 4L )
expect_equal( a$is.hv( TRUE ), TRUE )
expect_equal( a$is.eq.mean(), FALSE )
expect_equal( a$is.eq.sd(), TRUE )


## ggd.nls.freq.all
# normal test
expect_warning( expect_warning(
    ggds <- ggd.nls.freq.all( df ),
    "Warning for kind = 3 :" ) )
ggds$best
ggds$cor
expect_true( all( vapply( 1:16, function( i )
                  any( ggds$detail[[i]]$start.level == 0:3 ), TRUE ) ) )
expect_true( ggds$best.cor > 0.984 )

# normal test (set "start" of the result to "start" argument)
lists <- lapply( 1:16, function( kind ) ggds$detail[[kind]]$start )
expect_warning( expect_warning(
    ggds <- ggd.nls.freq.all( df, start = lists ),
    "Warning for kind = 3 :" ) )
expect_true( all( vapply( 1:16,
    function( i )
    {
        identical( ggds$detail[[i]]$start.level, NA_integer_ ) &&
        identical( ggds$detail[[i]]$start, lists[[i]] )
    }, TRUE ) ) )

# not.use.nls = TRUE is ignored where start.level = 100.
expect_warning( expect_warning( expect_warning(
    ggds <- ggd.nls.freq.all( df, start.level = 100, not.use.nls = TRUE ),
    "not.use.nls does not work with start.level is default 100." ),
    "Warning for kind = 3 :" ) )
expect_true( all( vapply( 1:16, function( i )
    {
        expect_error( expect_equal_ggd( ggds$detail[[i]]$start.obj, ggds$obj[[i]] ), "not equal" )

        identical( ggds$detail[[i]]$obj, ggds$obj[[i]] ) &&
        !is.null( ggds$detail[[i]]$nls.out )
    }, TRUE ) ) )

## open connection to output TeX formatted texts.
tex.out.for.ggd.nls.freq.all <- function( ggds )
{
    texfile <- tempfile( "tex-out", fileext = ".txt" )
    TeX.output.connection <- file( texfile )
    open( TeX.output.connection, "a" )
    announce_snapshot_file( name = texfile )
    on.exit( close( TeX.output.connection ) )

    for ( i in 1:length( ggds$obj ) )
    {
        expect_identical( ggds$obj[[i]]$kind.index, as.integer( i ) )
        writeLines( "################################################################", con = TeX.output.connection )
        writeLines( paste( "## kind.index:", i ), con = TeX.output.connection )
        writeLines( paste( "# [tex.d]", ggd:::kinds[i] ), con = TeX.output.connection )
        ggds$obj[[i]]$tex.d( con = TeX.output.connection, format.num = function( x ) format( x, digits = 3 ) )
        writeLines( "##------------------------------------------------------------##", con = TeX.output.connection )
        writeLines( paste( "# [tex.p]", ggd:::kinds[i] ), con = TeX.output.connection )
        ggds$obj[[i]]$tex.p( con = TeX.output.connection, format.num = function( x ) format( x, digits = 3 ) )
    }

    for ( i in 1:length( ggds$obj ) )
    {
        writeLines( "################################################################", con = TeX.output.connection )
        writeLines( paste( "## kind.index:", i ), con = TeX.output.connection )
        writeLines( paste( "# [tex]", ggd:::kinds[i] ), con = TeX.output.connection )
        ggds$obj[[i]]$tex( con = TeX.output.connection, format.num = function( x ) format( x, digits = 3 ) )
    }

    texfile
}

test_that( "Output TeX formatted texts",
{
    expect_snapshot_file( tex.out.for.ggd.nls.freq.all( ggds ), "tex-out.txt" )
} )
rm( tex.out.for.ggd.nls.freq.all )

plot.freq.and.d( ggds$obj[[1]], x, freq )
plot.freq.and.d( ggds$obj[[2]], x, freq )
plot.freq.and.d( ggds$obj[[3]], x, freq )
plot.freq.and.d( ggds$obj[[4]], x, freq )
plot.freq.and.d( ggds$obj[[5]], x, freq )
plot.freq.and.d( ggds$obj[[6]], x, freq )
plot.freq.and.d( ggds$obj[[7]], x, freq )
plot.freq.and.d( ggds$obj[[8]], x, freq )
plot.freq.and.d( ggds$obj[[9]], x, freq )
plot.freq.and.d( ggds$obj[[10]], x, freq )
plot.freq.and.d( ggds$obj[[11]], x, freq )
plot.freq.and.d( ggds$obj[[12]], x, freq )
plot.freq.and.d( ggds$obj[[13]], x, freq )
plot.freq.and.d( ggds$obj[[14]], x, freq )
plot.freq.and.d( ggds$obj[[15]], x, freq )

plot.freq.and.d( ggds$best, x, freq )

## ggd.start.template
# normal test
expect_equal( ggd.start.template(  1 ), list( mean = 0,                     sqrt.sd = 1 ) )
expect_equal( ggd.start.template(  2 ), list( mean.1 = 0, mean.2 = 0,       sqrt.sd = 1 ) )
expect_equal( ggd.start.template(  3 ), list( mean = 0,                     sqrt.sd.1 = 1, sqrt.sd.2 = 1 ) )
expect_equal( ggd.start.template(  4 ), list( mean.1 = 0, mean.2 = 0,       sqrt.sd.1 = 1, sqrt.sd.2 = 1 ) )
expect_equal( ggd.start.template(  5 ), list( mean.1 = 0, mean.2 = 0,       sqrt.sd = 1 ) )
expect_equal( ggd.start.template(  6 ), list( mean = 0,                     sqrt.sd.1 = 1, sqrt.sd.2 = 1 ) )
expect_equal( ggd.start.template(  7 ), list( mean.1 = 0, mean.2 = 0,       sqrt.sd.1 = 1, sqrt.sd.2 = 1 ) )
expect_equal( ggd.start.template(  8 ), list( mean.1 = 0, mean.2 = 0,       sqrt.sd = 1 ) )
expect_equal( ggd.start.template(  9 ), list( mean = 0,                     sqrt.sd.1 = 1, sqrt.sd.2 = 1 ) )
expect_equal( ggd.start.template( 10 ), list( mean.1 = 0, mean.2 = 0,       sqrt.sd.1 = 1, sqrt.sd.2 = 1 ) )
expect_equal( ggd.start.template( 11 ), list( mean.1 = 0, mean.2 = 0, mean.3 = 0,
                                                                            sqrt.sd = 1 ) )
expect_equal( ggd.start.template( 12 ), list( mean = 0,                     sqrt.sd.1 = 1, sqrt.sd.2 = 1, sqrt.sd.3 = 1 ) )
expect_equal( ggd.start.template( 13 ), list( mean.1 = 0, mean.2 = 0, mean.3 = 0,
                                                                            sqrt.sd.1 = 1, sqrt.sd.2 = 1, sqrt.sd.3 = 1 ) )
expect_equal( ggd.start.template( 14 ), list( mean.1.1 = 0, mean.1.2 = 0,
                                              mean.2.1 = 0, mean.2.2 = 0,   sqrt.sd = 1 ) )
expect_equal( ggd.start.template( 15 ), list( mean = 0,                     sqrt.sd.1.1 = 1, sqrt.sd.1.2 = 1,
                                                                            sqrt.sd.2.1 = 1, sqrt.sd.2.2 = 1 ) )
expect_equal( ggd.start.template( 16 ), list( mean.1.1 = 0, mean.1.2 = 0,   sqrt.sd.1.1 = 1, sqrt.sd.1.2 = 1,
                                              mean.2.1 = 0, mean.2.2 = 0,   sqrt.sd.2.1 = 1, sqrt.sd.2.2 = 1 ) )
expect_equal( ggd.start.template( "Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions" ),
              list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 ) )
expect_equal( ggd.start.template( NA ), NULL )
expect_equal( ggd.start.template( 0 ), NULL )
expect_equal( ggd.start.template( 18 ), NULL )

# normal test
lists <- ggd.init.start()
lists[[ 1]] <- list( mean = 0,                      sqrt.sd = sqrt( 1 ) )
lists[[ 2]] <- list( mean.1 = -1, mean.2 = 1,       sqrt.sd = sqrt( 1 ) )
lists[[ 3]] <- list( mean = 0,                      sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
lists[[ 4]] <- list( mean.1 = -1, mean.2 = 1,       sqrt.sd.1 = sqrt( 1 ), sqrt.sd.2 = sqrt( 1 ) )
lists[[ 5]] <- list( mean.1 = -1, mean.2 = 1,       sqrt.sd = sqrt( 1 ) )
lists[[ 6]] <- list( mean = 0,                      sqrt.sd.1 = sqrt( 1 ), sqrt.sd.2 = sqrt( 1 ) )
lists[[ 7]] <- list( mean.1 = -1, mean.2 = 1,       sqrt.sd.1 = sqrt( 1 ), sqrt.sd.2 = sqrt( 1 ) )
lists[[ 8]] <- list( mean.1 = 0, mean.2 = 0,        sqrt.sd = sqrt( 1 ) )
lists[[ 9]] <- list( mean = 0,                      sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
lists[[10]] <- list( mean.1 = 0, mean.2 = 0,        sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
lists[[11]] <- list( mean.1 = 0, mean.2 = 0, mean.3 = 0,
                                                    sqrt.sd = sqrt( 1 ) )
lists[[12]] <- list( mean = 0,                      sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ), sqrt.sd.3 = sqrt( 2 ) )
lists[[13]] <- list( mean.1 = 0, mean.2 = 0, mean.3 = 0,
                                                    sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ), sqrt.sd.3 = sqrt( 2 ) )
lists[[14]] <- list( mean.1.1 = 0, mean.1.2 = 0,
                     mean.2.1 = 0, mean.2.2 = 0,    sqrt.sd = sqrt( 1 ) )
lists[[15]] <- list( mean = 0,                      sqrt.sd.1.1 = sqrt( 2 ), sqrt.sd.1.2 = sqrt( 1 ),
                                                    sqrt.sd.2.1 = sqrt( 2 ), sqrt.sd.2.2 = sqrt( 1 ) )
lists[[16]] <- list( mean.1.1 = 0, mean.1.2 = 0,    sqrt.sd.1.1 = sqrt( 2 ), sqrt.sd.1.2 = sqrt( 1 ),
                     mean.2.1 = 0, mean.2.2 = 0,    sqrt.sd.2.1 = sqrt( 2 ), sqrt.sd.2.2 = sqrt( 1 ) )

df.2 <- data.frame( x.2 = x, freq.2 = freq )

# normal test
expect_warning( expect_warning(
expect_warning( expect_warning(
    ggds <- ggd.nls.freq.all( df.2, x = "x.2", freq = 2, start = lists ),
    "Warning for kind = 3 :" ) ),
    "Warning for kind = 16 :" ) )
ggds$cor
expect_true( all( vapply( 1:16,
    function( i )
    {
        identical( ggds$detail[[i]]$start.level, NA_integer_ ) &&
        identical( ggds$detail[[i]]$start, lists[[i]] )
    }, TRUE ) ) )
expect_identical( ggds$best$kind.index, 14L )
expect_true( ggds$best.cor > 0.984 )
plot.freq.and.d( ggds$best, x, freq )

# normal test
expect_warning( expect_warning(
    ggds <- ggd.nls.freq.all( df.2, x = 1, freq = "freq.2", control = list( maxiter = 300 ) ),
    "Warning for kind = 3 :" ) )
expect_true( ggds$best.cor > 0.984 )

# normal test (although control is set NULL, warnOnly will be added)
expect_warning( expect_warning(
expect_warning( expect_warning(
ggds <- ggd.nls.freq.all( df, control = NULL ),
    "Warning for kind = 3 :" ) ),
    "Warning for kind = 11 :" ) )

# normal test
expect_warning( expect_warning(
    ggds <- ggd.nls.freq.all( df, control = list( maxiter = 300 ) ),
    "Warning for kind = 3 :" ) )

# normal and error test
expect_message( expect_message(
expect_message( expect_message(
    ggds <- ggd.nls.freq.all( df, control = list( maxiter = 50, warnOnly = FALSE ) ),
    "Error for kind = 3 :" ),  "All of nls functions have failed" ),
    "Error for kind = 11 :" ), "All of nls functions have failed" )

expect_true( all( vapply( 1:length( ggds$obj ),
    function( i )
    {
        expect_s4_class( ggds$obj[[i]], "GGD" )
        if ( any( i == c( 3, 11 ) ) )
        {
            expect_s3_class( ggds$detail[[i]], "error" )
        }
        else
        {
            expect_identical( ggds$obj[[i]], ggds$detail[[i]]$obj )
            expect_equal( ggds$obj[[i]]$kind, ggd:::kinds[i] )
        }
        TRUE
    }, TRUE ) ) )

expect_cleared( ggds$obj[[3]] )
expect_cleared( ggds$obj[[11]] )
expect_identical( ggds$best$kind.index, 14L )
expect_true( ggds$best.cor > 0.984 )
plot.freq.and.d( ggds$best, x, freq )

# normal test
expect_warning( expect_warning(
    expect_output( ggds <- ggd.nls.freq.all( df, cor.method = "spearman", trace = TRUE ), "par =" ),
    "Warning for kind = 3 :" ) )
ggds$cor
expect_true( ggds$best.cor > 0.96 )
expect_true( ggd.cor.vs.freq( ggds$best, x, freq, cor.method = "spearman" ) == ggds$best.cor )
expect_false( ggd.cor.vs.freq( ggds$best, x, freq, cor.method = "pearson" ) == ggds$best.cor )
plot.freq.and.d( ggds$best, x, freq )

