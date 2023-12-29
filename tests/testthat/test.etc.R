################################################################################################
## This file is to test cases which are not tested in other files.
################################################################################################

# For interactive test, load setup.R expressly.
if ( file.exists( "tests/testthat" ) ) source( "tests/testthat/setup.R" )

a <- GGD$new()

################################################################################################
## Tests for various conditions
################################################################################################

#### adjust.kind.index for redundant cmp
a <- ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), grad = "h" )
expect_equal( a$kind, "Mean-Differed Sigma-Differed Horizontal Gradational Distribution" )
expect_equal( a$mix.type, 2 )
a$adjust.cmp( 4 )   # calls adjust.kind.index
expect_equal( a$kind, "Mean-Differed Sigma-Differed Horizontal Gradational Distribution" )
expect_equal( a$mix.type, 4 )

expect_error( a$set.cmp( grad = "v2" ),
              "Indicated grad is not appropriate for cmp" )

a$adjust.cmp()
a$set.cmp( grad = "v2" )
expect_equal( a$kind, "2-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )
expect_equal( a$mix.type, 3 )
a$adjust.cmp( 4 )   # calls adjust.kind.index
expect_equal( a$kind, "2-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )
expect_equal( a$mix.type, 4 )
a$adjust.cmp( grad = "v" )
expect_equal( a$kind, "2-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )
expect_equal( a$mix.type, 3 )

#### adjust.cmp for too large cmp
a <- ggd.set.cmp( data.frame( mean = 0, sd = 1 ), grad = "hv" )
a$cmp <- data.frame( mean = -2:2, sd = 1:5 )    ## Don't do it! (this is just for test)
expect_warning( a$adjust.cmp(),
                "The number of the cmp field rows is too large. 5th and after rows are discarded" )
expect_equal( a$mix.type, 4 )
expect_equal( a$cmp$mean, -2:1 )
expect_equal( a$cmp$sd, 1:4 )
## Now object a is broken because a data frame has been set into cmp filed directly.
##
## Executing following 2 methods can repair the object (but not recommended).
a$adjust.kind.index()
a$adjust.median.mean.sd()

#### ggd.get.t3.cmp
expect_error( ggd.get.t3.cmp( c( 0, 0 ), c( 1, 1 ) ) )
expect_error( ggd.get.t3.cmp( c( 0, 0 ), c( 1, 1 ), "h" ) )
expect_error( ggd.get.t3.cmp( c( 0, 0 ), c( 1, 1 ), "hv" ) )

expect_identical( ggd.get.t3.cmp( rep( 1, 3 ), rep( 2, 3 ) ),
                  data.frame( mean = c( 1, 1 ), sd = c( 2, 2 ) ) )

expect_identical( ggd.get.t3.cmp( rep( 1.5, 3 ), rep( 2.5, 3 ), "default" ),
                  data.frame( mean = c( 1.5, 1.5 ), sd = c( 2.5, 2.5 ) ) )

expect_identical( ggd.get.t3.cmp( rep( 0.5, 3 ), rep( 1.5, 3 ), "v2" ),
                  data.frame( mean = c( 0.5, 0.5 ), sd = c( 1.5, 1.5 ) ) )

expect_identical( ggd.get.t3.cmp( rep( 1.5, 3 ), rep( 2, 3 ), "v3" ),
                  data.frame( mean = c( 1.5, 1.5, 1.5 ), sd = c( 2, 2, 2 ) ) )

expect_identical( ggd.get.t3.cmp( c( 1, 0.5, 1 ), c( 2, 1, 2 ), "v2" ),
                  data.frame( mean = c( 1, 0.5 ), sd = c( 2, 1 ) ) )

expect_identical( ggd.get.t3.cmp( c( 1, 0.5, 1 ), c( 2, 1, 2 ), "v3" ),
                  data.frame( mean = c( 1, 0.5, 1 ), sd = c( 2, 1, 2 ) ) )

expect_identical( ggd.get.t3.cmp( c( 1, 1, 0.5 ), c( 2, 2, 1 ), "v2" ),
                  data.frame( mean = c( 1, 1, 0.5 ), sd = c( 2, 2, 1 ) ) )

#### invalid mix.type
a <- ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), grad = "h" )
a$mix.type <- -1L       ## Don't do it! (this is just for test)
expect_error( a$adjust.kind.index(), "mix.type is invalid" )
expect_error( a$d( 0 ),     "mix.type is invalid" )
expect_error( a$p( 0 ),     "mix.type is invalid" )
expect_error( a$q( 0.5 ),   "mix.type is invalid" )
expect_error( a$r( 10 ),    "mix.type is invalid" )

#### out-of-range probability
a <- GGD$new()
expect_equal( a$q( -1 ), NaN )
expect_equal( a$q( 1.01 ), NaN )

#### cleared object
a$clear()
a$adjust.cmp()
a$adjust.cmp.rownames()
expect_equal( nrow( a$cmp ), 0 )
a$adjust.kind.index()
expect_identical( c( a$kind.index, a$kind, a$mix.type ),
                  c( integer(), character(), integer() ) )
a$adjust.median.mean.sd()
expect_identical( c( a$median, a$mean, a$sd, a$lsd, a$usd, a$lsd.abs.error, a$usd.abs.error ),
                  rep( NaN, 7 ) )

a$clear()
expect_false( a$is.eq.mean() )
expect_false( a$is.eq.sd() )
expect_false( a$is.normal() )
expect_false( a$is.h() )
expect_false( a$is.h( TRUE ) )
expect_false( a$is.v2() )
expect_false( a$is.v2( TRUE ) )
expect_false( a$is.v3() )
expect_false( a$is.v3( TRUE ) )
expect_false( a$is.hv() )
expect_false( a$is.hv( TRUE ) )
expect_false( a$is.symmetric() )
expect_identical( a$d( c( -1, 0, 0.5, 1, 2 ) ), rep( NaN, 5 ) )
expect_identical( a$p( c( -1, 0, 0.5, 1, 2 ) ), rep( NaN, 5 ) )
expect_identical( a$q( c( -1, 0, 0.5, 1, 2 ) ), rep( NaN, 5 ) )
expect_identical( a$r( 5 ), rep( NaN, 5 ) )
expect_error( expect_output( expect_equal(
        a$tex( tex.out ), NULL ) ), "no output" )
expect_error( expect_output( expect_equal(
        a$tex.d( tex.out ), NULL ) ), "no output" )
expect_error( expect_output( expect_equal(
        a$tex.p( tex.out ), NULL ) ), "no output" )

#### ggd.ncmp.for empty or invalid mix.type
expect_equal( ggd.ncmp.for( NULL, mix.type = NULL ), 0 )
expect_equal( ggd.ncmp.for( NULL, mix.type = integer() ), 0 )
expect_equal( ggd.ncmp.for( NULL, mix.type = NA ), 0 )
expect_identical( ggd.ncmp.for( mix.type = -1 ), NA_integer_ )
expect_identical( ggd.ncmp.for( mix.type = 5 ), NA_integer_ )

# "v" for alias of "v2"
expect_equal( ggd.ncmp.for( "v" ), 2 )

#### ggd.mix.type.for empty or invalid kind, mix.type
expect_identical( ggd.mix.type.for( NULL, kind = NULL ), integer() )
expect_identical( ggd.mix.type.for( NULL, kind = integer() ), integer() )
expect_identical( ggd.mix.type.for( NULL, kind = NA ), NA_integer_ )
expect_error( ggd.mix.type.for( kind = -1 ),
              "kind for index -1 is undefined" )
expect_error( ggd.mix.type.for( kind = 17 ),
              "kind for index 17 is undefined" )
expect_error( ggd.mix.type.for( kind = "Binomial Distribution" ),
              "'Binomial Distribution' does not match any character strings of kinds" )

expect_identical( ggd.mix.type.for( NULL, mix.type = NULL ), integer() )
expect_identical( ggd.mix.type.for( NULL, mix.type = integer() ), integer() )
expect_identical( ggd.mix.type.for( NULL, mix.type = NA ), NA_integer_ )
expect_equal( ggd.mix.type.for( NULL, mix.type = -1 ), -1 )
expect_equal( ggd.mix.type.for( NULL, mix.type = 5 ), 5 )

#### lengths of default values of ggd.mix.type.for
expect_identical( ggd.mix.type.for( NULL, kind = 1:2 ), c( 0L, 1L ) )
expect_identical( ggd.mix.type.for( NULL, kind = c( "2.*Ver", "Hor" ) ), c( 3L, 2L ) )
expect_identical( ggd.mix.type.for( NULL, mix.type = 1:10 ), 1:10 )

# "v" for alias of "v2"
expect_identical( ggd.mix.type.for( "v" ), 3L )

#### sd.norm.mxp with various length vectors
sds <- sd.norm.mxp( -1, 1:5 * 0.5, pnorm( 1:5 * 0.5, -1, 2 ) )
expect_equal( length( sds ), 5 )
expect_equal( sds, rep( 2, 5 ) )    # Due to calculation errors, identical cannot be used.

sds <- sd.norm.mxp( -2:3, -1:4 * 0.25,
                    pnorm( -1:4 * 0.25, -2:3, c( 2, 3, 0.5, 1.25, 3.5, 4.25 ) ) )
expect_equal( length( sds ), 6 )
expect_equal( sds, c( 2, 3, 0.5, 1.25, 3.5, 4.25 ) )

sds <- sd.norm.mxp( -2:3, 0.5,
                    pnorm( rep( 0.5, 6 ), -2:3, c( 4, 2, 1.5, 2.75, 3, 4.75 ) ) )
expect_equal( length( sds ), 6 )
expect_equal( sds, c( 4, 2, 1.5, 2.75, 3, 4.75 ) )

sds <- sd.norm.mxp( -3:2, -1:4 * 1.5, 0.75 )
expect_equal( length( sds ), 6 )
expect_equal( pnorm( -1:4 * 1.5, -3:2, sds ), rep( 0.75, length( sds ) ) )

sds <- sd.norm.mxp( 1:6, -2:3 * 1.5, c( 0.2, 0.4 ) )
expect_equal( length( sds ), 6 )
expect_equal( pnorm( -2:3 * 1.5, 1:6, sds ), c( 0.2, 0.4, 0.2, 0.4, 0.2, 0.4 ) )


################################################################################################
## Is is.symmetric correct?
################################################################################################

################################################################################################
#' Plot the probability density function of a GGD object
#'
#' @param   obj         A GGD object.
#' @param   x.range     A vector of range of x-coordinates.
#' @param   file        The file path of png file for output.
#'                      If NULL, plot will be done to the current device.
#' @return  The value of file argument.
################################################################################################
plot.d <- function( obj, x.range, file = NULL )
{
    if ( !is.null( file ) )
    {
        png( file )
        on.exit( dev.off() )
    }

    plot( seq( x.range[1], x.range[2], 0.01 ), obj$d( seq( x.range[1], x.range[2], 0.01 ) ),
          type = "l", xlab = "", ylab = "",
          xlim = x.range,
          ylim = c( 0, max( obj$d( seq( x.range[1], x.range[2], 0.01 ) ) ) ) )

    file
}

################################################################################################
#' Compute lower standard deviation via integral function
#'
#' @param   obj     A GGD object.
################################################################################################
lsd.via.integrate <- function( obj )
{
    sqrt( integrate( function( x ) ( x - obj$mean )^2 * obj$d( x ), -Inf, obj$mean )$value * 2 )
}

test.is.symmetric.correct <- function()
{
    # mix.type = 0
    a <- ggd.set.cmp( data.frame( mean = -5, sd = 2.5 ) )
    expect_true( a$is.symmetric() )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( -12, 2 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.0_01.png" )

    # mix.type = 1
    a$set.cmp( data.frame( mean = c( 7, 7 ), sd = c( 2.8, 0.64 ) ), this.mix.type = 1 )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( 0, 14 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.1_01.png" )

    a$set.cmp( data.frame( mean = c( -1.2, 7.6 ), sd = c( 3, 3 ) ), this.mix.type = 1 )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( -8, 14 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.1_02.png" )

    # mix.type = 2
    a$set.cmp( data.frame( mean = c( 6.5, 6.5 ), sd = c( 1.6, 0.73 ) ), this.mix.type = 2 )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( 1, 12 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.2_01.png" )

    a$set.cmp( data.frame( mean = c( -1.5, 0.5 ), sd = c( 1.2, 1.2 ) ), this.mix.type = 2 )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( -6, 5 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.2_02.png" )

    # grad = "v2"
    a$set.cmp( data.frame( mean = c( 4.5, 4.5 ), sd = c( 1.7, 0.63 ) ), grad = "v2" )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( 0, 9 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v2_01.png" )

    a$set.cmp( data.frame( mean = c( 1, 2 ), sd = c( 1.75, 1.75 ) ), grad = "v2" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 7 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v2_02.png" )

    # grad = "v3"
    a$set.cmp( data.frame( mean = rep( 1, 3 ), sd = c( 1.5, 0.75, 1.5 ) ),
               grad = "v3" )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_01.png" )

    a$set.cmp( data.frame( mean = rep( 1, 3 ), sd = c( 0.75, 1.5, 1.5 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_02.png" )

    a$set.cmp( data.frame( mean = rep( 1, 3 ), sd = c( 1.5, 1.5, 0.75 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_03.png" )


    a$set.cmp( data.frame( mean = c( 1.5, 1, 1.5 ), sd = c( 1.5, 1.75, 1.5 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_04.png" )

    a$set.cmp( data.frame( mean = c( 0.5, 1, 1.5 ), sd = c( 1.5, 1.75, 1.5 ) ),
               grad = "v3" )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ), tolerance = 1e-7 )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_05.png" )

    a$set.cmp( data.frame( mean = c( 0.5, 1, 1.5 ), sd = c( 1.75, 1.25, 1.25 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_06.png" )

    a$set.cmp( data.frame( mean = c( 0.5, 1, 1.5 ), sd = c( 1.25, 1.25, 1.75 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_07.png" )

    a$set.cmp( data.frame( mean = c( -0.5, -1, -1.5 ), sd = c( 1.5, 1.75, 1.5 ) ),
               grad = "v3" )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ), tolerance = 5e-7 )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( -6, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_08.png" )

    a$set.cmp( data.frame( mean = c( -0.5, -1, -1.5 ), sd = c( 1.25, 1.25, 1.75 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -6, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_09.png" )

    a$set.cmp( data.frame( mean = c( -0.5, -1, -1.5 ), sd = c( 1.75, 1.25, 1.25 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -6, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_10.png" )

    a$set.cmp( data.frame( mean = c( -1.5, -0.5, 0.5 ), sd = c( 2, 1.75, 2 ) ),
               grad = "v3" )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( -6, 5 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_11.png" )

    a$set.cmp( data.frame( mean = c( -1.5, -0.5, 0.5 ), sd = c( 1.5, 1.5, 2 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -6, 5 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_12.png" )

    a$set.cmp( data.frame( mean = c( -1.5, -0.5, 0.5 ), sd = c( 2, 1.5, 1.5 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -6, 5 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_13.png" )


    a$set.cmp( data.frame( mean = c( 1.5, 1, 1.5 ), sd = rep( 1, 3 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_14.png" )

    a$set.cmp( data.frame( mean = c( 0.5, 1, 1.5 ), sd = rep( 1, 3 ) ),
               grad = "v3" )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ), tolerance = 5e-8 )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_15.png" )

    a$set.cmp( data.frame( mean = c( 1.5, 1, 0.5 ), sd = rep( 1, 3 ) ),
               grad = "v3" )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ), tolerance = 3e-6 )
    expect_equal( a$sd, a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_16.png" )

    a$set.cmp( data.frame( mean = c( 1.5, 0.5, 1 ), sd = rep( 1, 3 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_17.png" )

    a$set.cmp( data.frame( mean = c( 0.5, 1.5, 1 ), sd = rep( 1, 3 ) ),
               grad = "v3" )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -4, 6 ), tempfile( "graph", fileext = ".png" ) ),
                          "graph_mix.type.3_v3_18.png" )

    # mix.type = 4
    file.no <- 0
    a$set.cmp( data.frame( mean = rep( 1, 4 ), sd = c( 0.75, 0.75, 1, 1 ) ),
               this.mix.type = 4 )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -3, 5 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = rep( 1, 4 ), sd = c( 0.75, 1, 0.75, 1 ) ),
               this.mix.type = 4 )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_equal( a$lsd.abs.error == 0, TRUE )
    expect_equal( a$usd.abs.error == 0, TRUE )
    expect_snapshot_file( plot.d( a, c( -3, 5 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = rep( 1, 4 ), sd = c( 0.75, 1, 1, 0.75 ) ),
               this.mix.type = 4 )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -3, 5 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = rep( 1, 4 ), sd = c( 1, 0.75, 0.75, 1 ) ),
               this.mix.type = 4 )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -3, 5 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = rep( 1, 4 ), sd = c( 1, 0.75, 1, 0.75 ) ),
               this.mix.type = 4 )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_equal( a$lsd.abs.error == 0, TRUE )
    expect_equal( a$usd.abs.error == 0, TRUE )
    expect_snapshot_file( plot.d( a, c( -3, 5 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = rep( 1, 4 ), sd = c( 1, 1, 0.75, 0.75 ) ),
               this.mix.type = 4 )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -3, 5 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    means <- list( c( -1, 0, 1, 0 ), c( 0, -1, 1, 0 ), c( 1, 0, -1, 0 ), c( 0, 0, 1, -1 ),
                   c( -1, 1, 0, 0 ), c( -1, 0, 0, 1 ), c( 1, -1, 0, 0 ), c( 0, 1, -1, 0 ),
                   c( 0, -1, 0, 1 ), c( 1, 0, 0, -1 ), c( 0, 1, 0, -1 ), c( 0, 0, -1, 1 ) )
    for ( mean in means )
    {
        a$set.cmp( data.frame( mean = mean, sd = c( 2, 1, 2, 1 ) ), this.mix.type = 4 )
        if ( mean[1] == mean[3] || mean[2] == mean[4] )
        {
            expect_true( a$is.symmetric() )
            expect_equal( a$p( a$mean ), 0.5 )
            expect_equal( a$sd, lsd.via.integrate( a ) )
            expect_equal( a$sd, a$usd )
            expect_equal( a$lsd.abs.error == 0, TRUE )
            expect_equal( a$usd.abs.error == 0, TRUE )
        }
        else
        {
            expect_false( a$is.symmetric() )
            expect_false( a$sd == a$lsd )
            expect_false( a$sd == a$usd )
        }
        expect_snapshot_file( plot.d( a, c( -4, 4 ), tempfile( "graph", fileext = ".png" ) ),
                              sprintf( "graph_mix.type.4_%02d.png",
                              ( file.no <- file.no + 1 ) ) )
    }
    rm( means )

    a$set.cmp( data.frame( mean = c( -2, -0.75, 1, -0.25 ), sd = c( 2, 1, 2, 1 ) ),
               this.mix.type = 4 )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_equal( a$lsd.abs.error == 0, TRUE )
    expect_equal( a$usd.abs.error == 0, TRUE )
    expect_snapshot_file( plot.d( a, c( -5, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = c( -0.75, -2, 1, -0.25 ), sd = c( 2, 1, 2, 1 ) ),
               this.mix.type = 4 )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -5, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = c( 1, -0.75, -2, -0.25 ), sd = c( 2, 1, 2, 1 ) ),
               this.mix.type = 4 )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_equal( a$lsd.abs.error == 0, TRUE )
    expect_equal( a$usd.abs.error == 0, TRUE )
    expect_snapshot_file( plot.d( a, c( -5, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = c( -0.25, -0.75, 1, -2 ), sd = c( 2, 1, 2, 1 ) ),
               this.mix.type = 4 )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -5, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = c( -2, 1, -0.75, -0.25 ), sd = c( 2, 1, 2, 1 ) ),
               this.mix.type = 4 )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -5, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = c( -2, -0.75, -0.25, 1 ), sd = c( 2, 1, 2, 1 ) ),
               this.mix.type = 4 )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -5, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = c( -2, -0.75, 1, -0.25 ), sd = c( 1, 2, 1, 2 ) ),
               this.mix.type = 4 )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_equal( a$lsd.abs.error == 0, TRUE )
    expect_equal( a$usd.abs.error == 0, TRUE )
    expect_snapshot_file( plot.d( a, c( -5, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = c( -2, -0.75, 1, -0.25 ), sd = c( 2, 1, 1, 2 ) ),
               this.mix.type = 4 )
    expect_false( a$is.symmetric() )
    expect_false( a$sd == a$lsd )
    expect_false( a$sd == a$usd )
    expect_snapshot_file( plot.d( a, c( -5, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )

    a$set.cmp( data.frame( mean = c( -2, -0.75, 1, -0.25 ), sd = rep( 2, 4 ) ),
               this.mix.type = 4 )
    expect_true( a$is.symmetric() )
    expect_equal( a$p( a$mean ), 0.5 )
    expect_equal( a$sd, lsd.via.integrate( a ) )
    expect_equal( a$sd, a$usd )
    expect_equal( a$lsd.abs.error == 0, TRUE )
    expect_equal( a$usd.abs.error == 0, TRUE )
    expect_snapshot_file( plot.d( a, c( -5, 4 ), tempfile( "graph", fileext = ".png" ) ),
                          sprintf( "graph_mix.type.4_%02d.png", ( file.no <- file.no + 1 ) ) )
}

test_that( "Whether is symmetric correct",
{
    test.is.symmetric.correct()
} )
rm( test.is.symmetric.correct )

################################################################################################
## Read/Write a GGD object with CSV file
################################################################################################

check.read.write.csv <- function( obj )
{
    # write.csv via non-method function
    csvfile <- tempfile( fileext = ".csv" )
    result.1 <- withVisible( ggd.write.csv( obj, csvfile ) )
    expect_equal( result.1$value, NULL )
    expect_false( result.1$visible )

    # read.csv via non-method function
    result.2 <- withVisible( ggd.read.csv( csvfile ) )
    expect_s4_class( result.2$value, "GGD" )
    expect_true( result.2$visible )
    expect_equal_ggd( result.2$value, obj )
    unlink( csvfile )

    # write.csv via method
    csvfile <- tempfile( fileext = ".csv" )
    result.3 <- withVisible( obj$write.csv( csvfile ) )
    expect_equal( result.3$value, NULL )
    expect_false( result.3$visible )

    # read.csv via method
    result.2$value$clear()
    result.4 <- withVisible( result.2$value$read.csv( csvfile ) )
    expect_s4_class( result.4$value, "GGD" )
    expect_identical( result.4$value, result.2$value )
    expect_false( result.4$visible )
    expect_equal_ggd( result.2$value, obj )
    unlink( csvfile )
}

## read.csv/write.csv tests
a$set.cmp( data.frame( mean = 4, sd = 2.5 ), grad = "normal" )
check.read.write.csv( a )

a$set.cmp( data.frame( mean = 4, sd = 2.5 ), this.mix.type = 1 )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )
check.read.write.csv( a )

a$set.cmp( data.frame( mean = 4, sd = 2.5 ), grad = "h" )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
check.read.write.csv( a )

a$set.cmp( data.frame( mean = 4, sd = 2.5 ), grad = "v2" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
check.read.write.csv( a )

a$set.cmp( data.frame( mean = 4, sd = 2.5 ), grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
check.read.write.csv( a )

a$set.cmp( data.frame( mean = 4, sd = 2.5 ), grad = "hv" )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
check.read.write.csv( a )

a$set.cmp( data.frame( mean = c( -2, 1 ), sd = c( 8, 12 ) ), grad = "h" )
check.read.write.csv( a )

a$set.cmp( data.frame( mean = c( -0.423198, 1.71489423 ), sd = c( 0.4123, 4.1532 ) ), grad = "v2" )
check.read.write.csv( a )

a$set.cmp( data.frame( mean = c( 1.2095e-8, -0.000547813, -1.198567e-9 ),
                       sd = c( 0.56789, 1.23456, 2.7890123 ) ), grad = "v3" )
check.read.write.csv( a )

a$set.cmp( data.frame( mean = c( 1.2, 0.987, 1.2 ),
                       sd = c( 1.2356, 1.1978, 1.2356 ) ), grad = "v3" )
check.read.write.csv( a )

a$set.cmp( data.frame( mean = c( 1.912205, -2.12978e-10, -0.095678919356745, 0.09375 ),
                       sd = c( 1.125, 3.875, 2.3125, 0.046875 ) ), grad = "hv" )
check.read.write.csv( a )

## console output tests
expect_output( expect_equal(
    ggd.write.csv( a, "" ), NULL ),
    paste0( '^"4","mean","sd"\n',
            '"n.1.1",1.91220[0-9]+,1.125\n',
            '"n.1.2",-2.1297[0-9]+e-10,3.875\n',
            '"n.2.1",-0.09567891935674[0-9]+,2.3125\n',
            '"n.2.2",0.0937[0-9]+,0.046875$' ) )

expect_output( expect_equal(
    ggd.write.csv( a ), NULL ),
    paste0( '^"4","mean","sd"\n',
            '"n.1.1",1.91220[0-9]+,1.125\n',
            '"n.1.2",-2.1297[0-9]+e-10,3.875\n',
            '"n.2.1",-0.09567891935674[0-9]+,2.3125\n',
            '"n.2.2",0.0937[0-9]+,0.046875$' ) )

a$set.cmp( data.frame( mean = c( -2, 1 ), sd = c( 8, 12 ) ), grad = "h" )
expect_output( expect_equal(
    a$write.csv( "" ), NULL ),
    paste0( '^"2","mean","sd"\n',
            '"n.1",-2,8\n',
            '"n.2",1,12$' ) )

a$set.cmp( data.frame( mean = c( -2, 1 ), sd = c( 8, 12 ) ), grad = "h" )
expect_output( expect_equal(
    a$write.csv(), NULL ),
    paste0( '^"2","mean","sd"\n',
            '"n.1",-2,8\n',
            '"n.2",1,12$' ) )

## cleared object
a$clear()
csvfile <- tempfile( fileext = ".csv" )
ggd.write.csv( a, csvfile )
b <- ggd.read.csv( csvfile )
expect_na_ggd( b )
unlink( csvfile )
rm( b )

csvfile <- tempfile( fileext = ".csv" )
a$write.csv( csvfile )
a$read.csv( csvfile )
expect_na_ggd( a )
unlink( csvfile )

## NA object
csvfile <- tempfile( fileext = ".csv" )
ggd.write.csv( a, csvfile )
b <- ggd.read.csv( csvfile )
expect_na_ggd( b )
unlink( csvfile )
rm( b )

csvfile <- tempfile( fileext = ".csv" )
a$write.csv( csvfile )
a$clear()
a$read.csv( csvfile )
expect_na_ggd( a )
unlink( csvfile )

## Error cases
# no file or bad file format
csvfile <- tempfile( fileext = ".csv" )
expect_warning( expect_error( a$read.csv( csvfile ) ), "No such file or directory" )
expect_cleared( a ); a <- GGD$new()
expect_warning( expect_error( ggd.read.csv( csvfile ) ), "No such file or directory" )

ggd:::cat.table( data.frame( mean = c( 1, 2 ) ), csvfile, "2" )
expect_error( a$read.csv( csvfile ), "File format error" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.read.csv( csvfile ), "File format error" )
unlink( csvfile )

csvfile <- tempfile( fileext = ".csv" )
ggd:::cat.table( data.frame( sd = c( 1, 2 ) ), csvfile, "2" )
expect_error( a$read.csv( csvfile ), "File format error" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.read.csv( csvfile ), "File format error" )
unlink( csvfile )

# invalid header
csvfile <- tempfile( fileext = ".csv" )
ggd:::cat.table( data.frame( sd = c( 1, 2 ), mean = c( 1, 2 ) ), csvfile, "2" )
expect_error( a$read.csv( csvfile ), "Invalid file header" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.read.csv( csvfile ), "Invalid file header" )
unlink( csvfile )

# invalid mix.type
csvfile <- tempfile( fileext = ".csv" )
ggd:::cat.table( data.frame( mean = c( 1, 2 ), sd = c( 1, 2 ) ), csvfile, "-1" )
expect_error( a$read.csv( csvfile ), "The value of mix.type is invalid" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.read.csv( csvfile ), "The value of mix.type is invalid" )
unlink( csvfile )

csvfile <- tempfile( fileext = ".csv" )
ggd:::cat.table( data.frame( mean = c( 1, 2 ), sd = c( 1, 2 ) ), csvfile, "5" )
expect_error( a$read.csv( csvfile ), "The value of mix.type is invalid" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.read.csv( csvfile ), "The value of mix.type is invalid" )
unlink( csvfile )

# not-appropriate mix.type
csvfile <- tempfile( fileext = ".csv" )
ggd:::cat.table( data.frame( mean = c( 1, 2 ), sd = c( 1, 2 ) ), csvfile, "NA" )
expect_error( a$read.csv( csvfile ), "NA for kind or mix.type can be specified only if cmp has no rows" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.read.csv( csvfile ), "NA for kind or mix.type can be specified only if cmp has no rows" )
unlink( csvfile )

csvfile <- tempfile( fileext = ".csv" )
ggd:::cat.table( data.frame( mean = c( 1, 2 ), sd = c( 1, 2 ) ), csvfile, "0" )
expect_error( a$read.csv( csvfile ), "Indicated mix.type is not appropriate for cmp" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.read.csv( csvfile ), "Indicated mix.type is not appropriate for cmp" )
unlink( csvfile )

csvfile <- tempfile( fileext = ".csv" )
ggd:::cat.table( data.frame( mean = c( 1, 2, 2, 3 ), sd = c( 1, 2, 1, 2 ) ), csvfile, "2" )
expect_error( a$read.csv( csvfile ), "Indicated mix.type is not appropriate for cmp" )
expect_cleared( a ); a <- GGD$new()
expect_error( ggd.read.csv( csvfile ), "Indicated mix.type is not appropriate for cmp" )
unlink( csvfile )

# It works, but not recommended.
csvfile <- tempfile( fileext = ".csv" )
ggd:::cat.table( data.frame( mean = c( 1, 2 ), sd = c( 1, 2 ) ), csvfile, "4" )
a$read.csv( csvfile )
expect_match( a$kind, "Mean-Differed Sigma-Differed Horizontal" )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
unlink( csvfile )

# It also works, but not recommended also.
csvfile <- tempfile( fileext = ".csv" )
ggd:::cat.table( data.frame( mean = c( 1, 1, 1 ), sd = c( 2, 2, 2 ) ), csvfile, "0" )
a$read.csv( csvfile )
expect_match( a$kind, "Normal" )
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )
unlink( csvfile )


################################################################################################
## Applying to components
################################################################################################

#### round.cmp
# dg.mean and dg.sd are different
a$set.cmp( data.frame( mean = c( 0.857, 1.264 ),
                         sd = c( 0.938, 1.254 ) ), grad = "h" )
result <- withVisible( a$round.cmp( 2, 1 ) )
expect_false( result$visible )
expect_identical( result$value, a )

expect_identical( a$cmp$mean, c( 0.86, 1.26 ) )
expect_identical( a$cmp$sd,   c( 0.9, 1.3 ) )
b <- ggd.set.cmp( data.frame( mean = c( 0.86, 1.26 ),
                            sd = c( 0.9, 1.3 ) ), grad = "h" )
expect_equal_ggd( a, b )

# only 1st argument is indicated
a$set.cmp( data.frame( mean = c( -19.753, 20.268, 18.767, -10.958 ),
                         sd = c( 105.847, 127.3692, 168.767, 121.983 ) ), grad = "hv" )
a$round.cmp( 2 )
expect_identical( a$cmp$mean, c( -19.75, 20.27, 18.77, -10.96 ) )
expect_identical( a$cmp$sd,   c( 105.85, 127.37, 168.77, 121.98 ) )
b$set.cmp( data.frame( mean = c( -19.75, 20.27, 18.77, -10.96 ),
                         sd = c( 105.85, 127.37, 168.77, 121.98 ) ), grad = "hv" )
expect_equal_ggd( a, b )

# default, and it is transformed to a horizontal gradation.
a$set.cmp( data.frame( mean = c( 19.253, 19.868, 18.767, 20.197 ),
                         sd = c( 102.847, 94.732, 103.049, 95.382 ) ), grad = "hv" )
a$round.cmp()
expect_identical( a$cmp$mean, c( 19, 20, 19, 20 ) )
expect_identical( a$cmp$sd,   c( 103, 95, 103, 95 ) )
b$set.cmp( data.frame( mean = c( 19, 20, 19, 20 ),
                         sd = c( 103, 95, 103, 95 ) ), grad = "hv" )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 3 )
expect_identical( a$cmp$mean, c( 19, 20 ) )
expect_identical( a$cmp$sd,   c( 103, 95 ) )
rm( b )

# it is transformed to a normal distribution.
a$set.cmp( data.frame( mean = c( 19.753, 20.268 ),
                         sd = c( 105.847, 106.368 ) ), this.mix.type = 1 )
a$round.cmp()
expect_equal( a$mix.type, 1 )
expect_identical( a$cmp$mean, c( 20, 20 ) )
expect_identical( a$cmp$sd, c( 106, 106 ) )
b <- ggd.set.cmp( data.frame( mean = c( 20, 20 ),
                          sd = c( 106, 106 ) ), mix.type = 1 )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 20 )
expect_identical( a$cmp$sd,   106 )


a$set.cmp( data.frame( mean = c( 19.753, 20.268 ),
                         sd = c( 105.847, 106.368 ) ), this.mix.type = 2 )
a$round.cmp()
expect_equal( a$mix.type, 2 )
expect_identical( a$cmp$mean, c( 20, 20 ) )
expect_identical( a$cmp$sd, c( 106, 106 ) )
b$set.cmp( data.frame( mean = c( 20, 20 ),
                         sd = c( 106, 106 ) ), this.mix.type = 2 )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 20 )
expect_identical( a$cmp$sd,   106 )


a$set.cmp( data.frame( mean = c( 19.753, 20.268 ),
                         sd = c( 105.847, 106.368 ) ), grad = "v2" )
a$round.cmp()
expect_equal( a$mix.type, 3 )
expect_identical( a$cmp$mean, c( 20, 20 ) )
expect_identical( a$cmp$sd, c( 106, 106 ) )
b$set.cmp( data.frame( mean = c( 20, 20 ),
                         sd = c( 106, 106 ) ), grad = "v2" )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 20 )
expect_identical( a$cmp$sd,   106 )


a$set.cmp( data.frame( mean = c( 19.753, 20.268, 19.839 ),
                         sd = c( 105.847, 106.368, 105.678 ) ), grad = "v3" )
a$round.cmp()
expect_equal( a$mix.type, 3 )
expect_identical( a$cmp$mean, c( 20, 20, 20 ) )
expect_identical( a$cmp$sd, c( 106, 106, 106 ) )
b$set.cmp( data.frame( mean = c( 20, 20, 20 ),
                         sd = c( 106, 106, 106 ) ), grad = "v3" )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 20 )
expect_identical( a$cmp$sd,   106 )


a$set.cmp( data.frame( mean = c( 19.753, 20.268, 19.755, 20.269 ),
                         sd = c( 105.847, 106.368, 105.845, 106.369 ) ), grad = "hv" )
a$round.cmp()
expect_equal( a$mix.type, 4 )
expect_identical( a$cmp$mean, c( 20, 20, 20, 20 ) )
expect_identical( a$cmp$sd, c( 106, 106, 106, 106 ) )
b$set.cmp( data.frame( mean = c( 20, 20, 20, 20 ),
                         sd = c( 106, 106, 106, 106 ) ), grad = "hv" )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 20 )
expect_identical( a$cmp$sd,   106 )
rm( b )

# only 2nd argument is indicated, negative value
a$set.cmp( data.frame( mean = c( -19.753, 20.268, 18.767, -10.958 ),
                         sd = c( 105.844, 127.3628, 168.764, 121.986 ) ), grad = "hv" )
a$round.cmp( dg.sd = -1 )
expect_identical( a$cmp$mean, c( -20, 20, 19, -11 ) )
expect_identical( a$cmp$sd,   c( 110, 130, 170, 120 ) )

# long digit
a$set.cmp( data.frame( mean = c( 0.85741781236485, 1.26789314678164 ),
                         sd = c( 0.93346819023648, 1.25321890471287 ) ), grad = "h" )
a$round.cmp( 10, 11 )
expect_identical( a$cmp$mean, c( 0.8574178124,  1.2678931468 ) )
expect_identical( a$cmp$sd,   c( 0.93346819024, 1.25321890471 ) )

# NULL or integer() is indicated
a$set.cmp( data.frame( mean = c( 0.4121235325156, 1.556712355634 ),
                         sd = c( 0.6144324355615, 1.413623515512 ) ), grad = "h" )
b <- a$copy()
a$round.cmp( 3, NULL )
expect_identical( a$cmp$mean, c( 0.412, 1.557 ) )
expect_true( all( a$cmp$mean != b$cmp$mean ) )
expect_identical( a$cmp$sd, b$cmp$sd )

a <- b$copy()
a$round.cmp( NULL, 3 )
expect_identical( a$cmp$mean, b$cmp$mean )
expect_identical( a$cmp$sd, c( 0.614, 1.414 ) )
expect_true( all( a$cmp$sd != b$cmp$sd ) )

a <- b$copy()
a$round.cmp( NULL )
expect_equal_ggd( a, b )

a <- b$copy()
a$round.cmp( 3, integer() )
expect_identical( a$cmp$mean, c( 0.412, 1.557 ) )
expect_true( all( a$cmp$mean != b$cmp$mean ) )
expect_identical( a$cmp$sd, b$cmp$sd )

a <- b$copy()
a$round.cmp( integer(), 3 )
expect_identical( a$cmp$mean, b$cmp$mean )
expect_identical( a$cmp$sd, c( 0.614, 1.414 ) )
expect_true( all( a$cmp$sd != b$cmp$sd ) )

a <- b$copy()
a$round.cmp( integer() )
expect_equal_ggd( a, b )
rm( b )

# vectors with length > 1 are indicated
a$set.cmp( data.frame( mean = c( 2.818943789, 2.214470148, 2.894672837, 2.98471434 ),
                         sd = c( 1.781436281, 1.981647894, 1.784123519, 1.64968712 ) ) )
b <- a$copy()
a$round.cmp( 1:4, c( 2, 3 ) )
expect_identical( a$cmp$mean, c( 2.8, 2.21, 2.895, 2.9847 ) )
expect_identical( a$cmp$sd, c( 1.78, 1.982, 1.78, 1.650 ) )

a <- b$copy()
a$round.cmp( c( 3, 2 ), 1:4 )
expect_identical( a$cmp$mean, c( 2.819, 2.21, 2.895, 2.98 ) )
expect_identical( a$cmp$sd, c( 1.8, 1.98, 1.784, 1.6497 ) )

a <- b$copy()
a$round.cmp( 1:3, 2:4 )
expect_identical( a$cmp$mean, c( 2.8, 2.21, 2.895, 3.0 ) )
expect_identical( a$cmp$sd, c( 1.78, 1.982, 1.7841, 1.65 ) )

a <- b$copy()
a$round.cmp( 1:4 )
expect_identical( a$cmp$mean, c( 2.8, 2.21, 2.895, 2.9847 ) )
expect_identical( a$cmp$sd, c( 1.8, 1.98, 1.784, 1.6497 ) )

# Error case
a <- b$copy()
expect_error( a$round.cmp( list( 1, 2, 3 ) ) )
expect_equal_ggd( a, b )

a <- b$copy()
expect_error( a$round.cmp( 3, list( 1, 2, 3 ) ) )
expect_equal_ggd( a, b )
rm( b )

a$set.cmp( data.frame( mean = c( 0.572, 1.069 ), sd = c( 0.046, 0.912 ) ) )
expect_error( a$round.cmp( 1 ), "sd must be positive" )

#### signif.cmp
# dg.mean and dg.sd are different
a$set.cmp( data.frame( mean = c( 0.857, 1.264 ),
                         sd = c( 0.938, 1.254 ) ), grad = "h" )
result <- withVisible( a$signif.cmp( 2, 1 ) )
expect_false( result$visible )
expect_identical( result$value, a )

expect_identical( a$cmp$mean, c( 0.86, 1.3 ) )
expect_identical( a$cmp$sd,   c( 0.9, 1 ) )
b <- ggd.set.cmp( data.frame( mean = c( 0.86, 1.3 ),
                            sd = c( 0.9, 1 ) ), grad = "h" )
expect_equal_ggd( a, b )

a$set.cmp( data.frame( mean = c( 0.572, 1.069 ), sd = c( 0.046, 0.912 ) ) )
a$signif.cmp( 2, 1 )
expect_identical( a$cmp$mean, c( 0.57, 1.1 ) )
expect_identical( a$cmp$sd,   c( 0.05, 0.9 ) )
b <- ggd.set.cmp( data.frame( mean = c( 0.57, 1.1 ),
                            sd = c( 0.05, 0.9 ) ), grad = "h" )
expect_equal_ggd( a, b )

# it is transformed to a normal distribution.
a$set.cmp( data.frame( mean = c( 0.897, 0.922 ),
                         sd = c( 1.628, 1.576 ) ), this.mix.type = 1 )
a$signif.cmp( 1, 2 )
expect_equal( a$mix.type, 1 )
expect_identical( a$cmp$mean, c( 0.9, 0.9 ) )
expect_identical( a$cmp$sd,   c( 1.6, 1.6 ) )
b <- ggd.set.cmp( data.frame( mean = c( 0.9, 0.9 ),
                            sd = c( 1.6, 1.6 ) ), mix.type = 1 )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 0.9 )
expect_identical( a$cmp$sd,   1.6 )


a$set.cmp( data.frame( mean = c( 0.897, 0.922 ),
                         sd = c( 1.628, 1.576 ) ), grad = "h" )
a$signif.cmp( 1, 2 )
expect_equal( a$mix.type, 2 )
expect_identical( a$cmp$mean, c( 0.9, 0.9 ) )
expect_identical( a$cmp$sd,   c( 1.6, 1.6 ) )
b <- ggd.set.cmp( data.frame( mean = c( 0.9, 0.9 ),
                            sd = c( 1.6, 1.6 ) ), grad = "h" )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 0.9 )
expect_identical( a$cmp$sd,   1.6 )


a$set.cmp( data.frame( mean = c( 0.897, 0.922 ),
                         sd = c( 1.628, 1.576 ) ), grad = "v2" )
a$signif.cmp( 1, 2 )
expect_equal( a$mix.type, 3 )
expect_identical( a$cmp$mean, c( 0.9, 0.9 ) )
expect_identical( a$cmp$sd,   c( 1.6, 1.6 ) )
b <- ggd.set.cmp( data.frame( mean = c( 0.9, 0.9 ),
                            sd = c( 1.6, 1.6 ) ), grad = "v2" )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 0.9 )
expect_identical( a$cmp$sd,   1.6 )

a$set.cmp( data.frame( mean = c( 0.897, 0.922, 0.878 ),
                         sd = c( 1.628, 1.576, 1.569 ) ), grad = "v3" )
a$signif.cmp( 1, 2 )
expect_equal( a$mix.type, 3 )
expect_identical( a$cmp$mean, c( 0.9, 0.9, 0.9 ) )
expect_identical( a$cmp$sd,   c( 1.6, 1.6, 1.6 ) )
b <- ggd.set.cmp( data.frame( mean = c( 0.9, 0.9, 0.9 ),
                            sd = c( 1.6, 1.6, 1.6 ) ), grad = "v3" )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 0.9 )
expect_identical( a$cmp$sd,   1.6 )

a$set.cmp( data.frame( mean = c( 0.897, 0.922, 0.878, 0.937 ),
                         sd = c( 1.628, 1.576, 1.569, 1.618 ) ), grad = "hv" )
a$signif.cmp( 1, 2 )
expect_equal( a$mix.type, 4 )
expect_identical( a$cmp$mean, c( 0.9, 0.9, 0.9, 0.9 ) )
expect_identical( a$cmp$sd,   c( 1.6, 1.6, 1.6, 1.6 ) )
b <- ggd.set.cmp( data.frame( mean = c( 0.9, 0.9, 0.9, 0.9 ),
                            sd = c( 1.6, 1.6, 1.6, 1.6 ) ), grad = "hv" )
expect_equal_ggd( a, b )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 0.9 )
expect_identical( a$cmp$sd,   1.6 )

# only 1st argument is indicated
a$set.cmp( data.frame( mean = c( -19.753, 20.268, 18.767, -10.958 ),
                         sd = c( 105.847, 127.3692, 168.7676, 121.983 ) ), grad = "hv" )
a$signif.cmp( 2 )
expect_identical( a$cmp$mean, c( -20, 20, 19, -11 ) )
expect_identical( a$cmp$sd,   c( 110, 130, 170, 120 ) )
b$set.cmp( data.frame( mean = c( -20, 20, 19, -11 ),
                         sd = c( 110, 130, 170, 120 ) ), grad = "hv" )
expect_equal_ggd( a, b )
rm( b )

# default
a$set.cmp( data.frame( mean = c( 19.253544, 19.868484, 18.767269, 20.197321 ),
                         sd = c( 102.84743, 94.732672, 103.04978, 95.382467 ) ), grad = "hv" )
a$signif.cmp()
expect_identical( a$cmp$mean, c( 19.2535, 19.8685, 18.7673, 20.1973 ) )
expect_identical( a$cmp$sd,   c( 102.847, 94.7327, 103.050, 95.3825 ) )
b <- ggd.set.cmp( data.frame( mean = c( 19.2535, 19.8685, 18.7673, 20.1973 ),
                            sd = c( 102.847, 94.7327, 103.050, 95.3825 ) ), grad = "hv" )
expect_equal_ggd( a, b )
rm( b )

# only 2nd argument is indicated
a$set.cmp( data.frame( mean = c( -19.753, 20.26858, 18.7671258, -10.958 ),
                         sd = c( 105.844, 127.3628, 168.764, 121.986 ) ), grad = "hv" )
a$signif.cmp( dg.sd = 2 )
expect_identical( a$cmp$mean, c( -19.753, 20.2686, 18.7671, -10.958 ) )
expect_identical( a$cmp$sd,   c( 110, 130, 170, 120 ) )

# long digit
a$set.cmp( data.frame( mean = c( 0.85741781236485, 1.26789314678164 ),
                         sd = c( 0.93346819023648, 1.25321890471287 ) ), grad = "h" )
a$signif.cmp( 10, 11 )
expect_identical( a$cmp$mean, c( 0.8574178124,  1.267893147 ) )
expect_identical( a$cmp$sd,   c( 0.93346819024, 1.2532189047 ) )

# NULL or integer() is indicated
a$set.cmp( data.frame( mean = c( 0.4121235325156, 1.556712355634 ),
                         sd = c( 0.6144324355615, 1.413623515512 ) ), grad = "h" )
b <- a$copy()
a$signif.cmp( 3, NULL )
expect_identical( a$cmp$mean, c( 0.412, 1.56 ) )
expect_true( all( a$cmp$mean != b$cmp$mean ) )
expect_identical( a$cmp$sd, b$cmp$sd )

a <- b$copy()
a$signif.cmp( NULL, 3 )
expect_identical( a$cmp$mean, b$cmp$mean )
expect_identical( a$cmp$sd, c( 0.614, 1.41 ) )
expect_true( all( a$cmp$sd != b$cmp$sd ) )

a <- b$copy()
a$signif.cmp( NULL )
expect_equal_ggd( a, b )

a <- b$copy()
a$signif.cmp( 3, integer() )
expect_identical( a$cmp$mean, c( 0.412, 1.56 ) )
expect_true( all( a$cmp$mean != b$cmp$mean ) )
expect_identical( a$cmp$sd, b$cmp$sd )

a <- b$copy()
a$signif.cmp( integer(), 3 )
expect_identical( a$cmp$mean, b$cmp$mean )
expect_identical( a$cmp$sd, c( 0.614, 1.41 ) )
expect_true( all( a$cmp$sd != b$cmp$sd ) )

a <- b$copy()
a$signif.cmp( integer() )
expect_equal_ggd( a, b )
rm( b )

# vectors with length > 1 are indicated
a$set.cmp( data.frame( mean = c( 2.818943789, 2.214470148, 2.894672837, 2.98471434 ),
                         sd = c( 1.781436281, 1.981647894, 1.784123519, 1.64968712 ) ) )
b <- a$copy()
a$signif.cmp( 1:4, c( 2, 3 ) )
expect_identical( a$cmp$mean, c( 3, 2.2, 2.89, 2.985 ) )
expect_identical( a$cmp$sd, c( 1.8, 1.98, 1.8, 1.65 ) )

a <- b$copy()
a$signif.cmp( c( 3, 2 ), 1:4 )
expect_identical( a$cmp$mean, c( 2.82, 2.2, 2.89, 3.0 ) )
expect_identical( a$cmp$sd, c( 2, 2.0, 1.78, 1.650 ) )

a <- b$copy()
a$signif.cmp( 1:3, 2:4 )
expect_identical( a$cmp$mean, c( 3, 2.2, 2.89, 3 ) )
expect_identical( a$cmp$sd, c( 1.8, 1.98, 1.784, 1.6 ) )

a <- b$copy()
a$signif.cmp( 1:4 )
expect_identical( a$cmp$mean, c( 3, 2.2, 2.89, 2.985 ) )
expect_identical( a$cmp$sd, c( 2, 2.0, 1.78, 1.650 ) )

# Error case
a <- b$copy()
expect_error( a$signif.cmp( list( 1, 2, 3 ) ) )
expect_equal_ggd( a, b )

a <- b$copy()
expect_error( a$signif.cmp( 3, list( 1, 2, 3 ) ) )
expect_equal_ggd( a, b )
rm( b )

#### apply.cmp
## both f.mean and f.sd are indicated
# simple calculation
a$set.cmp( data.frame( mean = c( -1, 0, 1 ),
                         sd = c( 1.5, 1, 2 ) ), grad = "v3" )
result <- withVisible( a$apply.cmp( function( mean ) mean + 1, function( sd ) sd + 0.5 ) )
expect_false( result$visible )
expect_identical( result$value, a )

expect_identical( a$cmp$mean, c( 0, 1, 2 ) )
expect_identical( a$cmp$sd,   c( 2, 1.5, 2.5 ) )
b <- ggd.set.cmp( a$cmp, grad = "v3" )
expect_equal_ggd( a, b )


a$apply.cmp( function( ... ) { argl <- list( ... ); argl[[1]] + 2 },
             function( ... ) { argl <- list( ... ); argl[[1]] - 0.5 } )
expect_identical( a$cmp$mean, c( 2, 3, 4 ) )
expect_identical( a$cmp$sd,   c( 1.5, 1, 2 ) )


a$set.cmp( data.frame( mean = c( -1, 0, 1, 0.5 ),
                         sd = c( 1.25, 0.5, 1.5, 1 ) ), grad = "hv" )
result <- withVisible( a$apply.cmp( function( mean ) mean + 1, function( sd ) sd + 0.5 ) )
expect_identical( a$cmp$mean, c( 0, 1, 2, 1.5 ) )
expect_identical( a$cmp$sd,   c( 1.75, 1, 2, 1.5 ) )
b <- ggd.set.cmp( a$cmp, grad = "hv" )
expect_equal_ggd( a, b )
rm( b )


a$set.cmp( data.frame( mean = c( -1, 0, 1 ),
                         sd = c( 2, 1, 2 ) ), grad = "v3" )
a$apply.cmp( function( mean ) { mean + 1:3 }, function( sd ) { sd + 1:3 * 0.5 } )
expect_identical( a$cmp$mean, c( 0, 2, 4 ) )
expect_identical( a$cmp$sd,   c( 2.5, 2, 3.5 ) )


a$set.cmp( data.frame( mean = c( 1, 0.5, -0.5 ),
                         sd = c( 1.25, 2, 1.5 ) ), grad = "v3" )
a$apply.cmp(
    function( mean )
    {
        mean[1] <- mean[1] - 1.5
        mean
    },
    function( sd )
    {
        sd[2] <- sd[2] - 0.5
        sd[3] <- sd[3] - 0.25
        sd
    } )
expect_identical( a$cmp$mean, c( -0.5, 0.5, -0.5 ) )
expect_identical( a$cmp$sd,   c( 1.25, 1.5, 1.25 ) )
expect_equal( a$kind, "2-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )

a$adjust.cmp()
expect_equal( a$mix.type, 3 )
expect_identical( a$cmp$mean, c( -0.5, 0.5 ) )
expect_identical( a$cmp$sd,   c( 1.25, 1.5 ) )


a$set.cmp( data.frame( mean = c( 1, 0.5, -0.5, 1.5 ),
                         sd = c( 1.25, 2, 1.5, 2.5 ) ), grad = "hv" )
a$apply.cmp( function( x ) x[4:1], function( x ) x[1:4] + 2 )
expect_identical( a$cmp$mean, c( 1.5, -0.5, 0.5, 1 ) )
expect_identical( a$cmp$sd,   c( 3.25, 4, 3.5, 4.5 ) )

# it is transformed to a normal distribution.
a$set.cmp( data.frame( mean = c( 0, 1 ), sd = c( 1, 2 ) ), this.mix.type = 1 )
a$apply.cmp( function( mean ) rep( 0.5, 2 ), function( sd ) rep( 1.25, 2 ) )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 1 )
expect_identical( a$cmp$mean, c( 0.5, 0.5 ) )
expect_identical( a$cmp$sd,   c( 1.25, 1.25 ) )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 0.5 )
expect_identical( a$cmp$sd,   1.25 )

a$set.cmp( data.frame( mean = c( 0, 1 ), sd = c( 1, 2 ) ), grad = "h" )
a$apply.cmp( function( mean ) rep( 0.5, 2 ), function( sd ) rep( 1.25, 2 ) )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 2 )
expect_identical( a$cmp$mean, c( 0.5, 0.5 ) )
expect_identical( a$cmp$sd,   c( 1.25, 1.25 ) )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 0.5 )
expect_identical( a$cmp$sd,   1.25 )

a$set.cmp( data.frame( mean = c( 0, 1 ), sd = c( 1, 2 ) ), grad = "v2" )
a$apply.cmp( function( mean ) rep( 0.5, 2 ), function( sd ) rep( 1.25, 2 ) )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 3 )
expect_identical( a$cmp$mean, c( 0.5, 0.5 ) )
expect_identical( a$cmp$sd,   c( 1.25, 1.25 ) )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 0.5 )
expect_identical( a$cmp$sd,   1.25 )

a$set.cmp( data.frame( mean = c( 0, 1 ), sd = c( 1, 2 ) ), grad = "v3" )
a$apply.cmp( function( mean ) rep( 0.5, 3 ), function( sd ) rep( 1.25, 3 ) )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 3 )
expect_identical( a$cmp$mean, c( 0.5, 0.5, 0.5 ) )
expect_identical( a$cmp$sd,   c( 1.25, 1.25, 1.25 ) )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 0.5 )
expect_identical( a$cmp$sd,   1.25 )

a$set.cmp( data.frame( mean = c( 0, 1 ), sd = c( 1, 2 ) ), grad = "hv" )
a$apply.cmp( function( mean ) rep( 0.5, 4 ), function( sd ) rep( 1.25, 4 ) )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 4 )
expect_identical( a$cmp$mean, c( 0.5, 0.5, 0.5, 0.5 ) )
expect_identical( a$cmp$sd,   c( 1.25, 1.25, 1.25, 1.25 ) )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_identical( a$cmp$mean, 0.5 )
expect_identical( a$cmp$sd,   1.25 )

# using GGD object fields
a$set.cmp( data.frame( mean = c( 1, 0.5, 0.25 ),
                         sd = c( 1.25, 2, 1.5 ) ), grad = "v3" )
a$apply.cmp( function( mean, obj ) { obj$cmp$sd }, function( sd, obj ) { obj$cmp$mean } )
expect_identical( a$cmp$mean, c( 1.25, 2, 1.5 ) )
expect_identical( a$cmp$sd,   c( 1, 0.5, 0.25 ) )


a$set.cmp( data.frame( mean = c( 0.25, 0, -0.25 ),
                         sd = c( 2, 1, 2 ) ), grad = "v3" )
expect_true( a$is.symmetric() )
a$apply.cmp(
    function( mean, obj )
    {
        if ( obj$is.symmetric() )
        {
            mean[3] <- mean[3] + 1
        }
        mean
    },
    function( sd, obj )
    {
        if ( obj$is.symmetric() )
        {
            sd[1] <- sd[1] - 0.5
        }
        sd
    } )
expect_identical( a$cmp$mean, c( 0.25, 0, 0.75 ) )
expect_identical( a$cmp$sd,   c( 1.5, 1, 2 ) )


a$set.cmp( data.frame( mean = c( 1, 0.5, 0.25 ),
                         sd = c( 1.25, 2, 1.5 ) ), grad = "v3" )
b <- a$copy()
a$apply.cmp(
    function( mean, ... )
    {
        argl <- list( ... )
        rep( argl[[1]]$mean, 3 )
    },
    function( sd, ... )
    {
        argl <- list( ... )
        rep( argl[[1]]$sd, 3 )
    } )
expect_identical( a$cmp$mean, rep( b$mean, 3 ) )
expect_identical( a$cmp$sd,   rep( b$sd, 3 ) )
expect_identical( a$kind.index, 1L )

## only f.mean is indicated
a$set.cmp( data.frame( mean = c( -1, 0, 1, 0.5 ),
                         sd = c( 1.25, 0.5, 1.5, 1 ) ), grad = "hv" )
a$apply.cmp( f.mean = function( mean ) mean - 1 )
expect_identical( a$cmp$mean, c( -2, -1, 0, -0.5 ) )
expect_identical( a$cmp$sd,   c( 1.25, 0.5, 1.5, 1 ) )
b <- ggd.set.cmp( a$cmp, grad = "hv" )
expect_equal_ggd( a, b )

a$apply.cmp( function( mean ) abs( mean ) )
expect_identical( a$cmp$mean, c( 2, 1, 0, 0.5 ) )
expect_identical( a$cmp$sd,   c( 1.25, 0.5, 1.5, 1 ) )
b <- ggd.set.cmp( a$cmp, grad = "hv" )
expect_equal_ggd( a, b )
rm( b )

## only f.sd is indicated
a$set.cmp( data.frame( mean = c( -1, 0, 1, 0.5 ),
                         sd = c( 1.25, 0.5, 1.5, 1 ) ), grad = "hv" )
a$apply.cmp( f.sd = function( sd ) sd * 2 )
expect_identical( a$cmp$mean, c( -1, 0, 1, 0.5 ) )
expect_identical( a$cmp$sd,   c( 2.5, 1, 3, 2 ) )
b <- ggd.set.cmp( a$cmp, grad = "hv" )
expect_equal_ggd( a, b )
rm( b )

## no function is indicated
a$set.cmp( data.frame( mean = c( -1, 0, 1, 0.5 ),
                         sd = c( 1.25, 0.5, 1.5, 1 ) ), grad = "hv" )
b <- a$copy()
a$apply.cmp()
expect_equal_ggd( a, b )

a$apply.cmp( f.mean = NULL, f.sd = NULL )
a$apply.cmp()
expect_equal_ggd( a, b )
rm( b )

## Error case
a$set.cmp( data.frame( mean = c( -1, 0, 1, 0.5 ),
                         sd = c( 1.25, 0.5, 1.5, 1 ) ), grad = "hv" )
b <- a$copy()
expect_error( a$apply.cmp( function() { 0 }, function() { 1 } ),
              "Function for f.mean must accept 1 or 2 arguments" )
expect_error( a$apply.cmp( function( x, y, z ) { sqrt( x^2 + y^2 + z^2 ) }, function() { 1 } ),
              "Function for f.mean must accept 1 or 2 arguments" )

expect_error( a$apply.cmp( function( mean ) { mean + 1 }, function() { 1 } ),
              "Function for f.sd must accept 1 or 2 arguments" )
expect_error( a$apply.cmp( function( mean ) { mean + 1 }, function( x, y, z ) { sqrt( x^2 + y^2 + z^2 ) } ),
              "Function for f.sd must accept 1 or 2 arguments" )

expect_error( a$apply.cmp( function( mean ) { NULL } ),
              "Length of value of f.mean is different from the input vector" )
expect_error( a$apply.cmp( function( mean ) { mean[1:3] } ),
              "Length of value of f.mean is different from the input vector" )
expect_error( a$apply.cmp( function( mean ) { c( mean, Inf ) } ),
              "Length of value of f.mean is different from the input vector" )
expect_error( a$apply.cmp( function( mean ) { mean * 2 }, function( sd ) { NULL } ),
              "Length of value of f.sd is different from the input vector" )
expect_error( a$apply.cmp( function( mean ) { mean * 2 }, function( sd ) { sd[1:3] } ),
              "Length of value of f.sd is different from the input vector" )
expect_error( a$apply.cmp( function( mean ) { mean + 2 }, function( sd ) { c( sd, 10 ) } ),
              "Length of value of f.sd is different from the input vector" )

expect_error( a$apply.cmp( function( mean ) { c( 0, 1, 2, Inf ) } ),
              "Elements of cmp must be finite" )
expect_error( a$apply.cmp( function( mean ) { c( 0, 1, 2, -Inf ) } ),
              "Elements of cmp must be finite" )
expect_error( a$apply.cmp( function( mean ) { mean }, function( sd ) { c( 0.5, 1, 2, Inf ) } ),
              "Elements of cmp must be finite" )
expect_error( a$apply.cmp( function( mean ) { mean }, function( sd ) { c( 0.5, 1, 2, 0 ) } ),
              "sd must be positive" )
expect_error( a$apply.cmp( function( mean ) { mean }, function( sd ) { c( 0.5, 1, 2, -1 ) } ),
              "sd must be positive" )
expect_error( a$apply.cmp( function( mean ) { c( 0, 1, 2, NaN ) } ),
              "Rows of cmp must be complete cases" )
expect_error( a$apply.cmp( function( mean ) { mean }, function( sd ) { c( 0.5, 1, NaN, 1 ) } ),
              "Rows of cmp must be complete cases" )
expect_error( a$apply.cmp( function( mean ) { c( 0, 1, 2, NA ) } ),
              "Rows of cmp must be complete cases" )
expect_error( a$apply.cmp( function( mean ) { mean }, function( sd ) { c( 0.5, 1, NA, 1 ) } ),
              "Rows of cmp must be complete cases" )

expect_equal_ggd( a, b )
rm( b )

# Adjusting cmp field in f.mean (or f.sd): never recommended.
a$set.cmp( data.frame( mean = c( 1, 0, 1 ),
                         sd = c( 1.25, 0.5, 1.25 ) ), grad = "v3" )
expect_true( a$is.v2() )
expect_error( a$apply.cmp(
    function( mean, obj )
    {
        obj$adjust.cmp()
        obj$mean + c( 1, 2 )
    } ) )
#expect_equal( nrow( a$cmp ), 2 )   # It may be expected but don't run it.


################################################################################################
## 1 line writing of methods
################################################################################################
# GGD$new() can connect methods which return a GGD object.
a <- GGD$new()$set.cmp( data.frame( mean = c( 1.203, 1.312 ),
                                      sd = c( 0.969, 1.237 ) ), grad = "v2" )$round.cmp( 2 )
b <- ggd.set.cmp( data.frame( mean = c( 1.20, 1.31 ),
                            sd = c( 0.97, 1.24 ) ), grad = "v2" )
expect_equal_ggd( a, b )

# A reference class can connect methods which return itself.
csvfile <- tempfile( fileext = ".csv" )
a$set.cmp( data.frame( mean = c( 0.857, 1.264 ),
                         sd = c( 0.964, 1.237 ) ), grad = "v2" )$
    apply.cmp( function( mean, ... ) { mean + 2 } )$round.cmp( 2 )$adjust.cmp.rownames()$
    adjust.cmp( grad = "v3" )$adjust.kind.index()$adjust.median.mean.sd()$write.csv( csvfile )

b <- ggd.set.cmp( data.frame( mean = c( 2.86, 3.26, 2.86 ),
                            sd = c( 0.96, 1.24, 0.96 ) ), grad = "v3" )
expect_equal_ggd( a, b )

b$clear()
b$read.csv( csvfile )
expect_equal_ggd( a, b )

a$clear()$read.csv( csvfile )$apply.cmp( function( mean, ... ) { mean - 1 } )$round.cmp( 2 )
b$set.cmp( data.frame( mean = c( 1.86, 2.26, 1.86 ),
                         sd = c( 0.96, 1.24, 0.96 ) ), grad = "v3" )
expect_equal_ggd( a, b )
unlink( csvfile )

# nls.freq method can connect other methods using $obj.
csvfile <- tempfile( fileext = ".csv" )
df <- data.frame( x = seq( -2, 2, 0.2 ),
                  freq = c( 156076, 237265, 267947, 309733, 438590, 473217, 482163,
                            627630, 650366, 589660, 695975, 694195, 612552, 674896,
                            658579, 486660, 439381, 351997, 221480, 169332, 136742 ) )
a$nls.freq( df, grad = "h" )$obj$signif.cmp( 3 )$write.csv( csvfile )

b <- GGD$new()
b$nls.freq( df, grad = "h" )
b$signif.cmp( 3 )
expect_equal_ggd( a, b )

b$clear()
b$read.csv( csvfile )
expect_equal_ggd( a, b )
unlink( csvfile )

# trace.q method can connect other methods using $obj.
csvfile <- tempfile( fileext = ".csv" )
qt <- data.frame( x = c( -0.167, 0.2, 1.61 ), p = c( 0.3, 0.5, 0.7 ) )
a$trace.q( qt, grad = "h" )$obj$signif.cmp( 4 )$write.csv( csvfile )

b <- GGD$new()
b$trace.q( qt, grad = "h" )
b$signif.cmp( 4 )
expect_equal_ggd( a, b )

b$clear()
b$read.csv( csvfile )
expect_equal_ggd( a, b )
unlink( csvfile )
rm( b )

