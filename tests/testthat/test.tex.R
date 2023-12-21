################################################################################################
## This file is to test TeX-formatted output with snapshots.
################################################################################################

# For interactive test, load setup.R expressly.
if ( file.exists( "tests/testthat" ) ) source( "tests/testthat/setup.R" )

a <- GGD$new()
tmpfile <- tempfile( fileext = ".txt" )

#### Tests

#### Normal output for each mix.type
a$clear()
a$tex( tmpfile )
expect_error( expect_warning( readLines( tmpfile ), "No such file or directory" ) )

## tex function
# mix.type = 0
test_that( "Normal output: mix.type = 0",
{
    a$set.cmp( data.frame( mean = 1, sd = 0.5 ), this.mix.type = 0 )
    a$tex( tmpfile )
    x.tex <- readLines( tmpfile )
    expect_snapshot( cat( x.tex, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 1
test_that( "Normal output: mix.type = 1",
{
    a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), this.mix.type = 1 )
    a$tex( tmpfile )
    x.tex <- readLines( tmpfile )
    expect_snapshot( cat( x.tex, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 2
test_that( "Normal output: mix.type = 2",
{
    a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), this.mix.type = 2 )
    a$tex( tmpfile )
    x.tex <- readLines( tmpfile )
    expect_snapshot( cat( x.tex, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 3, vgrad.2
test_that( "Normal output: mix.type = 3, vgrad.2",
{
    a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), grad = "v2" )
    a$tex( tmpfile )
    x.tex <- readLines( tmpfile )
    expect_snapshot( cat( x.tex, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 3, vgrad.3
test_that( "Normal output: mix.type = 3, vgrad.3",
{
    a$set.cmp( data.frame( mean = 1:3, sd = 1:3 * 0.5 ), grad = "v3" )
    a$tex( tmpfile )
    x.tex <- readLines( tmpfile )
    expect_snapshot( cat( x.tex, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 4
test_that( "Normal output: mix.type = 4",
{
    a$set.cmp( data.frame( mean = 1:4, sd = 1:4 * 0.5 ), this.mix.type = 4 )
    a$tex( tmpfile )
    x.tex <- readLines( tmpfile )
    expect_snapshot( cat( x.tex, sep = "\n" ) )
    unlink( tmpfile )
    rm( x.tex )
} )


## tex.d function
# mix.type = 0
test_that( "tex.d: mix.type = 0",
{
    a$set.cmp( data.frame( mean = 1, sd = 0.5 ), this.mix.type = 0 )
    a$tex.d( tmpfile )
    x.tex.d <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.d, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 1
test_that( "tex.d: mix.type = 1",
{
    a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), this.mix.type = 1 )
    a$tex.d( tmpfile )
    x.tex.d <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.d, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 2
test_that( "tex.d: mix.type = 2",
{
    a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), this.mix.type = 2 )
    a$tex.d( tmpfile )
    x.tex.d <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.d, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 3, vgrad.2
test_that( "tex.d: mix.type = 3, vgrad.2",
{
    a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), grad = "v2" )
    a$tex.d( tmpfile )
    x.tex.d <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.d, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 3, vgrad.3
test_that( "tex.d: mix.type = 3, vgrad.3",
{
    a$set.cmp( data.frame( mean = 1:3, sd = 1:3 * 0.5 ), grad = "v3" )
    a$tex.d( tmpfile )
    x.tex.d <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.d, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 4
test_that( "tex.d: mix.type = 4",
{
    a$set.cmp( data.frame( mean = 1:4, sd = 1:4 * 0.5 ), this.mix.type = 4 )
    a$tex.d( tmpfile )
    x.tex.d <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.d, sep = "\n" ) )
    unlink( tmpfile )
    rm( x.tex.d )
} )


## tex.p function
# mix.type = 0
test_that( "tex.p: mix.type = 0",
{
    a$set.cmp( data.frame( mean = 1, sd = 0.5 ), this.mix.type = 0 )
    a$tex.p( tmpfile )
    x.tex.p <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.p, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 1
test_that( "tex.p: mix.type = 1",
{
    a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), this.mix.type = 1 )
    a$tex.p( tmpfile )
    x.tex.p <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.p, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 2
test_that( "tex.p: mix.type = 2",
{
    a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), this.mix.type = 2 )
    a$tex.p( tmpfile )
    x.tex.p <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.p, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 3, vgrad.2
test_that( "tex.p: mix.type = 3, vgrad.2",
{
    a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), grad = "v2" )
    a$tex.p( tmpfile )
    x.tex.p <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.p, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 3, vgrad.3
test_that( "tex.p: mix.type = 3, vgrad.3",
{
    a$set.cmp( data.frame( mean = 1:3, sd = 1:3 * 0.5 ), grad = "v3" )
    a$tex.p( tmpfile )
    x.tex.p <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.p, sep = "\n" ) )
    unlink( tmpfile )
} )

# mix.type = 4
test_that( "tex.p: mix.type = 4",
{
    a$set.cmp( data.frame( mean = 1:4, sd = 1:4 * 0.5 ), this.mix.type = 4 )
    a$tex.p( tmpfile )
    x.tex.p <- readLines( tmpfile )
    expect_snapshot( cat( x.tex.p, sep = "\n" ) )
    unlink( tmpfile )
    rm( x.tex.p )
} )


#### Output to console
## Since the purpose of these tests is
## simply to test whether each function can output to the console,
## we do not use snapshots to check the output contents.

## tex function
a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), this.mix.type = 2 )
expect_output( a$tex(), "[\\]begin[{]align[}]\n" )
expect_output( a$tex(), "g.x.[^\n]+[,]..\n" )
expect_output( a$tex(), "[\\]Psi.x.[^\n]+[,]..\n" )

## tex.d function
expect_output( a$tex.d(), "[\\]begin[{]align[}]\n" )
expect_output( a$tex.d(), "g.x.[^\n]+[,]..\n" )
expect_error( expect_output( a$tex.d(), "[\\]Psi.x.[^\n]+[,]..\n" ), "does not match" )

## tex.p function
expect_output( a$tex.p(), "[\\]begin[{]align[}]\n" )
expect_error( expect_output( a$tex.p(), "g.x.[^\n]+[,]..\n" ), "does not match" )
expect_output( a$tex.p(), paste( "[\\]Psi.x.[^\n]+[,]..\n" ) )


#### Changing sep value
# normal tests
test_that( "Changing sep value",
{
    a$set.cmp( data.frame( mean = 1, sd = 0.5 ), this.mix.type = 0 )
    expect_snapshot( a$tex( sep = "--" ) )
    expect_snapshot( a$tex.d( sep = "+++" ) )
    expect_snapshot( a$tex.p( sep = "****" ) )
} )

# Error case
b <- a$copy()
expect_error( a$tex.d( sep = a ) )
expect_equal_ggd( a, b )
rm( b )


#### Changing comma value

################################################################################################
#' Check whether "," and the last "." are output or not according to the comma flag
#'
#' @param   method  A tex family method to be checked.
#' @return  Invisible \code{method}.
################################################################################################
expect_proper_tex_comma_output <- function( method )
{
    # Positions of the commas are checked by snapshot tests.
    # Here just whether "," and the last "." are output is checked.
    expect_output( method( comma = TRUE ), "[\\]begin[{]align[}]\n" )
    expect_output( method( comma = TRUE ), ",[\\]" )
    expect_output( method( comma = TRUE ), "[\\]mu[^=]*=[ \t_0-9.]+," )
    expect_output( method( comma = TRUE ), "[.]\n[ \t]*[\\]end[{]array[}]" )

    expect_output( method( comma = FALSE ), "[\\]begin[{]align[}]\n" )
    expect_error( expect_output(
        method( comma = FALSE ), ",[\\]" ), "does not match" )
    expect_error( expect_output(
        method( comma = FALSE ), "[\\]mu[^=]*=[ \t_0-9.]+," ), "does not match" )
    expect_error( expect_output(
        method( comma = FALSE ), "[.]\n[ \t]*[\\]end[{]array[}]" ), "does not match" )
    invisible( method )
}

# mix.type = 0
a$set.cmp( data.frame( mean = 1, sd = 0.5 ), this.mix.type = 0 )
expect_proper_tex_comma_output( a$tex )
expect_proper_tex_comma_output( a$tex.d )
expect_proper_tex_comma_output( a$tex.p )

# mix.type = 1
a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), this.mix.type = 1 )
expect_proper_tex_comma_output( a$tex )
expect_proper_tex_comma_output( a$tex.d )
expect_proper_tex_comma_output( a$tex.p )

# mix.type = 2
a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), this.mix.type = 2 )
expect_proper_tex_comma_output( a$tex )
expect_proper_tex_comma_output( a$tex.d )
expect_proper_tex_comma_output( a$tex.p )

# mix.type = 3, vgrad.2
a$set.cmp( data.frame( mean = 1:2, sd = 1:2 * 0.5 ), grad = "v2" )
expect_proper_tex_comma_output( a$tex )
expect_proper_tex_comma_output( a$tex.d )
expect_proper_tex_comma_output( a$tex.p )

# mix.type = 3, vgrad.3
a$set.cmp( data.frame( mean = 1:3, sd = 1:3 * 0.5 ), grad = "v3" )
expect_proper_tex_comma_output( a$tex )
expect_proper_tex_comma_output( a$tex.d )
expect_proper_tex_comma_output( a$tex.p )

# mix.type = 4
a$set.cmp( data.frame( mean = 1:4, sd = 1:4 * 0.5 ), this.mix.type = 4 )
expect_proper_tex_comma_output( a$tex )
expect_proper_tex_comma_output( a$tex.d )
expect_proper_tex_comma_output( a$tex.p )

rm( expect_proper_tex_comma_output )


#### Changing format.num function
## The appropriateness of the functions and return values given to format.num is
## left to the user of the package.
## Here, two typical functions are checked for correct application to each number.

# mix.type = 0
a$set.cmp( data.frame( mean = 3.141592653589, sd = 7.932384624433 ), this.mix.type = 0 )
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu[ =]+3[.]14[^0-9]*[\\]sigma[ =]+7[.]93[^0-9]" )                  # 3.14, 7.93

expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu[ =]+3[.]14159[^0-9]*[\\]sigma[ =]+7[.]93238[^0-9]" )            # 3.14159, 7.93238

# mix.type = 1
a$set.cmp( data.frame( mean = c( 3.141592653589, 7.932384624433 ),
                         sd = c( 8.327950288419, 7.169399375105 ) ), this.mix.type = 1 )
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_1[ =]+3[.]14[^0-9]*[\\]sigma_1[ =]+8[.]33[^0-9]" )              # 3.14, 7.93
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_2[ =]+7[.]93[^0-9]*[\\]sigma_2[ =]+7[.]17[^0-9]" )              # 8.33, 7.17

expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_1[ =]+3[.]14159[^0-9].*[\\]sigma_1[ =]+8[.]32795[^0-9]" )       # 3.14159, 8.32795
expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_2[ =]+7[.]93238[^0-9].*[\\]sigma_2[ =]+7[.]16940[^0-9]" )       # 7.93238, 7.16940

# mix.type = 2
a$set.cmp( data.frame( mean = c( 3.141592653589, 7.932384624433 ),
                         sd = c( 8.327950288419, 7.169399375105 ) ), this.mix.type = 2 )
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_1[ =]+3[.]14[^0-9]*[\\]sigma_1[ =]+8[.]33[^0-9]" )              # 3.14, 7.93
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_2[ =]+7[.]93[^0-9]*[\\]sigma_2[ =]+7[.]17[^0-9]" )              # 8.33, 7.17

expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_1[ =]+3[.]14159[^0-9].*[\\]sigma_1[ =]+8[.]32795[^0-9]" )       # 3.14159, 8.32795
expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_2[ =]+7[.]93238[^0-9].*[\\]sigma_2[ =]+7[.]16940[^0-9]" )       # 7.93238, 7.16940

# mix.type = 3, vgrad.2
a$set.cmp( data.frame( mean = c( 3.141592653589, 7.932384624433 ),
                         sd = c( 8.327950288419, 7.169399375105 ) ), this.mix.type = 3 )
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_1[ =]+3[.]14[^0-9]*[\\]sigma_1[ =]+8[.]33[^0-9]" )              # 3.14, 7.93
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_2[ =]+7[.]93[^0-9]*[\\]sigma_2[ =]+7[.]17[^0-9]" )              # 8.33, 7.17

expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_1[ =]+3[.]14159[^0-9].*[\\]sigma_1[ =]+8[.]32795[^0-9]" )       # 3.14159, 8.32795
expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_2[ =]+7[.]93238[^0-9].*[\\]sigma_2[ =]+7[.]16940[^0-9]" )       # 7.93238, 7.16940

# mix.type = 3, vgrad.3
a$set.cmp( data.frame( mean = c( 3.141592653589, 7.932384624433, 8.327950288419 ),
                         sd = c( 7.169399375105, 8.209749445923, 0.781640628620 ) ),
           this.mix.type = 3 )
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_1[ =]+3[.]14[^0-9]*[\\]sigma_1[ =]+7[.]17[^0-9]" )              # 3.14, 7.17
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_2[ =]+7[.]93[^0-9]*[\\]sigma_2[ =]+8[.]21[^0-9]" )              # 7.93, 8.21
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_3[ =]+8[.]33[^0-9]*[\\]sigma_3[ =]+0[.]782[^0-9]" )             # 8.33, 0.782

expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_1[ =]+3[.]14159[^0-9].*[\\]sigma_1[ =]+7[.]16940[^0-9]" )       # 3.14159, 7.16940
expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_2[ =]+7[.]93238[^0-9].*[\\]sigma_2[ =]+8[.]20975[^0-9]" )       # 7.93238, 8.20975
expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_3[ =]+8[.]32795[^0-9].*[\\]sigma_3[ =]+0[.]78164[^0-9]" )       # 8.32795, 0.78164

# mix.type = 4
a$set.cmp( data.frame( mean = c( 3.141592653589, 7.932384624433,
                                 8.327950288419, 7.169399375105 ),
                         sd = c( 8.209749445923, 0.781640628620,
                                 8.998628034825, 3.421170679821 ) ),
           this.mix.type = 4 )
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_[{]1,1[}][ =]+3[.]14[^0-9].*[\\]sigma_[{]1,1[}][ =]+8[.]21[^0-9]" )     # 3.14, 8.21
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_[{]1,2[}][ =]+7[.]93[^0-9].*[\\]sigma_[{]1,2[}][ =]+0[.]782[^0-9]" )    # 7.93, 0.782
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_[{]2,1[}][ =]+8[.]33[^0-9].*[\\]sigma_[{]2,1[}][ =]+9[^0-9]" )          # 8.33, 9
expect_output( a$tex( format.num = function( x ) format( x, digit = 3 ) ),
    "[\\]mu_[{]2,2[}][ =]+7[.]17[^0-9].*[\\]sigma_[{]2,2[}][ =]+3[.]42[^0-9]" )     # 7.17, 3.42

expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_[{]1,1[}][ =]+3[.]14159[^0-9]..*[\\]sigma_[{]1,1[}][ =]+8[.]20975[^0-9]" )  # 3.14159, 8.20975
expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_[{]1,2[}][ =]+7[.]93238[^0-9].*[\\]sigma_[{]1,2[}][ =]+0[.]78164[^0-9]" )   # 7.93238, 0.78164
expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_[{]2,1[}][ =]+8[.]32795[^0-9].*[\\]sigma_[{]2,1[}][ =]+8[.]99863[^0-9]" )   # 8.32795, 8.99863
expect_output( a$tex( format.num = function( x ) sprintf( "%.5f", x ) ),
    "[\\]mu_[{]2,2[}][ =]+7[.]16940[^0-9].*[\\]sigma_[{]2,2[}][ =]+3[.]42117[^0-9]" )   # 7.16940, 3.42117

# -- from author ----------------------------------------------
# Sorry, developers.
# My incidental attempt to use "pi" as sample numbers
# made the test sources a bit difficult to read.
# However, I think the results seem to be pretty good samples.
# -------------------------------------------------------------


#### Changing frac.env value
## Currently, only the tex and tex.p methods are tested.
## Since there is no part which frac.env effects in expressions output by the tex.d method.
test_that( "Changing frac.env value",
{
    a$set.cmp( data.frame( mean = 1:2, sd = 1:2 + 0.5 ), grad = "v2" )
    expect_snapshot( a$tex( frac.env = "array" ) )
    expect_snapshot( a$tex( frac.env = "aligned" ) )
    expect_snapshot( a$tex( frac.env = "gathered" ) )
    expect_snapshot( a$tex( frac.env = "default" ) )
} )

# Error case
expect_error( a$tex( frac.env = "multline" ), "should be one of" )


#### Outputs of eq.mean and eq.sd objects

# mix.type = 1
test_that( "Outputs of eq.mean and eq.sd objects: mix.type = 1",
{
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = 1:2 + 0.5 ), this.mix.type = 1 )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = 1:2, sd = c( 1.5, 1.5 ) ), this.mix.type = 1 )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 1.5, 1.5 ) ), this.mix.type = 1 )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 1.5, 1.5 ) ), this.mix.type = 1 )$tex.d() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 1.5, 1.5 ) ), this.mix.type = 1 )$tex.p() )
} )

# mix.type = 2
test_that( "Outputs of eq.mean and eq.sd objects: mix.type = 2",
{
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = 1:2 + 0.5 ), this.mix.type = 2 )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = 1:2, sd = c( 1.5, 1.5 ) ), this.mix.type = 2 )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 1.5, 1.5 ) ), this.mix.type = 2 )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 1.5, 1.5 ) ), this.mix.type = 2 )$tex.d() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 1.5, 1.5 ) ), this.mix.type = 2 )$tex.p() )
} )

# mix.type = 3, vgrad.2
test_that( "Outputs of eq.mean and eq.sd objects: mix.type = 3, vgrad.2",
{
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = 1:2 + 0.5 ), grad = "v2" )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = 1:2, sd = c( 1.5, 1.5 ) ), grad = "v2" )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 1.5, 1.5 ) ), grad = "v2" )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 1.5, 1.5 ) ), grad = "v2" )$tex.d() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 1.5, 1.5 ) ), grad = "v2" )$tex.p() )
} )

# mix.type = 3, vgrad.3
test_that( "Outputs of eq.mean and eq.sd objects: mix.type = 3, vgrad.3",
{
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1, 1 ), sd = 1:3 + 0.5 ), grad = "v3" )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = 1:3, sd = c( 1.5, 1.5, 1.5 ) ), grad = "v3" )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1, 1 ), sd = c( 1.5, 1.5, 1.5 ) ), grad = "v3" )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1, 1 ), sd = c( 1.5, 1.5, 1.5 ) ), grad = "v3" )$tex.d() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1, 1 ), sd = c( 1.5, 1.5, 1.5 ) ), grad = "v3" )$tex.p() )

    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1, 2 ), sd = c( 1.5, 1.5, 2.5 ) ), grad = "v3" )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 2, 1, 2 ), sd = c( 2.5, 1.5, 2.5 ) ), grad = "v3" )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 2, 1, 1 ), sd = c( 2.5, 1.5, 1.5 ) ), grad = "v3" )$tex() )
} )

# mix.type = 4
test_that( "Outputs of eq.mean and eq.sd objects: mix.type = 4 -- 1/2",
{
    expect_snapshot( a$set.cmp( data.frame( mean = rep( 1, 4 ), sd = 1:4 + 0.5 ), this.mix.type = 4 )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = 1:4, sd = rep( 1.5, 4 ) ), this.mix.type = 4 )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = rep( 1, 4 ), sd = rep( 1.5, 4 ) ), this.mix.type = 4 )$tex() )
    expect_snapshot( a$set.cmp( data.frame( mean = rep( 1, 4 ), sd = rep( 1.5, 4 ) ), this.mix.type = 4 )$tex.d() )
    expect_snapshot( a$set.cmp( data.frame( mean = rep( 1, 4 ), sd = rep( 1.5, 4 ) ), this.mix.type = 4 )$tex.p() )
} )

test_that( "Outputs of eq.mean and eq.sd objects: mix.type = 4 -- 2/2",
{
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1, 2, 2 ), sd = c( 1.5, 1.5, 2.5, 2.5 ) ),
                                this.mix.type = 4 )$tex.p() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 2, 1, 1, 2 ), sd = c( 2.5, 1.5, 1.5, 2.5 ) ),
                                this.mix.type = 4 )$tex.p() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 2, 1, 2 ), sd = c( 1.5, 2.5, 1.5, 2.5 ) ),
                                this.mix.type = 4 )$tex.p() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 2, 2, 2 ), sd = c( 1.5, 2.5, 2.5, 2.5 ) ),
                                this.mix.type = 4 )$tex.p() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 2, 1, 1 ), sd = c( 1.5, 2.5, 1.5, 1.5 ) ),
                                this.mix.type = 4 )$tex.p() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1, 2, 1 ), sd = c( 1.5, 1.5, 2.5, 1.5 ) ),
                                this.mix.type = 4 )$tex.p() )
    expect_snapshot( a$set.cmp( data.frame( mean = c( 1, 1, 1, 2 ), sd = c( 1.5, 1.5, 1.5, 2.5 ) ),
                                this.mix.type = 4 )$tex.p() )
} )

