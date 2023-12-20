################################################################################################
## This file is to test ggd.set.cmp and other functions to adjust fields.
################################################################################################

# For interactive test, load setup.R expressly.
if ( file.exists( "tests/testthat" ) ) source( "tests/testthat/setup.R" )

a <- GGD$new()

################################################################################################
## set.cmp
################################################################################################

## Empty data.frame
a$set.cmp( data.frame( mean = numeric(), sd = numeric() ) )
expect_identical( a$kind.index,     integer() )
expect_identical( a$kind,           character() )
expect_identical( a$mix.type,       integer() )
expect_equal( nrow( a$cmp ), 0 )
expect_identical( a$median,         NaN )
expect_identical( a$mean,           NaN )
expect_identical( a$sd,             NaN )
expect_identical( a$lsd,            NaN )
expect_identical( a$usd,            NaN )
expect_identical( a$lsd.abs.error,  NaN )
expect_identical( a$usd.abs.error,  NaN )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.kind = NA )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.kind = NA_complex_ )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.kind = NA_real_ )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.kind = NA_integer_ )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.kind = NA_character_ )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.kind = NaN )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.kind = character() )
expect_na_ggd( a, 0 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.mix.type = NA )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.mix.type = NA_complex_ )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.mix.type = NA_real_ )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.mix.type= NA_integer_ )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.mix.type = NA_character_ )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.mix.type = NaN )
expect_na_ggd( a, 1 )

a$set.cmp( data.frame( mean = numeric(), sd = numeric() ), this.mix.type = integer() )
expect_na_ggd( a, 0 )


a <- ggd.set.cmp( data.frame( mean = numeric(), sd = numeric() ) )
expect_na_ggd( a, 0 )

a <- ggd.set.cmp( data.frame( mean = numeric(), sd = numeric() ), kind = NaN )
expect_na_ggd( a, 1 )

a <- ggd.set.cmp( data.frame( mean = numeric(), sd = numeric() ), mix.type = NA )
expect_na_ggd( a, 1 )

a <- ggd.set.cmp( data.frame( mean = numeric(), sd = numeric() ), mix.type = NaN )
expect_na_ggd( a, 1 )

## [this.]kind
expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
              this.kind = "Mean-Differed Sigma-Differed Horizontal Gradational Distribution" )$mix.type, 2 )
expect_equal( a$mix.type, 2 )
expect_equal( a$cmp$mean, c( -0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 1, 2 ) )

expect_warning( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                           this.kind = "Horizontal",
                           this.mix.type = 3 ),
                "Indicated kind does not match to the result." )
expect_equal( a$mix.type, 3 )
expect_equal( a$cmp$mean, c( -0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 1, 2 ) )

expect_warning( a <- ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 1 ) ),
                              kind = "Mean-Differed Sigma-Differed Horizontal" ),
                "Indicated kind does not match to the result" )
expect_equal( a$mix.type, 2 )
expect_equal( a$kind, "Mean-Differed Sigma-Equaled Horizontal Gradational Distribution" )
expect_equal( a$cmp$mean, c( -0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 1, 1 ) )

expect_no_warning( a <- ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 1 ) ),
                                     kind = "Horizontal" ) )
expect_equal( a$mix.type, 2 )
expect_equal( a$kind, "Mean-Differed Sigma-Equaled Horizontal Gradational Distribution" )
expect_equal( a$cmp$mean, c( -0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 1, 1 ) )

expect_warning( a <- ggd.set.cmp( data.frame( mean = c( 0.5, 0.5 ), sd = c( 1, 1.5 ) ),
                                  kind = 10 ),
                "Indicated kind does not match to the result" )
expect_equal( a$mix.type, 3 )
expect_equal( a$kind.index, 9 )
expect_equal( a$mix.type, 3 )
expect_equal( a$cmp$mean, c( 0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 1, 1.5 ) )

expect_no_warning( a <- ggd.set.cmp( data.frame( mean = c( 0.5, 0.5 ), sd = c( 1.2, 1.5 ) ),
                                     kind = c( "Horizontal", "Vertical" ) ) )
expect_equal( a$mix.type, 2 )
expect_equal( a$kind, "Mean-Equaled Sigma-Differed Horizontal Gradational Distribution" )
expect_equal( a$cmp$mean, c( 0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 1.2, 1.5 ) )

expect_no_warning( a <- ggd.set.cmp( data.frame( mean = c( 0.5, 0.5 ), sd = c( 1.2, 1.7 ) ),
                                     kind = list( 3, 5, 10 ) ) )
expect_equal( a$mix.type, 1 )
expect_equal( a$kind.index, 3 )
expect_equal( a$cmp$mean, c( 0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 1.2, 1.7 ) )

expect_no_warning(
    b <- ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 1.5 ) ), kind = a ) )
expect_equal( b$kind.index, 4 )
expect_equal( b$kind.index == a$kind.index, FALSE )
expect_equal( b$mix.type, a$mix.type )
expect_equal( b$cmp$mean, c( -0.5, 0.5 ) )
expect_equal( b$cmp$sd, c( 1, 1.5 ) )

a$clear()
a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 2, 3 ) ), this.kind = b )
expect_equal( a$mix.type, 1 )
expect_equal( a$kind.index, b$kind.index )
expect_equal( a$cmp$mean, c( -0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 2, 3 ) )

rm( b )

a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 2, 3 ) ), this.kind = a )
expect_equal( a$mix.type, 1 )
expect_equal( a$kind, "Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions" )
expect_equal( a$cmp$mean, c( 1, 1 ) )
expect_equal( a$cmp$sd, c( 2, 3 ) )


## [this.]mix.type
expect_equal( a$set.cmp( data.frame( mean = c( 0.5, 0.5 ), sd = c( 2, 2 ) ), this.mix.type = 0 )$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )

expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), this.mix.type = 1 )$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )
expect_false( a$is.h() )
expect_false( a$is.v2() )
expect_false( a$is.v3() )
expect_false( a$is.hv() )

expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), this.mix.type = 2 )$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.h() )
expect_false( a$is.v2() )
expect_false( a$is.v3() )
expect_true( a$is.hv() )

expect_equal( a$set.cmp( this.mix.type = 1 )$mix.type, 1 )  # retaining cmp and changing mix.type field.
expect_equal( nrow( a$cmp ), 2 )
expect_equal(a$cmp$mean, c( -0.5, 0.5 ) )
expect_equal(a$cmp$sd, c( 1, 2 ) )
expect_false( a$is.h() )
expect_false( a$is.v2() )
expect_false( a$is.v3() )
expect_false( a$is.hv() )

expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), this.mix.type = 3 )$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_false( a$is.h() )
expect_true( a$is.v2() )
expect_true( a$is.v3() )
expect_true( a$is.hv() )

expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0.5, -0.5 ), sd = c( 1, 2, 1 ) ),
                         this.mix.type = 3 )$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_equal( a$cmp$mean, c( -0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 1, 2 ) )
expect_false( a$is.h() )
expect_true( a$is.v2() )
expect_true( a$is.v3() )
expect_true( a$is.hv() )

# When this.mix.type = 4 for 2-rows data.frame, previous mix.type affects to construct cmp field.
# This case is virtical gradation, follows previous mix.type = 3.
expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), this.mix.type = 4 )$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_false( a$is.h() )
expect_true( a$is.v2() )
expect_true( a$is.v3() )
expect_true( a$is.hv() )

# When this.mix.type = 4 for 2-rows data.frame, previous mix.type affects to construct cmp field.
# This case is horizontal gradation.
expect_equal( a$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1, 2 ) ), this.mix.type = 2 )$mix.type, 2 )
expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), this.mix.type = 4 )$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.h() )
expect_false( a$is.v2() )
expect_false( a$is.v3() )
expect_true( a$is.hv() )

expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0, 0.5 ), sd = c( 1, 1.5, 2 ) ) )$mix.type, 3 )
expect_false( a$is.h() )
expect_false( a$is.v2() )
expect_true( a$is.v3() )
expect_false( a$is.hv() )

expect_equal( a$set.cmp( data.frame( mean = c( -0.5, -0.5, 0.5, 0.5 ), sd = c( 1, 1.5, 2, 2.5 ) ) )$mix.type, 4 )
expect_false( a$is.h() )
expect_false( a$is.v2() )
expect_false( a$is.v3() )
expect_true( a$is.hv() )

expect_equal( ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), mix.type = 1 )$mix.type, 1 )

## grad
a <- ggd.set.cmp( data.frame( mean = 1, sd = 0.5 ), grad = "normal" )
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )

a$clear()
expect_equal( a$set.cmp( data.frame( mean = c( -0.5 ), sd = c( 1 ) ), grad = "normal",
                         this.mix.type = 2 )$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )

a$clear()
expect_equal( a$set.cmp( data.frame( mean = c( -0.5, -0.5 ), sd = c( 1, 1 ) ), grad = "no",
                         this.mix.type = 3 )$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )

a <- ggd.set.cmp( data.frame( mean = 1, sd = 0.5 ), grad = "h" )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )

a$clear()
expect_equal( a$set.cmp( data.frame( mean = c( -0.5 ), sd = c( 1 ) ), grad = "h",
                         this.mix.type = 1 )$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )

a <- ggd.set.cmp( data.frame( mean = 1, sd = 0.5 ), grad = "v2" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )

a$clear()
expect_warning(
        a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), grad = "v2",
                   this.kind = 7,
                   this.mix.type = 4 ),
        "Indicated kind does not match to the result" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )

a <- ggd.set.cmp( data.frame( mean = 1, sd = 0.5 ), grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )

a$clear()
expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), grad = "v3" )$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_equal( a$cmp$mean, c( -0.5, 0.5, -0.5 ) )
expect_equal( a$cmp$sd, c( 1, 2, 1 ) )

# set mix.type = 4 where current mix.type = 3.
expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), grad = "hv" )$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_equal( a$cmp$mean, c( -0.5, 0.5, -0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 1, 2, 1, 2 ) )
expect_false( a$is.h() )
expect_true( a$is.v2() )

# set mix.type = 4 for cleared object.
a$clear()
expect_equal( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), grad = "hv" )$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_equal( a$cmp$mean, c( -0.5, -0.5, 0.5, 0.5 ) )
expect_equal( a$cmp$sd, c( 1, 1, 2, 2 ) )
expect_true( a$is.h() )
expect_false( a$is.v2() )

a <- ggd.set.cmp( data.frame( mean = 1, sd = 0.5 ), grad = "hv" )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )

expect_equal( a$set.cmp( data.frame( mean = c( -0.5 ), sd = c( 1 ) ), grad = "h" )$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )

a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), grad = "v" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )

a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )

a$set.cmp( data.frame( mean = rep( 0, 4 ), sd = rep( 1, 4 ) ), grad = "h" )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )

a$set.cmp( data.frame( mean = rep( 0, 4 ), sd = rep( 1, 4 ) ), grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )


## Basic Errors
b <- a$copy()   # take a backup
expect_error( ggd.set.cmp( numeric() ), "cmp must be a data frame" )
expect_error( a$set.cmp( numeric() ), "cmp must be a data frame" )

expect_error( a$set.cmp( data.frame( mean = rep( 0, 2 ), mean.2 = rep( 1, 2 ), sd = rep( 1, 2 ) ) ),
              "cmp must have 2 columns named 'mean' and 'sd'" )
expect_error( ggd.set.cmp( data.frame( mean.2 = rep( 1, 2 ), sd = rep( 1, 2 ) ) ),
              "cmp must have 2 columns named 'mean' and 'sd'" )
expect_error( ggd.set.cmp( data.frame( mean = rep( 1, 2 ), sd.2 = rep( 1, 2 ) ) ),
              "cmp must have 2 columns named 'mean' and 'sd'" )

expect_error( a$set.cmp( data.frame( mean = 1:5, sd = rep( 1, 5 ) ) ),
              "The number of cmp rows is too large" )

expect_error( a$set.cmp( data.frame( mean = 1:4, sd = c( "a", "b", "c", "Deck the halls" ) ) ),
              "Elements of cmp must be numeric" )
expect_error( ggd.set.cmp( data.frame( mean = c( "H", "e", "ll", "o" ), sd = 1:4 ) ),
              "Elements of cmp must be numeric" )

expect_error( a$set.cmp( data.frame( mean = 1:4, sd = c( 1, NA, 3, 4 ) ) ),
              "Rows of cmp must be complete cases" )
expect_error( ggd.set.cmp( data.frame( mean = c( NaN, 2, 3, 4 ), sd = 1:4 ) ),
              "Rows of cmp must be complete cases" )

expect_error( a$set.cmp( data.frame( mean = 1:4, sd = c( 1, 1.2, 1.3, Inf ) ) ),
              "Elements of cmp must be finite" )
expect_error( ggd.set.cmp( data.frame( mean = c( -Inf, 0, 1, 2 ), sd = 1:4 ) ),
              "Elements of cmp must be finite" )

expect_error( a$set.cmp( data.frame( mean = 1:4, sd = c( -1, 1, 2, 3 ) ) ),
              "sd must be positive" )
expect_error( ggd.set.cmp( data.frame( mean = 1:4, sd = 0:3 ) ),
              "sd must be positive" )


expect_error( a$set.cmp( data.frame( mean = 0, sd = 1 ), this.kind = NA ),
              "NA for kind or mix.type can be specified only if cmp has no rows" )
expect_error( a$set.cmp( data.frame( mean = 0, sd = 1 ), this.mix.type = NA ),
              "NA for kind or mix.type can be specified only if cmp has no rows" )
expect_error( ggd.set.cmp( data.frame( mean = 0, sd = 1 ), kind = NA ),
              "NA for kind or mix.type can be specified only if cmp has no rows" )
expect_error( ggd.set.cmp( data.frame( mean = 0, sd = 1 ), mix.type = NA ),
              "NA for kind or mix.type can be specified only if cmp has no rows" )


expect_equal( ggd.kind.index( c( "Poisson Distribution", "Random Distribution",
                                 "Normal Distribution", NULL ) ),
              c( NA, NA, 1 ) )

expect_error( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                         this.kind = "Poisson Distribution" ),
              "'Poisson Distribution' does not match any character strings of kinds" )

expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                           kind = "Random Distribution" ),
              "'Random Distribution' does not match any character strings of kinds" )

expect_error( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                         this.kind = -1 ),
              "kind for index -1 is undefined" )

expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                           kind = 17 ),
              "kind for index 17 is undefined" )

expect_error( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                         this.mix.type = -1 ),
              "mix.type -1 is undefined" )

expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                           mix.type = 5 ),
              "mix.type 5 is undefined" )

expect_error( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                         grad = "aaaaa" ), "should be one of" )
expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                           grad = "bbbbb" ), "should be one of" )

expect_error( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                         grad = c( "no", "v" ) ), "1" )
expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                           grad = c( "v", "hv" ) ), "1" )

expect_error( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                         grad = NA ) )
expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                           grad = NA ) )

expect_error( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                         grad = character() ) )
expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                           grad = character() ) )

expect_error( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                         grad = a ) )
expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ),
                           grad = a ) )

expect_equal_ggd( a, b )
rm( b )


## Combinatorial tests
a$set.cmp( data.frame( mean = c( 0, 1 ), sd = c( 1.1, 1.2 ) ), grad = "v3" )
expect_equal( a$kind, "2-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )
expect_equal( a$mix.type, 3 )
expect_equal( a$cmp$mean, c( 0, 1, 0 ) )
expect_equal( a$cmp$sd, c( 1.1, 1.2, 1.1 ) )

b <- a$copy()
expect_error( a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), this.mix.type = 0 ),
              "Indicated mix.type is not appropriate for cmp" )

expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0, 0.5 ), sd = c( 1, 0.5, 2 ) ), mix.type = 1 ),
              "Indicated mix.type is not appropriate for cmp" )
expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0, 0.5 ), sd = c( 1, 0.5, 2 ) ), mix.type = 2 ),
              "Indicated mix.type is not appropriate for cmp" )
expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0, 0.5 ), sd = c( 1, 0.5, 2 ) ), grad = "v2" ),
              "Indicated grad is not appropriate for cmp" )
expect_error( ggd.set.cmp( data.frame( mean = c( -0.5, 0, 0.5 ), sd = c( 1, 0.5, 2 ) ), mix.type = 4 ),
              "Indicated mix.type is not appropriate for cmp" )

expect_error( ggd.set.cmp( data.frame( mean = rep( 0, 4 ), sd = c( 1, 0.5, 1.5, 2 ) ), kind = "Mean of.*2 Normal" ),
              "Indicated kind is not appropriate for cmp" )
expect_error( ggd.set.cmp( data.frame( mean = rep( 0, 4 ), sd = c( 1, 0.5, 1.5, 2 ) ), kind = "Horizontal" ),
              "Indicated kind is not appropriate for cmp" )
expect_error( ggd.set.cmp( data.frame( mean = rep( 0, 4 ), sd = c( 1, 0.5, 1.5, 2 ) ), mix.type = 3 ),
              "Indicated mix.type is not appropriate for cmp" )
expect_error( ggd.set.cmp( data.frame( mean = rep( 0, 4 ), sd = c( 1, 0.5, 1.5, 2 ) ), grad = "v3" ),
              "Indicated grad is not appropriate for cmp" )
expect_equal_ggd( a, b )
rm( b )

# Changing only mix.type
a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), this.mix.type = 2 )
a$set.cmp( data.frame( mean = c( -0.5, 0.5 ), sd = c( 1, 2 ) ), this.mix.type = 1 )
expect_equal( a$mix.type, 1 )


################################################################################################
## adjust.cmp
################################################################################################

## Basic Errors
a <- GGD$new()
expect_error( a$adjust.cmp( -1 ), "mix.type -1 is undefined" )
expect_error( a$adjust.cmp( 5 ), "mix.type 5 is undefined" )
expect_error( a$adjust.cmp( NA ), "NA for mix.type can be specified only if cmp has no rows" )

a$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1, 2 ) ) )
a$mix.type <- integer()     ## Don't do it! (this is just for test)
expect_error( a$adjust.cmp(), "Cannot identify current mix.type" )
expect_error( a$adjust.cmp( 2 ), "Cannot identify current mix.type" )

a$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1, 2 ) ), this.mix.type = 1 )
expect_error( a$adjust.cmp( 0 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 2 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 3 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 4 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( grad = "v3" ), "Not appropriate for current distribution model" )

a$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1, 2 ) ), this.mix.type = 2 )
expect_error( a$adjust.cmp( 0 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 1 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 3 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( grad = "v3" ), "Not appropriate for current distribution model" )

a$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1, 2 ) ), this.mix.type = 3 )
expect_error( a$adjust.cmp( 0 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 1 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 2 ), "Not appropriate for current distribution model" )

a$set.cmp( data.frame( mean = c( 0, 0, 0 ), sd = 1:3 ), this.mix.type = 3 )
expect_error( a$adjust.cmp( 0 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 1 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 2 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 4 ), "Not appropriate for current distribution model" )

a$set.cmp( data.frame( mean = rep( 0, 4 ), sd = 1:4 ), this.mix.type = 4 )
expect_error( a$adjust.cmp( 0 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 1 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 2 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( 3 ), "Not appropriate for current distribution model" )
expect_error( a$adjust.cmp( grad = "v3" ), "Not appropriate for current distribution model" )

## cleared object

a$clear()
a$adjust.cmp( NA )
expect_identical( a$kind, character() )
expect_identical( a$kind.index, integer() )
expect_identical( a$mix.type, NA_integer_ )
a$adjust.kind.index()
expect_identical( a$kind, NA_character_ )
expect_identical( a$kind.index, NA_integer_ )
expect_identical( a$mix.type, NA_integer_ )

a$clear()
for ( i in 0:4 )
{
    expect_error( a$adjust.cmp( i ), "Not appropriate for current distribution model" )
    expect_identical( a$mix.type, integer() )
}
a$adjust.cmp( NA )
for ( i in 0:4 )
{
    expect_error( a$adjust.cmp( i ), "Not appropriate for current distribution model" )
    expect_identical( a$mix.type, NA_integer_ )
}


## normal distribution

a <- GGD$new()
a$adjust.cmp( 2 )
a$mix.type <- integer()     ## Don't do it! (this is just for test)
a$adjust.cmp()
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )
expect_true( a$is.normal() )

a$adjust.cmp()
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )
expect_true( a$is.normal() )

a$adjust.cmp( 1 )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )

a$adjust.cmp( 2 )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )

a$adjust.cmp( 3 )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )

a$adjust.cmp( 4 )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.normal() )

a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )
expect_true( a$is.normal() )

a$adjust.cmp( grad = "hv" )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.normal() )

a$adjust.cmp( grad = "v3" )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_true( a$is.normal() )

a$adjust.cmp( grad = "v2" )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )

a$adjust.cmp( grad = "h" )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )

##

a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 2, 2 ) ), this.mix.type = 1 )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )
expect_true( a$is.normal() )

a$adjust.cmp( 1 )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp( 2 )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )

a$adjust.cmp( 1 )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp( 3 )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )

a$adjust.cmp( 1 )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp( grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_true( a$is.normal() )

a$adjust.cmp( 1 )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp( 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.normal() )

a$adjust.cmp( this.mix.type = 2,
              grad = "normal" )
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )
expect_true( a$is.normal() )


a$set.cmp( data.frame( mean = c( 1 ), sd = c( 2 ) ), this.mix.type = 2 )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )
expect_true( a$is.normal() )

a$adjust.cmp( 2 )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp( 3 )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )

a$adjust.cmp( 2 )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp( grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_true( a$is.normal() )

a$adjust.cmp( 2 )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp( 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.normal() )

a$adjust.cmp( this.mix.type = 0,
              grad = "h" )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )


a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 2, 2 ) ), this.mix.type = 3 )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )
expect_true( a$is.normal() )

a$adjust.cmp( 3 )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp( grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_true( a$is.normal() )

a$adjust.cmp( 3 )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )
a$adjust.cmp( 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.normal() )

a$adjust.cmp( this.mix.type = 1, grad = "v2" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.normal() )


a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 2, 2 ) ), grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_true( a$is.normal() )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )
expect_true( a$is.normal() )

a$adjust.cmp( grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
a$adjust.cmp( 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.normal() )

a$adjust.cmp( this.mix.type = 3, grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_true( a$is.normal() )


a$set.cmp( data.frame( mean = c( 1, 1 ), sd = c( 2, 2 ) ), this.mix.type = 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.normal() )
a$adjust.cmp()
expect_equal( a$mix.type, 0 )
expect_equal( nrow( a$cmp ), 1 )
expect_true( a$is.normal() )

a$adjust.cmp( this.mix.type = 3, grad = "hv" )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.normal() )


## mean of 2 normal distributions

a$set.cmp( data.frame( mean = 0:1, sd = 1:2 ), this.mix.type = 1 )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )
a$adjust.cmp()
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )

a$adjust.cmp( 1 )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )

expect_error( a$adjust.cmp( 2 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )

expect_error( a$adjust.cmp( 3 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )

expect_error( a$adjust.cmp( grad = "v3" ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )

expect_error( a$adjust.cmp( 4 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 1 )
expect_equal( nrow( a$cmp ), 2 )


## horizontal gradation

a$set.cmp( data.frame( mean = 0:1, sd = 1:2 ), this.mix.type = 2 )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.h() )
a$adjust.cmp()
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.h() )

expect_error( a$adjust.cmp( 1 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.h() )

a$adjust.cmp( 2 )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.h() )

expect_error( a$adjust.cmp( 3 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.h() )

expect_error( a$adjust.cmp( grad = "v3" ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.h() )

a$adjust.cmp( 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.h() )

a$adjust.cmp()
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.h() )

a$adjust.cmp( 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.h() )

a$adjust.cmp( 2 )
expect_equal( a$mix.type, 2 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.h() )


## vertical gradation, 2 components

a$set.cmp( data.frame( mean = 0:1, sd = 1:2 ), this.mix.type = 3 )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.v2() )
a$adjust.cmp()
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.v2() )

expect_error( a$adjust.cmp( 1 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.v2() )

expect_error( a$adjust.cmp( 2 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.v2() )

a$adjust.cmp( 3 )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.v2() )

a$adjust.cmp( grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_true( a$is.v3() )

a$adjust.cmp( 3 )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.v2() )

a$adjust.cmp( grad = "v3" )
a$adjust.cmp( grad = "v2" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.v2() )

a$adjust.cmp( 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.v2() )

a$adjust.cmp()
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.v2() )

a$adjust.cmp( 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.v2() )

a$adjust.cmp( 3 )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 2 )
expect_true( a$is.v2() )

a$adjust.cmp( 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.v2() )

a$adjust.cmp( grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_true( a$is.v3() )


## vertical gradation, 3 components

a$set.cmp( data.frame( mean = -1:1, sd = 1:3 ), grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_false( a$is.v2() )
expect_true( a$is.v3() )
a$adjust.cmp()
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_false( a$is.v2() )
expect_true( a$is.v3() )

expect_error( a$adjust.cmp( 1 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_false( a$is.v2() )
expect_true( a$is.v3() )

a$adjust.cmp( 3 )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_false( a$is.v2() )
expect_true( a$is.v3() )

expect_error( a$adjust.cmp( grad = "v2" ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_false( a$is.v2() )
expect_true( a$is.v3() )

a$adjust.cmp( grad = "v3" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_false( a$is.v2() )
expect_true( a$is.v3() )

expect_error( a$adjust.cmp( 4 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 3 )
expect_equal( nrow( a$cmp ), 3 )
expect_false( a$is.v2() )
expect_true( a$is.v3() )


## horizontal-vertical gradation

a$set.cmp( data.frame( mean = c( -1, 0, 0, 1 ), sd = 1:4 ), this.mix.type = 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.hv() )
a$adjust.cmp()
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.hv() )

expect_error( a$adjust.cmp( 1 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.hv() )

expect_error( a$adjust.cmp( 2 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.hv() )

expect_error( a$adjust.cmp( 3 ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.hv() )

expect_error( a$adjust.cmp( grad = "v3" ), "Not appropriate for current distribution model" )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.hv() )

a$adjust.cmp( 4 )
expect_equal( a$mix.type, 4 )
expect_equal( nrow( a$cmp ), 4 )
expect_true( a$is.hv() )


################################################################################################
## ggd.mix.type.for.kind.index
################################################################################################

expect_error( ggd.mix.type.for.kind.index( 0 ), "kind.index 0 is undefined" )
expect_error( ggd.mix.type.for.kind.index( 17 ), "kind.index 17 is undefined" )
expect_equal( ggd.mix.type.for.kind.index( 1 ), 0 )
expect_equal( ggd.mix.type.for.kind.index( c( 2:7 ) ), c( 1, 1, 1, 2, 2, 2 ) )
expect_equal( ggd.mix.type.for.kind.index( 8:16 ), c( rep( 3, 6 ), rep( 4, 3 ) ) )
expect_equal( ggd.mix.type.for.kind.index( c( 1, 2, NA, 4, NaN, 6 ) ), c( 0, 1, NA, 1, NA, 2 ) )


################################################################################################
## adjust.cmp.rownames
################################################################################################

a$clear()
a$adjust.cmp.rownames()
expect_equal( rownames( a$cmp ), character() )

a <- ggd.set.cmp( data.frame( mean = 0, sd = 1 ) )
expect_equal( rownames( a$cmp ), "n.1" )

a$adjust.cmp( this.mix.type = 1 )
expect_equal( rownames( a$cmp ), c( "n.1", "n.2" ) )

a$cmp <- data.frame( mean = rep( 0, 2 ), cmp = rep( 1.2, 2 ) )
a$adjust.cmp.rownames()
expect_equal( rownames( a$cmp ), c( "n.1", "n.2" ) )

## These processes are just for test.
## Don't assign values directly to the fields of a GGD object.

a$mix.type <- 2L
a$adjust.cmp.rownames()
expect_equal( rownames( a$cmp ), c( "n.1", "n.2" ) )

a$mix.type <- 3L
a$adjust.cmp.rownames()
expect_equal( rownames( a$cmp ), c( "n.1", "n.2" ) )

a$cmp <- data.frame( mean = rep( 0, 3 ), cmp = rep( 1.3, 3 ) )
a$adjust.cmp.rownames()
expect_equal( rownames( a$cmp ), c( "n.1", "n.2", "n.3" ) )

a$mix.type <- 4L
a$cmp <- data.frame( mean = rep( 0, 4 ), cmp = rep( 1.4, 4 ) )
a$adjust.cmp.rownames()
expect_equal( rownames( a$cmp ), c( "n.1.1", "n.1.2", "n.2.1", "n.2.2" ) )

a$mix.type <- 3L
expect_error( a$adjust.cmp.rownames() )

a$mix.type <- 5L
a$cmp <- data.frame( mean = rep( 0, 5 ), cmp = rep( 1.5, 5 ) )
expect_error( a$adjust.cmp.rownames(), "mix.type is invalid" )

a$clear()


################################################################################################
## adjust.median.mean.sd
################################################################################################

means <- seq( -2, 2, 0.4 )
sds <- seq( 0.4, 3.2, 0.4 )

# normal distribution
for ( mean in means )
vapply( mean,
function( mean )
{
    vapply( sds,
    function( sd )
    {
        vapply( 1:4,
        function( mix.type )
        {
            a$set.cmp( data.frame( mean = mean, sd = sd ), this.mix.type = mix.type )
            a$adjust.median.mean.sd()
            expect_equal( nrow( a$cmp ), ifelse( mix.type < 4, 2, 4 ) )
            expect_equal( a$median, mean )
            expect_equal( a$mean, mean )
            expect_equal( a$sd, sd )
            expect_equal( a$lsd, sd )
            expect_equal( a$usd, sd )
            expect_equal( a$lsd.abs.error, 0 )
            expect_equal( a$usd.abs.error, 0 )
            TRUE
        }, TRUE )
        TRUE
    }, TRUE )
    TRUE
}, TRUE )

vapply( means,
function( mean )
{
    vapply( sds,
    function( sd )
    {
        a$set.cmp( data.frame( mean = mean, sd = sd ), grad = "v3" )
        a$adjust.median.mean.sd()
        expect_equal( nrow( a$cmp ), 3 )
        expect_equal( a$median, mean )
        expect_equal( a$mean, mean )
        expect_equal( a$sd, sd )
        expect_equal( a$lsd, sd )
        expect_equal( a$usd, sd )
        expect_equal( a$lsd.abs.error, 0 )
        expect_equal( a$usd.abs.error, 0 )
        TRUE
    }, TRUE )
    TRUE
}, TRUE )

# horizontal gradation
vapply( means,
function( mean.1 )
{
    vapply( sds,
    function( sd.1 )
    {
        vapply( means,
        function( mean.2 )
        {
            vapply( sds,
            function( sd.2 )
            {
                a.1 <- ggd.set.cmp( data.frame( mean = c( mean.1, mean.2 ), sd = c( sd.1, sd.2 ) ),
                                    mix.type = 2 )
                a.2 <- ggd.set.cmp( data.frame( mean = c( mean.1, mean.2 ), sd = c( sd.1, sd.2 ) ),
                                    mix.type = 4 )
                a.2$adjust.median.mean.sd()
                expect_equal( a.1$median,   a.2$median )
                expect_equal( a.1$mean,     a.2$mean )
                expect_equal( a.1$sd,       a.2$sd )
                expect_equal( a.1$lsd,      a.2$lsd )
                expect_equal( a.1$usd,      a.2$usd )
                expect_equal( a.1$lsd.abs.error, 0 )
                expect_equal( a.1$usd.abs.error, 0 )
                expect_equal( a.2$lsd.abs.error, 0 )
                expect_equal( a.2$usd.abs.error, 0 )
                TRUE
            }, TRUE )
            TRUE
        }, TRUE )
        TRUE
    }, TRUE )
    TRUE
}, TRUE )

# vertical gradation
vapply( means,
function( mean.1 )
{
    vapply( sds,
    function( sd.1 )
    {
        vapply( means,
        function( mean.2 )
        {
            vapply( sds,
            function( sd.2 )
            {
                a.1 <- ggd.set.cmp( data.frame( mean = c( mean.1, mean.2 ), sd = c( sd.1, sd.2 ) ),
                                    mix.type = 3 )
                a.2 <- ggd.set.cmp( data.frame( mean = c( mean.1, mean.2 ), sd = c( sd.1, sd.2 ) ),
                                    mix.type = 3 )$adjust.cmp( this.mix.type = 4 )
                a.3 <- ggd.set.cmp( data.frame( mean = c( mean.1, mean.2 ), sd = c( sd.1, sd.2 ) ),
                                    grad = "v3" )
                a.2$adjust.median.mean.sd()
                expect_equal( a.1$median,   a.2$median )
                expect_equal( a.1$mean,     a.2$mean )
                expect_equal( a.1$sd,       a.2$sd )
                expect_equal( a.1$lsd,      a.2$lsd )
                expect_equal( a.1$usd,      a.2$usd )
                expect_equal( a.1$lsd.abs.error, 0 )
                expect_equal( a.1$usd.abs.error, 0 )
                expect_equal( a.2$lsd.abs.error, 0 )
                expect_equal( a.2$usd.abs.error, 0 )

                expect_equal( a.1$median,   a.3$median )
                expect_equal( a.1$mean,     a.3$mean )
                expect_equal( a.1$sd,       a.3$sd )
                expect_equal( a.1$lsd,      a.3$lsd )
                expect_equal( a.1$usd,      a.3$usd )
                expect_equal( a.3$lsd.abs.error, 0 )
                expect_equal( a.3$usd.abs.error, 0 )
                TRUE
            }, TRUE )
            TRUE
        }, TRUE )
        TRUE
    }, TRUE )
    TRUE
}, TRUE )

