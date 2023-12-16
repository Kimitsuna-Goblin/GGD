################################################################################################
## This file is for common processes for tests.
################################################################################################

library( "ggd" )
library( "testthat" )

if ( any( ls() == "clear.passed.cov" ) ) clear.passed.cov() else source( "tests/testthat/helper-coverage.R" )
if ( !any( ls() == "expect_cleared" ) ) source( "tests/testthat/helper-expectations.R" )
if ( !any( ls() == "stop.if.message" ) ) source( "tests/testthat/helper-stop.if.message.R" )
