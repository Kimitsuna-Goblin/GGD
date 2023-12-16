################################################################################################
## This file is for helper to check all messages and warnings are tested.
################################################################################################

################################################################################################
#' Execute a source file and stop when a message or warning occurs.
#' @param   testfile    A file path of the source file.
################################################################################################
stop.if.message <- function( testfile )
{
    withCallingHandlers( source( testfile ),
        message = function( m )
        {
            stop( paste( "Message has output:", m$message ) )
        },
        warning = function( w )
        {
            stop( paste( "Warning has occurred:", w$message ) )
        } )
}
