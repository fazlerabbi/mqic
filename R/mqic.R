#' mqic Class
#' 
#' This function defines mqic class to analyze MQIC
#' patient data.
#' 
#' @title mqic: Defines mqic class
#' @examples 
#' \dontrun{
#' mqic <- mqic()
#' }
#' @export
#' @rdname mqic
mqic <- function() 
{
  UseMethod("mqic")
}

library(ggplot2)
library(corrplot)

#' @return \code{mqic}
#'
#' @rdname mqic default
mqic.default <- function()
{
  me <- list(
    data = NULL    
  )
  
  # Set the name for the class
  class(me) <- "mqic"
  return (me)
}