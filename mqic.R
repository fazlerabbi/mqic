#' Class definition of mqic.
#'
#' mqic is a package to analyze patient data from using the
#' dataset from MQIC. 
#'
#' @param data.file The name of the data file to be read.
#'
#' @return clsObject mqic Class instance.
#'
#' @examples
#' library('mqic')
#'
#' \dontrun{mqic('example.csv')}

library(ggplot2)

mqic <- function() 
{
  UseMethod("mqic")
}

mqic.default <- function()
{
  me <- list(
    data = NULL    
  )
  
  # Set the name for the class
  class(me) <- "mqic"
  return (me)
}