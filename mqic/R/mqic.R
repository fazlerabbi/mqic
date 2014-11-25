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
#' \dontrun{new mqic('example.csv')}

mqic <- function(x) 
{
  UseMethod("mqic", x)
}

mqic.default <- function(data.file)
{
  me <- list(
    data = readcsv(me, data.file)    
  )
  
  # Set the name for the class
  class(me) <- "mqic"
  return (me)
}