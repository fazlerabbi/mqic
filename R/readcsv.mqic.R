#' readcsv Function
#' 
#' This function reads the comma separated values (.csv) file.
#' The data is loaded into the mqic instance as mqic$data. 
#'
#' @title readcsv: Draws a covariance matrix
#' @param clsObject mqic Class instance.
#' @param data.file The name of the data file to be read.
#' @return clsObject mqic Class instance.
#' @examples
#' \dontrun{
#' mqic <- readcsv(mqic, "MQIC.csv")
#' }
#' 
#' @export
#' @rdname readcsv
readcsv <- function(clsObject, data.file, filename = NULL)
{
    UseMethod("readcsv")
}

#' @return \code{NULL}
#'
#' @rdname readcsv default
readcsv.default <- function(clsObject, data.file, filename = NULL) 
{
    print("readcsv doesn't know how to handle this object");
    return(NULL)
}


#' @return \code{mqic}
#'
#' @rdname readcsv mqic
readcsv.mqic <- function(clsObject, data.file, filename = NULL)
{
    if (is.null(filename)) {
        filename <- paste(getwd(), "/../data/", data.file, sep = "")
    }
    if (grepl('\\.zip$', filename))
    {
        tmp.dir <- tempdir()
        tmp.path <- file.path(tmp.dir, data.file)
        file.copy(filename, tmp.path)
        unzip(filename, exdir = tmp.dir)
        filename <- file.path(tmp.dir, sub('\\.zip$', '', data.file))
    }
  
    clsObject$data <- read.csv(
        filename,
        header = TRUE,
        sep = ','
    )
    return(clsObject)
}