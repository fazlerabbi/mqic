#' Reads a comma separated values (.csv) file.
#'
#' This function will load a data set stored in the CSV file format into
#' the specified global variable binding.
#'
#' @param clsObject mqic Class instance.
#' @param data.file The name of the data file to be read.
#'
#' @return clsObject mqic Class instance.
#'
#' @examples
#' library('mqic')
#'
#' \dontrun{readcsv(mqic, 'example.csv')}

readcsv <- function(clsObject, data.file)
{
  UseMethod("readcsv", clsObject)
}

readcsv.default <- function(clsObject) 
{
  print("readcsv doesn't know how to handle this object");
  return(NULL)
}

readcsv.mqic <- function(clsObject, data.file)
{
  filename = paste('data/', data.file);
  
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