#' meanBMI Function
#' 
#' Fetches the Highest and lowest mean BMI.
#' 
#' @param clsObject Instance of mqic class
#' @return 
#'
#' @examples
#' library('mqic')
#'
#' \dontrun{meanBMI(clsObject)}

source("mqic.R")
source("readcsv.mqic.R")


meanBMI <- function(clsObject)
{
  UseMethod("meanBMI", clsObject)
}

meanBMI <- function(clsObject) 
{
  print("meanBMI doesn't know how to handle this object");
  return(NULL)
}

meanBMI <- function(clsObject)
{
  data <- subset(clsObject$data, select = c("STATE", "BMI_MEAN"))
  orderByMeanBMI <- data[order(-data$BMI_MEAN),]     
  
  output <- rbind(orderByMeanBMI[1,], orderByMeanBMI[nrow(orderByMeanBMI),])
  
  print (output)
}
