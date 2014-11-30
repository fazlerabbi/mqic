#' meanBMI Function
#' 
#' This function returns the highest and the lowest 
#' mean BMI with the corresponding states.
#' 
#' @title meanBMI: Returns the higest and the lowest BMI
#' @param clsObject Instance of mqic class
#' @examples 
#' \dontrun{
#' mqic <- mqic()
#' mqic <- readcsv(mqic, "MQIC.csv")
#' meanBMI(mqic)
#' }
#' 
#' @export
#' @rdname meanBMI
meanBMI <- function(clsObject)
{
    UseMethod("meanBMI")
}

#' @return \code{NULL}
#'
#' @rdname meanBMI default
meanBMI.default <- function(clsObject) 
{
    print("meanBMI doesn't know how to handle this object");
    return(NULL)
}

#' @return \code{output}
#'
#' @rdname meanBMI mqic
meanBMI.mqic <- function(clsObject)
{
    data <- subset(clsObject$data, select = c("STATE", "BMI_MEAN"))
    orderByMeanBMI <- data[order(-data$BMI_MEAN),]     
  
    output <- rbind(orderByMeanBMI[1,], orderByMeanBMI[nrow(orderByMeanBMI),])
  
    return (output)
}
