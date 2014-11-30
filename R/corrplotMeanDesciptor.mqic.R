#' corrplotMeanDescriptor Function
#' 
#' Displays a covariance matrix indicating the 
#' correlations among the mean descriptors
#' 
#' @title corrplotMeanDescriptor: Draws a covariance matrix
#' @param clsObject Instance of mqic class
#' @examples 
#' \dontrun{
#' mqic <- mqic()
#' readcsv(mqic, "MQIC.csv")
#' corrplotMeanDescriptor(mqic)
#' }
#' 
#' @export
#' @rdname corrplotMeanDescriptor
corrplotMeanDescriptor <- function(clsObject)
{
    UseMethod("corrplotMeanDescriptor")
}

#' @return \code{NULL}
#'
#' @rdname corrplotMeanDescriptor default
corrplotMeanDescriptor.default <- function(clsObject) 
{
    print("corrplotMeanDescriptor doesn't know how to handle this object");
    return(NULL)
}

#' @return \code{NULL}
#'
#' @rdname corrplotMeanDescriptor mqic
corrplotMeanDescriptor.mqic <- function(clsObject)
{
    data <- clsObject$data
       
    # Subset of mean descriptors only
    data <- subset(data, select = c(
        A1C_MEAN,
        A1C_STDDEV,
        WEIGHT_MEAN,
        WEIGHT_STDDEV,
        BMI_MEAN,
        BMI_STDDEV,
        FBG_MEAN,
        FBG_STDDEV,
        SBP_MEAN,
        SBP_STDDEV,
        DBP_MEAN,
        DBP_STDDEV
    ))
  
    M <- cor(data)
    corrplot(M, method = "number")
}
