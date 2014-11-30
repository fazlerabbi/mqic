#' corrplotMeanDescriptor Function
#' 
#' Displays a covariance matrix indicating the 
#' correlations among the mean descriptors
#' 
#' @title corrplotMeanDescriptor: Draws a covariance matrix
#' @param clsObject Instance of mqic class
#' @param disease Disease name  
#' @examples 
#' \dontrun{
#' mqic <- mqic()
#' readcsv(mqic, "MQIC.csv")
#' corrplotMeanDescriptor(mqic, "diabetes")
#' }
#' 
#' @export
#' @rdname corrplotMeanDescriptor
corrplotMeanDescriptor <- function(clsObject, disease = NULL)
{
    UseMethod("corrplotMeanDescriptor")
}

#' @return \code{NULL}
#'
#' @rdname corrplotMeanDescriptor default
corrplotMeanDescriptor.default <- function(clsObject, disease = NULL) 
{
    print("corrplotMeanDescriptor doesn't know how to handle this object");
    return(NULL)
}

#' @return \code{NULL}
#'
#' @rdname corrplotMeanDescriptor mqic
corrplotMeanDescriptor.mqic <- function(clsObject, disease = NULL)
{
    data <- clsObject$data
  
    # Fetch data with the given disease category    
    if ( ! is.null(disease) && disease %in% c(1, 2)) {
        data <- subset(
            data, 
            data$DISEASE_CATEGORY == disease
        )
    }
  
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

#mqic <- mqic();
#mqic <- readcsv(mqic, "MQIC.csv", "c:\\MQIC.csv")
#corrplotMeanDescriptor(mqic,2)