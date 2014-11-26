#' corrplotMeanDescriptor Function
#' 
#' Aggregates the number of patients by states.
#' 
#' @param clsObject Instance of mqic class
#' @param ... disease, colNames, top
#'
#' @return countByStates data.frame with patient count by states.
#'
#' @examples
#' library('mqic')
#'
#' \dontrun{corrplotMeanDescriptor(
#'    clsObject, 
#'    list("disease"=1, "colNames"=c("State","TotalPatients"), "top"=5)
#'  }

source("mqic.R")
source("readcsv.mqic.R")

corrplotMeanDescriptor <- function(clsObject, disease = NULL)
{
  UseMethod("corrplotMeanDescriptor", clsObject, disease = NULL)
}

corrplotMeanDescriptor <- function(clsObject, disease = NULL) 
{
  print("corrplotMeanDescriptor doesn't know how to handle this object");
  return(NULL)
}

corrplotMeanDescriptor <- function(clsObject, disease = NULL)
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

mqic <- mqic();
mqic <- readcsv(mqic, "MQIC.csv", "c:\\MQIC.csv")
corrplotMeanDescriptor(mqic,2)