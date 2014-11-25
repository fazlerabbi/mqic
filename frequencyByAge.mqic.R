#' frequencyByAge Function
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
#' \dontrun{frequencyByAge(
#'    clsObject, 
#'    list("disease"=1, "colNames"=c("State","TotalPatients"), "top"=5)
#'  }

source("mqic.R")
source("readcsv.mqic.R")

frequencyByAge <- function(clsObject, ...)
{
  UseMethod("frequencyByAge", clsObject)
}

frequencyByAge <- function(clsObject, ...) 
{
  print("frequencyByAge doesn't know how to handle this object");
  return(NULL)
}

frequencyByAge <- function(clsObject, ...)
{
  data <- clsObject$data
  args <- list(...)
  if (is.list(args[[1]])) args <- args[[1]]
    
  # Fetch data with the given disease category    
  if ( ! is.null(args$disease) && args$disease %in% c(1, 2)) {
    data <- subset(
      data, 
      data$DISEASE_CATEGORY == args$disease
    )
  }
  
  # Change age category labels
  data$AGE_CATEGORY <- factor(
      data$AGE_CATEGORY,
      levels = c(1,2,3,4),
      labels = c("18-44", "45-64", "65-79", "80+")
  )
  
  # Count the number of patients grouped by Age
  countByAge <- aggregate(
    data$PATIENTS, 
    list(AGE_CATEGORY=data$AGE_CATEGORY), 
    FUN=sum
  )
  
  countByAge <- countByAge[order(-countByAge$x),] 
  
  # Change column names
  if (! is.null(args$colNames) && length(args$colNames) == 2) {
    colnames(countByAge) <- args$colNames
  }
  
  return (countByAge)
}

mqic = mqic();
mqic = readcsv(mqic, "MQIC.csv", "c:\\MQIC.csv")
frequencyByAge(mqic, list("disease"=2, "colNames"=c("Age","TotalPatients")))