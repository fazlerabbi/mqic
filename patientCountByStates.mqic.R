#' patientCountByStates Function
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
#' \dontrun{patientCountByStates(
#'    clsObject, 
#'    list("disease"=1, "colNames"=c("State","TotalPatients"), "top"=5)
#'  }

source("mqic.R")
source("readcsv.mqic.R")

patientCountByStates <- function(clsObject, ...)
{
  UseMethod("patientCountByStates", clsObject)
}

patientCountByStates <- function(clsObject, ...) 
{
  print("patientCountByStates doesn't know how to handle this object");
  return(NULL)
}

patientCountByStates <- function(clsObject, ...)
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
  
  # Count the number of patients grouped by State
  countByStates <- aggregate(
    data$PATIENTS, 
    list(State=data$STATE), 
    FUN=sum
  )
  
  # Fetch top n rows
  nRows = nrow(data);
  
  if ( ! is.null(args$top) && args$top <= nRows && args$top >= 0) {
    countByStates <- countByStates[order(-countByStates$x),]    
    countByStates <- countByStates[1:args$top,]
  }
    
  # Change column names
  if (! is.null(args$colNames) && length(args$colNames) == 2) {
    colnames(countByStates) <- args$colNames
  }
  
  return (countByStates)
}
