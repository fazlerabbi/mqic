#' patientCountByStates Function
#' 
#' This function returns the number of patients by states.
#' 
#' @title patientCountByStates: Number of patients by states
#' @param clsObject Instance of mqic class
#' @param list disease, colNames, top 
#' @return countByStates Patient count by states 
#' @examples
#' \dontrun{
#' mqic <- mqic()
#' mqic <- readcsv(mqic, "MQIC.csv")
#' patientCountByStates(mqic, list("disease"=1, "colNames"=c("State","TotalPatients"), "top"=5))
#' }
#' 
#' @export
#' @rdname patientCountByStates
patientCountByStates <- function(clsObject, ...)
{
    UseMethod("patientCountByStates")
}


#' @return \code{NULL}
#'
#' @rdname patientCountByStates default
patientCountByStates.default <- function(clsObject, ...) 
{
    print("patientCountByStates doesn't know how to handle this object");
    return(NULL)
}

#' @return \code{countByStates}
#'
#' @rdname patientCountByStates mqic
patientCountByStates.mqic <- function(clsObject, ...)
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
