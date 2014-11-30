#' frequencyByAge Function
#' 
#' This function counts the number of patients by age category.
#' Age categories being:
#' 1 = 18 years to 44 years
#' 2 = 45 to 64 years
#' 3 = 65 to 79 years
#' 4 = 80+ years
#' 
#' @title frequencyByAge: Number of patients by age
#' @param clsObject Instance of mqic class
#' @param list disease, colNames
#' @return countByAge Patient count by age 
#' @examples
#' library('mqic')
#'
#' \dontrun{
#' mqic <- mqic()
#' mqic <- readcsv(mqic, "MQIC.csv")
#' frequencyByAge(mqic, list("disease"="hypertension", "colNames"=c("AgeCategory","TotalPatients")))
#' }
#' 
#' @export
#' @rdname frequencyByAge
frequencyByAge <- function(clsObject, ...)
{
    UseMethod("frequencyByAge")
}

#' @return \code{NULL}
#'
#' @rdname frequencyByAge default
frequencyByAge.default <- function(clsObject, ...) 
{
    print("frequencyByAge doesn't know how to handle this object");
    return(NULL)
}


#' @return \code{countByAge}
#'
#' @rdname frequencyByAge mqic
frequencyByAge.mqic <- function(clsObject, ...)
{
    data <- clsObject$data
    args <- list(...)
    
    if (is.list(args[[1]])) args <- args[[1]]
    
    if ( ! is.null(args$disease)) {
        if ( ! args$disease %in% c("diabetes", "hypertension")) {
            stop("Valid disease is 'diabetes' or 'hypertension' only.")
        }
        if (args$disease == 'diabetes') {
            args$disease <- 1;
        } else {
            args$disease <- 2;
        }
      
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
