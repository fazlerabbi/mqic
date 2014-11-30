#' pieChartPatientCountByStates Function
#' 
#' This function draws a pie chart for the 
#' number of patients by states.
#' 
#' @title pieChartPatientCountByStates: Draws a pie chart for the number of patients 
#' @param clsObject Instance of mqic class
#' @param disease Disease name  
#' @examples 
#' \dontrun{
#' mqic <- mqic()
#' mqic <- readcsv(mqic, "MQIC.csv")
#' pieChartPatientCountByStates(mqic, "diabetes")
#' }
#' 
#' @export
#' @rdname pieChartPatientCountByStates
pieChartPatientCountByStates <- function(clsObject, disease = NULL)
{
    UseMethod("pieChartPatientCountByStates")
}


#' @return \code{NULL}
#'
#' @rdname pieChartPatientCountByStates default
pieChartPatientCountByStates.default <- function(clsObject, disease) 
{
    print("pieChartPatientCountByStates doesn't know how to handle this object");
    return(NULL)
}


#' @return \code{NULL}
#'
#' @rdname pieChartPatientCountByStates mqic
pieChartPatientCountByStates.mqic <- function(clsObject, disease = NULL)
{
    if ( ! is.null(disease)) {
        if ( ! disease %in% c("diabetes", "hypertension")) {
            stop("Valid disease is 'diabetes' or 'hypertension' only.")
        }
        if (disease == 'diabetes') {
            disease <- 1;
        } else {
          disease <- 2;
        }
    }
    
    data = patientCountByStates(
        clsObject, 
        list("disease" = disease, "colNames" = c("State", "Count"))
    ); 
  
    p <- ggplot(data, aes(x=1, y=Count, fill=State)) +
        # title of the graph
        ggtitle("Number of diabetes patients by states") +
        # black border around pie slices
        geom_bar(stat="identity", color='black') +
        # remove legends
        guides(fill=FALSE) +
        # polar coordinates
        coord_polar(theta='y') +
        # label aesthetics
        theme(axis.ticks=element_blank(),  # the axis ticks
            axis.title=element_blank(),  # the axis labels
            axis.text.y=element_blank(), # the 0.75, 1.00, 1.25 labels
            axis.text.x=element_text(color='black')) +
        # pie slice labels
        scale_y_continuous(
            breaks=cumsum(data$Count) - data$Count/2,
            labels=data$State
        )
    
    print(p)    
}

