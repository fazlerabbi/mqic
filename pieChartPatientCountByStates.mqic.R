#' pieChartPatientCountByStates Function
#' 
#' Draws a pie chart of number of patients by states.
#' 
#' @param clsObject Instance of mqic class
#' @param disease Disease category
#' @return 
#'
#' @examples
#' library('mqic')
#'
#' \dontrun{pieChartPatientCountByStates(clsObject, 1)}

source("patientCountByStates.mqic.R")

pieChartPatientCountByStates <- function(clsObject, disease = NULL)
{
  UseMethod("pieChartPatientCountByStates", clsObject, disease = NULL)
}

pieChartPatientCountByStates <- function(clsObject, disease.category) 
{
  print("pieChartPatientCountByStates doesn't know how to handle this object");
  return(NULL)
}

pieChartPatientCountByStates <- function(clsObject, disease = NULL)
{
  if ( ! is.null(disease) && ! disease %in% c(1,2)) {
    stop("Disease category can be either 1 or 2.")
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
