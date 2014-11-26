#' boxplotMeanDescriptor Function
#' 
#' Correlate using scatter plot.
#' 
#' @param clsObject Instance of mqic class
#' @return 
#'
#' @examples
#' library('mqic')
#'
#' \dontrun{boxplotMeanDescriptor(clsObject)}

source("mqic.R")
source("readcsv.mqic.R")


boxplotMeanDescriptor <- function(clsObject, meanDescriptor, disease = NULL)
{
  UseMethod("boxplotMeanDescriptor", clsObject, meanDescriptor, disease = NULL)
}

boxplotMeanDescriptor <- function(clsObject, meanDescriptor, disease = NULL) 
{
  print("boxplotMeanDescriptor doesn't know how to handle this object");
  return(NULL)
}

boxplotMeanDescriptor <- function(clsObject, meanDescriptor, disease = NULL)
{
   
  # Fetch data with the given disease category    
  if ( ! is.null(disease) && disease %in% c(1, 2)) {
    data <- subset(
      clsObject$data, 
      clsObject$data$DISEASE_CATEGORY == disease
    )
  }
  data <- subset(data, select=c(meanDescriptor))
  
  colnames(data) <- c("col1")
  
  p <- ggplot(data, aes(x=factor(0), col1)) +
      geom_boxplot() + 
      theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
         
  print(p)
  
}

mqic = mqic();
mqic = readcsv(mqic, "MQIC.csv", "c:\\MQIC.csv")
#boxplotMeanDescriptor(mqic, "BMI_MEAN", "SBP_MEAN")
#boxplotMeanDescriptor(mqic, "BMI_MEAN")
boxplotMeanDescriptor(mqic, "SBP_MEAN", 2)


 