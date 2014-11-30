#' boxplotMeanDescriptor Function
#' 
#' Make a box plot (showing means, deviations) of a mean descriptor 
#' (A1C, weight, BMI, FBG, SBP, DBP etc.) for patients with hypertension 
#' or diabetes.
#' 
#' @title boxplotMeanDescriptor: Draws box plots for a mean descriptor
#' @param clsObject nstance of mqic class
#' @param meanDescriptor Column name for the mean descriptor
#' @param disease Disease name  
#' @examples 
#' \dontrun{
#' mqic <- mqic()
#' mqic <- readcsv(mqic, "MQIC.csv")
#' boxplotMeanDescriptor(mqic, "A1C_MEAN", "diabetes")
#' }
#' 
#' @export
#' @rdname boxplotMeanDescriptor
boxplotMeanDescriptor <- function(clsObject, meanDescriptor, disease = NULL)
{
    UseMethod("boxplotMeanDescriptor")
}

#' @return \code{NULL}
#' @rdname boxplotMeanDescriptor default
boxplotMeanDescriptor.default <- function(clsObject, meanDescriptor, disease = NULL) 
{
    print("boxplotMeanDescriptor doesn't know how to handle this object");
    return(NULL)
}

#' @return \code{NULL}
#' @rdname boxplotMeanDescriptor mqic
boxplotMeanDescriptor.mqic <- function(clsObject, meanDescriptor, disease = NULL)
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
            axis.ticks.x=element_blank()
        )
    print(p)  
}

#mqic = mqic();
#mqic = readcsv(mqic, "MQIC.csv", "c:\\MQIC.csv")
#boxplotMeanDescriptor(mqic, "BMI_MEAN", "SBP_MEAN")
#boxplotMeanDescriptor(mqic, "BMI_MEAN")
#boxplotMeanDescriptor(mqic, "SBP_MEAN", 2)

 