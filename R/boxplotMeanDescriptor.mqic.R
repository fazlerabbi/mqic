#' boxplotMeanDescriptor Function
#' 
#' Make a box plot (showing means, deviations) of a mean descriptor 
#' (A1C, weight, BMI, FBG, SBP, DBP etc.) for patients with hypertension 
#' or diabetes.
#' 
#' @title boxplotMeanDescriptor: Draws box plots for a mean descriptor
#' @param clsObject nstance of mqic class
#' @param meanDescriptor Column name for the mean descriptor
#' @examples 
#' \dontrun{
#' mqic <- mqic()
#' mqic <- readcsv(mqic, "MQIC.csv")
#' boxplotMeanDescriptor(mqic, "A1C_MEAN")
#' }
#' 
#' @export
#' @rdname boxplotMeanDescriptor
boxplotMeanDescriptor <- function(clsObject, meanDescriptor)
{
    UseMethod("boxplotMeanDescriptor")
}

#' @return \code{NULL}
#' @rdname boxplotMeanDescriptor default
boxplotMeanDescriptor.default <- function(clsObject, meanDescriptor) 
{
    print("boxplotMeanDescriptor doesn't know how to handle this object");
    return(NULL)
}

#' @return \code{NULL}
#' @rdname boxplotMeanDescriptor mqic
boxplotMeanDescriptor.mqic <- function(clsObject, meanDescriptor)
{ 
    # Get the subset of data filtered by the meanDescriptor  
    data <- clsObject$data
        
    # Set names to columns to create subset
    colnames(data)[which(names(data) == "DISEASE_CATEGORY")] <- "col1"
    colnames(data)[which(names(data) == meanDescriptor)] <- "col2"
    data <- subset(data, select=c(col1, col2))
    
    # Replace the disease category with disease name
    data$col1[data$col1 == 1] <- "diabetes"
    data$col1[data$col1 == 2] <- "hypertension"
        
    bp <- ggplot(data, aes(x=col1, y=col2)) +
        geom_boxplot() + 
        ylab(meanDescriptor) +
        theme(axis.title.x=element_blank())
    bp 
}

 