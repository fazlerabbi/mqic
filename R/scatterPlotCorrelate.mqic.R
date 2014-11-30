#' scatterPlotCorrelate Function
#' 
#' This function displays a scatterplot given
#' two descriptors.
#' 
#' @title scatterPlotCorrelate: Displays a scatterplot
#' @param clsObject Instance of mqic class
#' @param colx Descriptor along the x axis
#' @param coly Descriptor along the y axis
#' @examples 
#' \dontrun{
#' mqic <- mqic()
#' mqic <- readcsv(mqic, "MQIC.csv")
#' scatterPlotCorrelate(mqic, "BMI_MEAN", "A1C_MEAN")
#' }
#' 
#' @export
#' @rdname scatterPlotCorrelate
scatterPlotCorrelate <- function(clsObject, colx, coly)
{
    UseMethod("scatterPlotCorrelate")
}

#' @return \code{NULL}
#'
#' @rdname scatterPlotCorrelate default
scatterPlotCorrelate.default <- function(clsObject, colx, coly) 
{
    print("scatterPlotCorrelate doesn't know how to handle this object");
    return(NULL)
}

#' @return \code{NULL}
#'
#' @rdname scatterPlotCorrelate mqic
scatterPlotCorrelate.mqic <- function(clsObject, colx, coly)
{
    data <- subset(clsObject$data, select=c(colx, coly))
    colnames(data) <- c("col1", "col2")
  
    p <- ggplot(data, aes(x=col1, y=col2)) + 
        geom_point() + 
            geom_smooth(
            method=lm,   # Add linear regression lines
            se=FALSE,    # Don't add shaded confidence region
            fullrange=T
        ) +
        xlab(colx) +
        ylab(coly)
    
    print(p)
    cor(data$col1, data$col2)
}

#mqic = mqic();
#mqic = readcsv(mqic, "MQIC.csv", "c:\\MQIC.csv")
#scatterPlotCorrelate(mqic, "BMI_MEAN", "SBP_MEAN")
#scatterPlotCorrelate(mqic, "BMI_MEAN", "A1C_MEAN")

 