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
    
    correlation <- cor(data$col1, data$col2)
    # round correlation to 2 decimal places
    correlation <- format(round(correlation, 2), nsmall = 2)
    
    # Show correlation measure in the title
    plot_title <- paste("Linear regression. Correlation(r)=", correlation, sep = "")
    
    p <- ggplot(data, aes(x=col1, y=col2)) + 
        geom_point() + 
            geom_smooth(
            method=lm,   # Add linear regression lines
            se=FALSE,    # Don't add shaded confidence region
            fullrange=T
        ) +
        ggtitle(plot_title) + 
        theme(plot.title = element_text(lineheight=.8, face="bold")) +    
        xlab(colx) +
        ylab(coly)
    
    print(p)
    
}
