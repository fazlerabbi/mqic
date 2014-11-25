#' scatterPlotCorrelate Function
#' 
#' Correlate using scatter plot.
#' 
#' @param clsObject Instance of mqic class
#' @return 
#'
#' @examples
#' library('mqic')
#'
#' \dontrun{scatterPlotCorrelate(clsObject)}

source("mqic.R")
source("readcsv.mqic.R")


scatterPlotCorrelate <- function(clsObject, colx, coly)
{
  UseMethod("scatterPlotCorrelate", clsObject, colx, coly)
}

scatterPlotCorrelate <- function(clsObject, colx, coly) 
{
  print("scatterPlotCorrelate doesn't know how to handle this object");
  return(NULL)
}

scatterPlotCorrelate <- function(clsObject, colx, coly)
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

mqic = mqic();
mqic = readcsv(mqic, "MQIC.csv", "c:\\MQIC.csv")
#scatterPlotCorrelate(mqic, "BMI_MEAN", "SBP_MEAN")
scatterPlotCorrelate(mqic, "BMI_MEAN", "A1C_MEAN")

 