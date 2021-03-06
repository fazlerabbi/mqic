% Generated by roxygen2 (4.0.2.9000): do not edit by hand
% Please edit documentation in R/frequencyByAge.mqic.R
\name{frequencyByAge}
\alias{frequencyByAge}
\title{frequencyByAge: Number of patients by age}
\usage{
frequencyByAge(clsObject, ...)
}
\arguments{
\item{clsObject}{Instance of mqic class}

\item{list}{disease, colNames}
}
\value{
countByAge Patient count by age
}
\description{
frequencyByAge Function
}
\details{
This function counts the number of patients by age category.
Age categories being:
1 = 18 years to 44 years
2 = 45 to 64 years
3 = 65 to 79 years
4 = 80+ years
}
\examples{
library('mqic')

\dontrun{
mqic <- mqic()
mqic <- readcsv(mqic, "MQIC.csv")
frequencyByAge(mqic, list("disease"="hypertension", "colNames"=c("AgeCategory","TotalPatients")))
}
}

