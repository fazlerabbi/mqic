\name{readcsv.mqic}
\alias{readcsv.mqic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readcsv.mqic(clsObject, data.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clsObject}{
%%     ~~Describe \code{clsObject} here~~
}
  \item{data.file}{
%%     ~~Describe \code{data.file} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (clsObject, data.file) 
{
    filename = paste("data/", data.file)
    if (grepl("\\.zip$", filename)) {
        tmp.dir <- tempdir()
        tmp.path <- file.path(tmp.dir, data.file)
        file.copy(filename, tmp.path)
        unzip(filename, exdir = tmp.dir)
        filename <- file.path(tmp.dir, sub("\\.zip$", "", data.file))
    }
    clsObject$data <- read.csv(filename, header = TRUE, sep = ",")
    return(clsObject)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
