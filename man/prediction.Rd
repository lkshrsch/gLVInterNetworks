\name{prediction}
\alias{prediction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predictions for parameterized models on new variable states
}
\description{
Given a fitted model, predict future timepoints starting from given starting points or random starting points. Compare prediction with test data from the inference result object. Only for result assessment.
}
\usage{
prediction(result, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{result}{
%%     ~~Describe \code{result} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (result, data) 
{
    x0 <- data$testData[1, -1]
    pred <- solveLV_bio(result$Parms, data$testData[1:nrow(data$testData), 
        1], x0)
    Coef_of_determination <- R_squared(pred, data$testData)
    result$R_squared_test_Data <- Coef_of_determination
    r <- c()
    for (i in 1:10) {
        x0 <- runif(n = data$species, 0, 1)
        new <- solveLV_bio(data$Parms, times = 0:50, States = x0)
        pred2 <- solveLV_bio(result$Parms, new[, 1], x0)
        r[i] <- R_squared(pred2, new)
    }
    result$R_squared_new_Data <- mean(r)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
