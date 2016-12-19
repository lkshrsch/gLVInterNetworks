\name{assessment}
\alias{assessment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assess results from inference algorithm}
\description{
Assesses inference result comparing estimated parameters with original simulated parameters (thus only 
works for in silico data), and stores the comparison in three objects: Quantitative, Qualitative1 and 
Qualitative2  
}
\usage{
assessment(result, Parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{result}{
%%     ~~Describe \code{result} here~~
}
  \item{Parms}{
%%     ~~Describe \code{Parms} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Quantitative}{Ratio of parameter estimates that contain the original parameter within their computed 
.95 confidence interval  }
\item{Qualitative1}{Ratio of edges in the inferred network that were correctly recovered  }
\item{Qualitative2}{Ratio of significant edges in the inferred network that were correctly recovered  }
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
function (result, Parms) 
{
    "still need to check how many zeros are in Parms and correct for qualitative..."
    quantitative <- correctly_intervalized_estimates(c(result$Parms), 
        c(result$SE), c(Parms))
    quantitative <- sum(quantitative)/length(quantitative)
    qualitative <- compare_matrix(c(result$Parms), c(Parms))
    qualitative <- sum(qualitative)/length(qualitative)
    is_not_significant <- correctly_intervalized_estimates(c(result$Parms), 
        c(result$SE), rep(0, length(Parms)))
    is_zero <- c(Parms) == 0
    qualitative2 <- is_not_significant == is_zero
    qualitative2 <- sum(qualitative2)/length(qualitative2)
    result$quantitative = quantitative
    result$qualitative1 = qualitative
    result$qualitative2 = qualitative2
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
