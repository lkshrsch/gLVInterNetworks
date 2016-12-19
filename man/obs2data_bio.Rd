\name{obs2data_bio}
\alias{obs2data_bio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correctly format observations into input data for the model}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
obs2data_bio(res)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
%%     ~~Describe \code{res} here~~
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
function (res) 
{
    obs = t(res[, -1])
    X = rbind(1, obs)
    X = X[, 1:ncol(X) - 1]
    Y <- slope(res[, -1])
    Y = t(Y)
    Y[is.nan(Y)] <- 0
    Y[is.infinite(Y)] <- 0
    return(list(Y, X))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
