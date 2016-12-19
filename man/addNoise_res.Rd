\name{addNoise_res}
\alias{addNoise_res}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add stochasticity to in silico generated data}
\description{
Internal function used in \code{generate_data()} to provide expected measurement errors addinf gaussian noise }
\usage{
addNoise_res(res, sd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{sd}{
%%     ~~Describe \code{sd} here~~
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
function (res, sd) 
{
    noise <- matrix(rnorm(0, sd, n = length(res[, -1])), nrow = nrow(res), 
        ncol = ncol(res[, -1]))
    res[, -1] <- res[, -1] + noise
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
