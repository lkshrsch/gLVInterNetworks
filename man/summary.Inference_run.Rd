\name{summary.Inference_run}
\alias{summary.Inference_run}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.Inference_run(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class Inference_run, as returned by \code{gLVlinearRegression()} or \code{gLVnonlinearRegression()}
}
  \item{\dots}{
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Summary of the estimated coefficients and significance values
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

summary.Inference_run <- 
function(object, ...){
  tval <- c(run$Parms/run$SE)
  TAB <- cbind(Estimate = c(run$Parms), StdErr = c(run$SE), t.value = tval, p.value = 2*pt(-abs(tval), df = run$df))
  class(TAB) <- 'summary.Inference_run'
  return(TAB)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
