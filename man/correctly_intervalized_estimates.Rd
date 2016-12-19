\name{correctly_intervalized_estimates}
\alias{correctly_intervalized_estimates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if confidence intervals of parameter estimates contain true coefficients}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
correctly_intervalized_estimates(background, SE, Parms)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{background}{
%%     ~~Describe \code{background} here~~
}
  \item{SE}{
%%     ~~Describe \code{SE} here~~
}
  \item{Parms}{
%%     ~~Describe \code{Parms} here~~
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
function (background, SE, Parms) 
{
    CI95 <- SE * 2
    up_estimates <- background + CI95
    down_estimates <- background - CI95
    return(down_estimates < c(Parms) & c(Parms) < up_estimates)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
