\name{parameter_matrix}
\alias{parameter_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correctly format a parameter vector into a matrix as input for the model}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
parameter_matrix(pars, numberSpecies)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{numberSpecies}{
%%     ~~Describe \code{numberSpecies} here~~
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
function (pars, numberSpecies) 
{
    "set parameters as matrix format as required by solveLV function"
    par_matrix = matrix(data = pars, nrow = numberSpecies)
    return(par_matrix)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
