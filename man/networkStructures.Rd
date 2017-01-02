\name{networkStructures}
\alias{networkStructures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate list of network structures for testing with compareStructures()
}
\description{
In order to compare explicit network structures based on prior knowledge of the 
system or to test for different solutions, this function generates either a complete 
list containing all possible network topologies for n species, or generates a list 
based on a pre-specified list of network structures to test.
}
\usage{networkStructures <- function(n, upperBound = 10, lowerBound = -10, testStructures = c("+--+--","++----"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Optional. Generate a list containing all possible network 
structures in a system containing n species}
  \item{upperBound}{The upper numeric constraint for the parameter values. Default 
is 10}
  \item{lowerBound}{The lower numeric constraint for the parameter values. Default 
is -10}
 \item{testStructures}{Optional. A list containing strings with specific network 
structures the user wishes to compare. The structures should contain only "+" or 
"-" characters representing the qualitative values for the parameter matrix.}
}
\value{
Returns lists describing the network structures to test, and upper and lower values 
which will be given as input for the nonlinear regression.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

