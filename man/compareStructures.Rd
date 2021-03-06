\name{compareStructures}
\alias{compareStructures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parameter estimation over a list of different network structures for comparison of 
different models and analysis of uniqueness of solutions}
\description{
Given a list of network structures, estimate gLV parameters constrained to maintain 
such structure, and compare the results from each fitted model.
}
\usage{
compareStructures <- function(data = mydata, structures = myStructures, verbose=TRUE, iterations = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Matrix or table containing time series of measurements in longitudinal form where 
first column corresponds to the time
points and subsequent columns correspond to each model variable}
  \item{structures}{
List of interaction network structures as generated by function networkStructures()}
  \item{verbose}{
Boolean flag. Set to TRUE to print live output on the number of steps performed}
\item{iterations}{Number of times the regression algorithm should be performed on 
each structure. The default is 1 for only one parameter estimation run per 
structure. If set to integers x > 1 the algorithm runs x times and keeps the best 
solution according to the goodness of fit. The number of different solutions found 
and the number of times the optimal solution was achieved is kept in the regression 
attributes in order to evaluate how many local maximas were found, and how certain 
the user can be that the kept optimal could approximate a global optimum point.}
}
\value{
Returns a list containing two objects, a list displaying all networks run and their 
respective goodness of fit, and a list containing full results for each run}
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

