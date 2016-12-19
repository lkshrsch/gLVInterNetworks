\name{nonlinear_Regression}
\alias{nonlinear_Regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parameter estimation through gradient search of continuous nonlinear gLV model}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nonlinear_Regression(data, parms0 = NULL, ftol = 1e-8 , ptol = 1e-8, maxiter = 100, lowerbound = rep(-20,ncol(data$obs)*(ncol(data$obs)-1)), upperbound = rep(20,ncol(data$obs)*(ncol(data$obs)-1)), method = "Marq")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data input containing a time series of observations in longitudinal matrix form}
  \item{parms0}{
Optional. Starting parameter vector. Default = Zero vector}
  \item{ftol}{
Objective function output tolerance before stopping iterative optimization}
  \item{ptol}{
Parameter change tolerance in output of objective function}
 \item{maxiter}{
Maximal number of iterations allowed before breaking the gradient search algorithm}
 \item{lowerbound}{
Numerical vector of equal length as parameter vector describing lower bound for constrained parameter search
}
 \item{upperbound}{
Numerical vector of equal length as parameter vector describing upper bound for constrained parameter search
}
 \item{method}{
Method used for optimization of the objective function. Default is "Marq" for Leverberg-Marquandt
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
