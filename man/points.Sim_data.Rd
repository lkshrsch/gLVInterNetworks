\name{points.Sim_data}
\alias{points.Sim_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add points corresponding to the original observations on top of the model solutions}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
points.Sim_data(x, index = NULL, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object of class Sim_data}
  \item{index}{
If wished to plot the original measurements of only one variable for visualization purposes, write index of variable}
  \item{\dots}{
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
function(x, index=NULL , ...){

  if(!is.null(index)){
    points(x = x$obs[,1], y= x$obs[,index+1], col=index)
  }else{
  for(i in 1:ncol(x$obs[,-1])){
    points(x = x$obs[,1], y= x$obs[,i+1], col=i)
  }
  }
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

