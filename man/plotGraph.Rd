\name{plotGraph}
\alias{plotGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot interaction network}
\description{
Plots a graph representing the interaction network described by the data
}
\usage{
plotGraph(x, vsize = 0.1, main = NULL, verbose = FALSE, keepNames = FALSE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object containing parameter matrix in form of x$Parms
}
  \item{vsize}{
Integer inversely proportional to the size of the nodes 
}
  \item{main}{
Title of the plot
}
  \item{verbose}{
Include edge values in output network
}
  \item{keepNames}{
Set to TRUE if plotted Network should keep the names of the variables as given in the observations table. Default = FALSE
}
  \item{\dots}{
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
igraph object
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
Lukas Hirsch
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data <- generate_data(2,2,100,0.1, 20)
#plotGraph(data)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{  }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
