\name{draw_interaction_network}
\alias{draw_interaction_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add stochasticity to in silico generated data}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw_interaction_network(network, main=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network}{
%%     ~~Describe \code{network} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
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
function(network, main=NULL){
  
  igraph::plot.igraph(network,layout = igraph::layout.fruchterman.reingold , 
vertex.label.color="black",edge.color="black", edge.curved=TRUE, main=main)
  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
