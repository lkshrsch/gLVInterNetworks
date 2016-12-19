\name{generate_interaction_network}
\alias{generate_interaction_network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add stochasticity to in silico generated data}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generate_interaction_network(parms_matrix, observations, substrate=FALSE, vsize = 
0.1, verbose = FALSE, keepNames = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parms_matrix}{
%%     ~~Describe \code{parms_matrix} here~~
}
  \item{observations}{
%%     ~~Describe \code{observations} here~~
}
  \item{substrate}{
%%     ~~Describe \code{substrate} here~~
}
  \item{vsize}{
Inversely proportional to the size of nodes in network output. Default is 0.1 and 
often must be set to another value (through try and error) for proper visualization
}  
  \item{verbose}{
Set to TRUE to label edges to the corresponding interaction parameter number
}
  \item{keepNames}{
Set to TRUE to label the nodes in the network with the names of the variables in the 
observations table
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
