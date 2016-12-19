\name{regularized_linear_regression}
\alias{regularized_linear_regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute regularized parameter estimates for the linear model}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
regularized_linear_regression(X,Y, alpha, cvfolds)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{data} here~~
}
}
\arguments{
  \item{Y}{
%%     ~~Describe \code{data} here~~
}
}
\arguments{
  \item{alpha}{
%%     ~~Describe \code{data} here~~
}
}
\arguments{
  \item{cvfolds}{
%%     ~~Describe \code{data} here~~
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
function(X,Y, alpha, cvfolds){
  
  
  #require(glmnet)
  
  sol = matrix(0, nrow = nrow(Y), ncol = nrow(X))
  
  # X <- X[-1,]  # Get rid of intercept , it is automatically placed by glmnet
  
  
  mse = 0
  
  
  
  for(i in 1:nrow(Y)){
    
    
    cv <- glmnet::cv.glmnet(t(X)[,-1],Y[i,], alpha=alpha ,nfolds = cvfolds,standardize=T)
    
    small.lambda.index <- which(cv$lambda == cv$lambda.1se)
    small.lambda.betas <- cv$glmnet.fit$beta[,small.lambda.index]
    small.lambda.betas <- c(cv$glmnet.fit$a0[small.lambda.index],small.lambda.betas)
    sol[i,] = small.lambda.betas
    mse[i] = cv$cvm[small.lambda.index]
    #print(cv$lambda.1se)
  }
  
  mse = mean(mse)

  return(list(sol,mse))
  
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
