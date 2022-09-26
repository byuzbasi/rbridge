\name{coef.cv.rbridge}
\alias{coef.cv.rbridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{make predictions from a "cv.rbridge" object.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coef.cv.rbridge(object, s = c("lambda.1se", "lambda.min"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Fitted "bridge" model object.
}
  \item{s}{
Value of the penalty parameter '\code{lambda.min}' or '\code{lambda.1se}'.
}
  \item{\dots}{
Additional arguments for compatibility
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
set.seed(2019)
beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
p <- length(beta)
beta <- matrix(beta, nrow = p, ncol = 1)
p.active <- which(beta != 0)

### Restricted Matrix and vector
c1 <- c(1,1,0,0,1,0,0,0)
R.mat <- matrix(c1,nrow = 1, ncol = p)
r.vec <- as.matrix(c(6.5),1,1)

n = 100
X = matrix(rnorm(n*p),n,p)
y = X\%*\%beta + rnorm(n) 

model1 <- cv.rbridge(X, y, q = 1, R.mat, r.vec)
coef(model1,s='lambda.min')
coef(model1,s='lambda.1se')

model2 <- cv.rbridge(X, y, q = 2, R.mat, r.vec)
coef(model2,s='lambda.min')
coef(model2,s='lambda.1se')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
