\name{bridge}
\alias{bridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fit a Bridge Estimation
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bridge(X, y, q = 1, lambda.min = ifelse(n > p, 0.001, 0.05), nlambda = 100, lambda, eta = 1e-07, converge = 10^10)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{X}{
\code{X} matrix as in rbridge.
}
  \item{y}{
response \code{y} as in rbridge.
}
  \item{q}{
the degree of norm as in rbridge.
}
  \item{lambda.min}{
the minimum value of lambda as in rbridge.
}
  \item{nlambda}{
the length of the squence of lambda.
}
  \item{lambda}{
A user supplied lambda sequence. By default, the program compute a squence of values the length of nlambda. 
}
  \item{eta}{
is a preselected small positive threshold value as in rbridge.
}
  \item{converge}{
is the value of converge as in rbridge.
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

n = 100
X = matrix(rnorm(n*p),n,p)
y = X\%*\%beta + rnorm(n) 

model1 <- cv.bridge(X, y, q = 1)
print(model1)

model2 <- cv.bridge(X, y, q = 2)
print(model2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
