\name{cv.bridge}
\alias{cv.bridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cross-validation for bridge
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cv.bridge(X, y, q, lambda, nfolds = 10, lambda.min = ifelse(n > p, 0.001, 0.05), nlambda = 100, eta = 1e-07, converge = 10^10, num_threads = 10)
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
  \item{lambda}{
is the squence of lambda; default is \code{NULL}, and \code{cv.bridge} chooses its own sequence.
}
  \item{nfolds}{
number of folds - default is 10.
}
  \item{lambda.min}{
the minimum value of lambda as in rbridge.
}
  \item{nlambda}{
the length of the squence of lambda.
}
  \item{eta}{
is same as rbridge.
}
  \item{converge}{
is same as rbridge.
}
  \item{num_threads}{
Number of threads used for parallel computation over the folds.
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

model1 <- cv.bridge(X, y, q = 0.5)
print(model1)
coef(model1,s="lambda.min")
coef(model1,s="lambda.1se")
plot(model1)
predict(model1,newx=X[1:5,], s="lambda.min", type="response")
predict(model1, s="lambda.min",type="coefficient")
predict(model1, s="lambda.1se",type="coefficient")


model2 <- cv.bridge(X, y, q = 1)
print(model2)
coef(model2,s='lambda.min')
coef(model2,s='lambda.1se')
plot(model2)
predict(model2,newx=X[1:5,], s="lambda.min", type="response")
predict(model2, s="lambda.min",type="coefficient")
predict(model2, s="lambda.1se",type="coefficient")


model3 <- cv.bridge(X, y, q = 1)
print(model3)
coef(model3,s='lambda.min')
coef(model3,s='lambda.1se')
plot(model3)
predict(model3,newx=X[1:5,], s="lambda.min", type="response")
predict(model3, s="lambda.min",type="coefficient")
predict(model3, s="lambda.1se",type="coefficient")



model4 <- cv.bridge(X, y, q = 2)
print(model4)
coef(model4,s='lambda.min')
coef(model4,s='lambda.1se')
plot(model4)
predict(model4,newx=X[1:5,], s="lambda.min", type="response")
predict(model4, s="lambda.min",type="coefficient")
predict(model4, s="lambda.1se",type="coefficient")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line