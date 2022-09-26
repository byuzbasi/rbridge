\name{rbridge}
\alias{rbridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fit a Restricted Bridge Estimation
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rbridge(X, y, q = 1, R, r, lambda.min = ifelse(n > p, 0.001, 0.05), nlambda = 100, lambda, eta = 1e-07, converge = 10^10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  The design matrix. rbridge standardizes the data and includes an intercept by default
}
  \item{y}{
  The response vector.
}
  \item{q}{ is the degree of norm which includes ridge regression with \code{q=2} and lasso estimates with \code{q=1} as special cases
}
  \item{R}{
  is \code{m} by \code{p} \code{(m<p)} matrix of constants.
}
  \item{r}{
  is a \code{m}-vector of known prespecified constants. If it is given true restriction, then \deqn{r - R\beta = 0.} Values for \code{r} should be given as a matrix. See "Examples".
}
  \item{lambda.min}{
  The smallest value for lambda if \code{n>p} is \code{0.001} and \code{0.05} otherwise.
}
  \item{nlambda}{
  The number of lambda values - default is \code{100}
}
  \item{lambda}{
  A user supplied lambda sequence. By default, the program compute a squence of values the length of nlambda. 
}
  \item{eta}{
  is a preselected small positive threshold value. It is deleted \code{jth} variable to make the algorithm stable and also is excluded \code{jth} variable from the final model. Default is \code{1e-07}.
}
  \item{converge}{
 is the value of converge. Defaults is \code{10^10}. In each iteration, it is calculated by sum of square the change in linear predictor for each coefficient. The algorithm iterates until \code{converge > eta}.
 
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

\author{Bahadir Yuzbasi, Mohammad Arashi, Fikri Akdeniz\cr 
Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
p <- length(beta)
beta <- matrix(beta, nrow = p, ncol = 1)
p.active <- which(beta != 0)

### Restricted Matrix and vector
### Res 1
c1 <- c(1,1,0,0,1,0,0,0)
R1.mat <- matrix(c1,nrow = 1, ncol = p)
r1.vec <- as.matrix(c(6.5),1,1)

### Res 2
c2 <- c(-1,1,0,0,1,0,0,0)
R2.mat <- matrix(c2,nrow = 1, ncol = p)
r2.vec <- matrix(c(0.5),nrow = 1, ncol = 1)

### Res 3
R3.mat <- t(matrix(c(c1,c2),nrow = p, ncol = 2))
r3.vec <- matrix(c(6.5,0.5),nrow = 2, ncol = 1)

### Res 4
R4.mat = diag(1,p,p)[-p.active,]
r4.vec <- matrix(rep(0,p-length(p.active)),nrow = p-length(p.active), ncol = 1)


n = 100
X = matrix(rnorm(n*p),n,p)
y = X\%*\%beta + rnorm(n) 

model1 <- rbridge(X, y, q = 1, R1.mat, r1.vec)
print(model1)

model2 <- rbridge(X, y, q = 1, R2.mat, r2.vec)
print(model2)

model3 <- rbridge(X, y, q = 1, R3.mat, r3.vec)
print(model3)

model4 <- rbridge(X, y, q = 1, R4.mat, r4.vec)
print(model4)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
