\name{plot.cv.rbridge}
\alias{plot.cv.rbridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot the cross-validation curve produced by cv.rbridge
%%  ~~function to do ... ~~
}
\description{
Produces a coefficient profile plot of the coefficient paths for a fitted "rbridge" object.
}
\usage{
plot.cv.rbridge(object, sign.lambda = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
fitted "rbridge" model
}
  \item{sign.lambda}{
Either plot against log(lambda) (default which means \code{1}) or its negative if \code{sign.lambda=-1}.
}
  \item{\dots}{
Other graphical parameters to plot.
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

### Restricted Matrix and vector
### Res 1
c1 <- c(1,1,0,0,1,0,0,0)
R1.mat <- matrix(c1,nrow = 1, ncol = p)
r1.vec <- as.matrix(c(6.5),1,1)

### Res 2
c2 <- c(-1,1,0,0,1,0,0,0)
R2.mat <- matrix(c2,nrow = 1, ncol = p)
r2.vec <- matrix(c(0.5),nrow = 1, ncol = 1)

n = 100
X = matrix(rnorm(n*p),n,p)
y = X\%*\%beta + rnorm(n) 

model1 <- cv.rbridge(X, y, q = 1, R1.mat, r1.vec)
plot(model1)

model2 <- cv.rbridge(X, y, q = 2, R1.mat, r1.vec)
plot(model2)

model3 <- cv.rbridge(X, y, q = 1, R2.mat, r2.vec)
plot(model3)

model4 <- cv.rbridge(X, y, q = 2, R2.mat, r2.vec)
plot(model4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
