\name{plot.cv.bridge}
\alias{plot.cv.bridge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot the cross-validation curve produced by cv.bridge
%%  ~~function to do ... ~~
}
\description{
Produces a coefficient profile plot of the coefficient paths for a fitted "bridge" object.
}
\usage{
plot.cv.bridge(object, sign.lambda = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
fitted "bridge" model
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

n = 100
X = matrix(rnorm(n*p),n,p)
y = X\%*\%beta + rnorm(n) 

model1 <- cv.bridge(X, y, q = 1)
plot(model1)

model2 <- cv.bridge(X, y, q = 2)
plot(model2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
