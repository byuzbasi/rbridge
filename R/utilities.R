######################################################################
## These functions are minor modifications or directly copied from the
## glmnet package:
##        Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).
## Regularization Paths for Generalized Linear Models via Coordinate
#   Descent.
##        Journal of Statistical Software, 33(1), 1-22.
##        URL http://www.jstatsoft.org/v33/i01/.
## The reason they are copied here is because they are internal functions
## and hence are not exported into the global environment.
## The original comments and header are preserved.

.validate_bridge_data <- function(X, y, q, eta, converge) {
  if (!is.matrix(X) || !is.numeric(X) || nrow(X) < 2L || ncol(X) < 1L)
    stop("X must be a numeric design matrix with at least two rows and one column")
  if (length(y) != nrow(X) || any(!is.finite(X)) || any(!is.finite(y)))
    stop("X and y must be finite and have matching observation counts")
  if (any(colSums(sweep(X, 2L, colMeans(X), "-")^2) == 0))
    stop("X contains a constant column; supply predictors with nonzero variation")
  if (length(q) != 1L || !is.finite(q) || q <= 0 || q > 2)
    stop("q must be a finite number greater than 0 and at most 2")
  if (length(eta) != 1L || !is.finite(eta) || eta <= 0)
    stop("eta must be a finite positive number")
  if (length(converge) != 1L || !is.finite(converge) || converge < 0)
    stop("converge must be a finite nonnegative initial distance")
  invisible(NULL)
}

.validate_bridge_lambda <- function(lambda) {
  if (!is.numeric(lambda) || !length(lambda) ||
      any(!is.finite(lambda)) || any(lambda < 0))
    stop("lambda must be a nonempty numeric vector of finite nonnegative values")
  invisible(NULL)
}

.validate_bridge_restriction <- function(R, r, p) {
  if (!is.matrix(R) || !is.numeric(R) || ncol(R) != p ||
      nrow(R) < 1L || any(!is.finite(R)))
    stop("R must be a finite numeric matrix with one column per predictor")
  if (!is.numeric(r) || length(r) != nrow(R) || any(!is.finite(r)) ||
      (!is.null(dim(r)) && !identical(dim(r), c(nrow(R), 1L))))
    stop("r must be a finite numeric vector or one-column matrix with one value per restriction")
  invisible(NULL)
}

getmin <- function (lambda, cvm, cvsd) 
{
  cvmin = min(cvm, na.rm = TRUE)
  idmin = cvm <= cvmin
  lambda.min = max(lambda[idmin], na.rm = TRUE)
  idmin = match(lambda.min, lambda)
  semin = (cvm + cvsd)[idmin]
  idmin = cvm <= semin
  lambda.1se = max(lambda[idmin], na.rm = TRUE)
  list(lambda.min = lambda.min, lambda.1se = lambda.1se)
}

nonzeroCoef = function (beta, bystep = FALSE) 
{
  ### bystep = FALSE means which variables were ever nonzero
  ### bystep = TRUE means which variables are nonzero for each step
  nr=nrow(beta)
  if (nr == 1) {#degenerate case
    if (bystep) 
      apply(beta, 2, function(x) if (abs(x) > 0) 
        1
        else NULL)
    else {
      if (any(abs(beta) > 0)) 
        1
      else NULL
    }
  }
  else {
    beta=abs(beta)>0 # this is sparse
    which=seq(nr)
    ones=rep(1,ncol(beta))
    nz=as.vector((beta%*%ones)>0)
    which=which[nz]
    if (bystep) {
      if(length(which)>0){
        beta=as.matrix(beta[which,,drop=FALSE])
        nzel = function(x, which) if (any(x)) 
          which[x]
        else NULL
        which=apply(beta, 2, nzel, which)
        if(!is.list(which))which=data.frame(which)# apply can return a matrix!!
        which
      }
      else{
        dn=dimnames(beta)[[2]]
        which=vector("list",length(dn))
        names(which)=dn
        which
      }
      
    }
    else which
  }
}


er.bars <-
  function(x, upper, lower, width = 0.02, ...)
  {
    xlim <- range(x)
    barw <- diff(xlim) * width
    segments(x, upper, x, lower, ...)
    segments(x - barw, upper, x + barw, upper, ...)
    segments(x - barw, lower, x + barw, lower, ...)
    range(upper, lower)
  }

