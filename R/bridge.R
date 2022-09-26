bridge <- function(X, y, q=1, lambda.min=ifelse(n>p,.001,.05), nlambda=100,
                    lambda, eta=1e-7, converge=10^10) {
  
  # Coersion
  if (class(X) != "matrix") {
    tmp <- try(X <- model.matrix(~0+., data=X), silent=TRUE)
    if (class(tmp)[1] == "try-error") stop("X must be a matrix or able to be coerced to a matrix")
  }
  if (storage.mode(X)=="integer") storage.mode(X) <- "double"
  if (class(y) != "numeric") {
    tmp <- try(y <- as.numeric(y), silent=TRUE)
    if (class(tmp)[1] == "try-error") stop("y must numeric or able to be coerced to numeric")
  }
  
  
  # Error checking
  if (nlambda < 2) stop("nlambda must be at least 2")
  if (q > 2) stop("a must be less than 2; choose a small positive number instead")
  if (any(is.na(y)) | any(is.na(X))) stop("Missing data (NA's) detected.  Take actions (e.g., removing cases, removing features, imputation) to eliminate missing data before passing X and y to ncvreg")
  if (length(y) != nrow(X)) stop("X and y do not have the same number of observations")
  
  ## Set up XX, yy, lambda
  stdX <- standard(X)
  XX <- stdX$xx
  p <- ncol(XX)
  ns <- c(1:p)
  yy <- y - mean(y)
  n <- length(yy)
  
  if (missing(lambda)) {
    #lambda <- as.double(rev(sort(Lambdas_Grid(XX, yy,q,lambda.min, nlambda))))
    lambda <- Lambdas_Grid(XX, yy,q,lambda.min, nlambda)
    user.lambda <- FALSE
  } else {
    nlambda <- length(lambda)
    user.lambda <- TRUE
  }
  
  ## Fit
  fit <- Bridge(XX, yy, q, lambda, converge,eta)
  constant <- rep(mean(y),nlambda)
  betas <- matrix(fit, p, nlambda)
  #loss <- res[[2]]
  #iter <- res[[3]]
  
  
  ## Unstandardize
  unbetas <- matrix(0, nrow=(ncol(X)+1), ncol=length(lambda))
  #bbetas <- betas/attr(XX, "scale")[ns]
  #bbetas <- betas # if unstandard
  bbetas <- betas/as.vector(stdX$s)
  unbetas[ns+1,] <- bbetas  
  unbetas[1,] <- constant - crossprod(as.vector(stdX$c), bbetas)
  
  
  
  ## Names
  varnames <- if (is.null(colnames(X))) paste("V",1:ncol(X),sep="") else colnames(X)
  varnames <- c("(Intercept)", varnames)
  dimnames(unbetas) <- list(varnames, lambda)
  
  
  ## Output
  output <- structure(list(betas = unbetas,
                           lambda = lambda),
                      class = "bridge")
  output
}
