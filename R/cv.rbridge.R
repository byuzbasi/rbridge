cv.rbridge = function (X, y, q, R,r, lambda,nfolds=10,
                          lambda.min=ifelse(n>p,.001,.05), nlambda=100,
                          eta=1e-7, converge=10^10,num_threads = 10) {
  if (class(X) != "matrix") {
    tmp <- try(X <- model.matrix(~0 + ., data = X), silent = TRUE)
    if (class(tmp)[1] == "try-error") 
      stop("X must be a matrix or able to be coerced to a matrix")
  }
  if (storage.mode(X) == "integer") 
    storage.mode(X) <- "double"
  if (class(y) != "numeric") {
    tmp <- try(y <- as.numeric(y), silent = TRUE)
    if (class(tmp)[1] == "try-error") 
      stop("y must numeric or able to be coerced to numeric")
  }
  
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
  
  
  #R <- R1.mat; r <- r1.vec;
  fit <- CV_RBridge(XX,yy,q, lambda, R,r, eta=eta, converge=converge, num_folds=nfolds ,num_threads=10)
  

  
  cve <- apply(fit, 1, sum)
  #cvse <- apply(fit, 1, sd)/sqrt(nlambda)
  cvse <- sqrt(apply(scale(t(fit), cve, FALSE)^2, 2, mean,na.rm = TRUE)/(n - 1))
  #cvse <- sqrt(apply(fit, 1, sd,na.rm = TRUE)/(n - 1))
  
  
  #rbridge.fit = RBridge(XX, yy, q, lambda, R, r)
  rbridge.fit = rbridge(X, y, q, R,r,lambda=lambda,eta=eta)
  
  nz = sapply(predict(rbridge.fit, type = "nonzero"),
              length)
  

  
  #rbridge.fit$betas[abs(rbridge.fit$betas)<1e-07] <- 0 # think about that
  
  
  val <- list(cve = cve, cvse = cvse, cvup = cve + cvse, 
              cvlo = cve - cvse, lambda = lambda, nz=nz, 
              betas = rbridge.fit$betas )
  lamin <- getmin(lambda, cve, cvse) 
  obj = c(val, as.list(lamin))
  class(obj) = "cv.rbridge"
  obj
  
}



