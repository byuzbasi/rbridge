#' @title Fit a bridge coefficient path
#' @description
#' Fit a bridge-penalized least-squares path using local quadratic approximation.
#' @param X Numeric design matrix with \code{n} observation rows and \code{p}
#' predictor columns. Supply finite values and predictors with nonzero variation;
#' omit an intercept column. A data frame is converted using \code{model.matrix(~0+., X)};
#' restrictions and future predictions must use the resulting predictor order.
#' @param y Numeric response vector of length \code{n}, containing finite values.
#' @param q Bridge-penalty exponent, a finite number in \code{(0, 2]}.
#' Values 1 and 2 specify the L1 and L2 penalty forms. Estimation uses local
#' quadratic approximation (LQA).
#' @param lambda Optional nonempty numeric vector of finite, nonnegative penalty
#' values. Its order is retained. When supplied, it determines the fitted path
#' and overrides \code{nlambda} and the grid argument \code{lambda.min}.
#' @param lambda.min Positive absolute endpoint of the automatically generated
#' penalty grid. The default is \code{0.001} when \code{n > p}, and \code{0.05}
#' otherwise. This argument is a penalty value, not a ratio. In CV results,
#' the field also named \code{lambda.min} instead denotes the selected penalty.
#' @param nlambda Number of automatically generated penalty values, an integer
#' of at least 2. The default is \code{100}; ignored when \code{lambda} is supplied.
#' @param eta Positive numerical tolerance, with default \code{1e-7}.
#' Used for coefficient elimination and stopping on squared coefficient changes.
#' @param converge Nonnegative initial convergence-distance value, retained for
#' compatibility. The default is \code{1e10}. Iteration stops when the updated
#' squared coefficient change is at most \code{eta}, or at the historical cap.
#'
#' @details
#' Predictors are centered and divided by their centered root-mean-square
#' (RMS) deviations, using the observation count \code{n} as the divisor.
#' The response is centered. The intercept is reconstructed after fitting,
#' and returned coefficients are expressed on the original predictor scale.
#' The generated penalties are logarithmically spaced between an internally
#' computed starting value and the absolute endpoint \code{lambda.min}.
#'
#' Let \eqn{Z} be the \eqn{n} by \eqn{p} centered, RMS-standardized
#' design, \eqn{y_c} the centered response vector of length \eqn{n}, and
#' \eqn{b} the \eqn{p}-dimensional standardized slope vector.
#' For exponent \eqn{q \in (0,2]} and penalty weight \eqn{\lambda \ge 0},
#' Equation (1) defines the penalized least-squares target:
#' \deqn{\min_{b}
#' \left\{\|y_c-Zb\|_2^2+\lambda\sum_{j=1}^{p}|b_j|^q\right\}. \qquad (1)}
#' In Equation (1), \eqn{\|\cdot\|_2^2} is the squared Euclidean norm,
#' so its first term is the unnormalized residual sum of squares.
#' The index \eqn{j} runs over the predictors; the second term is the bridge
#' penalty, with \eqn{\lambda} controlling its weight relative to residual error.
#'
#' The fitting routine applies the article's LQA updates, coefficient elimination
#' and stopping rules. The criterion is nonconvex when \eqn{q < 1}, so the
#' iterative result is not a guarantee of a global minimum. The getting-started
#' vignette explains the penalty geometry and the fixed-LQA restriction correction.
#'
#' @return A list of class \code{bridge} with components:
#' \item{betas}{Numeric coefficient matrix with \code{p + 1} rows and
#' \code{length(lambda)} columns, one column per fitted penalty.
#' The first row is \code{(Intercept)}; the remaining rows are original-scale
#' slopes in predictor order. Column names identify the fitted penalties.}
#' \item{lambda}{Numeric vector of penalties, in fitted column order.}
#'
#' @references Yuzbasi, B., Arashi, M. and Akdeniz, F. (2021).
#' Penalized regression via the restricted bridge estimator.
#' Soft Computing, 25, 8401--8416. \doi{10.1007/s00500-021-05763-9}.
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{cv.bridge}}, \code{\link{coef.bridge}}, \code{\link{predict.bridge}}
#' @examples
#' # Reproducible synthetic demonstration: 100 observations, eight predictors.
#' set.seed(2019)
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' n <- 100
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- as.numeric(X %*% beta + rnorm(n))
#'
#' # Five penalties keep this help example fast; the default is 100.
#' # L1 and L2 penalty forms on the same observations.
#' model1 <- bridge(X, y, q = 1, nlambda = 5)
#' model2 <- bridge(X, y, q = 2, nlambda = 5)
#' dim(model1$betas)
#' # Extract one fitted column and predict five observations.
#' selected <- model1$lambda[2]
#' coef(model1, s = selected)
#' predict(model1, newx = X[1:5, , drop = FALSE], s = selected)
#' @export
bridge <- function(X, y, q=1, lambda.min=ifelse(n>p,.001,.05), nlambda=100,
                    lambda, eta=1e-7, converge=10^10) {
  
  # Coersion
  if (!is(X, "matrix")) {
    tmp <- try(X <- model.matrix(~0+., data=X), silent=TRUE)
    if (class(tmp)[1] == "try-error") stop("X must be a matrix or able to be coerced to a matrix")
  }
  if (storage.mode(X)=="integer") 
    storage.mode(X) <- "double"
  
  if (!is(y, "numeric")) {
    tmp <- try(y <- as.numeric(y), silent=TRUE)
    if (class(tmp)[1] == "try-error") stop("y must numeric or able to be coerced to numeric")
  }
  
  
  # Error checking
  if (any(is.na(y)) | any(is.na(X))) stop("Missing data (NA's) detected.  Take actions (e.g., removing cases, removing features, imputation) to eliminate missing data before passing X and y to ncvreg")
  if (length(y) != nrow(X)) stop("X and y do not have the same number of observations")
  
  .validate_bridge_data(X, y, q, eta, converge)

  ## Set up XX, yy, lambda
  stdX <- standard(X)
  XX <- stdX$xx
  p <- ncol(XX)
  ns <- c(1:p)
  yy <- y - mean(y)
  n <- length(yy)
  
  if (missing(lambda)) {
    if (length(nlambda) != 1L || !is.finite(nlambda) ||
        nlambda != as.integer(nlambda) || nlambda < 2L)
      stop("nlambda must be an integer of at least 2")
    if (length(lambda.min) != 1L || !is.finite(lambda.min) || lambda.min <= 0)
      stop("lambda.min must be a finite positive number")
    #lambda <- as.double(rev(sort(Lambdas_Grid(XX, yy,q,lambda.min, nlambda))))
    lambda <- Lambdas_Grid(XX, yy,q,lambda.min, nlambda)
    user.lambda <- FALSE
  } else {
    nlambda <- length(lambda)
    user.lambda <- TRUE
  }
  .validate_bridge_lambda(lambda)
  
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
