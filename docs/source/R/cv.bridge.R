#' @title Cross-validation for bridge regression
#' @description
#' Evaluate a fitted penalty path using contiguous validation folds and return
#' the full-data coefficients together with two penalty selectors.
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
#' @param nfolds Number of validation folds, an integer between 2 and
#' \code{n}. The default is \code{10}.
#' @param num_threads Requested fold-level workers, a positive integer.
#' The default is \code{10}; the effective count is capped at \code{nfolds}.
#' Builds without OpenMP execute folds sequentially. Use \code{rbridge_info()}
#' to inspect the compiled support.
#'
#' @details
#' Fold assignment follows the supplied row order. The \code{nfolds}
#' validation sets are contiguous blocks; their sizes can differ by one.
#' Centering and RMS scaling use all observations before fold allocation,
#' and each fold fit also reconstructs its training-set intercept.
#' These historical preprocessing and error-bar conventions are retained.
#' The reported validation error aggregates held-out predictions at each
#' fitted penalty, and coefficients in \code{betas} come from a final fit
#' to all observations.
#'
#' For one fixed penalty, let \eqn{K} be \code{nfolds}, \eqn{n} the total
#' observation count, \eqn{I_k} the validation indices in fold \eqn{k},
#' and \eqn{e_i} the held-out response residual for observation \eqn{i}.
#' Equation (1) defines a fold's error contribution \eqn{d_k}, the pooled
#' validation MSE, and the historical error-bar width:
#' \deqn{
#' \begin{aligned}
#' d_k&=\frac{1}{n}\sum_{i\in I_k}e_i^2,\\
#' \mathrm{cve}&=\sum_{k=1}^{K}d_k,\\
#' \mathrm{cvse}&=\sqrt{\frac{1}{K(n-1)}
#' \sum_{k=1}^{K}(d_k-\mathrm{cve})^2}. \qquad (1)
#' \end{aligned}}
#' The folds partition all observations, so the sum in Equation (1) weights
#' each held-out observation equally. The final quantity supplies the plotted
#' bar width and the historical \code{lambda.1se} threshold. Its centering
#' uses the pooled \code{cve}, not the average fold contribution, and it is
#' not a conventional foldwise standard error.
#'
#' \code{predict()} defaults to the minimum-error penalty \code{lambda.min}.
#' \code{coef()} defaults to \code{lambda.1se}. Supply \code{s} explicitly
#' to use the same penalty for both operations.
#' Use \code{plot()} to inspect the curve, error bars and selected penalties.
#'
#' @return A list of class \code{cv.bridge} with components:
#' \item{cve}{Numeric vector of validation mean squared errors, one per penalty.
#' Squared holdout residuals are summed across folds and divided by the total
#' observation count, so each observation has the same weight.}
#' \item{cvse}{Numeric vector from the historical CV error-bar calculation.
#' It is not the conventional standard error of foldwise mean squared errors.}
#' \item{cvup, cvlo}{Upper and lower curves, computed as \code{cve + cvse}
#' and \code{cve - cvse}.}
#' \item{lambda}{Numeric penalty vector in fitted order.}
#' \item{nz}{Number of nonzero slopes, excluding the intercept, in each full-data fit.}
#' \item{betas}{Full-data coefficient matrix with \code{p + 1} rows and
#' \code{length(lambda)} columns. The first row is \code{(Intercept)};
#' the remaining rows are original-scale slopes in predictor order.}
#' \item{lambda.min}{Fitted penalty with the smallest \code{cve}.
#' Ties select the largest penalty among the minimizers. This selected value
#' is distinct from the input argument specifying the generated grid endpoint.}
#' \item{lambda.1se}{Largest fitted penalty whose \code{cve} is no greater
#' than \code{cve + cvse} evaluated at the selected \code{lambda.min}.
#' The name follows the historical convention; the threshold uses the
#' package's \code{cvse} calculation described above.}
#'
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{bridge}}, \code{\link{plot.cv.bridge}}, \code{\link{coef.cv.bridge}}, \code{\link{predict.cv.bridge}}
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
#' # Three contiguous folds and one worker keep this demonstration small.
#' model1 <- cv.bridge(X, y, q = 1,
#'                      nlambda = 5, nfolds = 3, num_threads = 1)
#' model2 <- cv.bridge(X, y, q = 2,
#'                      nlambda = 5, nfolds = 3, num_threads = 1)
#' # Inspect both selectors and explicitly choose the minimum-error fit.
#' c(minimum_error = model1$lambda.min, historical_1se = model1$lambda.1se)
#' coef(model1, s = "lambda.min")
#' predict(model1, newx = X[1:5, , drop = FALSE], s = "lambda.min")
#' plot(model1)
#' @export
cv.bridge = function (X, y, q, lambda,nfolds=10,
                       lambda.min=ifelse(n>p,.001,.05), nlambda=100,
                       eta=1e-7, converge=10^10,num_threads = 10) {
  if (!is(X, "matrix")) {
    tmp <- try(X <- model.matrix(~0+., data=X), silent=TRUE)
    if (class(tmp)[1] == "try-error") 
      stop("X must be a matrix or able to be coerced to a matrix")
  }
  if (storage.mode(X)=="integer") 
    storage.mode(X) <- "double"
  
  if (!is(y, "numeric")) {
    tmp <- try(y <- as.numeric(y), silent=TRUE)
    if (class(tmp)[1] == "try-error") 
      stop("y must numeric or able to be coerced to numeric")
  }
  
  


  
  if (length(nfolds) != 1L || !is.finite(nfolds) ||
      nfolds != as.integer(nfolds) || nfolds < 2L || nfolds > nrow(X))
    stop("nfolds must be an integer between 2 and nrow(X)")
  if (length(num_threads) != 1L || !is.finite(num_threads) ||
      num_threads != as.integer(num_threads) || num_threads < 1L)
    stop("num_threads must be a positive integer")
  if (length(y) != nrow(X) || anyNA(X) || anyNA(y) ||
      any(!is.finite(X)) || any(!is.finite(y)))
    stop("X and y must be finite and have matching observation counts")

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
    lambda <- Lambdas_Grid(XX, yy,q,lambda.min, nlambda)
    user.lambda <- FALSE
  } else {
    nlambda <- length(lambda)
    user.lambda <- TRUE
  }
  .validate_bridge_lambda(lambda)
  
  
  #R <- R1.mat; r <- r1.vec;
  fit <- CV_Bridge(XX,yy,q, lambda, converge=converge, num_folds=nfolds,num_threads=num_threads, eta=eta)
  
  
  
  cve <- apply(fit, 1, sum)
  #cvse <- apply(fit, 1, sd)/sqrt(nlambda)
  cvse <- sqrt(apply(scale(t(fit), cve, FALSE)^2, 2, mean,na.rm = TRUE)/(n - 1))
  #cvse <- sqrt(apply(fit, 1, sd,na.rm = TRUE)/(n - 1))
  
  
  
  bridge.fit = bridge(X, y, q,lambda=lambda, eta=eta, converge=converge)
  
  nz = sapply(predict(bridge.fit, type = "nonzero"),
              length)
  
  val <- list(cve = cve, cvse = cvse, cvup = cve + cvse, 
              cvlo = cve - cvse, lambda = lambda, nz=nz, 
              betas = bridge.fit$betas )
  lamin <- getmin(lambda, cve, cvse) 
  obj = c(val, as.list(lamin))
  class(obj) = "cv.bridge"
  obj
  
}



