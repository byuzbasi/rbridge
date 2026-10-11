scalar_predict <- function(object, newx, s, type) {
  if (type == "nonzero") {
    return(nonzeroCoef(object$betas[-1L, , drop = FALSE], bystep = TRUE))
  }
  if (is.numeric(s)) {
    index <- match(s, object$lambda)
    if (!length(index) || anyNA(index))
      stop("Numeric s must match values in object$lambda")
  } else {
    if (length(s) != 1L) stop("s must specify one named selector")
    selected <- object[[s]]
    if (is.null(selected)) {
      if (s == "lambda.min") selected <- min(object$lambda)
      else stop("lambda.1se requires a cross-validated model; use numeric s for a path model")
    }
    index <- match(selected, object$lambda)
    if (anyNA(index)) stop("Selected lambda is absent from the fitted path")
  }
  coefficients <- object$betas[, index, drop = FALSE]
  if (type == "coefficients") return(coefficients)
  if (missing(newx)) stop("newx must be supplied for response predictions")
  if (is.numeric(newx) && is.null(dim(newx))) newx <- matrix(newx, nrow = 1L)
  if (!is.matrix(newx) || !is.numeric(newx))
    stop("newx must be a numeric vector or matrix")
  if (ncol(newx) + 1L != nrow(object$betas))
    stop("newx does not have the right number of columns")
  output <- cbind(1, newx) %*% coefficients
  if (ncol(output) == 1L) as.numeric(output) else output
}
#' @title Predict from a fitted cv.rbridge object
#' @description
#' Compute response predictions, extract coefficients, or identify nonzero
#' predictors from a fitted cv.rbridge object.
#' @param object Fitted object returned by \code{\link{cv.rbridge}}, with \code{p} predictors in its training design.
#' @param newx Numeric matrix with one row per new observation and the
#' same \code{p} columns, in the same order, as the training design.
#' A numeric vector of length \code{p} represents one observation.
#' Values are supplied on the original predictor scale. Required only for
#' \code{type = "response"}.
#' @param type Output to return: \code{"response"} for fitted response values,
#' \code{"coefficients"} for intercept and slopes, or \code{"nonzero"}
#' for predictor indices over the entire fitted path.
#' @param s Penalty selection, using numeric fitted values or a character name.
#' Numeric values must match \code{object$lambda} exactly; no interpolation is used.
#' The character selectors are:
#' \describe{
#' \item{\code{"lambda.min"}}{Select the penalty with minimum CV error.}
#' \item{\code{"lambda.1se"}}{Select using the historical CV error bars.}
#' }
#' The default is \code{lambda.min}.
#' @param ... Additional arguments accepted for compatibility.
#' @details
#' Prediction uses the fitted intercept and original-scale slopes.
#' Columns of \code{newx} are matched by position, not reordered by their names.
#' For \code{type = "nonzero"}, all fitted penalties are inspected and
#' \code{s} does not restrict the returned indices.
#' The defaults differ between \code{coef()} and \code{predict()}:
#' coefficient extraction uses \code{lambda.1se}, whereas response prediction
#' uses \code{lambda.min}. Specify the same \code{s} in both calls when comparing
#' coefficients and their predictions.
#' Returned slopes are on the original predictor scale; the linear
#' restrictions concern standardized slopes. See \code{\link{rbridge}} for
#' converting scales before checking restriction residuals.
#'
#' @return For \code{type = "response"}, a numeric vector for one
#' selected penalty or a matrix with one row per new observation and one
#' column per selected penalty. For \code{type = "coefficients"}, a numeric
#' matrix with \code{p + 1} rows, including the intercept, even for one penalty.
#' For \code{type = "nonzero"}, a pathwise collection of 1-based predictor
#' indices excluding the intercept. Its historical container can simplify
#' depending on the support pattern and number of predictors.
#'
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{coef.cv.rbridge}}, \code{\link{cv.rbridge}}
#' @examples
#' # Reproducible synthetic demonstration: 100 observations, eight predictors.
#' set.seed(2019)
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' n <- 100
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- as.numeric(X %*% beta + rnorm(n))
#' # A user-specified sum of standardized slopes: b[1] + b[2] + b[5] = 6.5.
#' # This target is illustrative; it is not asserted to equal the generating sum.
#' R1.mat <- matrix(c(1, 1, 0, 0, 1, 0, 0, 0), nrow = 1)
#' r1.vec <- matrix(6.5, nrow = 1, ncol = 1)
#'
#' # Five penalties keep this help example fast; the default is 100.
#' model1 <- cv.rbridge(X, y, q = 1, R = R1.mat, r = r1.vec, nlambda = 5,
#'                      nfolds = 3, num_threads = 1)
#' # Predict five observations at one explicitly selected penalty.
#' predict(model1, newx = X[1:5, , drop = FALSE], s = "lambda.min")
#' # Extract coefficients using the complete type name.
#' predict(model1, s = "lambda.min", type = "coefficients")
#' # Predictor indices for every fitted penalty; the intercept is excluded.
#' predict(model1, type = "nonzero")
#' @export
predict.cv.rbridge = function(object, newx, s=c("lambda.min","lambda.1se"),
                              type = c("response", "nonzero","coefficients"), ...) {
  type <- match.arg(type)
  if (!is.numeric(s)) s <- match.arg(s)
  scalar_predict(object, newx, s, type)
}




#' @title Extract coefficients from a fitted cv.rbridge object
#' @description
#' Extract original-scale intercept and slope coefficients from a fitted
#' cross-validated cv.rbridge object.
#' @param object Fitted object returned by \code{\link{cv.rbridge}}, with \code{p} predictors in its training design.
#' @param s Penalty selection, using numeric fitted values or a character name.
#' Numeric values must match \code{object$lambda} exactly; no interpolation is used.
#' The character selectors are:
#' \describe{
#' \item{\code{"lambda.min"}}{Select the penalty with minimum CV error.}
#' \item{\code{"lambda.1se"}}{Select using the historical CV error bars.}
#' }
#' The default is \code{lambda.1se}.
#' @param ... Additional arguments accepted for compatibility.
#' @details
#' The returned matrix retains its dimensions for a single selected penalty.
#' The defaults differ between \code{coef()} and \code{predict()}:
#' coefficient extraction uses \code{lambda.1se}, whereas response prediction
#' uses \code{lambda.min}. Specify the same \code{s} in both calls when comparing
#' coefficients and their predictions.
#' Returned slopes are on the original predictor scale; the linear
#' restrictions concern standardized slopes. See \code{\link{rbridge}} for
#' converting scales before checking restriction residuals.
#'
#' @return Numeric matrix with \code{p + 1} rows and one column per
#' selected penalty, even when only one penalty is selected. The first row
#' is \code{(Intercept)}; subsequent rows are original-scale slopes in the
#' training predictor order.
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{predict.cv.rbridge}}, \code{\link{cv.rbridge}}
#' @examples
#' # Reproducible synthetic demonstration: 100 observations, eight predictors.
#' set.seed(2019)
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' n <- 100
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- as.numeric(X %*% beta + rnorm(n))
#' # A user-specified sum of standardized slopes: b[1] + b[2] + b[5] = 6.5.
#' # This target is illustrative; it is not asserted to equal the generating sum.
#' R1.mat <- matrix(c(1, 1, 0, 0, 1, 0, 0, 0), nrow = 1)
#' r1.vec <- matrix(6.5, nrow = 1, ncol = 1)
#'
#' # Five penalties keep this help example fast; the default is 100.
#' model1 <- cv.rbridge(X, y, q = 1, R = R1.mat, r = r1.vec, nlambda = 5,
#'                      nfolds = 3, num_threads = 1)
#' # A single selected penalty still gives a coefficient matrix.
#' coefficients <- coef(model1, s = "lambda.min")
#' coefficients
#' dim(coefficients)
#' # The default uses the historical error-bar selector.
#' coef(model1)
#' @export
coef.cv.rbridge=function(object, s=c("lambda.1se","lambda.min"),...) {
  if (!is.numeric(s)) s <- match.arg(s)
  predict(object, s=s, type="coefficients")
}



#' @title Predict from a fitted rbridge object
#' @description
#' Compute response predictions, extract coefficients, or identify nonzero
#' predictors from a fitted rbridge object.
#' @param object Fitted object returned by \code{\link{rbridge}}, with \code{p} predictors in its training design.
#' @param newx Numeric matrix with one row per new observation and the
#' same \code{p} columns, in the same order, as the training design.
#' A numeric vector of length \code{p} represents one observation.
#' Values are supplied on the original predictor scale. Required only for
#' \code{type = "response"}.
#' @param type Output to return: \code{"response"} for fitted response values,
#' \code{"coefficients"} for intercept and slopes, or \code{"nonzero"}
#' for predictor indices over the entire fitted path.
#' @param s Numeric fitted penalty value(s).
#' Values must match \code{object$lambda} exactly; no interpolation is used.
#' Omitting this argument returns all fitted penalties.
#'
#' The character selector \code{lambda.min} requests the smallest fitted
#' penalty. Use a CV object for the selector \code{lambda.1se}.
#' @param ... Additional arguments accepted for compatibility.
#' @details
#' Prediction uses the fitted intercept and original-scale slopes.
#' Columns of \code{newx} are matched by position, not reordered by their names.
#' For \code{type = "nonzero"}, all fitted penalties are inspected and
#' \code{s} does not restrict the returned indices.
#' Returned slopes are on the original predictor scale; the linear
#' restrictions concern standardized slopes. See \code{\link{rbridge}} for
#' converting scales before checking restriction residuals.
#'
#' @return For \code{type = "response"}, a numeric vector for one
#' selected penalty or a matrix with one row per new observation and one
#' column per selected penalty. For \code{type = "coefficients"}, a numeric
#' matrix with \code{p + 1} rows, including the intercept, even for one penalty.
#' For \code{type = "nonzero"}, a pathwise collection of 1-based predictor
#' indices excluding the intercept. Its historical container can simplify
#' depending on the support pattern and number of predictors.
#'
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{coef.rbridge}}, \code{\link{rbridge}}
#' @examples
#' # Reproducible synthetic demonstration: 100 observations, eight predictors.
#' set.seed(2019)
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' n <- 100
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- as.numeric(X %*% beta + rnorm(n))
#' # A user-specified sum of standardized slopes: b[1] + b[2] + b[5] = 6.5.
#' # This target is illustrative; it is not asserted to equal the generating sum.
#' R1.mat <- matrix(c(1, 1, 0, 0, 1, 0, 0, 0), nrow = 1)
#' r1.vec <- matrix(6.5, nrow = 1, ncol = 1)
#'
#' # Five penalties keep this help example fast; the default is 100.
#' model1 <- rbridge(X, y, q = 1, R = R1.mat, r = r1.vec, nlambda = 5)
#' # Predict five observations at one explicitly selected penalty.
#' predict(model1, newx = X[1:5, , drop = FALSE], s = model1$lambda[2])
#' # Extract coefficients using the complete type name.
#' predict(model1, s = model1$lambda[2], type = "coefficients")
#' # Predictor indices for every fitted penalty; the intercept is excluded.
#' predict(model1, type = "nonzero")
#' @export
predict.rbridge = function(object, newx, s=c("lambda.min","lambda.1se"),
                              type = c("response", "nonzero","coefficients"), ...) {
  type <- match.arg(type)
  if (missing(s)) s <- object$lambda
  else if (!is.numeric(s)) s <- match.arg(s)
  scalar_predict(object, newx, s, type)
}

#' @title Extract coefficients from a fitted rbridge object
#' @description
#' Extract original-scale intercept and slope coefficients from a fitted
#' rbridge object.
#' @param object Fitted object returned by \code{\link{rbridge}}, with \code{p} predictors in its training design.
#' @param s Numeric fitted penalty value(s).
#' Values must match \code{object$lambda} exactly; no interpolation is used.
#' Omitting this argument returns all fitted penalties.
#'
#' The character selector \code{lambda.min} requests the smallest fitted
#' penalty. Use a CV object for the selector \code{lambda.1se}.
#' @param ... Additional arguments accepted for compatibility.
#' @details
#' The returned matrix retains its dimensions for a single selected penalty.
#' Returned slopes are on the original predictor scale; the linear
#' restrictions concern standardized slopes. See \code{\link{rbridge}} for
#' converting scales before checking restriction residuals.
#'
#' @return Numeric matrix with \code{p + 1} rows and one column per
#' selected penalty, even when only one penalty is selected. The first row
#' is \code{(Intercept)}; subsequent rows are original-scale slopes in the
#' training predictor order.
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{predict.rbridge}}, \code{\link{rbridge}}
#' @examples
#' # Reproducible synthetic demonstration: 100 observations, eight predictors.
#' set.seed(2019)
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' n <- 100
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- as.numeric(X %*% beta + rnorm(n))
#' # A user-specified sum of standardized slopes: b[1] + b[2] + b[5] = 6.5.
#' # This target is illustrative; it is not asserted to equal the generating sum.
#' R1.mat <- matrix(c(1, 1, 0, 0, 1, 0, 0, 0), nrow = 1)
#' r1.vec <- matrix(6.5, nrow = 1, ncol = 1)
#'
#' # Five penalties keep this help example fast; the default is 100.
#' model1 <- rbridge(X, y, q = 1, R = R1.mat, r = r1.vec, nlambda = 5)
#' # A single selected penalty still gives a coefficient matrix.
#' coefficients <- coef(model1, s = model1$lambda[2])
#' coefficients
#' dim(coefficients)
#' # Omitting s extracts the full fitted coefficient path.
#' dim(coef(model1))
#' @export
coef.rbridge=function(object, s=c("lambda.1se","lambda.min"),...) {
  if (missing(s)) s <- object$lambda
  else if (!is.numeric(s)) s <- match.arg(s)
  predict(object, s=s, type="coefficients")
}



#### Bridge
#' @title Predict from a fitted cv.bridge object
#' @description
#' Compute response predictions, extract coefficients, or identify nonzero
#' predictors from a fitted cv.bridge object.
#' @param object Fitted object returned by \code{\link{cv.bridge}}, with \code{p} predictors in its training design.
#' @param newx Numeric matrix with one row per new observation and the
#' same \code{p} columns, in the same order, as the training design.
#' A numeric vector of length \code{p} represents one observation.
#' Values are supplied on the original predictor scale. Required only for
#' \code{type = "response"}.
#' @param type Output to return: \code{"response"} for fitted response values,
#' \code{"coefficients"} for intercept and slopes, or \code{"nonzero"}
#' for predictor indices over the entire fitted path.
#' @param s Penalty selection, using numeric fitted values or a character name.
#' Numeric values must match \code{object$lambda} exactly; no interpolation is used.
#' The character selectors are:
#' \describe{
#' \item{\code{"lambda.min"}}{Select the penalty with minimum CV error.}
#' \item{\code{"lambda.1se"}}{Select using the historical CV error bars.}
#' }
#' The default is \code{lambda.min}.
#' @param ... Additional arguments accepted for compatibility.
#' @details
#' Prediction uses the fitted intercept and original-scale slopes.
#' Columns of \code{newx} are matched by position, not reordered by their names.
#' For \code{type = "nonzero"}, all fitted penalties are inspected and
#' \code{s} does not restrict the returned indices.
#' The defaults differ between \code{coef()} and \code{predict()}:
#' coefficient extraction uses \code{lambda.1se}, whereas response prediction
#' uses \code{lambda.min}. Specify the same \code{s} in both calls when comparing
#' coefficients and their predictions.
#'
#' @return For \code{type = "response"}, a numeric vector for one
#' selected penalty or a matrix with one row per new observation and one
#' column per selected penalty. For \code{type = "coefficients"}, a numeric
#' matrix with \code{p + 1} rows, including the intercept, even for one penalty.
#' For \code{type = "nonzero"}, a pathwise collection of 1-based predictor
#' indices excluding the intercept. Its historical container can simplify
#' depending on the support pattern and number of predictors.
#'
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{coef.cv.bridge}}, \code{\link{cv.bridge}}
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
#' model1 <- cv.bridge(X, y, q = 1, nlambda = 5,
#'                      nfolds = 3, num_threads = 1)
#' # Predict five observations at one explicitly selected penalty.
#' predict(model1, newx = X[1:5, , drop = FALSE], s = "lambda.min")
#' # Extract coefficients using the complete type name.
#' predict(model1, s = "lambda.min", type = "coefficients")
#' # Predictor indices for every fitted penalty; the intercept is excluded.
#' predict(model1, type = "nonzero")
#' @export
predict.cv.bridge = function(object, newx, s=c("lambda.min","lambda.1se"),
                              type = c("response", "nonzero","coefficients"), ...) {
  type <- match.arg(type)
  if (!is.numeric(s)) s <- match.arg(s)
  scalar_predict(object, newx, s, type)
}




#' @title Extract coefficients from a fitted cv.bridge object
#' @description
#' Extract original-scale intercept and slope coefficients from a fitted
#' cross-validated cv.bridge object.
#' @param object Fitted object returned by \code{\link{cv.bridge}}, with \code{p} predictors in its training design.
#' @param s Penalty selection, using numeric fitted values or a character name.
#' Numeric values must match \code{object$lambda} exactly; no interpolation is used.
#' The character selectors are:
#' \describe{
#' \item{\code{"lambda.min"}}{Select the penalty with minimum CV error.}
#' \item{\code{"lambda.1se"}}{Select using the historical CV error bars.}
#' }
#' The default is \code{lambda.1se}.
#' @param ... Additional arguments accepted for compatibility.
#' @details
#' The returned matrix retains its dimensions for a single selected penalty.
#' The defaults differ between \code{coef()} and \code{predict()}:
#' coefficient extraction uses \code{lambda.1se}, whereas response prediction
#' uses \code{lambda.min}. Specify the same \code{s} in both calls when comparing
#' coefficients and their predictions.
#'
#' @return Numeric matrix with \code{p + 1} rows and one column per
#' selected penalty, even when only one penalty is selected. The first row
#' is \code{(Intercept)}; subsequent rows are original-scale slopes in the
#' training predictor order.
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{predict.cv.bridge}}, \code{\link{cv.bridge}}
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
#' model1 <- cv.bridge(X, y, q = 1, nlambda = 5,
#'                      nfolds = 3, num_threads = 1)
#' # A single selected penalty still gives a coefficient matrix.
#' coefficients <- coef(model1, s = "lambda.min")
#' coefficients
#' dim(coefficients)
#' # The default uses the historical error-bar selector.
#' coef(model1)
#' @export
coef.cv.bridge=function(object, s=c("lambda.1se","lambda.min"),...) {
  if (!is.numeric(s)) s <- match.arg(s)
  predict(object, s=s, type="coefficients")
}



#' @title Predict from a fitted bridge object
#' @description
#' Compute response predictions, extract coefficients, or identify nonzero
#' predictors from a fitted bridge object.
#' @param object Fitted object returned by \code{\link{bridge}}, with \code{p} predictors in its training design.
#' @param newx Numeric matrix with one row per new observation and the
#' same \code{p} columns, in the same order, as the training design.
#' A numeric vector of length \code{p} represents one observation.
#' Values are supplied on the original predictor scale. Required only for
#' \code{type = "response"}.
#' @param type Output to return: \code{"response"} for fitted response values,
#' \code{"coefficients"} for intercept and slopes, or \code{"nonzero"}
#' for predictor indices over the entire fitted path.
#' @param s Numeric fitted penalty value(s).
#' Values must match \code{object$lambda} exactly; no interpolation is used.
#' Omitting this argument returns all fitted penalties.
#'
#' The character selector \code{lambda.min} requests the smallest fitted
#' penalty. Use a CV object for the selector \code{lambda.1se}.
#' @param ... Additional arguments accepted for compatibility.
#' @details
#' Prediction uses the fitted intercept and original-scale slopes.
#' Columns of \code{newx} are matched by position, not reordered by their names.
#' For \code{type = "nonzero"}, all fitted penalties are inspected and
#' \code{s} does not restrict the returned indices.
#'
#' @return For \code{type = "response"}, a numeric vector for one
#' selected penalty or a matrix with one row per new observation and one
#' column per selected penalty. For \code{type = "coefficients"}, a numeric
#' matrix with \code{p + 1} rows, including the intercept, even for one penalty.
#' For \code{type = "nonzero"}, a pathwise collection of 1-based predictor
#' indices excluding the intercept. Its historical container can simplify
#' depending on the support pattern and number of predictors.
#'
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{coef.bridge}}, \code{\link{bridge}}
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
#' model1 <- bridge(X, y, q = 1, nlambda = 5)
#' # Predict five observations at one explicitly selected penalty.
#' predict(model1, newx = X[1:5, , drop = FALSE], s = model1$lambda[2])
#' # Extract coefficients using the complete type name.
#' predict(model1, s = model1$lambda[2], type = "coefficients")
#' # Predictor indices for every fitted penalty; the intercept is excluded.
#' predict(model1, type = "nonzero")
#' @export
predict.bridge = function(object, newx, s=c("lambda.min","lambda.1se"),
                              type = c("response", "nonzero","coefficients"), ...) {
  type <- match.arg(type)
  if (missing(s)) s <- object$lambda
  else if (!is.numeric(s)) s <- match.arg(s)
  scalar_predict(object, newx, s, type)
}

#' @title Extract coefficients from a fitted bridge object
#' @description
#' Extract original-scale intercept and slope coefficients from a fitted
#' bridge object.
#' @param object Fitted object returned by \code{\link{bridge}}, with \code{p} predictors in its training design.
#' @param s Numeric fitted penalty value(s).
#' Values must match \code{object$lambda} exactly; no interpolation is used.
#' Omitting this argument returns all fitted penalties.
#'
#' The character selector \code{lambda.min} requests the smallest fitted
#' penalty. Use a CV object for the selector \code{lambda.1se}.
#' @param ... Additional arguments accepted for compatibility.
#' @details
#' The returned matrix retains its dimensions for a single selected penalty.
#'
#' @return Numeric matrix with \code{p + 1} rows and one column per
#' selected penalty, even when only one penalty is selected. The first row
#' is \code{(Intercept)}; subsequent rows are original-scale slopes in the
#' training predictor order.
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{predict.bridge}}, \code{\link{bridge}}
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
#' model1 <- bridge(X, y, q = 1, nlambda = 5)
#' # A single selected penalty still gives a coefficient matrix.
#' coefficients <- coef(model1, s = model1$lambda[2])
#' coefficients
#' dim(coefficients)
#' # Omitting s extracts the full fitted coefficient path.
#' dim(coef(model1))
#' @export
coef.bridge=function(object, s=c("lambda.1se","lambda.min"),...) {
  if (missing(s)) s <- object$lambda
  else if (!is.numeric(s)) s <- match.arg(s)
  predict(object, s=s, type="coefficients")
}



#' @title Plot the restricted bridge cross-validation curve
#' @description
#' Display validation mean squared error over the fitted penalty grid,
#' with historical error bars, counts of nonzero slopes and both selected penalties.
#' @param x Fitted CV object returned by \code{\link{cv.rbridge}}.
#' @param sign.lambda Horizontal-axis direction. The default \code{1}
#' plots \code{log(lambda)}; \code{-1} reverses it using \code{-log(lambda)}.
#' Use positive fitted penalties for the logarithmic axis.
#' @param ... Graphical arguments passed to the initial base-R \code{plot()}
#' call, such as \code{main}, \code{xlab}, \code{ylab} or \code{ylim}.
#' @details
#' Points show \code{x$cve}; bars span \code{x$cvlo} to \code{x$cvup}.
#' The bars use the historical \code{cvse} calculation described in
#' \code{\link{cv.rbridge}}, rather than conventional foldwise standard errors.
#' The top axis shows full-data nonzero-slope counts, excluding the intercept.
#' Vertical dotted lines mark \code{lambda.min} and \code{lambda.1se};
#' the lines coincide when both selectors choose the same fitted penalty.
#' The curve is a diagnostic for the supplied grid and folds; its shape
#' depends on the data and need not be U-shaped.
#' @return Draws the plot and returns \code{NULL} invisibly.
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{cv.rbridge}}, \code{\link{coef.cv.rbridge}}
#' @examples
#' # Reproducible synthetic demonstration: 100 observations, eight predictors.
#' set.seed(2019)
#' beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
#' p <- length(beta)
#' beta <- matrix(beta, nrow = p, ncol = 1)
#' n <- 100
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' y <- as.numeric(X %*% beta + rnorm(n))
#' # A user-specified sum of standardized slopes: b[1] + b[2] + b[5] = 6.5.
#' # This target is illustrative; it is not asserted to equal the generating sum.
#' R1.mat <- matrix(c(1, 1, 0, 0, 1, 0, 0, 0), nrow = 1)
#' r1.vec <- matrix(6.5, nrow = 1, ncol = 1)
#'
#' # Five penalties keep this help example fast; the default is 100.
#' model1 <- cv.rbridge(X, y, q = 1, R = R1.mat, r = r1.vec, nlambda = 5,
#'                      nfolds = 3, num_threads = 1)
#' plot(model1, main = "Cross-validation over the fitted penalty grid")
#' # Reverse the log-penalty axis without refitting the model.
#' plot(model1, sign.lambda = -1)
#' @export
plot.cv.rbridge=function(x,sign.lambda=1,...){
  object = x
  xlab="log(Lambda)"
  if(sign.lambda<0)xlab=paste("-",xlab,sep="")
  plot.args=list(x=sign.lambda*log(object$lambda),
                 y=object$cve,ylim=range(object$cvup,object$cvlo),
                 xlab=xlab,ylab='Mean-Squared Error',type="n")
  new.args <- list(...)
  if (length(new.args)) 
    plot.args[names(new.args)] <- new.args
  do.call("plot", plot.args)
  er.bars(sign.lambda*log(object$lambda),object$cvup,object$cvlo,width=0.01,col="darkgrey")
  points(sign.lambda*log(object$lambda),object$cve,pch=20,col="red")
  axis(side=3,at=sign.lambda*log(object$lambda),labels=paste(object$nz),tick=FALSE,line=0)
  abline(v=sign.lambda*log(object$lambda.min),lty=3)
  abline(v=sign.lambda*log(object$lambda.1se),lty=3)
  invisible()
}



### Bridge
#' @title Plot the bridge cross-validation curve
#' @description
#' Display validation mean squared error over the fitted penalty grid,
#' with historical error bars, counts of nonzero slopes and both selected penalties.
#' @param x Fitted CV object returned by \code{\link{cv.bridge}}.
#' @param sign.lambda Horizontal-axis direction. The default \code{1}
#' plots \code{log(lambda)}; \code{-1} reverses it using \code{-log(lambda)}.
#' Use positive fitted penalties for the logarithmic axis.
#' @param ... Graphical arguments passed to the initial base-R \code{plot()}
#' call, such as \code{main}, \code{xlab}, \code{ylab} or \code{ylim}.
#' @details
#' Points show \code{x$cve}; bars span \code{x$cvlo} to \code{x$cvup}.
#' The bars use the historical \code{cvse} calculation described in
#' \code{\link{cv.bridge}}, rather than conventional foldwise standard errors.
#' The top axis shows full-data nonzero-slope counts, excluding the intercept.
#' Vertical dotted lines mark \code{lambda.min} and \code{lambda.1se};
#' the lines coincide when both selectors choose the same fitted penalty.
#' The curve is a diagnostic for the supplied grid and folds; its shape
#' depends on the data and need not be U-shaped.
#' @return Draws the plot and returns \code{NULL} invisibly.
#' @author Bahadir Yuzbasi \cr Maintainer: Bahadir Yuzbasi \email{b.yzb@hotmail.com}
#' @seealso \code{\link{cv.bridge}}, \code{\link{coef.cv.bridge}}
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
#' model1 <- cv.bridge(X, y, q = 1, nlambda = 5,
#'                      nfolds = 3, num_threads = 1)
#' plot(model1, main = "Cross-validation over the fitted penalty grid")
#' # Reverse the log-penalty axis without refitting the model.
#' plot(model1, sign.lambda = -1)
#' @export
plot.cv.bridge=function(x,sign.lambda=1,...){
  object = x
  xlab="log(Lambda)"
  if(sign.lambda<0)xlab=paste("-",xlab,sep="")
  plot.args=list(x=sign.lambda*log(object$lambda),
                 y=object$cve,ylim=range(object$cvup,object$cvlo),
                 xlab=xlab,ylab='Mean-Squared Error',type="n")
  new.args <- list(...)
  if (length(new.args)) 
    plot.args[names(new.args)] <- new.args
  do.call("plot", plot.args)
  er.bars(sign.lambda*log(object$lambda),object$cvup,object$cvlo,width=0.01,col="darkgrey")
  points(sign.lambda*log(object$lambda),object$cve,pch=20,col="red")
  axis(side=3,at=sign.lambda*log(object$lambda),labels=paste(object$nz),tick=FALSE,line=0)
  abline(v=sign.lambda*log(object$lambda.min),lty=3)
  abline(v=sign.lambda*log(object$lambda.1se),lty=3)
  invisible()
}
