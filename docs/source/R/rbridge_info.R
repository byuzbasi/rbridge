#' Compiled engine capabilities
#'
#' Report whether this installed package was compiled with OpenMP support.
#' This reports a build capability, not a measured speedup or the number of
#' threads used in a particular fit. CV uses at most one worker per fold.
#'
#' @return A list with three components:
#' \item{version}{Installed package version, as a character string.}
#' \item{openmp}{Logical flag indicating compiled OpenMP support.}
#' \item{openmp_version}{OpenMP specification date reported by the compiler,
#' or zero when support is absent.}
#' @details Without OpenMP, CV runs sequentially for any \code{num_threads}.
#'   With OpenMP, \code{num_threads} requests fold-level workers. Set BLAS
#'   threads to one before starting R to avoid nested parallelism.
#' @examples
#' rbridge_info()
#' @export
rbridge_info <- function() {
  c(list(version = as.character(utils::packageVersion("rbridge"))), Engine_Info())
}
