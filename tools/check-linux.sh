#!/bin/sh
# Local container entry point: dependencies must already be installed.
set -eu
if [ "$#" -ne 2 ]; then
  echo "Usage: check-linux.sh SOURCE_ARCHIVE NEW_CHECK_DIRECTORY" >&2
  exit 2
fi
task_archive="$1"
task_output="$2"
if [ -e "$task_output" ]; then
  echo "Refusing to overwrite an existing Linux check" >&2
  exit 1
fi
mkdir -p "$task_output"
cd "$task_output"
Rscript --vanilla - <<'RS' > preflight.log 2>&1
needed <- c("Rcpp", "RcppArmadillo", "Matrix", "testthat", "knitr", "rmarkdown")
stopifnot(all(vapply(needed, requireNamespace, logical(1), quietly = TRUE)))
variables <- c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
               "BLIS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS", "NUMEXPR_NUM_THREADS")
stopifnot(all(Sys.getenv(variables) == "1"))
cat(R.version.string, "\n", R.version$platform, "\n")
print(.libPaths())
for (p in needed) cat(p, as.character(packageVersion(p)), "\n")
print(Sys.getenv(variables))
cat("Remote incoming feasibility:", Sys.getenv("_R_CHECK_CRAN_INCOMING_REMOTE_", "default"), "\n")
RS
# PDF manual compilation is covered by the macOS check; no TeX is installed here.
R CMD check --as-cran --no-manual "$task_archive" > check.log 2>&1
