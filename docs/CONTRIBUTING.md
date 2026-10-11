# Contributing to rbridge

Report bugs with a minimal reproducible example, the expected and
observed behavior, and the output of
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html). Use
synthetic or shareable data. Include the fitted lambda grid and the
restriction dimensions when relevant.

Keep changes focused. Document any change affecting coefficient scales,
CV folds, penalty grids, stopping rules or numerical results. Numerical
changes need independent reference tests and comparison with the
historical implementation.

From the source directory, build the package with `R CMD build .`. Run
`R CMD check --as-cran` on the resulting source archive from its
containing directory, using one BLAS/OpenMP thread. Keep examples short
and reproducible.

Preserve copyright and license statements for adapted code. Do not
include local workspace files, compiled binaries or private datasets in
source submissions.
