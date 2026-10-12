# rbridge 1.0.4

* Apply the same Armadillo OpenMP configuration to all C++ translation units,
  including the generated Rcpp wrappers. This fixes the inconsistent sparse
  matrix type definitions reported by CRAN's GCC link-time optimization check.
* Preserve explicit parallelism across cross-validation folds, with sequential
  Armadillo operations inside each fold. Estimation methods, numerical kernels,
  public interfaces and defaults are unchanged.

# rbridge 1.0.3

* Restore default prediction and coefficient extraction on current R versions.
* Path objects return the full coefficient or prediction path when `s` is
  omitted. Numeric `s` selects exact fitted lambda values. For path objects,
  `s = "lambda.min"` selects the smallest fitted lambda; `lambda.1se` requires CV.
* Forward `num_threads` and `converge` from cross-validation wrappers, and honor
  the supplied initial `converge` value in the solver. The default is unchanged.
* Reject nonfinite data, constant predictors, invalid penalty parameters,
  invalid fold counts and incompatible restriction dimensions before fitting.
* Replace deprecated Armadillo scalar conversion and remove obsolete C++11
  requests. Reuse the fixed design cross-product in the restricted solver.
* Update authorship, dependencies, published article citation and documentation.
* Cache fixed and active-set cross-products in both solvers. Solve the restricted
  bridge and restriction right-hand sides together instead of repeating matrix
  inversions. Singular or numerically unresolved systems now fail explicitly;
  no automatic approximate solution is substituted.
* Reuse CV data slices and compute prediction paths in one matrix multiplication.
  Request portable OpenMP flags where available, with a sequential fallback.
  Report the installed build capability with the additive `rbridge_info()` API.
  Worker failures are propagated after threads finish.
* Add 36 frozen synthetic regression cases, independent constrained-ridge and
  uneven-fold CV checks, thread consistency tests and locally built guides.
* Document a completed 96-shard synthetic benchmark with 48 passing numerical
  comparisons. Casewise median speed ratios are 3.14x for bridge, 2.59x for
  rbridge, 2.99x for cv.bridge and 2.57x for cv.rbridge against the first repaired
  candidate. Reported ratios are limited to the recorded designs and environment.
* Preserve the historical solver, lambda grid, folds, thresholds and the
  standardized coefficient scale used for restrictions. Returned coefficients
  remain on the original predictor scale.

# rbridge 1.0.2

* Previous CRAN release, published on 2020-02-29.
