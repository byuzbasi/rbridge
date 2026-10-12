## CRAN LTO/ASAN maintenance update

rbridge 1.0.4 responds to the CRAN check notice with a 2026-11-02 deadline.
Version 1.0.3 was published on 2026-10-11. Its OpenMP builds gave the engine
and generated Rcpp wrappers different Armadillo configurations, causing the
reported state_type / SpMat One Definition Rule warnings under GCC LTO.

ARMA_DONT_USE_OPENMP is now set in both Makevars files for every C++
translation unit. The file-local definition is removed. Explicit OpenMP
parallelism across CV folds remains enabled. Numerical implementation bodies,
R interfaces, defaults, restriction scale, seeds and test fixtures are unchanged.

Source archive SHA-256:
e12194b1ffb8df813488e4312d2f2b7d5505ab101129bfb711f9149398772e67.

## Checks

* R 4.6.0, macOS arm64, R CMD check --as-cran:
  0 errors, 0 warnings, 2 explained notes; 353 test expectations pass.
* Windows R-release 4.6.1, GCC 14.3.0:
  0 errors, 0 warnings, 1 explained note; 353 test expectations pass.
  https://win-builder.r-project.org/88XpELhaZzM1/
* Windows R-devel 2026-10-10 r90659 ucrt, GCC 14.3.0:
  0 errors, 0 warnings, 1 explained note; 353 test expectations pass.
  https://win-builder.r-project.org/FTtaJ1w913jw/

All three checks pass examples, vignette rebuilding and the PDF manual.
Windows HTML manual validation also passes. The Windows binary DESCRIPTION,
including the unique packaging timestamp, README and generated examples match
this exact source candidate.

The incoming note is "Days since last update: 1". This prompt maintenance
submission addresses the CRAN check notice. The additional macOS note concerns
an old local HTML Tidy; HTML validation passes on both Windows checks.

## Additional Linux compiler checks

Ubuntu Linux x86_64, GCC 11.4.0, R 4.3.0, RcppArmadillo 15.6.0-1:

* GCC LTO with -flto -Wodr -Werror=odr -Werror=lto-type-mismatch:
  1.0.3 reproduces the reported type mismatch and fails the strict link;
  1.0.4 compiles without those diagnostics and passes all 353 expectations.
* GCC AddressSanitizer with -fsanitize=address -fno-omit-frame-pointer:
  instrumented 1.0.4 passes all 353 expectations, including CV thread consistency.
  A deliberate compiler-only heap overflow confirms ASAN is active.
  Leak detection is disabled for the ordinary R process; memory-access checks
  remain enabled. This is package instrumentation, not a sanitizer-built R.

The original CRAN GCC-ASAN detail log currently returns HTTP 404. The 1.0.3
smoke did not reproduce an ASAN diagnostic in this local toolchain, which is
older than the reported GCC 16 LTO environment; no unseen diagnostic or exact
CRAN-environment reproduction is claimed. The 1.0.4 sanitizer result above is
measured. Historical benchmark outputs are preserved; no production run was
performed for this maintenance update.
