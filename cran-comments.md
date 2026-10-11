## Reinstatement of an archived package

Please reinstate rbridge with this 1.0.3 maintenance update. Version 1.0.2
was archived on 2026-01-15. This update fixes deprecated Armadillo conversion,
removes the obsolete C++11 request and unused dplyr dependency, and updates
package authorship, the article citation, tests, help and vignettes.

The public source repository is https://github.com/byuzbasi/rbridge.
The documentation website is https://byuzbasi.github.io/rbridge/.

## Source candidate and checks

Publication source archive SHA-256:
`3c452f50cffb51e2b2a0a8c04fbdc37a567d068c75ca5feef885b08c83225042`.

R 4.6.0 on macOS arm64, R CMD check --as-cran: 0 errors, 0 warnings,
2 notes; all 353 test expectations pass. Examples, vignette rebuilding,
PDF manual, Rd syntax and cross-references pass. CRAN incoming feasibility
was checked with remote access enabled. No URL issues were reported.

* Incoming feasibility identifies an archived package/new submission.
  Reinstatement is requested; the earlier archival issues have been addressed.
* HTML manual validation is skipped because the local HTML Tidy is too old.
  Rd checks, examples and PDF manual compilation pass.

Windows R-release and Windows R-devel results for this exact archive remain
pending. They will be reviewed and recorded before CRAN submission.
This archive differs from the preceding locally checked candidate only in
DESCRIPTION URL fields and README publication links/instructions; all
numerical R/C++ code, tests, help and vignette files are identical.

The preceding candidate (SHA-256
`d076136551fefcce331406a370c892ea1f724a64f9395da7027f47944dcc2329`)
passed R CMD check on R 4.6.0/macOS arm64 with 0 errors, 0 warnings, 0 notes
and 353 passing test expectations. Examples, vignette rebuilding and the
PDF manual passed. This was an ordinary local check, not --as-cran.

Earlier Windows R-release and R-devel checks of the same numerical code
passed with 0 errors, 0 warnings and 1 note each (353 test expectations).
Their archive SHA-256 was
`b5dd215ae082216fef7436c4bf7ec527af0b4bdf564930166bba06577ee508c9`.
Those reports precede the final documentation and authorship changes.
The incoming note concerned reinstatement of an archived package and the
surnames Yuzbasi, Arashi and Akdeniz in the published article citation.

## Validation scope

The estimator, standardized restriction scale, preprocessing, contiguous
folds, historical CV error-bar formula, thresholds, iteration cap and API
are preserved. The default penalty path contains 100 lambdas. Documentation
explains coefficient scales, exact fitted-penalty selection, CV defaults,
return dimensions, and the illustrative restriction targets. Small help
examples use five penalties and three folds where applicable.

A completed serial benchmark contains 96 validated shards and 48 passing
comparisons with the first repaired 1.0.3 candidate. The maximum relative
coefficient/CV difference is 5.51e-14. Timing claims are limited to the
recorded designs and environment. The benchmark is development tooling
and is not executed during package checks. No full paper reproduction or
production simulation is claimed.

The package and published-article citation retain Bahadir Yuzbasi, Mohammad
Arashi and Fikri Akdeniz. Bahadir Yuzbasi is the sole maintainer; the website
footer identifies him as code and website developer. Existing utility-code
attribution to glmnet contributors is preserved.
