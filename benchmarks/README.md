# Reproducing the recorded engine benchmark

The published timing evidence is in `inst/doc/benchmarks/`: 48 comparisons, 1440 individual timings, summary tables, graphics and a SHA-256 manifest. The seed, full grid and software/hardware provenance are in `provenance.json`. The 96 original synthetic timing shards are in `reported/run/`; their manifest is regenerated only to redact local R-framework paths in sessionInfo.txt. The original/public manifest hashes and transformation are recorded separately. All shard and configuration bytes are unchanged.

The reference source in `reference/` is the first repaired 1.0.3 candidate. Only package-builder and archive-owner metadata were normalized; numerical source files are unchanged. Its checksum transformation is recorded separately. It is not the original CRAN 1.0.2 source. Both engines share version 1.0.3; source and installed-engine hashes distinguish them.

Install the reference into a separate library, and install the current source into another library. Development dependencies are digest, jsonlite and microbenchmark for timings, and knitr/ggplot2 for analysis. Package runtime dependencies remain those in DESCRIPTION. No tool installs dependencies automatically.

Run development commands from the repository root with `Rscript --vanilla`. For a new comparison, first run `Rscript --vanilla tools/benchmark-package.R design BASELINE_LIBRARY CANDIDATE_LIBRARY NEW_OUTPUT`, then run a small `smoke` in another new directory. Set OMP_NUM_THREADS, OPENBLAS_NUM_THREADS, MKL_NUM_THREADS, BLIS_NUM_THREADS, VECLIB_MAXIMUM_THREADS and NUMEXPR_NUM_THREADS to 1 before starting R. The full run uses the design output and --resume; it is a user-run workload. Resume requires the same configuration, driver, engines and environment.

`reported/frozen-driver.R` preserves the exact driver used for the recorded benchmark. Its legacy hash ordering needs LC_ALL=en_US.UTF-8. The current development driver uses byte ordering and a new fingerprint schema. Rebuilding an engine establishes a new signature; cross-platform replay does not reproduce the exact installed binary hashes or the measured times.

Analyze a verified completed run with `Rscript --vanilla tools/summarize-benchmark.R RUN NEW_ANALYSIS`. This command checks manifests and numerical gates and does not fit models. The recorded comparison uses one machine, one data seed and a fixed engine order. It measures serial implementation speed and is not a convergence or accuracy study.
