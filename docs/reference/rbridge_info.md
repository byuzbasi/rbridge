# Compiled engine capabilities

Report whether this installed package was compiled with OpenMP support.
This reports a build capability, not a measured speedup or the number of
threads used in a particular fit. CV uses at most one worker per fold.

## Usage

``` r
rbridge_info()
```

## Value

A list with `version`, the installed package version; `openmp`, a
logical build capability; and `openmp_version`, the OpenMP specification
date reported by the compiler (zero if absent).

## Details

Without OpenMP, CV runs sequentially for any `num_threads`. With OpenMP,
`num_threads` requests fold-level workers. Set BLAS threads to one
before starting R to avoid nested parallelism.

## Examples

``` r
rbridge_info()
#> $version
#> [1] "1.0.3"
#> 
#> $openmp
#> [1] TRUE
#> 
#> $openmp_version
#> [1] 202011
#> 
```
