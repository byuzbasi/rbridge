# Plot a 'cv.bridge' object function

Plots the cross-validation curve, and upper and lower standard deviation
curves, as a function of the lambda values used.

## Usage

``` r
# S3 method for class 'cv.bridge'
plot(x, sign.lambda = 1, ...)
```

## Arguments

- x:

  Design matrix.

- sign.lambda:

  Either plot against `log(lambda)` (default) or its negative if
  sign.`lambda=-1`.

- ...:

  Other graphical parameters to plot

## Author

Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz  
Maintainer: Bahadir Yuzbasi <b.yzb@hotmail.com>
