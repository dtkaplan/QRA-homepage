# definitions of storybook functions

# eventually these should go in a package, like {mosaicCalc}
osc <- function(t) sin(2*pi*t)
hillside <- function(t) pnorm(t)
hill <- function(t) dnorm(t)
flat <- unity <- function(x) 1.0
steady <- id <- function(x) x
double <- function(t) 2^t
doublings <- function(x) log2(x)
recip <- function(x) 1/x
magnitude <- tennings <- function(x) log10(x)
ten_folds <- function(x) 10^x

