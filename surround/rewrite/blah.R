### blah.R --- 
## Filename: blah.R
## Description: 
## Author: Noah Peart
## Created: Wed Feb 18 15:18:16 2015 (-0500)
## Last-Updated: Wed Feb 18 16:48:04 2015 (-0500)
##           By: Noah Peart
######################################################################

interp <- function(v0, v1, num) {
    vals <- seq(v0, v1, length = num)
    return( v0*(1-vals) + v1*vals )
}

v0 <- 1
v1 <- 4
len <- 20
xs <- seq(v0, v1, length = len)
ys <- interp(v0, v1, len)

## B-spline basis
## Cubic
mat <- (1/6)*matrix(c(-1, 3, -3, 1,
                      3, -6, 3, 0,
                      -3, 0, 3, 0,
                      1, 4, 1, 0), 4, 4, byrow = TRUE)

t0 <- 0
t1 <- 1
vs <- function(x) matrix(c(x**3, x**2, x, 1), 1, 4)
