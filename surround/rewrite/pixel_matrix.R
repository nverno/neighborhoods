### pixel_matrix.R --- 
## Filename: pixel_matrix.R
## Description: Create pixel matrix for a neighborhood
## Author: Noah Peart
## Created: Tue Nov 11 16:30:42 2014 (-0500)
## Last-Updated: Tue Nov 11 17:55:34 2014 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/neighborhoods/surround/rewrite/spheres.R")
source("~/work/neighborhoods/surround/rewrite/create_test.R")

rads <- 2*pi / 100

## Find index into matrix from radian value
## mdim: dimension of matrix (square)
mat_ind <- function(rad, mdim=100) {
    step_size <- 2*pi/mdim
    return ( round(rad/step_size) )
}

mat_ind <- seq(0, 2*pi, length.out=100)
mat <- matrix(0, nrow=100, ncol=100)

for (n in 1:nrow(nbrs)) {
    theta <- cone_theta(n)[["theta"]]  # cone angle
    pos <- pos_vec_ellipse(n[["x"]], n[["y"]], n[["z"]], n[["crdepth"]], n[["ht"]])
    ## horizontal radians
    pol <- cart2pol(n[["x"]], n[["y"]])
    hrad <- c(-1,1)*theta/2 + pol[["theta"]]
    ## Vertical radians
    t2n_angle <- t2n_theta(targ, nbr)
    vrad <- c(-1,1)*theta/2 + v_rad(pos[["x"]], targ[["ht"]]-pos[["z"]])
    mat[(mat_ind >= min(hrad)) & (mat_ind < max(hrad))][mat_ind >= min(vrad) & mat_ind < max(vrad)] = 1
}

## Angle target and neighbor centroid
t2n_theta <- function(targ, nbr) {
    h <- targ[["ht"]] - nbr[["ht"]]
    d <- nbr$dist
    theta <- asin(h/d)
    return ( theta )
}

## Determine vertical radian from positive x-axis (0 radians)
## NOTE: relative to origin (0, 0, 0)
v_rad <- function(x, z) {
    theta <- atan(z/x)
    theta[x < 0] <- theta[x < 0] + pi
    theta[x >= 0 & z < 0] <- theta[x >= 0 & z < 0] + 2*pi
    return ( theta )
}
