### pixel_matrix.R --- 
## Filename: pixel_matrix.R
## Description: Create pixel matrix for a neighborhood
## Author: Noah Peart
## Created: Tue Nov 11 16:30:42 2014 (-0500)
## Last-Updated: Fri Nov 14 15:43:09 2014 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/neighborhoods/surround/rewrite/spheres.R")
source("~/work/neighborhoods/surround/rewrite/create_test.R")

## Find index into matrix from radian value
## mdim: dimension of matrix (square)
mat_ind <- function(rad, mdim=100) {
    step_size <- 2*pi/mdim
    return ( round(rad/step_size) )
}

## Determine vertical radian from positive x-axis (0 radians)
## NOTE: relative to origin (0, 0, 0)
v_rad <- function(x, z) {
    if (x == 0 && z == 0)
        return ( 0 )
    theta <- atan(z/x)
    theta[x < 0] <- theta[x < 0] + pi
    theta[x >= 0 & z < 0] <- theta[x >= 0 & z < 0] + 2*pi
    return ( theta )
}

## Convert neighborhood into pixel matrix
## Rows are horizontal radians, cols are vertical
## positive x-axis is 0 radians in both dims
pixel_matrix <- function(size=10, targ, nbrs) {
    mat_ind <- seq(0, 2*pi, length.out=size)
    mat <- matrix(0, nrow=size, ncol=size)
    
    for (i in 1:nrow(nbrs)) {
        n <- nbrs[i,]
        theta <- cone_theta(n)[["theta"]]  # cone angle
        pos <- pos_vec_ellipse(n[["x"]], n[["y"]], n[["z"]], n[["crdepth"]], n[["ht"]])
        ## horizontal radians
        pol <- cart2pol(n[["x"]], n[["y"]])
        hrad <- c(-1,1)*theta/2 + pol[["theta"]]
        ## Vertical radians
        ## t2n_angle <- t2n_theta(targ, nbr)
        vrad <- c(-1,1)*theta/2 + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]])
        rows <- which((mat_ind >= min(hrad)) & (mat_ind <= max(hrad)))
        cols <- which((mat_ind >= min(vrad)) & (mat_ind <= max(vrad)))
        mat[rows, cols] <- 1
    }

    return ( mat )
}

## Image of matrix
image_hood <- function(size, targ, nbrs) {
    pmat <- pixel_matrix(size, targ, nbrs)
    mat_ind <- seq(0, 2*pi, length.out = size)
    image(mat_ind, mat_ind, pmat, xlab = "Horizontal Radians",
          ylab = "Vertical Radians",
          main = "Neighborhood Image Relative to Target Crown\n(Positive x-axis, Radians=0)")
    abline(h=pi, v=pi)
}
