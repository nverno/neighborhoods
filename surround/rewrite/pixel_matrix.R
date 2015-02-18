### pixel_matrix.R --- 
## Filename: pixel_matrix.R
## Description: Create pixel matrix for a neighborhood
## Author: Noah Peart
## Created: Tue Nov 11 16:30:42 2014 (-0500)
## Last-Updated: Wed Feb 18 16:48:08 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/neighborhoods/surround/rewrite/spheres.R")
source("~/work/neighborhoods/surround/rewrite/create_test.R")

## Find nearest pixel in matrix from radian value
## mdim: dimension of matrix (square)
nearest_pixel <- function(rad, mdim=100) {
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
## 
pixel_matrix <- function(targ, nbrs, size=10, precise=TRUE) {
    mat_ind <- seq(0, 2*pi, length.out=size)
    mat <- matrix(0, nrow=size, ncol=size)
    inc <- 2*pi/size
        
    for (i in 1:nrow(nbrs)) {
        n <- nbrs[i,]
        theta <- cone_theta(n)[["theta"]]  # cone angle
        pos <- pos_vec_ellipse(n[["x"]], n[["y"]], n[["z"]], n[["crdepth"]], n[["ht"]])
        ## horizontal/vertical radians
        pol <- cart2pol(n[["x"]], n[["y"]])
        hrad <- c(-1,1)*theta/2 + pol[["theta"]]
        vrad <- c(-1,1)*theta/2 + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]])

        ## Fill matrix (square version)
        rows <- which(mat_ind >= min(hrad) - inc & mat_ind <= max(hrad) + inc)
        cols <- which(mat_ind >= min(vrad) - inc & mat_ind <= max(vrad) + inc)
        
        if (!precise) {
            mat[rows, cols] <- 1
        } else {
            ## More precise version, find pixels within inscribed circle
            ## Check square for pixels <= theta/2 euclidean distance
            dmat <- euc(t(as.matrix(expand.grid(rows*2*pi/size, cols*2*pi/size))),
                        c(mean(hrad), mean(vrad)))
            dmat <- matrix(ifelse(dmat <= theta/2, 1, 0), nrow=length(rows), ncol=length(cols))
            mat[rows, cols] <- mat[rows, cols] + dmat
            ## sum(dmat <= theta/2)/length(dmat)  # should approach pi/4 if circle exactly inscribed
        }
    }
    return ( mat )
}

## ## distance matrix for pixels w/in square range
## size = 100
## inc <- 2*pi/size
## h_range <- nearest_pixel(hrad)
## v_range <- nearest_pixel(vrad)
## seqr <- max(diff(h_range), diff(v_range))
## h_seq <- seq(h_range[1]*2*pi/size, h_range[2]*2*pi/size, length.out = seqr)
## v_seq <- seq(v_range[1]*2*pi/size, v_range[2]*2*pi/size, length.out = seqr)

## dmat <- euc(t(as.matrix(expand.grid(h_seq, v_seq))), c(mean(hrad), mean(vrad)))
## sum(dmat <= theta/2)/length(dmat)

## Image of matrix
image_hood <- function(targ, nbrs, size=100, precise = TRUE) {
    pmat <- pixel_matrix(targ, nbrs, size=size, precise=precise)
    mat_ind <- seq(0, 2*pi, length.out = size)
    image(mat_ind, mat_ind, pmat, xlab = "Horizontal Radians",
          ylab = "Vertical Radians",
          main = "Neighborhood Image Relative to Target Crown\n(Positive x-axis, Radians=0)")
    abline(h=pi, v=pi)
    abline(v = seq(0, 2*pi, by = pi/2), lty = 2)
    ## text(4, 3, "Above Target")
    text(seq(0.5, 2*pi-0.5, length.out = 4), y=pi, c("South", "East", "North", "West"), cex=2)
    text(c(pi, pi), y = c(0.5, 2*pi-0.5), c("Above Target", "Below Target"), cex = 2)
}
