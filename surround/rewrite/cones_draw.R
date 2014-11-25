### cones_draw.R --- 
## Filename: cones_draw.R
## Description: Drawing a neighborhood full of cones
## Author: Noah Peart
## Created: Tue Nov 25 13:23:17 2014 (-0500)
## Last-Updated: Tue Nov 25 15:31:19 2014 (-0500)
##           By: Noah Peart
######################################################################
## NOTE: this is a testing platform where every neighbor is treated as a
## a cone, prior to combining cones and spheres into the same neighborhood.
source("~/work/neighborhoods/surround/rewrite/cones.R")

################################################################################
##
##      Create pixel matrix and image, treating all neighbors as cones
##
################################################################################
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
pixel_matrix <- function(targ, nbrs, size=10, precise=TRUE) {
    mat_ind <- seq(0, 2*pi, length.out=size)
    mat <- matrix(0, nrow=size, ncol=size)
    inc <- 2*pi/size
        
    for (i in 1:nrow(nbrs)) {
        n <- nbrs[i,]
        b <- ellipse_long_axis(n)
        a <- ellipse_short_axis(targ, n)
        h <- height_angle(n)
        use_triangle <- ifelse(a >= h, FALSE, TRUE)  # search for triangle or not
        pos <- c(n[["x"]], n[["y"]], n[["ht"]] - n[["crdepth"]] + n[["z"]])
        names(pos) <- c("x", "y", "z")

        ## horizontal/vertical radians
        pol <- cart2pol(n[["x"]], n[["y"]])
        hrad <- c(-1,1)*b/2 + pol[["theta"]]

        ## If target is above neighbor, the FRONT of the ellipse MUST be
        ## accounted for (as well as possibly the back), and if the target
        ## is below the neighbor the BACK of the ellipse MUST be accounted for
        ## *** Create the search window ***
        if (targ[["ht"]] >= pos[["z"]]) {  # target looking down at nbr
            vrad <- ifelse(use_triangle,
                           c(-1,1)*c(h,a) + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]]),
                           c(-1,1)*a + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]]) )
        } else {                           # target looking up at nbr
            vrad <- ifelse(use_triangle,
                           c(-1,1)*c(h,a) + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]]),
                           c(-1,1)*a + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]]) )
        }
        
        vrad_ellipse <- c(-1,1)*a + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]])
        vrad_both <- c(-1,1)*w_ht/2 + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]])

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
