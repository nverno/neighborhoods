### cones.R --- 
## Filename: cones.R
## Description: Conical neighbors
## Author: Noah Peart
## Created: Mon Nov 24 18:54:32 2014 (-0500)
## Last-Updated: Mon Nov 24 23:31:22 2014 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/neighborhoods/surround/rewrite/spheres.R")
source("~/work/neighborhoods/surround/rewrite/create_test.R")

## Neighbor radius
## To avoid problem of target being contained within the neighbor crown,
## the neighbor radius is the minimum of the distance between target and neighbor
## and the actual radius as computed from the neighbor crown horizontal area
neighbor_radius <- function(nbr, contain=FALSE) {
    if (!contain)
        return ( min(c(sqrt(nbr[["crarea"]]/pi), nbr[["dist"]])) )
    return ( sqrt(nbr[["crarea"]]/pi) )
}

## Find things related to base of conical neighbors
## Angle subtended by long axis of neighbor
ellipse_long_axis <- function(nbr) {
    r <- neighbor_radius(nbr, contain=FALSE)  # don't allow target to be fully obscured
    return ( 2 * atan(r/nbr[["dist"]]) )
}

## Angle subtended at target by vector from neighbor crown base to crown top
height_angle <- function(nbr) {
    base <- nbr[["z"]] + nbr[["ht"]] - nbr[["crdepth"]]
    stopifnot(base >= 0)  # base of neighbor shouldn't be negative
    top <- nbr[["z"]] + nbr[["ht"]]
    ab <- c(nbr[["x"]], nbr[["y"]], base)
    ac <- c(nbr[["x"]], nbr[["y"]], top)
    return ( acos(crossprod(ab, ac)/(sqrt(sum(ab**2)) * sqrt(sum(ac**2)))) )
}

## Find closest point on base of neighbor cone to target
base_closest_point <- function(nbr) {
    r <- neighbor_radius(nbr, contain=FALSE)  # don't allow target to be fully obscured
    n_xy <- c(nbr[["x"]], nbr[["y"]])  # neighbor position vector in xy plane
    e_xy <- n_xy/sqrt(sum(n_xy**2))  # unit vector from target to neighbor (xy only)
    p_closest <- c(n_xy - r*e_xy, nbr[["ht"]]- nbr[["crdepth"]] + nbr[["z"]])  # closest point on base
    names(p_closest) <- c("x", "y", "z")
    return ( p_closest )
}

## Angle subtended at target by vector from center of neighbor base to
## the point closest to target on the neighbor base
## NOTE: this is the polar angle of the short axis of the ellipse formed by the
## base of the neighbor from the point of view of the target crown
ellipse_short_axis <- function(targ, nbr) {
    p_closest <- base_closest_point(nbr)
    p_center <- c(nbr[["x"]], nbr[["y"]], p_closest[["z"]])
    p_targ <- c(0, 0, targ[["ht"]])
    tn <- p_center - p_targ  # vector from targ -> center neighbor base
    te <- p_closest - p_targ  # vector from targ -> edge of neighbor base closest to targ
    theta <- acos(crossprod(tn, te)/(sqrt(sum(tn**2))*sqrt(sum(te**2))))  # short axis in radians
    return ( theta )
}

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
        h <- height_angle(nbr)
        pos <- c(n[["x"]], n[["y"]], n[["ht"]] - n[["crdepth"]] + n[["z"]])
        names(pos) <- c("x", "y", "z")

        ## horizontal/vertical radians
        pol <- cart2pol(n[["x"]], n[["y"]])
        hrad <- c(-1,1)*b/2 + pol[["theta"]]
        w_ht <- ifelse(h < 2*a, 2*a, a+h)

        ## STOPPED HERE!!!
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
