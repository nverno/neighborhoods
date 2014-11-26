### cones_draw.R --- 
## Filename: cones_draw.R
## Description: Drawing a neighborhood full of cones
## Author: Noah Peart
## Created: Tue Nov 25 13:23:17 2014 (-0500)
## Last-Updated: Wed Nov 26 18:45:01 2014 (-0500)
##           By: Noah Peart
######################################################################
## NOTE: this is a testing platform where every neighbor is treated as a
## a cone, prior to combining cones and spheres into the same neighborhood.
source("~/work/neighborhoods/surround/rewrite/cones.R")
source("~/work/neighborhoods/surround/rewrite/create_test.R")

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

## Convert neighborhood into pixel matrix, all neighbors as cones
## Rows are horizontal radians, cols are vertical
## positive x-axis is 0 radians in both dims
pixel_matrix_cones <- function(targ, nbrs, size=10, precise=TRUE) {
    mat_ind <- seq(0, 2*pi, length.out=size)
    mat <- matrix(0, nrow=size, ncol=size)
    inc <- 2*pi/size
        
    for (i in 1:nrow(nbrs)) {
        n <- nbrs[i,]
        b <- ellipse_long_axis(n)
        a <- ellipse_short_axis(targ, n)
        h <- height_angle(n)

        use_tri <- ifelse(a >= h, FALSE, TRUE)  # search for triangle or not
        pos <- c(n[["x"]], n[["y"]], n[["ht"]] - n[["crdepth"]] + n[["z"]])
        names(pos) <- c("x", "y", "z")
        upper <- ifelse(targ[["ht"]] > pos[["z"]], TRUE, FALSE)

        ## horizontal radians
        pol <- cart2pol(n[["x"]], n[["y"]])
        hrad <- c(-1,1)*b + pol[["theta"]]

        ## If target is above neighbor, the FRONT of the ellipse MUST be
        ## accounted for (as well as possibly the back), and if the target
        ## is below the neighbor the BACK of the ellipse MUST be accounted for
        ## Vertical radians
        if (targ[["ht"]] >= pos[["z"]]) {  # target looking down at nbr
            if (use_tri) {
                vrad <- c(-1,1)*h + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]])
            } else {
                vrad <- c(-1,1)*a + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]])
            }
        } else {                           # target looking up at nbr
            if (use_tri) {
                vrad <- c(-1,1)*h + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]])
            } else {
                vrad <- c(-1,1)*a + v_rad(pos[["x"]], pos[["z"]] - targ[["ht"]]) 
            }
        }

        ## *** Create the search window ***
        rows <- which(mat_ind >= min(hrad) - inc & mat_ind <= max(hrad) + inc)
        cols <- which(mat_ind >= min(vrad) - inc & mat_ind <= max(vrad) + inc)
        
        if (!precise) {
            ## Fill matrix (recangles version)
            mat[rows, cols] <- 1
        } else {
            ## More precise version
            ## 1. Check for pixels within ellipse part of cone
            ## 2. Check for pixels within triangle part of cone
            rc <- expand.grid(x=rows*2*pi/size, y=cols*2*pi/size)
            pxs <- ellipse_pixels(xy=rc, hrad=hrad, vrad=vrad,
                                  a=a, b=b, h=h, use_tri=use_tri, upper=upper)
            mat[rows, cols] <- mat[rows, cols] + matrix(pxs, nrow=length(rows),
                                                        ncol=length(cols))
            ## sum(dmat <= theta/2)/length(dmat)  # should approach pi/4 if circle exactly inscribed
        }
    }
    return ( mat )
}

## Pixels within ellipse from search window
ellipse_pixels <- function(xy, hrad, vrad, a, b, h, use_tri, upper) {
    center <- c(mean(hrad), mean(vrad))  # center of base of cone
    ## Find ellipse pixels
    res <- ifelse((xy$x - center[1])**2/b**2 + (xy$y - center[2])**2/a**2 <= 1,
                  1, 0)
    if (use_tri) {  # find triangle pixels
        if (upper) {  # target looking down on nbr, triangle on upper side
            line1 <- line_bw_points(c(center[1] - b, center[2]),
                                    c(center[1], center[2] + h))
            line2 <- line_bw_points(c(center[1] + b, center[2]),
                                    c(center[1], center[2] + h))
        } else {
            line1 <- line_bw_points(c(center[1] - b, center[2]),
                                    c(center[1], center[2] - h))
            line2 <- line_bw_points(c(center[1] + b, center[2]),
                                    c(center[1], center[2] - h))
        }
        res[xy$x <= center[1] & xy$y >= center[2] &
                xy$y <= line1[["slope"]] * xy$x + line1[["int"]]] <- 1
        res[xy$x > center[1] & xy$y >= center[2] &
                xy$y <= line2[["slope"]] * xy$x + line2[["int"]]] <- 1
    }
    return ( res )
}

## center = c(mean(hrad), mean(vrad))
## plot(rc, type="n")
## plotellipse(rx = b, ry = a, mid = center, col = "green")
## polygon(x=c(center[1] - b, center[1], center[1] + b, center[1] - b),
##         y=c(center[2], center[2] + h, center[2], center[2]),
##         col = "darkgreen")
## points(rc)
## abline(v=center[1], col = "blue")
## abline(h=center[2], col = "red")

## Line between two points
line_bw_points <- function(p1, p2) {
    m <- (p2[2] - p1[2])/(p2[1] - p1[1])
    int <- p1[2] - m * p1[1]
    res <- c(m, int)
    names(res) <- c("slope", "int")
    return ( res )
}

## Image of matrix, all neighbors treated as cones
image_hood_cones <- function(targ, nbrs, size=100, precise = TRUE) {
    pmat <- pixel_matrix_cones(targ, nbrs, size=size, precise=precise)
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

## Make graphics
source("~/work/neighborhoods/surround/rewrite/create_test.R")
par(mfrow = c(1,2))
image_hood_cones(targ, nbrs, 1000, precise=T)
draw_hood_full(nbrs)
