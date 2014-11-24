### spheres.R --- 
## Filename: spheres.R
## Description: Spherical neighbors
## Author: Noah Peart
## Created: Mon Nov 10 12:45:00 2014 (-0500)
## Last-Updated: Mon Nov 24 18:55:11 2014 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/functions/functions-geometry.R")  # solid angle for cone
require(shape)  # plotellipse, plotcircle

## Z-value of center of mass of ellipse
z_ellipse <- function(height, z, crdepth) {
    return ( (height + z) - (crdepth/2) )
}
## z_ellipse(nbr$ht, nbr$z, nbr$crdepth)

## Position vector of ellipsoid neighbor (from origin)
pos_vec_ellipse <- function(x, y, z, crdepth, height) {
    z_mass <- z_ellipse(height, z, crdepth)
    out <- c(x, y, z_mass)
    names(out) <- c("x", "y", "z")
    return ( out )
}
## pos <- pos_vec_ellipse(nbr$x, nbr$y, nbr$z, nbr$crdepth, nbr$ht)

## Find opening angle, theta, of cone formed from target to spherical neighbor
## and neighbors crown radius, r
## NOTE: crown radius of neighbor is the minimum of its actual crown radius
## and the distance to target (to avoid condition where target's crown is
## enveloped by neighbor's crown)
cone_theta <- function(nbr) {
    r <- min(c(sqrt(nbr[["crarea"]]/pi), nbr[["dist"]]))  # if crown of nbr obscures target, set to dist
    d <- nbr$dist
    theta1 <- asin(r/d)
    theta <- theta1*2
    out <- c(r, theta)
    names(out) <- c("r", "theta")
    return ( out )
}

## Draw empty neighborhood
draw_hood_empty <- function(radius=2.5) {
    xyvals <- expand.grid(x=-radius:radius,
                          y=-radius:radius)
    plot(xyvals, type="n", main="Example Neighborhood")
    abline(h=-radius:radius, v=-radius:radius, lty=2)
    abline(h=c(-radius,radius),v=c(-radius,radius),lwd=2)
    points(0,0, col = "blue", pch=15)
}

## Draw neighbors(circles) in 2D and the tangents to them
draw_nbr_sphere <- function(nbr, radius=2.5) {
    ## Compute stuff: tangents, cone angle, nbr radius
    pos <- pos_vec_ellipse(nbr$x, nbr$y, nbr$z, nbr$crdepth, nbr$ht)
    pol <- cart2pol(pos[["x"]], pos[["y"]])
    nbr_cone <- cone_theta(nbr)
    tang_angle <- c(-1,1)*nbr_cone[["theta"]]/2 + pol[["theta"]]  # polar angles to tangents

    ## Draw neighbor
    points(nbr$x, nbr$y, col = "red", pch=16)
    plotellipse(rx=radius, ry=radius,
                mid = c(0,0), from=tang_angle[1], to = tang_angle[2])
    plotcircle(r=nbr_cone[["r"]], mid=c(nbr$x, nbr$y), lcol = "red", lty=2)
    tangent_points <- pol2cart(radius, tang_angle)
    lines(x=c(0, tangent_points[1,1]), y=c(0, tangent_points[1,2]))
    lines(x=c(0, tangent_points[2,1]), y=c(0, tangent_points[2,2]))
}

## Draw whole neighborhood
draw_hood_full <- function(nbrs, radius=2.5) {
    draw_hood_empty()
    for (n in 1:nrow(nbrs)) {
        draw_nbr_sphere(nbrs[n,])
    }

}

