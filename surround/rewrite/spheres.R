### spheres.R --- 
## Filename: spheres.R
## Description: Deal with spherical neighbors
## Author: Noah Peart
## Created: Mon Nov 10 12:45:00 2014 (-0500)
## Last-Updated: Mon Nov 10 14:18:31 2014 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/functions/functions-geometry.R")  # solid angle for cone

## Z-value of center of mass of ellipse
z_ellipse <- function(height, z, crdepth) {
    return ( (height + z) - (crdepth/2) )
}
## z_ellipse(nbr$ht, nbr$z, nbr$crdepth)

## Position vector of ellipsoid neighbor
pos_vec_ellipse <- function(x, y, z, crdepth, height) {
    z_mass <- z_ellipse(height, z, crdepth)
    out <- c(x, y, z_mass)
    names(out) <- c("x", "y", "z")
    return ( out )
}
## pos <- pos_vec_ellipse(nbr$x, nbr$y, nbr$z, nbr$crdepth, nbr$ht)

## Find opening angle, theta, of cone formed from target to spherical neighbor
## NOTE: crown radius of neighbor is the minimum of its actual crown radius
## and the distance to target (to avoid condition where target's crown is
## enveloped by neighbor's crown)
cone_theta <- function(nbr) {
    r <- min(sqrt(nbr[, "crarea"]/pi), nbr$dist)  # if crown of nbr obscures target, set to dist
    d <- nbr$dist
    theta1 <- asin(r/d)
    theta <- theta1*2
    return ( theta )
}

## Draw neighbors(circles) in 2D and the tangents to them
xyvals <- expand.grid(x=-radius:radius,
                      y=-radius:radius)
plot(xyvals, type="n", main="Example Neighborhood")
abline(h=-radius:radius, v=-radius:radius, lty=2)
abline(h=c(-radius,radius),v=c(-radius,radius),lwd=2)
points(0,0, col = "blue", pch=15)

symbols(nbr$x, nbr$y, circles = r, add=TRUE, inches=FALSE)
tangs <- pol2cart(sqrt(d^2-r^2), cart2pol(0,1)[2] + c(-1,1)*theta1)
lines(x=c(0, tangs[1,1]), y=c(0, tangs[1,2]))
lines(x=c(0, tangs[2,1]), y=c(0, tangs[2,2]))
