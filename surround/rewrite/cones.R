### cones.R --- 
## Filename: cones.R
## Description: Conical neighbors
## Author: Noah Peart
## Created: Mon Nov 24 18:54:32 2014 (-0500)
## Last-Updated: Tue Nov 25 13:40:57 2014 (-0500)
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

## Angle subtended at target by vector from middle of neighbor crown base to crown top
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
