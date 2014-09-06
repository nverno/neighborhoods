## This version just deals with the immediate neighborhood (ring) surrounding the
## target

## Load neighborhood functions, parameters, data,
## and create neighbor matrices
source("~/work/functions/neighborhoods/rewrite/final/run-mnm.R")

## Old surround functions
source("~/work/neighborhoods/surround/functions.R")

## Coordinate conversions
source("~/work/functions/functions-coordinates.R")

## Neighbor coordinates relative to target coordinates
## (i.e neighbor_X - target_X)
coords_rel <- c("x_rel", "y_rel", "z_rel")
coords <- c("x", "y", "z")

for (i in seq_along(coords)) {
    nMat[[coords_rel[i]]] <-  nMat[[coords[i]]] - nMat[["targets"]][[coords[i]]]
}

## Distances from target to neighbors
nMat[["distance"]] <- sqrt((nMat[["targets"]][["x"]] - nMat[["x"]])^2 +
                           (nMat[["targets"]][["y"]] - nMat[["y"]])^2)

## Aspect and slope by permanent plot
require(plyr)
pInfo <- ddply(dat, .(pplot), function(x) {
    data.frame(aspect = unique(x[["asp"]]),
               slope = unique(x[["slope"]]))
})

## Create matrix with number of components each neighbor is a part of




################################################################################
##
##                                  Sandbox
##
################################################################################
## Single target data
targ <- 3
mats <- which(unlist(lapply(nMat, function(x) !is.null(dim(x)))))
single <- lapply(nMat[mats], function(x) {
    x[targ,]
})

## Visualize
par(mfrow=c(1,2))

## Target and neighbors in the greater plot
xyvals <- expand.grid(x=-5:5, y=-5:5)
plot(xyvals, type = "n")
abline(h=-5:5, v = -5:5, lty = 2)
abline(h=c(-5,0,5), v = c(-5,0,5), lwd = 2)
points(single[["targets"]][["x"]], single[["targets"]][["y"]],
       pch = 16, col = "blue")
points(single[["x"]], single[["y"]], pch = 17, col = "red")

## Target's immediate neighborhood
xyvals <- expand.grid(x=-2:2,y=-2:2)
plot(xyvals$x, xyvals$y, type = "n")
abline(h = -1.5:1.5, v = -1.5:1.5, lty = 2)
points(0,0, col = "blue", pch = 15)
points(single[["x_rel"]], single[["y_rel"]], col = "red", pch = 16)

## create nbrs data.frame
nbrs <- data.frame(x = nMat[["x_rel"]][targ,],
                   y = nMat[["y_rel"]][targ,],
                   z = nMat[["y_rel"]][targ,],
                   distance = nMat[["distance"]][targ,],
                   size = nMat[["ba"]][targ,])
nbrs <- nbrs[complete.cases(nbrs), ]

## Indices of occupied components
nsize <- 9
C <- 2
oc         <-           # indices of occupied components
    components_occupied(cbind(nbrs$x,nbrs$y), nsize=nsize)
ccs        <-           # indices of occupied connected components
    connected_comps_occupied(oc=oc, nsize=nsize, C=C)

## Return list of connected components given a neighbor hood side length and
##  connected component size
## - Target is at (0,0), so component indices are relative to (0,0)
connected_comps <- function(C, nsize=9) {
    ## start from top right corner
    num_outer <- nouter(nsize)
    inds <- component_indices(nsize)
    comps <- lapply(1:nrow(inds), FUN = function(i) {
        iseq <- connected_comps_inds(i=i,C=C,nsize=nsize)
        inds[iseq, ]
    })
    return ( comps )
}


## Example neighborhood
ex_neighborhood <- function(radius=1.5, numQuads=NULL, nbrs=NULL,
                            numNebs=NULL, rand=TRUE, ...){
    xyvals <- expand.grid(x=-radius:radius, y=-radius:radius)
    plot(xyvals, type="n", main="Example Neighborhood")
    abline(h=-radius:radius, v=-radius:radius, lty=2)
    abline(h=c(-radius,radius),v=c(-radius,radius),lwd=2)
    points(0,0, col = "blue", pch=15)

    ## Add neighbors if there are any
    if (is.null(nbrs) && rand) {
        if (is.null(numNebs))
            numNebs <- sample(1:12,1)
        nbrs <- data.frame(x = sample(-1:1, numNebs, replace = T),
                           y = sample(-1:1, numNebs, replace = T))
        nbrs <- nbrs[-which(nbrs[["x"]] == 0 & nbrs[["y"]] == 0), ]
        print(sprintf("Random neighborhood with %s quadrats occupied:",
                      nrow(unique(nbrs))))
        print(unique(nbrs))

    }
    if (!is.null(nbrs))
        points(nbrs, col="red", pch = 16)

    ## Convert to polar coords
    pcoords <- t(apply(nbrs, 1, function(x) cart2pol(x = x[["x"]], y = x[["y"]])))
    pcoords <- pcoords[order(pcoords[,2]),] # order by polar degree

    ## Define quadrants and enumerate
    if (is.null(numQuads)) numQuads <- 8
    occ <- cut(pcoords[,2], breaks = seq(0, 2*pi, length.out = numQuads+1)) # enumerate

    ## Draw quadrant lines
    rad <- 2*pi / numQuads
    for (i in 1:(numQuads/2)) {
        pp <- rotate_point(1, 0, theta_r = rad * i)
        abline(0, pp[2]/pp[1], lwd = 3, col = "blue", lty = 2)
    }

    ## Fill in occupied quadrats
    occ <- as.integer(occ)

    ## **** WORKING ON THE POLYGONS... ****
    for (i in 1:numQuads) {
        if (i %in% occ) {
            pp1 <- rotate_point(radius, 0, theta_r = rad * (i - 1))
            slp1 <- pp1[2]/pp1[1]
            pp2 <- rotate_point(radius, 0, theta_r = rad * i)
            slp2 <- pp2[2]/pp2[1]
            filler <- seq(pp1[1], pp2[1], length.out = 100)
            xs <- c(0, filler, 0)
            ys <- c(0, sqrt(radius^2 - filler^2), 0)
            polygon(x = xs, y = ys, col = "steelblue", density = 10,
                    lty = 2, lwd = 2, angle = atan((slp1+slp2)/2) * 180/pi + 90)
        }
    }


}

## 45 degree surround
