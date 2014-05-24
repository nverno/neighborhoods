###############################################################################
##
## Create some nsi output from test data and sample plot
## - Create visuals and ranking of various neighborhood configurations
##
###############################################################################
source("~/work/neighborhoods/surround/functions.R")
source("~/work/functions/functions-neighborhood.R")

## load some test data and a sample plot
## - This makes neighbor matrices from plot 9
source("~/work/neighborhoods/surround/test/make-sample-data.R")

###############################################################################
##
## To do:
## - Function to create test data in a better way for more thorough testing
##
###############################################################################


###############################################################################
##
##                          Visualization
##
## Things I want to be able to look at:
##
## 1. Summary
## - Neighborhood grid with connected components shown, connected component size and
##   neighborhood size specified
## - Neighborhood grid with neighbor points
##
## 2. Images
## - Image of neighborhood (pixel intensity by quadrat MASS)
## - Image of neighborhood (pixel intensity by quadrat mass) + connected components
##   drawn somehow on top
##
## 3. Values
## - Grid of neighborhood with quadrat mass values
## - Grid of neighborhood with complete mass values (individual + connected components)
##
## 4. Rankings
## - Images sorted by NSI value + NSI value
##
###############################################################################


## set-up
disp <- (side_length-1) / 2 # neighborhood size (drawn around origin)
pgrid <- expand.grid(x=(-disp-1):(disp+1), y=(-disp-1):(disp+1))

## function to add appropriate gridlines
glines <- function() {
    abline(h = (-disp-1.5):(disp+1.5), v = (-disp-1.5):(disp+1.5) )
    abline(h = c(-disp-.5, disp+.5), v = c(-disp-.5, disp+.5),
           lwd = 2, col = "dark red")
}

## choose target
i <- 11
num_nebs <- abba_mats$number_neighbors[i]
nbrs <- data.frame(x=abba_mats$direction_x[i, 1:num_nebs],
                   y=abba_mats$direction_y[i, 1:num_nebs],
                   distance=abba_mats$distances[i, 1:num_nebs],
                   size=abba_mats$variable[i, 1:num_nebs])

## Compute neighborhood values
oc <- components_occupied(cbind(nbrs$x,nbrs$y), nsize=nsize) # indices of occupied components
ccs <- connected_comps_occupied(oc=oc, nsize=nsize, C=C) # indices of occupied connected components
oc_masses <- component_masses(oc=oc, alpha=alpha, beta=beta, theta=theta, nbrhood=nbrs,
                              nsize=9) # individual component masses
cc_masses <- connected_comps_masses(oc_masses=oc_masses, ccs=ccs, C=C, nsize=9) # connected component masses
nsind <- nsi(nbrs=nbrs, C=C, alpha=alpha, beta=beta, theta=theta, nsize=9)
cinds <- component_indices(nsize)

###############################################################################
##
##  Summary
##

## Function to draw connected components
## - takes as agruments the connected components (neighborhood ordering) and
##  the neighborhood size
cclines <- function(ccs, nsize) {

}

## Neighborhood grid with connected components shown, connected component size and
##   neighborhood size specified
plot(pgrid, type = "n", main = paste(nsize, "Quadrat Neighborhood Layout"))
glines()

draw.ellipse(x=c(-1), y=c(-0.5), 0.5, 1)

## Neighborhood grid with neighbor points
plot(pgrid, type = "n", main = paste("Neighborhood for tree", i, "in pplot", pnum,
                        "from year", abba_mats$yr[i]))
glines()
points(jitter(nbrs$x), jitter(nbrs$y))

###############################################################################
##
##  Images
##
library(lattice)
library(plotrix)

## Function to convert neighborhood ordering to image matrix ordering
## - neighborhood ordering: 1:num_outer clockwise from top right
## - image ordering: 1:n rowwise from origin
## Parameters:
## - ninds: list of neighborhood indices
## - nsize: neighborhood size (default 9 quadrats)
imgorder <- function(ninds, nsize = 9) {
    key = c(7,8,9,6,3,2,1,4) # key for nsize = 9
    if (nsize == 9)
        return ( key[ninds] )
}

## Image of neighborhood (pixel intensity by quadrat MASS)
mat <- matrix(NA, side_length, side_length)
img_oc <- imgorder(oc) # image ordered occupied components
img_cc <- imgorder(ccs) # image ordered occupied connected components
mat[img_oc] <- oc_masses
dev.new()
levelplot(mat, main = "Masses of individual components")

## Image of neighborhood (pixel intensity by quadrat mass) + connected components
##  drawn somehow on top
ccplus <- ccs
mat <- matrix(NA, side_length, side_length)
mat[img_oc] <- oc_masses
dev.new()
levelplot(mat, main = "Masses of individual components")
