###############################################################################
##
## Create some nsi output from test data and sample plot
## - Create visuals and ranking of various neighborhood configurations
##
###############################################################################
## These are sourced in make-sample-data.R
## source("~/work/neighborhoods/surround/functions.R")
## source("~/work/functions/functions-neighborhood.R")

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


################################################################################
##
##                                  Set-up
##
################################################################################
## choose target
i <- 7
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

################################################################################
##
##                            Plotting Functions
##
################################################################################
## function to add neighborhood gridlines
glines <- function(nsize=9) {
    disp <- (sqrt(nsize)-1) / 2 # neighborhood size (drawn around origin)
    abline(h = (-disp-1.5):(disp+1.5), v = (-disp-1.5):(disp+1.5) )
    abline(h = c(-disp-.5, disp+.5), v = c(-disp-.5, disp+.5),
           lwd = 2, col = "dark red")
}

lines(x=c(-1, 1), y=c(1, 1), lwd=100, col= alpha("blue", 0.5))
lines(x=c(0,1,1), y=c(1,1,0), lwd = 100, col = alpha("blue", 0.5))

## Draw connected components as semi-transparent bars of color
cc_lines <- function(cc, C, nsize) {
    require(scales) # for alpha
    inds <- component_indices(nsize = nsize) # indices of outer ring
    for (c in cc) {
        cc_inds <- connected_comps_inds(i=c,C=C,nsize=nsize)
        xs <- inds[cc_inds, "xs"]
        ys <- inds[cc_inds, "ys"]
        lines(xs, ys, col = alpha("blue", 0.5), lwd = 100)
    }
}


## Function to draw connected components as ellipses
## *** NOTE: only for C=2 and doesnt wrap around corners ***
## - takes as agruments a connected component (neighborhood ordering) and
##  the neighborhood size
cc_ellipse <- function(cc, C, nsize) {
    require(plotrix) # for ellipses
    inds <- component_indices(nsize = nsize) # indices of outer ring
    for (c in cc) {
        cc_inds <-connected_comps_inds(i=c,C=C,nsize=nsize)
        x <- sum(inds[cc_inds,1]) / C
        y <- sum(inds[cc_inds,2]) / C
        orient <- ceiling(c / (sqrt(nsize)-1)) %% 2 # 1: horizontal orientation
        ifelse(orient==1, { a = 1; b = 0.5 }, { a = .5; b = 1})
        draw.ellipse(x=x, y=y, a=a, b=b, lwd = 2)
    }
}

## Draws an empty neighborhood of specified size
draw_empty <- function(nsize=9, title="", numbers="none") {
    disp <- (sqrt(nsize)-1) / 2 # neighborhood size (drawn around origin)
    pgrid <- expand.grid(x=(-disp-1):(disp+1), y=(-disp-1):(disp+1))
    plot(pgrid, type = "n", main = title)
    if (numbers == "image") {
        text(pgrid, labels = 1:nrow(pgrid), cex=1.5)
    }
    if (numbers == "neighborhood"){
        inds <- component_indices(nsize = nsize) # indices of outer ring
        text(inds, labels=c(1:nouter(nsize)), cex=1.5)
    }
}


################################################################################
##
##                                  Summary
## - Neighborhood grid with connected components shown, connected component size and
##   neighborhood size specified
## - Neighborhood grid with neighbor points
##
################################################################################
library(plotrix)
library(scales)

## Neighborhood grid with connected components shown, connected component size and
##   neighborhood size specified
## nsize=9, C=2
ttl <- paste(nsize, "Quadrat Neighborhood Layout")
draw_empty(nsize, title = ttl, numbers = "neighborhood")
glines(nsize)
cc_lines(cc=1:(nsize-1), C=2, nsize=9) # add connected component bars
cc_ellipse(cc=1:(nsize-1), C=2, nsize=nsize) # add connected component ellipses

## nsize=9, C=3
ttl <- paste(nsize, "Quadrat Neighborhood Layout")
draw_empty(nsize, title = ttl, numbers = "neighborhood")
glines(nsize)
cc_lines(cc=2:3, C=3, nsize=9)

## nsize=25, C=2
ttl <- paste(25, "Quadrat Neighborhood Layout")
draw_empty(25, ttl)
glines(25)
cc_lines(cc=4, C=2, nsize=25)
cc_ellipse(cc=4, C=2, nsize=25)

## Neighborhood grid with neighbor points
ttl <- paste("Neighborhood for tree", i, "in pplot", pnum, "from year", abba_mats$yr[i])
draw_empty(nsize, title=ttl)
glines()
points(jitter(nbrs$x), jitter(nbrs$y))

################################################################################
##
##                                  Images
##
################################################################################
library(lattice)

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


################################################################################
##
##                                 Values
##
################################################################################
## - Grid of neighborhood with quadrat mass values
ttl <- paste("Individual Component Masses\n for tree", i, "in pplot", pnum, "from year", abba_mats$yr[i])
draw_empty(nsize, title=ttl)
glines()
xs <- cinds[oc, "xs"]
ys <- cinds[oc, "ys"]
text(xs, ys, labels = round(oc_masses, 4))


## - Grid of neighborhood with complete mass values (individual + connected components)
ttl <- paste("Total Component Masses\n for tree", i, "in pplot", pnum, "from year", abba_mats$yr[i])
draw_empty(nsize, title=ttl)
glines()
xs <- cinds[oc, "xs"]
ys <- cinds[oc, "ys"]
imasses <- rep(0, nrow(cinds))
imasses[oc] <- oc_masses # individual component masses
cmasses <- rep(0, nrow(cinds)) # cumulative connected component masses
for (n in 1:length(ccs)) {
    cc_inds <- connected_comps_inds(i=ccs[n],C=C,nsize=nsize)
    cmasses[cc_inds] <- cmasses[cc_inds] + cc_masses[n]
}
qmass <- imasses + cmasses # quadrat mass (cc + ic)
text(xs, ys, labels = round(qmass[qmass > 0], 4)) # total weight of quadrats to figure
cc_lines(cc=ccs, C=C, nsize=nsize) # add cc lines
cc_info(CC=ccs, CC_masses=cc_masses, C=C, nsize=nsize)

## Add line outside of quadrats and text for CC
cc_info <- function(CC, CC_masses, C=C, nsize=nsize) {
    require(scales) # for alpha
    inds <- component_indices(nsize = nsize) # indices of outer ring
    for (c in 1:length(CC)) {
        orient <- ceiling(CC[c] / (sqrt(nsize)-1)) %% 2 # horizontal orientation
        yoff <- xoff <- 0 # offets for CC line
        if (orient == 1)
            ifelse(inds[CC[c],"ys"] > 0, { yoff <- .6 }, { yoff <- -.6 })
        if (orient == 0)
            ifelse(inds[CC[c],"xs"] > 0, { xoff <- .6 }, { xoff <- -.6 })
        cc_inds <- connected_comps_inds(i=CC[c],C=C,nsize=nsize)
        xs <- inds[cc_inds, "xs"] + xoff
        ys <- inds[cc_inds, "ys"] + yoff
        points(xs, ys, type="b", pch=0,  lty=2, lwd=3) # add CC line
        text(mean(xs) + xoff/6, mean(ys) + yoff/6, labels = round(CC_masses[c],4))
    }
}


################################################################################
##
##                            Put It All Together
##
################################################################################
showNSI <- function(tree, NM, nsize=9, C=2, alpha=1, beta=1, theta=1, together=TRUE) {
    require(plotrix)
    require(scales)
    require(lattice)
    i <- tree
    num_nebs <- NM$number_neighbors[i]
    nbrs <- data.frame(x=NM$direction_x[i, 1:num_nebs],
                       y=NM$direction_y[i, 1:num_nebs],
                       distance=NM$distances[i, 1:num_nebs],
                       size=NM$variable[i, 1:num_nebs])

    ## Compute neighborhood values
    oc <- components_occupied(cbind(nbrs$x,nbrs$y), nsize=nsize) # indices of occupied components
    ccs <- connected_comps_occupied(oc=oc, nsize=nsize, C=C) # indices of occupied connected components
    oc_masses <- component_masses(oc=oc, alpha=alpha, beta=beta, theta=theta, nbrhood=nbrs,
                                  nsize=9) # individual component masses
    cc_masses <- connected_comps_masses(oc_masses=oc_masses, ccs=ccs, C=C, nsize=9) # connected component masses
    nsind <- nsi(nbrs=nbrs, C=C, alpha=alpha, beta=beta, theta=theta, nsize=9)
    cinds <- component_indices(nsize)

    if (together)
        dev.new()
        par(mfrow = c(2,2))

    ## Summary graphics to show components in this size neighborhood and numberings
    ttl <- paste(nsize, "Quadrat Neighborhood Layout")
    draw_empty(nsize, title = ttl, numbers = "neighborhood")
    glines(nsize)
    cc_lines(cc=1:(nsize-1), C=C, nsize=nsize) # add connected component bars
    cc_ellipse(cc=1:(nsize-1), C=C, nsize=nsize) # add connected component ellipses

    ## Neighborhood grid with neighbor points
    if (!together)
        dev.new()
    ttl <- paste("Neighborhood for tree", i, "in pplot", pnum, "from year", abba_mats$yr[i])
    draw_empty(nsize, title=ttl)
    glines()
    points(jitter(nbrs$x), jitter(nbrs$y), pch=24, cex=2, col="red", bg="dark green")

    ## - Grid of neighborhood with quadrat mass values
    if (!together)
        dev.new()
    ttl <- paste("Individual Component Masses\n for tree", i, "in pplot", pnum, "from year", abba_mats$yr[i])
    draw_empty(nsize, title=ttl)
    glines()
    xs <- cinds[oc, "xs"]
    ys <- cinds[oc, "ys"]
    text(xs, ys, labels = round(oc_masses, 4))


    ## - Grid of neighborhood with complete mass values (individual + connected components)
    if (!together)
        dev.new()
    ttl <- paste("Total Component Masses\n for tree", i, "in pplot", pnum, "from year", abba_mats$yr[i])
    draw_empty(nsize, title=ttl)
    glines()
    xs <- cinds[oc, "xs"]
    ys <- cinds[oc, "ys"]
    imasses <- rep(0, nrow(cinds))
    imasses[oc] <- oc_masses # individual component masses
    cmasses <- rep(0, nrow(cinds)) # cumulative connected component masses
    for (n in 1:length(ccs)) {
        cc_inds <- connected_comps_inds(i=ccs[n],C=C,nsize=nsize)
        cmasses[cc_inds] <- cmasses[cc_inds] + cc_masses[n]
    }
    qmass <- imasses + cmasses # quadrat mass (cc + ic)
    text(xs, ys, labels = round(qmass[qmass > 0], 4)) # total weight of quadrats to figure
    cc_lines(cc=ccs, C=C, nsize=nsize) # add CC lines
    cc_info(CC=ccs, CC_masses=cc_masses, C=C, nsize=nsize) # add CC info
    text(0,0, round(nsind, 4), font=2)

    ## Image of neighborhood (pixel intensity by quadrat MASS)
    dev.new()
    side_length <- sqrt(nsize)
    mat <- matrix(NA, side_length, side_length)
    img_oc <- imgorder(oc) # image ordered occupied components
    img_cc <- imgorder(ccs) # image ordered occupied connected components
    mat[img_oc] <- oc_masses
    levelplot(mat, main = "Masses of individual components")

}


################################################################################
##
##                                   Test
##
################################################################################
showNSI(tree=1, NM=abba_mats, nsize=9, C=2, alpha=1, beta=1, theta=1, together=TRUE)


## Get all the NSIs
nsis <- rep(NA, nrow(abba_mats$distances))
NM <- abba_mats
for (i in 1:length(nsis)) {
    num_nebs <- NM$number_neighbors[i]
    nbrs <- data.frame(x=NM$direction_x[i, 1:num_nebs],
                       y=NM$direction_y[i, 1:num_nebs],
                       distance=NM$distances[i, 1:num_nebs],
                       size=NM$variable[i, 1:num_nebs])

    nsind <- nsi(nbrs=nbrs, C=C, alpha=alpha, beta=beta, theta=theta, nsize=9)
    nsis[i] <- nsind
}
