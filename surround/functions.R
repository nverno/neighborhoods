###############################################################################
##
##  Functions for Neighborhood Surround Index (NSI) and testing NSI
##
###############################################################################
##
## Euclidean distance
euc <- function(p1, p2)
    sqrt((p1[1]-p2[1])^2 + (p1[2]-p2[2])^2)


## Returns a data.frame of indices in the outermost ring of neighborhood
##  given side length of neighborhood.
## The indices are ordered clockwise the top right corner of the neighborhood and
##  are centered around the origin
component_indices <- function(nsize=9) {
    side_length <- sqrt(nsize)
    inc <- (side_length-1)/2
    xs <- c((-inc) : (-inc + side_length-1), rep((-inc+side_length-1), (side_length-2)),
            (-inc + side_length-1) : (-inc), rep((-inc), (side_length-2)))
    ys <- c(rep(inc, side_length), (inc-1):(inc-1-(side_length-2)),
            rep((-inc), (side_length-1)), (-inc+1):(inc-1))
    return ( cbind(xs, ys) )
}


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


## Computes the Neighborhood Surround Index (NSI)
## - nbrs: dataframe with (x,y) coordinates of neighbors relative to target at (0,0),
##   neighbor sizes, and distances to neighbors
## - nsize: number of quadrats in neighborhood
## - C: number of quadrats that form a connected component
## - alpha, beta: parameters in neighbor mass calculation
## - theta: parameters for slope/aspect
nsi <- function(nbrs, C, alpha, beta, theta, nsize = 9, ...) {
    num_outer <- nouter(nsize)
    oc <- components_occupied(nbrs=cbind(nbrs$x,nbrs$y), nsize=nsize) # indices of occupied components
    ccs <- connected_comps_occupied(oc=oc, nsize=nsize, C=C) # indices of occupied connected components
    oc_masses <- component_masses(oc=oc, alpha = alpha, beta = beta,
                                  theta = theta, nbrhood=nbrs, nsize=nsize) # individual component masses
    ifelse(length(ccs) > 0,
       { cc_masses <- connected_comps_masses(oc_masses=oc_masses, ccs=ccs, C=C,
                                             nsize=nsize) }, # connected component masses
       { cc_masses <- 0 })
    return ( sum(c(oc_masses, cc_masses)) )
}


## Return list of connected component masses given a list of component masses,
##  a list of connected components, neighborhood side length,
##  and length of connected components
## - C: number of quadrats that form a connected component
## - oc_masses: masses of occupied components
## - ccs: list of occupied connected components
## - nsize: number of quadrats in neighborhoods
connected_comps_masses <- function(oc_masses, ccs, C, nsize = 9) {
    if (length(ccs) < 1 || length(oc_masses) < 1)
        return ( NULL )
    num_outer <- nouter(nsize)
    cc_masses <- sapply(1:length(ccs), function(i) {
        iseq <- connected_comps_inds(ccs[i],C,nsize)
        ii <- which(names(oc_masses) %in% iseq)
        sum( oc_masses[ii] ) # sum the occupied component masses
    })
    names(cc_masses) <- ccs
    return ( cc_masses )
}


## Returns list of neighborhood masses for a given list of occupied neighborhood
##  indices, neighborhood parameters (alpha, beta, theta), and
##  a data.frame containing neighbor information for target
##  (distance, direction_x, direction_y, sizes)
component_masses <- function(oc, alpha, beta, theta, nbrhood, nsize=9) {
    if (length(oc) < 1)
        return ( NULL )
    comps <- component_indices(nsize)
    masses <- sapply(oc, function(i) {
        inds <- comps[i,] # indices of occupied component
        nebs <- nbrhood[nbrhood$x == inds[1] & nbrhood$y == inds[2], ] # neighbors occupying component
        theta_1 <- ifelse(theta<0, 1/theta, theta)
        theta_1^unique(nebs$z) + sum( nebs$size^alpha / nebs$distance^beta )
    })
    names(masses) <- oc
    return ( masses )
}


## Returns indices of occupied connected components given indices of
##  occupied components, neighborhood side length, and connected componen length
## - oc: indices of occupied components
## - C: number of quadrats that form a connected component
## - nsize: number of quadrats in neighborhood
connected_comps_occupied <- function(oc, C, nsize=9) {
    if (length(oc) < 1)
        return ( NULL)
    num_outer <- nouter(nsize)
    ccs <- unlist(sapply(oc, function(i) {
        iseq <- connected_comps_inds(i,C,nsize)
        ifelse( all(iseq %in% oc), i, NA )
    }))
    return ( ccs[!is.na(ccs)] )
}


## Returns the indices of components of connected components (neighborhood ordering)
connected_comps_inds <- function(i, C, nsize=9) {
    num_outer <- nouter(nsize)
    stopifnot(i < (num_outer+1) & i > 0)
    iseq <- ( ( (i-1):(i+C-2) ) %% num_outer ) + 1
    return ( iseq )
}


## Returns list of indices of neighborhood components that are occupied
##  in a given neighborhood
## - nbrs: matrix of (x,y) coordinates of neighbors relative to target at (0,0)
## - side_length: length along one side of the neighborhood
components_occupied <- function(nbrs, nsize=9) {
    comps <- component_indices(nsize)
    complst <- split(comps, row(comps))
    nbrlst <- split(unique(nbrs), row(unique(nbrs)))
    return ( which(complst %in% nbrlst) )
}


## Returns the number of outer quadrats for a given neighborhood size
nouter <- function(nsize=9) {
    side_length <- sqrt(nsize)
    num_outer <- side_length + 2 * (side_length-1) + (side_length-2)
    return ( num_outer )
}

## Compute nsi for each row of neighborhoods matrices
## - INPUT:
##  - mnmagg: aggregated neighborhood matrices
##  - nPars: neighborhood parameters
nsi_agg <- function(mnmagg, nPars, ...) {
    nsis <-  sapply(1:nrow(mnmagg[["distances"]]), FUN = function(row) {
        nbrs <- data.frame(x = mnmagg[["direction_x"]][row,],
                           y = mnmagg[["direction_y"]][row,],
                           z = mnmagg[["direction_z"]][row,],
                           distance = mnmagg[["distances"]][row,],
                           size = mnmagg[["variable"]][row,])
        nbrs <- nbrs[complete.cases(nbrs), ]
        nPars$nbrs <- nbrs
        do.call(nsi, args = nPars)
    })

    return ( nsis )
}

