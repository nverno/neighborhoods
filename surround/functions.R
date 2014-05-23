###############################################################################
##
##  Functions for Neighborhood Surround Index (NSI) and testing NSI
##
###############################################################################

## Euclidean distance
euc <- function(p1, p2)
    sqrt((p1[1]-p2[1])^2 + (p1[2]-p2[2])^2)

nv <- function(nbrs, distance, alpha, beta) {
    return( nbrs^alpha / distance^beta )
}


## Returns a data.frame of indices in the outermost ring of neighborhood
##  given side length of neighborhood.
## The indices are ordered clockwise the top right corner of the neighborhood and
##  are centered around the origin
component_indices <- function(side_length) {
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
connected_comps <- function(side_length, C) {
    ## start from top right corner
    num_outer <- side_length + 2 * (side_length-1) + (side_length-2)
    inds <- component_indices(side_length)
    comps <- lapply(1:nrow(inds), FUN = function(i) {
        iend <- (i+C-1) %% nrow(inds)
        iseq <- i:iend
        if (iend < i)
            iseq <- c(i:nrow(inds), 0:iend)
        inds[iseq, ]
    })
    return ( comps )
}


## Computes the Neighborhood Surround Index (NSI)
## - nbrs: dataframe with (x,y) coordinates of neighbors relative to target at (0,0),
##   neighbor sizes, and distances to neighbors
## - side_length: length along one side of the neighborhood
## - C: number of quadrats that form a connected component
## - alpha, beta: parameters in neighbor mass calculation
## - theta: parameters for slope/aspect
nsi <- function(nbrs, side_length, C, alpha, beta, theta) {
    num_outer <- side_length + 2 * (side_length-1) + (side_length-2)
    oc <- components_occupied(cbind(nbrs$x,nbrs$y), side_length) # indices of occupied components
    ccs <- connected_comps_occupied(oc, side_length, C) # indices of occupied connected components
    oc_masses <- component_masses(oc, alpha, beta, theta, nbrs) # individual component masses
    cc_masses <- connected_comps_masses(oc_masses, ccs, C) # connected component masses
    return ( sum(c(oc_masses, cc_masses)) )
}


## Return list of connected component masses given a list of component masses,
##  a list of connected components, neighborhood side length,
##  and length of connected components
## - side_length: length along one side of the neighborhood
## - C: number of quadrats that form a connected component
## - oc_masses: masses of occupied components
## - ccs: list of occupied connected components
connected_comps_masses <- function(oc_masses, ccs, C) {
    num_outer <- side_length + 2 * (side_length-1) + (side_length-2)
    cc_masses <- sapply(1:length(ccs), function(i) {
        iend <- (i+C-1) %% (num_outer+1)
        iseq <- i:iend
        if (iend < i)
            ifelse(iend > 0, { iseq <- c(i:num_outer, 1:iend) },
                   { iseq <- c(i:num_outer, 1) })
        sum( oc_masses[iseq] ) # sum the occupied component masses
    })
    return ( cc_masses )
}


## Returns list of neighborhood masses for a given list of occupied neighborhood
##  indices, neighborhood parameters (alpha, beta, theta), and
##  a data.frame containing neighbor information for target
##  (distance, direction_x, direction_y, sizes)
component_masses <- function(oc, alpha, beta, theta, nbrhood) {
    comps <- component_indices(side_length)
    masses <- sapply(oc, function(i) {
        inds <- comps[i,] # indices of occupied component
        nebs <- nbrhood[nbrhood$x == inds[1] & nbrhood$y == inds[2], ] # neighbors occupying component
        sum( nebs$size^alpha / nebs$distance^beta )
    })
    return ( masses )
}


## Returns indices of occupied connected components given indices of
##  occupied components, neighborhood side length, and connected componen length
## - oc: indices of occupied components
## - C: number of quadrats that form a connected component
## - side_length: length along one side of the neighborhood
connected_comps_occupied <- function(oc, side_length, C) {
    num_outer <- side_length + 2 * (side_length-1) + (side_length-2)
    ccs <- unlist(sapply(oc, function(i) {
        iend <- (i+C-1) %% (num_outer+1)
        iseq <- i:iend
        if (iend < i)
            ifelse(iend > 0, { iseq <- c(i:num_outer, 1:iend) },
                   { iseq <- c(i:num_outer, 1) })
        ifelse( all(oc[iseq] %in% oc), oc[i], NA )
    }))
    return ( ccs[!is.na(ccs)] )
}

## Returns list of indices of neighborhood components that are occupied
##  in a given neighborhood
## - nbrs: matrix of (x,y) coordinates of neighbors relative to target at (0,0)
## - side_length: length along one side of the neighborhood
components_occupied <- function(nbrs, side_length) {
    comps <- component_indices(side_length)
    complst <- split(comps, row(comps))
    nbrlst <- split(unique(nbrs), row(unique(nbrs)))
    return ( which(complst %in% nbrlst) )
}
