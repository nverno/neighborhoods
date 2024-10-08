################################################################################
##
##                   Plotting Functions for Surround Stuff
##
################################################################################
## function to add neighborhood gridlines
glines <- function(nsize=9) {
  disp <- (sqrt(nsize)-1) / 2 # neighborhood size (drawn around origin)
  abline(h = (-disp-1.5):(disp+1.5), v = (-disp-1.5):(disp+1.5) )
  abline(h = c(-disp-.5, disp+.5), v = c(-disp-.5, disp+.5),
    lwd = 2, col = "dark red")
}


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
  inds <- component_indices(nsize = nsize)         # indices of outer ring
  for (c in cc) {
    cc_inds <-connected_comps_inds(i=c,C=C,nsize=nsize)
    x <- sum(inds[cc_inds,1]) / C
    y <- sum(inds[cc_inds,2]) / C
    orient <- ceiling(c / (sqrt(nsize)-1)) %% 2  # 1: horizontal orientation
    ifelse(orient==1, { a = 1; b = 0.5 }, { a = .5; b = 1})
    draw.ellipse(x=x, y=y, a=a, b=b, lwd = 2)
  }
}

## Draws an empty neighborhood of specified size
draw_empty <- function(nsize=9, title="", numbers="none") {
  disp <- (sqrt(nsize)-1) / 2                   # neighborhood size (drawn around origin)
  pgrid <- expand.grid(x=(-disp-1):(disp+1), y=(-disp-1):(disp+1))
  plot(pgrid, type = "n", main = title)
  if (numbers == "image") {
    text(pgrid, labels = 1:nrow(pgrid), cex=1.5)
  }
  if (numbers == "neighborhood"){
    inds <- component_indices(nsize = nsize)  # indices of outer ring
    text(inds, labels=c(1:nouter(nsize)), cex=1.5)
  }
}

## Add compass directions
compass_lines <- function(nsize=9) {
  disp <- (sqrt(nsize)-1) / 2 + 1  # displacement from origin to draw text
  text(c(-disp,-disp,disp,disp), c(-disp,disp,disp,-disp),
    labels = c("N","E","S","W"), font=2, cex = 2)
}

## Rotate point about origin by angle
## - x,y are initial point
## - angle is angle to rotate in degrees (counterclockwise)
rotate_point <- function(x, y, theta_r) {
  theta_r <- theta_r * pi/180 # theta in radians
  rot <- matrix(c(cos(theta_r), sin(theta_r), -sin(theta_r), cos(theta_r)),
    nrow = 2, ncol=2)
  p <- matrix(c(x,y))
  return ( rot %*% p )
}

## Add slope/aspect
## - arguments in degrees
## - Solid line above 0, dotted below 0
## - Adds an orthogonal line where plot is level with origin
slopeasp_lines <- function(asp, slp=NULL, nsize=9, addtext=TRUE) {
  disp <- (sqrt(nsize)-1) / 2 + 1  # displacement from origin to draw text
  aspect <- rotate_point(-1,-1,360-asp)
  lines(c(0,disp*aspect[1]), c(0, disp*aspect[2]),
    lwd = 3, col = "purple", lty=2) # downslope direction
  lines(c(0,-disp*aspect[1]), c(0, -disp*aspect[2]),
    lwd = 3, col = "purple", lty=1) # upslope direction
  ## Add orthogonal line
  ortho <- rotate_point(aspect[1],aspect[2],90)
  abline(a=0, b=ortho[2]/ortho[1], col="purple", lwd=1)
  if (!missing(slp)) {
    ## Add slope as text in center
    text(0,disp-.3, labels = paste("Slope:",slp), font=2)
  }
  if (addtext) {
    ## Add text saying what the aspect is
    text(0,disp, labels = paste("Aspect:",asp), font=2)
  }
}

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

## Add line outside of quadrats and text for CC
cc_info <- function(CC, CC_masses, C=C, nsize=nsize) {
  require(scales)                                      # for alpha
  inds <- component_indices(nsize = nsize)             # indices of outer ring
  for (c in 1:length(CC)) {
    orient <- ceiling(CC[c] / (sqrt(nsize)-1)) %% 2  # horizontal orientation
    yoff <- xoff <- 0                                # offets for CC line
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
showNSI <- function(tree, NM, nsize=9, C=2, alpha=1, beta=1, theta=1, together=TRUE,
  heat=FALSE) {
  require(plotrix)
  require(scales)
  require(lattice)
  i <- tree
  tag <- NM$id[i]
  num_nebs <- NM$number_neighbors[i]
  slope <- round(NM$slope)
  aspect <- NM$aspect
  nbrs <- data.frame(x=NM$direction_x[i, 1:num_nebs],
    y=NM$direction_y[i, 1:num_nebs],
    distance=NM$distances[i, 1:num_nebs],
    size=NM$variable[i, 1:num_nebs],
    z=NM$direction_z[i, 1:num_nebs])

  ## Compute neighborhood values
  oc         <-           # indices of occupied components
    components_occupied(cbind(nbrs$x,nbrs$y), nsize=nsize)
  ccs        <-           # indices of occupied connected components
    connected_comps_occupied(oc=oc, nsize=nsize, C=C)
  oc_masses  <-           # individual component masses
    component_masses(oc=oc, alpha=alpha, beta=beta, theta=theta, nbrhood=nbrs, nsize=nsize)
  cc_masses  <-           # connected component masses
    connected_comps_masses(oc_masses=oc_masses, ccs=ccs, C=C, nsize=nsize)
  nsind      <- nsi(nbrs=nbrs, C=C, alpha=alpha, beta=beta, theta=theta, nsize=nsize)
  cinds      <- component_indices(nsize)

  ## if (together)
  ##     dev.new()
  par(mfrow = c(2,2))

  ## Summary graphics to show components in this size neighborhood and numberings
  ttl <- paste(nsize, "Quadrat Neighborhood Layout")
  draw_empty(nsize, title = ttl, numbers = "neighborhood")
  glines(nsize)
  cc_lines(cc=1:(nsize-1), C=C, nsize=nsize)    # add connected component bars
  cc_ellipse(cc=1:(nsize-1), C=C, nsize=nsize)  # add connected component ellipses

  ## Neighborhood grid with neighbor points
  ## if (!together)
  ##     dev.new()
  ttl <- paste("Neighborhood for tree", tag, "in pplot", pnum, "from year", NM$yr[i])
  draw_empty(nsize, title=ttl)
  glines(nsize)
  compass_lines(nsize)
  slopeasp_lines(asp=aspect, slp=slope, nsize=nsize)
  points(jitter(nbrs$x), jitter(nbrs$y), pch=24, cex=2, col="red", bg="dark green")


  ## - Grid of neighborhood with quadrat mass values
  ## if (!together)
  ##     dev.new()
  ttl <- paste("Individual Component Masses\n for tree", tag, "in pplot", pnum, "from year", NM$yr[i])
  draw_empty(nsize, title=ttl)
  glines(nsize)
  compass_lines(nsize)
  slopeasp_lines(asp=aspect, slp=slope, nsize=nsize)
  xs <- cinds[oc, "xs"]
  ys <- cinds[oc, "ys"]
  if (length(oc_masses)>0)
    text(xs, ys, labels = round(oc_masses, 4))

  ## - Grid of neighborhood with complete mass values (individual + connected components)
  ## if (!together)
  ##     dev.new()
  ttl <- paste("Total Component Masses\n for tree", tag, "in pplot", pnum, "from year", NM$yr[i])
  draw_empty(nsize, title=ttl)
  glines(nsize)
  compass_lines(nsize)
  slopeasp_lines(asp=aspect, slp=slope, nsize=nsize)
  xs <- cinds[oc, "xs"]
  ys <- cinds[oc, "ys"]
  qmass <- rep(0, nrow(cinds)) # holds quadrat masses
  names(qmass) <- 1:nrow(cinds)
  qmass[oc] <- oc_masses                                     # add indiv. component masses
  qmass[as.numeric(names(cc_masses))] <-
    qmass[as.numeric(names(cc_masses))] + cc_masses        # add cc masses
  if(any(qmass != 0))
    text(xs, ys, labels = round(qmass[qmass != 0], 4))        # total weight of quadrats to figure
  cc_lines(cc=ccs, C=C, nsize=nsize)                           # add CC lines
  if (length(ccs) > 0)
    cc_info(CC=ccs, CC_masses=cc_masses, C=C, nsize=nsize)   # add CC info
  text(0,0, round(nsind, 4), font=2)

  ## Image of neighborhood (pixel intensity by quadrat MASS)
  ## if (heat) {
  ##     dev.new()
  ##     side_length <- sqrt(nsize)
  ##     mat <- matrix(NA, side_length, side_length)
  ##     img_oc <- imgorder(oc)   # image ordered occupied components
  ##     img_cc <- imgorder(ccs)  # image ordered occupied connected components
  ##     mat[img_oc] <- oc_masses
  ##     levelplot(mat, main = "Masses of individual components")
  ## }
}
