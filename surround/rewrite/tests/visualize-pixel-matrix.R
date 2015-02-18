### visualize-pixel-matrix.R --- 
## Filename: visualize-pixel-matrix.R
## Description: Visual tests for pixel matrix
## Author: Noah Peart
## Created: Wed Feb 18 13:53:11 2015 (-0500)
## Last-Updated: Wed Feb 18 16:48:09 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/nbrs3d/vis.R")  # 3d visualization stuff (p3d, rgl shapes)
source("~/work/neighborhoods/surround/rewrite/pixel_matrix.R")

draw_hood_full(nbrs = nbrs)  # 2D representation
p3d(nbrs)  # 3D
abclines3d(0, a = diag(4), col = "slategray")

## Add target crown to 3D plot
add_target <- function(targ, wire = FALSE, crwn_col = "darkred") {
    require(rgl)
    if (all(sapply(c(targ$x, targ$y, targ$z), is.null)))
        targ[, c("x", "y", "z")] <- c(0,0,0)  # target centered in plot
    tt <-  targ[complete.cases(targ[,c("x","y","z","dbh","ht","crdepth","crarea")]), ]
    p3d(tt, crwn_wire = wire, crwn_col = crwn_col)
}

add_target(targ)
