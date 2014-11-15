### show.R --- 
## Filename: show.R
## Description: Make and examine test neighborhoods with graphics 
## Author: Noah Peart
## Created: Fri Nov 14 16:20:16 2014 (-0500)
## Last-Updated: Fri Nov 14 20:40:56 2014 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/neighborhoods/surround/rewrite/spheres.R")
source("~/work/neighborhoods/surround/rewrite/create_test.R")
source("~/work/neighborhoods/surround/rewrite/pixel_matrix.R")

## nbrhood params
radius = 2.5
num_nebs = 7
size = 200

## gen. test data
targ <- samp[sample(1:nrow(samp),1),]
nbrs <- make_nbrs(targ, samp, num_nebs = num_nebs, radius = radius)

## Show stuff
hood_details <- function(targ, nbrs) {
    cat(sprintf("\nTarget Height:\t%0.2f\n\n", targ$ht))
    cat("Neighbor Details:\n\n")
    cat(sprintf("\tX\tY\tHeight\n"))
    for (i in 1:nrow(nbrs)) {
        pos <- pos_vec_ellipse(nbrs[i, "x"], nbrs[i, "y"], nbrs[i, "z"], nbrs[i, "crdepth"],
                               height = nbrs[i, "ht"])
        cat( sprintf( "\t%i\t%i\t%0.2f\n", pos[["x"]], pos[["y"]], pos[["z"]]) )
    }
}


regen <- function() {
    ## gen. test data
    targ <- samp[sample(1:nrow(samp),1),]
    nbrs <- make_nbrs(targ, samp, num_nebs = num_nebs, radius = radius)

    hood_details(targ, nbrs)
    par(mfrow = c(1,2))
    draw_hood_full(nbrs)
    image_hood(targ, nbrs, size = size, precise = TRUE)
}
