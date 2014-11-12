### create_test.R --- 
## Filename: create_test.R
## Description: Create test neighborhood data
## Author: Noah Peart
## Created: Tue Nov 11 16:31:59 2014 (-0500)
## Last-Updated: Tue Nov 11 19:00:52 2014 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/functions/functions-coordinates.R")
source("~/work/functions/functions-geometry.R")
source("~/work/ecodatascripts/vars/z-values/functions.R")

## Load data and pull out working subset (LOW elevation complete cases)
dat <- read.csv("~/work/data/moose/moose-long.csv")
samp <- dat[dat$elevcl == "L" & dat$stat == "ALIVE", c("spec", "ba", "ht", "crarea", "crdepth")]
samp <- samp[complete.cases(samp), ]  # Use this subset to sample neighbor variables
samp$shape <- ifelse(samp$spec %in% c("ABBA", "PIRU"), "cone", "sphere")

## Extract some bounds for simulating variables
slope_range <- range(dat[dat$elevcl == "L" & dat$stat == "ALIVE", "slope"])
asp_range <- range(dat[dat$elevcl == "L" & dat$stat == "ALIVE", "asp"])

## Generate some test neighbor data
make_nbrs <- function(targ, samp=samp, num_nebs=num_nebs, radius=radius) {
    nbrs <- data.frame(x = sample(-floor(radius):floor(radius), num_nebs, replace = T),
                       y = sample(-floor(radius):floor(radius), num_nebs, replace = T))
    nbrs$slope <- runif(1, min=slope_range[1], max=slope_range[2])
    nbrs$asp <- runif(1, min=asp_range[1], max=asp_range[2])
    nbrs$z <- zvals(nbrs[,c("x","y")], theta_S=nbrs$slope, theta_A=nbrs$asp)
    rows <- sample(1:nrow(samp), num_nebs, replace = TRUE)
    nbrs <- cbind(nbrs, samp[rows, ])
    nbrs$dist <- euc(rbind(nbrs$x, nbrs$y, z_ellipse(nbrs$ht, nbrs$z, nbrs$crdepth)),
                           c(0,0,targ$ht))
    nbrs <- nbrs[which(!(nbrs[["x"]] == 0 & nbrs[["y"]] == 0)), ]
    return ( nbrs )
}

## Test data.frame
radius = 2.5
num_nebs = 5
## targ <- samp[sample(nrow(samp), 1), ]
targ <- samp[8,]
nbrs <- make_nbrs(targ, samp, num_nebs = num_nebs, radius = radius)

