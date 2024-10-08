### create_test.R --- 

source("functions/functions-coordinates.R")
source("functions/functions-geometry.R")
source("ecodatascripts/vars/z-values/functions.R")
source("neighborhoods/surround/rewrite/spheres.R")

## Load data and pull out working subset (LOW elevation complete cases)
dat <- read.csv("data/moose/moose-long.csv")
samp <- dat[dat$stat == "ALIVE" & dat$time == 86, 
            c("x", "y", "z", "pplot", "spec", "dbh", "ba", "ht", "crarea", "crdepth")]

samp <- samp[complete.cases(samp), ]  # Use this subset to sample neighbor variables
samp <- samp[samp$ht > 0 & samp$ht > samp$crdepth,]
samp$shape <- ifelse(samp$spec %in% c("ABBA", "PIRU"), "cone", "sphere")

## Extract some bounds for simulating variables
slope_range <- range(dat[dat$elevcl == "L" & dat$stat == "ALIVE", "slope"])
asp_range <- range(dat[dat$elevcl == "L" & dat$stat == "ALIVE", "asp"])

## Generate some test neighbor data
make_nbrs <- function(targ, samp=samp, num_nebs=num_nebs, radius=radius) {
    nbrs <- data.frame(x = sample(-floor(radius):floor(radius), num_nebs, replace = T),
                       y = sample(-floor(radius):floor(radius), num_nebs, replace = T))
    slope <- runif(1, min=slope_range[1], max=slope_range[2])
    asp <- runif(1, min=asp_range[1], max=asp_range[2])
    nbrs$slope <- slope
    nbrs$asp <- asp
    ps <- matrix(c(nbrs$x, nbrs$y, rep(0, num_nebs), rep(1, num_nebs)), ncol = 4)
    nbrs$z <- zvals(ps, theta_a = asp, theta_s = slope)
    rows <- sample(1:nrow(samp), num_nebs, replace = TRUE)
    nbrs <- cbind(nbrs, samp[rows, ])
    nbrs$dist <- euc(rbind(nbrs$x, nbrs$y, z_ellipse(nbrs$ht, nbrs$z, nbrs$crdepth)),
                           c(0,0,targ$ht))
    nbrs <- nbrs[which(!(nbrs[["x"]] == 0 & nbrs[["y"]] == 0)), ]
    return ( nbrs )
}

## Test data.frame
radius = 5
num_nebs = 15
## targ <- samp[sample(nrow(samp), 1), ]
targ <- samp[sample(1:nrow(samp),1),]
nbrs <- make_nbrs(targ, samp, num_nebs = num_nebs, radius = radius)

tst <- samp[samp$pplot == 14, ]
