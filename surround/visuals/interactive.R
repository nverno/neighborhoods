################################################################################
##
##                Interactively examine neigborhood surrounds
##
################################################################################
source("~/work/functions/functions-neighborhood.R")
source("~/work/neighborhoods/surround/functions.R")
source("~/work/neighborhoods/surround/visuals/functions-graphics.R")
source("~/work/neighborhoods/surround/visuals/functions-interactive.R")
source("~/work/ecodatascripts/vars/z-values/functions.R")

## Choose plot number and year to look at
pnum <- 7
yr <- 98
dat <- read.csv("~/work/data/moose/moose-long.csv")

## Neighborhood parameters
nsize <- 9
C <- 2
alpha <- beta <- 1
theta <- .05

## Create neighborhoods
NM <- setup(pnum=pnum, yr=yr, data=dat)

## Get all the NSIs
nsis <- rep(NA, nrow(NM$distances))
for (i in 1:length(nsis)) {
    num_nebs <- NM$number_neighbors[i]
    nbrs <- data.frame(x=NM$direction_x[i, 1:num_nebs],
                       y=NM$direction_y[i, 1:num_nebs],
                       distance=NM$distances[i, 1:num_nebs],
                       size=NM$variable[i, 1:num_nebs])

    nsind <- nsi(nbrs=nbrs, C=C, alpha=alpha, beta=beta, theta=theta, nsize=9)
    nsis[i] <- nsind
}

## Dump objects to send to RStudi-udi-0h
dump(list=c("pnum","yr","nsize","C","alpha","beta","theta","NM","nsis"),
     file="~/work/neighborhoods/surround/test/matrices.R")

################################################################################
##
##                       Plot-level Summary Graphics
##
################################################################################
plot_level(pnum, yr, nsis, NM, dat)

## 3d showing slope/aspect
dev.new()
slope <- NM$slope
aspect <- NM$aspect
showslope(slope = slope, aspect = aspect)

################################################################################
##
##                            Interactive wrapper
##
################################################################################
tree <- 1
library(manipulate)
rr <- length(NM$id)
manipulate(
    showNSI(tree=tree, NM=NM, nsize=nsize, C=C, alpha = alpha, beta = beta, theta = theta,
            together = TRUE), tree = slider(1, rr, step=1)
    )

## showNSI(tree=2, NM=NM, nsize=9, C=2, alpha=1, beta=1, theta=1, together=TRUE)
