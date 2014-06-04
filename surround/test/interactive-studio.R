################################################################################
##
##                   Code for RStudio part of interactive
##
################################################################################
source("~/work/neighborhoods/surround/functions.R")
source("~/work/neighborhoods/surround/test/functions-graphics-rstudio.R")
source("~/work/neighborhoods/surround/test/functions-interactive.R")
source("~/work/ecodatascripts/vars/z-values/functions.R")
source("~/work/neighborhoods/surround/test/matrices.R")

dat <- read.csv("~/work/data/moose/moose-long.csv")

################################################################################
##
##                       Plot-level Summary Graphics
##
################################################################################
plot_level(pnum, yr, nsis, NM, dat)

## 3d showing slope/aspect
## dev.new()
slope <- NM$slope
aspect <- NM$aspect
showslope(slope = slope, aspect = aspect)

################################################################################
##
##                            Interactive wrapper
##
################################################################################
library(manipulate)
rr <- length(NM$id)
manipulate(
    showNSI(tree=tree, NM=NM, nsize=nsize, C=C, alpha = alpha, beta = beta, theta = theta,
            together = TRUE), tree = slider(1, rr, step=1)
    )

