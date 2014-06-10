################################################################################
##
##                   Code for RStudio part of interactive
##
################################################################################
source("~/work/neighborhoods/surround/functions.R")
source("~/work/neighborhoods/surround/visuals/functions-graphics.R")
source("~/work/neighborhoods/surround/visuals/functions-interactive.R")
source("~/work/ecodatascripts/vars/z-values/functions.R")
source("~/work/neighborhoods/surround/visuals/rstudio/data.R")

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
##                            Interactive wrappers
##
################################################################################
library(manipulate)
ids <- as.list(NM$id)
names(ids) <- ids
manipulate(
  target_tree(id=id, dat=dat, pnum=pnum, yr=yr, NM=NM),
  id = picker(ids)
)

rr <- length(NM$id)
manipulate(
    showNSI(tree=tree, NM=NM, nsize=nsize, C=C, alpha = alpha, beta = beta, theta = theta,
            together = TRUE), 
    tree = slider(1, rr, step=1),
    C = slider(2, 3, step = 1),
    nsize = picker("9"=9, "25"=25, "49"=49),
    alpha = slider(0, 2, step=0.5),
    beta = slider(0, 2, step=0.5),
    theta = slider(1, 2, step=0.01)
    )


  
  