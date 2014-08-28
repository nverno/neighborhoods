## Load neighborhood functions, parameters, data,
## and create neighbor matrices
source("~/work/functions/neighborhoods/rewrite/final/run-mnm.R")

## Make neighbor matrice coordinates relative to target coordinates
coords <- c("x", "y", "z")
for (coord in coords) {
    nMat[[coord]] <- nMat[["targets"]][[coord]] - nMat[[coord]]
}

## Add aspect and slope to neighborhoods
require(plyr)
pInfo <- ddply(dat, .(pplot), function(x) {
    data.frame(aspect = unique(x[["asp"]]),
               slope = unique(x[["slope"]]))
})


