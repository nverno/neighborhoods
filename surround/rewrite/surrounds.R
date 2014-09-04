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


## Single target data
mats <- which(unlist(lapply(nMat, function(x) !is.null(dim(x)))))
single <- lapply(nMat[mats], function(x) {
    x[3,]
})

## Visualize
xyvals <- expand.grid(x=-2:2,y=-2:2)
plot(xyvals$x, xyvals$y, type = "n")
abline(h = -1.5:1.5, v = -1.5:1.5, lty = 2)
points(0,0, col = "blue", pch = 15)
points(single[["x"]], single[["y"]], col = "red", pch = 16)

