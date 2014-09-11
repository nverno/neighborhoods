source("~/work/functions/functions-coordinates.R")

## Example surround where trees create there own shadow relative to their
##  size and distance from target tree
nbr_angle <- function(targSize, nbrData) {

}
ex_neighborhood_3 <- function(targSize=NULL, radius=1.5, numQuads=8, nbrs=NULL,
                              numNebs=NULL, rand=TRUE, maxSize=2,
                              minSize=0.001, numSpecs=4, addLegend=FALSE,
                              angleFunc=nbr_angle,...){
    xyvals <- expand.grid(x=-radius:radius,
                          y=-radius:radius)
    plot(xyvals, type="n", main="Example Neighborhood")
    abline(h=-radius:radius, v=-radius:radius, lty=2)
    abline(h=c(-radius,radius),v=c(-radius,radius),lwd=2)
    points(0,0, col = "blue", pch=15)

    make_nbrs <- function(numNebs=numNebs, radius=radius) {
        set.seed(as.numeric(Sys.time()))
        nbrs <- data.frame(x = sample(-floor(radius):floor(radius), numNebs, replace = T),
                           y = sample(-floor(radius):floor(radius), numNebs, replace = T),
                           size = sample(seq(minSize,maxSize, length.out = numNebs*2), replace = T),
                           spec = sample(1:numSpecs, replace = T))
        nbrs <- nbrs[which(!(nbrs[["x"]] == 0 & nbrs[["y"]] == 0)), ]
        return ( nbrs )
    }

    ## Target info
    if (is.null(targSize))
        targSize <- sample(seq(minSize, maxSize, length.out = numNebs*2), 1)

    ## Add neighbors if there are any
    if (is.null(nbrs) && rand) {
        if (is.null(numNebs))
            numNebs <- sample(1:12,1)
        nbrs <- make_nbrs(numNebs,radius)
        if (nrow(nbrs) < 1) {
            nbrs <- make_nbrs(numNebs=numNebs, radius=radius)
        }
        print(sprintf("Random neighborhood with %s quadrats occupied:",
                      nrow(unique(nbrs))))
        print(unique(nbrs))
    }
    if (!is.null(nbrs)) {
        points(nbrs, col=as.integer(nbrs$spec), pch = 17, cex = 2)
        if (addLegend)
            legend("topright", legend = c(sort(unique(nbrs$spec))), pch = 17,
                   col = sort(unique(nbrs$spec)))
    }

    ## Convert to polar coords
    pcoords <- data.frame(cart2pol(nbrs$x, nbrs$y))
    ## pcoords <- pcoords[order(pcoords[,"theta"]),] # order by polar degree

    ## Enumerate neighbors by quadrat numbering (starting from positive x-axis)
    pcoords$quad <- as.integer(cut(pcoords[,"theta"],
                                   breaks = seq(0, 2*pi, length.out = numQuads+1),
                                   right = FALSE))

    ## Find consecutive filled quadrats
    rad <- 2*pi / numQuads
    filled <- unique(pcoords$quad)
    if (length(filled) == numQuads) {
        pcoords$rad <- 2*pi             # full surround
    } else {
        quads <- rep(0, numQuads)
        quads[filled] <- 1
        strt <- which(quads == 0)[[1]] # start from first empty sector
        if (strt > 1)                  # reorder from new start
            quads <- c(quads[strt:length(quads)],
                       quads[1:(strt-1)])
        runs <- rle(quads)
        rads <- rep(0, length(runs$lengths))
        rads[runs$values==1] <- runs$lengths[runs$values==1] * rad
        sectorRads <- rep(rads, runs$lengths)
        if (strt > 1)                  # reorder to original start
            sectorRads <- c(sectorRads[length(sectorRads) - (strt - 2):0],
                            sectorRads[1:(length(sectorRads)-strt+1)])
        pcoords$rad <- sectorRads[pcoords$quad]
    }

    ## Draw quadrant lines
    ps <- data.frame(pol2cart(r = radius * 2,
                              theta = seq(0, 2*pi, by = rad)))
    for (i in 1:nrow(ps))
        lines(x = c(0, ps[i, "x"]),
              y = c(0, ps[i, "y"]),
              col = "blue", lty = 2)

    ## Fill in occupied quadrats
    for (i in 1:numQuads) {
        if (i %in% pcoords$quad) { # quadrant is occupied, fill it
            angle1 <- (i - 1) * rad
            angles <- seq(angle1, angle1+rad, length.out = 100)
            ps <- data.frame(pol2cart(r=radius, theta=angles))
            polygon(x = c(0, ps[["x"]], 0),
                    y = c(0, ps[["y"]], 0),
                    col = "steelblue", density = 10,
                    lty = 2, lwd = 2, angle = atan((2*angle1+rad)/2) * 180/pi + 90)
        }
    }

    ## Write in component radians
    print(pcoords)
    xy <- data.frame(pol2cart(pcoords$r, pcoords$theta))
    xy$rad <- round(pcoords$rad, 4)
    xy <- unique(xy)
    text(xy, labels = xy$rad)

    return ( list(nbrs=nbrs, pcoords=pcoords) )
}

