source("~/work/functions/functions-coordinates.R")

## Example neighborhood
ex_neighborhood <- function(radius=1.5, numQuads=8, nbrs=NULL,
                            numNebs=NULL, rand=TRUE, ...){
    xyvals <- expand.grid(x=-radius:radius,
                          y=-radius:radius)
    plot(xyvals, type="n", main="Example Neighborhood")
    abline(h=-radius:radius, v=-radius:radius, lty=2)
    abline(h=c(-radius,radius),v=c(-radius,radius),lwd=2)
    points(0,0, col = "blue", pch=15)

    make_nbrs <- function(numNebs) {
        nbrs <- data.frame(x = sample(-1:1, numNebs, replace = T),
                           y = sample(-1:1, numNebs, replace = T))
        nbrs <- nbrs[-which(nbrs[["x"]] == 0 & nbrs[["y"]] == 0), ]
        if (nrow(nbrs) < 1) nbrs <- make_nbrs(numNebs)
        return ( nbrs )
    }

    ## Add neighbors if there are any
    if (is.null(nbrs) && rand) {
        if (is.null(numNebs))
            numNebs <- sample(1:12,1)
        nbrs <- make_nbrs(numNebs)
        print(sprintf("Random neighborhood with %s quadrats occupied:",
                      nrow(unique(nbrs))))
        print(unique(nbrs))
    }
    if (!is.null(nbrs))
        points(nbrs, col="red", pch = 17, cex = 2)

    ## Convert to polar coords
    pcoords <- data.frame(cart2pol(nbrs$x, nbrs$y))
    pcoords <- pcoords[order(pcoords[,"theta"]),] # order by polar degree

    ## Enumerate neighbors by quadrat numbering (starting from positive x-axis)
    pcoords$quad <- as.integer(cut(pcoords[,"theta"],
                                   breaks = seq(0, 2*pi, length.out = numQuads+1),
                                   right = FALSE))

    ## Draw quadrant lines
    rad <- 2*pi / numQuads
    ps <- data.frame(pol2cart(r = radius * 2,
                              theta = seq(0, 2*pi, by = rad)))
    for (i in 1:nrow(ps))
        lines(x = c(0, ps[i, "x"]),
              y = c(0, ps[i, "y"]),
              col = "blue", lty = 2)

    ## Fill in occupied quadrats

    ## **** WORKING ON THE POLYGONS... ****
    for (i in 1:numQuads) {
        if (i %in% pcoords$quad) { # quadrant is occupied, fill it
            angle1 <- (i - 1) * rad
            angles <- seq(angle1, angle1+rad, length.out = 100)
            ps <- data.frame(pol2cart(r=1.5, theta=angles))
            polygon(x = c(0, ps[["x"]], 0),
                    y = c(0, ps[["y"]], 0),
                    col = "steelblue", density = 10,
                    lty = 2, lwd = 2, angle = atan((2*angle1+rad)/2) * 180/pi + 90)
        }

    }
}
