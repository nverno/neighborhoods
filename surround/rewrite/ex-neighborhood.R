## Example neighborhood
ex_neighborhood <- function(radius=1.5, numQuads=NULL, nbrs=NULL,
                            numNebs=NULL, rand=TRUE, ...){
    xyvals <- expand.grid(x=-radius:radius,
                          y=-radius:radius)
    plot(xyvals, type="n", main="Example Neighborhood")
    abline(h=-radius:radius, v=-radius:radius, lty=2)
    abline(h=c(-radius,radius),v=c(-radius,radius),lwd=2)
    points(0,0, col = "blue", pch=15)

    ## Add neighbors if there are any
    if (is.null(nbrs) && rand) {
        if (is.null(numNebs))
            numNebs <- sample(1:12,1)
        nbrs <- data.frame(x = sample(-1:1, numNebs, replace = T),
                           y = sample(-1:1, numNebs, replace = T))
        nbrs <- nbrs[-which(nbrs[["x"]] == 0 & nbrs[["y"]] == 0), ]
        print(sprintf("Random neighborhood with %s quadrats occupied:",
                      nrow(unique(nbrs))))
        print(unique(nbrs))

    }
    if (!is.null(nbrs))
        points(nbrs, col="red", pch = 16)

    ## Convert to polar coords
    pcoords <- t(apply(nbrs, 1, function(x)
                       cart2pol(x = x[["x"]], y = x[["y"]])))
    pcoords <- pcoords[order(pcoords[,2]),] # order by polar degree

    ## Define quadrants and enumerate
    if (is.null(numQuads)) numQuads <- 8
    occ <- cut(pcoords[,2], breaks = seq(0, 2*pi, length.out = numQuads+1)) # enumerate

    ## Draw quadrant lines
    rad <- 2*pi / numQuads
    for (i in 1:(numQuads/2)) {
        pp <- rotate_point(1, 0, theta_r = rad * i)
        abline(0, pp[2]/pp[1], lwd = 3, col = "blue", lty = 2)
    }

    ## Fill in occupied quadrats
    occ <- as.integer(occ)

    ## **** WORKING ON THE POLYGONS... ****
    for (i in 1:numQuads) {
        if (i %in% occ) {
            pp1 <- rotate_point(radius, 0, theta_r = rad * (i - 1))
            slp1 <- pp1[2]/pp1[1]
            pp2 <- rotate_point(radius, 0, theta_r = rad * i)
            slp2 <- pp2[2]/pp2[1]
            filler <- seq(pp1[1], pp2[1], length.out = 100)
            xs <- c(0, filler, 0)
            ys <- c(0, sqrt(radius^2 - filler^2), 0)
            polygon(x = xs, y = ys, col = "steelblue", density = 10,
                    lty = 2, lwd = 2, angle = atan((slp1+slp2)/2) * 180/pi + 90)
        }
    }


}
