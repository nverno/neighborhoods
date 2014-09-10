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

    make_nbrs <- function(numNebs=numNebs, radius=radius) {
        set.seed(as.numeric(Sys.time()))
        nbrs <- data.frame(x = sample(-floor(radius):floor(radius), numNebs, replace = T),
                           y = sample(-floor(radius):floor(radius), numNebs, replace = T))
        nbrs <- nbrs[which(!(nbrs[["x"]] == 0 & nbrs[["y"]] == 0)), ]
        return ( nbrs )
    }

    ## Add neighbors if there are any
    if (is.null(nbrs) && rand) {
        if (is.null(numNebs))
            numNebs <- sample(1:12,1)
        nbrs <- make_nbrs(numNebs,radius)
        if (nrow(nbrs) < 1) {
            nbrs <- make_nbrs(numNebs)
        }
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

    ## Calculate components
    rad <- 2*pi / numQuads
    filled <- unique(pcoords$quad)
    if (length(filled) == numQuads) {
        pcoords$rad <- 2*pi # full surround
    } else if (length(filled) == 1) {
        pcoords$rad <- rad # single neighbor
    } else {
        strt <- which(is.na(match(1:numQuads, filled)))[[1]] # start from 1st open sector
        ord <- filled
        ord[ord < strt] <- ord[ord < strt] + numQuads # reorder from new start sector
        ord <- sort(ord)
        runs <- rle(diff(ord)) # find runs of filled sectors
        if (!any(runs$values==1)) {
            pcoords$rad <- rad # no connected comps
        } else {
            rads <- rep(rad, length(runs$values)) # initial radians of each sector
            rads[which(runs$values==1)] <- runs$lengths[runs$values==1] * rad + rad # radians of continuous sectors
            runs$lengths[runs$values==1] <- runs$lengths[runs$values==1]+1 # increm. connected comps
            decrem <- which(runs$values == 1) - 1 # fix diff offset of 1
            decrem <- decrem[decrem > 1]
            if (length(decrem) > 0)
                runs$lengths[decrem] <- runs$lengths[decrem] - 1
            out <- rep(rads, runs$lengths)
            outInds <- (ord - 1) %% numQuads + 1
            pcoords$rad <- out[match(pcoords$quad, outInds)]
        }
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

aa <- 1:8
tst <- c(1,2,3,5,6)
