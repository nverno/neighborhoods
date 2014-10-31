################################################################################
##
##                            3-d Surround Model
##
################################################################################
source("~/work/functions/functions-coordinates.R")
source("~/work/functions/functions-geometry.R")
source("~/work/ecodatascripts/vars/z-values/functions.R")

## Load data and pull out working subset (LOW elevation complete cases)
dat <- read.csv("~/work/data/moose/moose-long.csv")
samp <- dat[dat$elevcl == "L" & dat$stat == "ALIVE", c("spec", "ba", "ht", "crarea", "crdepth")]
samp <- samp[complete.cases(samp), ]  # Use this subset to sample neighbor variables
samp$shape <- ifelse(samp$spec %in% c("ABBA", "PIRU"), "cone", "sphere")

## Extract some bounds for simulating variables
slope_range <- range(dat[dat$elevcl == "L" & dat$stat == "ALIVE", "slope"])
asp_range <- range(dat[dat$elevcl == "L" & dat$stat == "ALIVE", "asp"])

## Function to compute relative effect of neighbor on target
nbr_angle <- function(targSize, nbrData, ...) {
    dist <- euc(0, 0, nbrData[["x"]], nbrData[["y"]])
    rad <- ((nbrData[["size"]] / targSize) / dist^2) / 2
    theta <- asin(rad / dist)
}

## Case 1: Sphere
nbr <- nbrs[1, ]
sr_cone <- function(theta, deg=FALSE) {
    if (deg) theta <- theta * pi/180
    2*pi*(1 - cos(theta/2))
}

## First, find angle between tangents to curve
## For now, crown radius is the minimum of crown depth and horizontal radius
r <- min(sqrt(nbr[, "crarea"]/pi), nbr[, "crdepth"]/2)
d <- euc(c(0,0,0), nbr[, "x"], nbr[, "y"])
theta1 <- asin(r/d)
theta <- theta1*2
solidAngle <- sr_cone(theta)

## Draw neighbor circles and tangent line
symbols(nbr$x, nbr$y, circles = r, add=TRUE, inches=FALSE)
tangs <- pol2cart(sqrt(d^2-r^2), cart2pol(0,1)[2] + c(-1,1)*theta1)
lines(x=c(0, tangs[1,1]), y=c(0, tangs[1,2]))
lines(x=c(0, tangs[2,1]), y=c(0, tangs[2,2]))
abline()
## Case 2: Cone

radius = 2.5
num_nebs = 20

make_nbrs <- function(samp=samp, num_nebs=num_nebs, radius=radius) {
    slope <- runif(1, min=slope_range[1], max=slope_range[2])
    asp <- runif(1, min=asp_range[1], max=asp_range[2])
    rows <- sample(1:nrow(samp), num_nebs, replace = TRUE)
    nbrs <- data.frame(x = sample(-floor(radius):floor(radius), num_nebs, replace = T),
                       y = sample(-floor(radius):floor(radius), num_nebs, replace = T),
                       z = sample(-floor(radius):floor(radius), num_nebs, replace = T))
    nbrs <- cbind(nbrs, samp[rows, ])
    nbrs <- nbrs[which(!(nbrs[["x"]] == 0 & nbrs[["y"]] == 0)), ]
    return ( nbrs )
}


ex_neighborhood_3d <- function(samp=samp, targSize=NULL, radius=1.5, numQuads=8, nbrs=NULL,
                              num_nebs=NULL, rand=TRUE, maxSize=2,
                              minSize=0.001, numSpecs=4, addLegend=FALSE,
                              angleFunc=nbr_angle,...){
    xyvals <- expand.grid(x=-radius:radius,
                          y=-radius:radius)
    plot(xyvals, type="n", main="Example Neighborhood")
    abline(h=-radius:radius, v=-radius:radius, lty=2)
    abline(h=c(-radius,radius),v=c(-radius,radius),lwd=2)
    points(0,0, col = "blue", pch=15)


    ## Target info
    if (is.null(targ))
        targ <- samp[sample(nrow(samp), 1), ]

    ## Add neighbors if there are any
    if (is.null(nbrs) && rand) {
        if (is.null(num_nebs))
            num_nebs <- sample(1:12, 1)
        nbrs <- make_nbrs(samp, num_nebs, radius)
        if (nrow(nbrs) < 1) {
            nbrs <- make_nbrs(samp=samp, num_nebs=num_nebs, radius=radius)
        }
        print(sprintf("Random neighborhood with %s quadrats occupied:",
                      nrow(unique(nbrs))))
        print(unique(nbrs))
    }
    if (!is.null(nbrs)) {
        points(nbrs$x, nbrs$y, col=as.integer(nbrs$spec), pch = 17, cex = 2)
        if (addLegend)
            legend("topright", legend = c(sort(unique(nbrs$spec))), pch = 17,
                   col = sort(unique(nbrs$spec)))
    }

    ## Compute neighbor angle
    nbrs$angle <- do.call(angleFunc, list(targSize=targSize, nbrData=nbrs))

    ## Convert to polar coords
    pcoords <- data.frame(cart2pol(nbrs$x, nbrs$y))
    ## pcoords <- pcoords[order(pcoords[,"theta"]),] # order by polar degree

    ## Enumerate neighbors by quadrat numbering (starting from positive x-axis)
    pcoords$quad <- as.integer(cut(pcoords[,"theta"],
                                   breaks = seq(0, 2*pi, length.out = numQuads+1),
                                   right = FALSE))

    ## TODO add in 3d stuff here

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

