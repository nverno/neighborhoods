################################################################################
##
##                    Functions for Interactive NSI setup
##
################################################################################

################################################################################
##
##                               Set-up
##
################################################################################
setup <- function(pnum, yr, spec="ABBA", sr=2, dep.var="bagrowth", ind.var="ba",
  data=NULL) {
  dat <- data.frame()
  ifelse (missing(data),
    { dat <<- read.csv("data/moose/moose-long.csv") },
    { dat <- data })
  dat <- subset(dat, pplot %in% c(pnum))

  ## Keep just trees for specific year
  dat <- dat[dat$time == yr, ]

  ## define targets and neighbors
  targets <- subset(dat, bqudx < (12-sr) & bqudx > (-1 + sr) & bqudy < (12 - sr) &
    bqudy > (-1 + sr) & stat=="ALIVE")
  neighbors <- subset(dat, bqudx < 11 & bqudx > 0 & bqudy < 11 &
    bqudy > 0 & stat=="ALIVE")
  ## remove trees that dont satisfy certain conditions
  grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0)
  targs <- targets[grew,]

  ## make neighbor matrices using square radius (i.e bqudx,bqudy)
  neb_mats <- mnm(targs, neighbors, sr, ind.var=ind.var)

  return ( neb_mats )
}

################################################################################
##
##                       Plot-level Summary Graphics
##
################################################################################
plot_level <- function(pnum, yr, nsis, NM, dat) {
  ## NSI distribution
  ## dev.new()
  ttl <- paste("NSI Distribution for plot", pnum,", year", yr)
  hist(nsis, main=ttl)

  ## 3d showing slope/aspect
  ## dev.new()
  ## slope <- NM$slope
  ## aspect <- NM$aspect
  ## showslope(slope = slope, aspect = aspect)

  ## Scatter points scaled by NSI
  ## dev.new()
  samp <- dat[dat$time==yr & dat$pplot == pnum,]
  samp <- samp[samp$id %in% NM$id,]
  samp <- samp[order(NM$id),]
  ttl <- paste("Trees scaled by NSI, plot",pnum,", year",yr)
  plot(samp$x, samp$y, type = "n", main=ttl, xlim=c(min(samp$x,na.rm=T)-1, max(samp$x,na.rm=T)+1),
    ylim=c(min(samp$y,na.rm=T)-1, max(samp$y,na.rm=T)+1))
  symbols(x=samp$x, y=samp$y, circles=nsis, inches=1/3, ann=F, bg="steelblue2", fg=NULL,
    add = TRUE)
  text(jitter(samp$x), jitter(samp$y), labels=samp$id)
  abline(h=0,v=0)
}


## Show target and species of neighbors
## Target is outlined by a circle
target_tree <- function(id, dat, pnum, yr, NM) {
  require(ggplot2)
  ## Scatter points colored by neighbor, scaled by size
  samp <- dat[dat$time==yr & dat$pplot == pnum & !is.na(dat$y)
    & !is.na(dat$x),]
  samp$targ <- factor(rep(1, nrow(samp)), levels = c(1,4))
  samp[samp$id == id, "targ"] <- 4
  xc <- samp[samp$id == id, "x"]
  yc <- samp[samp$id == id, "y"]
  r <- 0.5

  ttl <- paste("Tree", id, ", plot", pnum,", year",yr)
  ggplot(samp, aes(x, y, color=spec, shape = targ, size = ba)) + geom_jitter() +
    annotate("path", x = xc + r*cos(seq(0, 2*pi, length.out=100)),
      y= yc + r*sin(seq(0, 2*pi, length.out=100)), color = "blue") +
    ggtitle(ttl) +
    geom_hline(aes(0,0)) + geom_vline(aes(0,0))

}
