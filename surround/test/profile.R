################################################################################
##
##                            Timing for nsi_agg
##
################################################################################

source("~/work/neighborhoods/surround/functions.R")
source("~/work/functions/functions-neighborhood.R")

################################################################################
##
##                                  Params
##
################################################################################
## Parameters
nsize <- 9
alpha <- beta <- 1
theta <- 1.05
C <- 2
dep.var <- "bagrowth"
ind.var <- "ba"
spec <- "ABBA"

## Derived parameters
side_length <- sqrt(nsize) # length along one side of the neighborhood
sr <- side_length - 1

nPars <- list(C=2, nsize=9, alpha=1, beta=1, theta=1.05)

################################################################################
##
##                                   Data
##
################################################################################
dat <- read.csv("~/work/data/moose/moose-long.csv")

## define targets and neighbors
## Use ABBAs that have a positive basal area growth as targets
## Neighbors are all species but must have status of ALIVE and be within normal plot
##  coordinates
targs <- subset(dat, spec == "ABBA" & !is.na(dat[,dep.var]) & dat[,dep.var] > 0)
## targets <- subset(dat, bqudx < (12-sr) & bqudx > (-1 + sr) & bqudy < (12 - sr) &
##                   bqudy > (-1 + sr) & stat=="ALIVE")

neighbors <- subset(dat, bqudx < 11 & bqudx > 0 & bqudy < 11 &
                    bqudy > 0 & stat=="ALIVE")

################################################################################
##
##                                  Profile
##
################################################################################
## Neighbor matrices
nm <- mnm_agg(targs, neighbors, sr)

Rprof("~/work/neighborhoods/surround/test/Rprof.out")
nsis <- nsi_agg(nm, nPars)
Rprof(NULL)
summaryRprof("~/work/neighborhoods/surround/test/Rprof.out")
