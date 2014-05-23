###############################################################################
##
## Create some test data for surround index
## Input format is neighborhood matrices:
## - In each matrix, rows correspond to a target
## - Matrices:
##  * distances between target and neighbors
##  * size of neighbors
##  * species of neighbor
##  * direction_x to neighbor from target
##  * direction_y to neighbor from target
##  * number_neighbors: the number of neighbors in a targets neighborhood
##
###############################################################################
source("~/work/neighborhoods/surround/functions.R")
source("~/work/functions/functions-neighborhood.R")

## Neighborhood variables
## - nsize is 9,25, etc
## - alpha, beta are direction, slope params
## - C is size of connected components
## - dep.var is neighbor size variable
## - ind.var is comparison variable between target and neighbor
##   (if only looking a neighbors larger than target, this variable determines
##    whether a neighbor is included in the neighborhood analysis)
## - spec: species of targets we are interested in (all species are used as neighbors)
nsize <- 9
alpha <- beta <- theta <- 1
C <- 2
dep.var <- "bagrowth"
ind.var <- "ba"
spec <- "ABBA"

## Derived parameters
side_length <- sqrt(nsize) # length along one side of the neighborhood
sr <- side_length - 1

###############################################################################
##
## Real data:
##
pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")
pnum <- 9
dat <- subset(pp, pplot %in% c(pnum))

## define targets and neighbors
targets <- subset(dat, bqudx < (12-sr) & bqudx > (-1 + sr) & bqudy < (12 - sr) &
                  bqudy > (-1 + sr) & stat=="ALIVE")
neighbors <- subset(dat, bqudx < 11 & bqudx > 0 & bqudy < 11 &
                    bqudy > 0 & stat=="ALIVE")
## remove trees that dont satisfy certain conditions
grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0)
abbas <- targets[grew,]

## make neighbor matrices using square radius (i.e bqudx,bqudy)
abba_mats <- mnm(abbas, neighbors, sr, ind.var=ind.var)

## compute nsi
i <- 1
num_nebs <- abba_mats$number_neighbors[i]
nbrs <- data.frame(x=abba_mats$direction_x[i:num_nebs],
                   y=abba_mats$direction_y[i:num_nebs],
                   distance=abba_mats$distances[i:num_nebs],
                   size=abba_mats$variable[i:num_nebs])

nsi(nbrs, side_length, C, alpha, beta, theta)


###############################################################################
##
## Create test cases, neighborhoods for single trees
##
###############################################################################
##
## Test case variables
## - srange: size range for neighbors
srange <- c(0.00007854*5^2, 0.00007854*50^2) # 5 - 50 cm DBH range

## Case 1: full surround, uniform neighbor size, single neighbor/quadrat
## - 1 neighbor each quadrat
## - all neighbors same size
targ <- c(0,0)
dirx1 <- c(rep(-1, 3), rep(0, 2), rep(1, 3))
diry1 <- c(-1,0,1,-1,1,-1,0,1)
dist1 <- apply(cbind(dirx1, diry1), 1, function(x)
               euc(targ, as.numeric(x)))
size1 <- rep(0.3, 8)

nebs <- data.frame(x = dirx1, y = diry1, distance = dist1, size = size1)


## Case 2: full surround, uniform neighbor size, single neighbor/quadrat
## - 1 neighbor each quadrat
## - all neighbors same size
targ <- c(0,0)
dirx1 <- c(rep(-1, 3), rep(0, 2), rep(1, 3))
diry1 <- c(-1,0,1,-1,1,-1,0,1)
dist1 <- apply(cbind(dirx1, diry1), 1, function(x)
               euc(targ, as.numeric(x)))
size1 <- rep(0.3, 8)

nebs <- data.frame(x = dirx1, y = diry1, distance = dist1, size = size1)

plot(nebs$x, nebs$y)
