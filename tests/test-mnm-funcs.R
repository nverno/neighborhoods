################################################################################
##
##               Test functions that create neighbor matrices
##
################################################################################
## Compile and load base mnm function and helpers
source("~/work/functions/functions-neighborhood.R")

## Test data: Moosilauke data in long format
dat <- read.csv("~/work/data/moose/moose-long.csv")

## Assign targets and neighbors
## Targets: ABBAs with positive basal area growth
## Neighbors: All species with status "ALIVE"
targets <- dat[dat$spec == "ABBA" &
               !is.na(dat$bagrowth) &
               dat$bagrowth > 0, ]
neighbors <- dat

################################################################################
##
##                               maxnebs_maxn
##
################################################################################
## Test different values of sr across all plots and time frames
print("Testing: maxnebs_maxn() with different 'sr' values on all plots and time periods")

sr <- 1
nebs1 <- maxnebs_disc(targets = targets$id, neighbors = neighbors$id,
                     plots = neighbors$pplot, stat = neighbors$stat,
                     bqudx = neighbors$bqudx, bqudy = neighbors$bqudy,
                     time = neighbors$time, sr = sr)
print(sprintf("sr = %s, maxn = %s", sr, nebs1))

sr <- 2
nebs2 <- maxnebs_disc(targets = targets$id, neighbors = neighbors$id,
                     plots = neighbors$pplot, stat = neighbors$stat,
                     bqudx = neighbors$bqudx, bqudy = neighbors$bqudy,
                     time = neighbors$time, sr = sr)
print(sprintf("sr = %s, maxn = %s", sr, nebs2))

sr <- 4
nebs4 <- maxnebs_disc(targets = targets$id, neighbors = neighbors$id,
                     plots = neighbors$pplot, stat = neighbors$stat,
                     bqudx = neighbors$bqudx, bqudy = neighbors$bqudy,
                      time = neighbors$time, sr = sr)
print(sprintf("sr = %s, maxn = %s", sr, nebs4))

tst <- sapply(4:27, FUN = function(pp) {
    plt <- dat[dat$pplot == pp, ]
    targs <- plt[plt$spec == "ABBA" &
                 !is.na(plt$bagrowth) &
                 plt$bagrowth > 0, ]
    maxn <- maxnebs_disc(targets = targs$id, neighbors = plt$id,
                         plots = plt$pplot, stat = plt$stat,
                         bqudx = plt$bqudx, bqudy = plt$bqudy,
                         time = plt$time, sr = sr)
   maxn
})

if (nebs4 %in% tst) {
    print(sprintf("Test passed, %s is the maximum in plot %s with radius %s",
                  nebs4, which(tst == nebs4)[1] + 3, sr))
} else {
    print(sprintf("Failed: %s is not a maximum in any of the plots with radius of %s",
                  nebs4, sr))
}

################################################################################
##
##                                    mnm
##
################################################################################
NM <- mnm(targets, neighbors, sr = 4)

################################################################################
##
##                            connected_comp_inds
##
################################################################################
## Test edge cases for proper wraparound of indices
## Case 1:
## - nsize=9, C=2
nsize <- 9
C <- 2
ii <- c(1,8,7) # test these starting indices
print("Testing: connected_comp_inds(i, C, nsize)")
print(sprintf("i = %s; nsize = %d; C = %d", paste(ii, collapse=","), nsize, C))
ans1 <- c(1,2)
res1 <- all(ans1 == connected_comps_inds(i=ii[1], C=C, nsize=nsize))
ans2 <- c(8,1)
res2 <- all(ans2 == connected_comps_inds(i=ii[2], C=C, nsize=nsize))
ans3 <- c(7,8)
res3 <- all(ans3 == connected_comps_inds(i=ii[3], C=C, nsize=nsize))
fails <- ii[which(c(res1, res2, res3) != TRUE)]
if (length(fails) == 0) {
    print("... Passed")
} else {
    print(paste("Failed: i = ", paste(fails, collapse = ", ")))
}

## Case 2:
## - nsize=9, C=3
nsize <- 9
C <- 3
ii <- c(1,8,7) # test these starting indices
print(sprintf("i = %s; nsize = %d; C = %d", paste(ii, collapse=","), nsize, C))
ans1 <- c(1,2,3)
res1 <- all(ans1 == connected_comps_inds(i=ii[1], C=C, nsize=nsize))
ans2 <- c(8,1,2)
res2 <- all(ans2 == connected_comps_inds(i=ii[2], C=C, nsize=nsize))
ans3 <- c(7,8,1)
res3 <- all(ans3 == connected_comps_inds(i=ii[3], C=C, nsize=nsize))
fails <- ii[which(c(res1, res2, res3) != TRUE)]
if (length(fails) == 0) {
    print("... Passed")
} else {
    print(paste("Failed: i = ", paste(fails, collapse = ", ")))
}
