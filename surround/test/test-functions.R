################################################################################
##
##                            Test NSI functions
##
################################################################################

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

################################################################################
##
##                              connected_comps
##
################################################################################
func <- "connected_comps"
vars <- c("C","nsize")
print(sprintf("Testing: %s(%s)", func, paste(vars, collapse = ",")))

## Case 1:
## - nsize=9, C=2
nsize <- 9
C <- 2
ii <- c(1,8) # test these starting indices
print(sprintf("inds = %s; nsize = %d; C = %d", paste(ii, collapse=","), nsize, C))
ans1 <- cbind(c(-1,0), c(1,1))
res1 <- all(ans1 == do.call(func, list(C=C, nsize=nsize))[[ii[1]]])
ans2 <- cbind(c(-1,-1), c(0,1))
res2 <- all(ans2 == do.call(func, list(C=C, nsize=nsize))[[ii[2]]])

fails <- ii[which(c(res1, res2) != TRUE)]
if (length(fails) == 0) {
    print("... Passed")
} else {
    print(paste("Failed: i = ", paste(fails, collapse = ", ")))
}

## Case 2:
## - nsize=9, C=3
nsize <- 9
C <- 3
ii <- c(1,8) # test these starting indices
print(sprintf("inds = %s; nsize = %d; C = %d", paste(ii, collapse=","), nsize, C))
ans1 <- cbind(c(-1,0,1), c(1,1,1))
res1 <- all(ans1 == do.call(func, list(C=C, nsize=nsize))[[ii[1]]])
ans2 <- cbind(c(-1,-1,0), c(0,1,1))
res2 <- all(ans2 == do.call(func, list(C=C, nsize=nsize))[[ii[2]]])

fails <- ii[which(c(res1, res2) != TRUE)]
if (length(fails) == 0) {
    print("... Passed")
} else {
    print(paste("Failed: i = ", paste(fails, collapse = ", ")))
}
