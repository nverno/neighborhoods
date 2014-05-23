# For each combination of environmental factors, the significance of the second
#  order parameter will be tested for each DBH-based neighborhood class:
#  'nmaxdbhcl', 'nmaxbacl', 'nsumbacl', 'nsumba5cl', 'nsumbabigcl', + same ones
#  ending in 'q'
#
# For each neighborhood size class fit, two column will be added to output dataset:
#   significance of 2nd order term, and its sign
#
# Looking for instances of sign/significance change due to neighborhoods...
#
source("significant-2nd-order.R")

# choose neighbor classes
class.cols <- intersect(grep("^n",names(dat)), grep("cl$|clq$",names(dat)))
n.vars <- names(dat)[class.cols]

# first part grep names with 'cl'
var.cols <- grep("cl", names(pvals))
var.key <- pvals[,var.cols]

# make list of subsetting environmental factors
factors <- unlist(apply(var.key, 1, function(x) {
    list(x) }), recursive = FALSE)

# rename neighbor classes uniformly (i.e 1:8)
nlevels <- length(levels(dat$nmaxdbhcl))
sapply(names(dat)[class.cols], function(x) {
    levels(dat[,x]) <<- 1:nlevels
})

# outputs a dataframe with pvals and signs for 2nd degree polynomial fits
#   to all possible combinations of environmental factor variables split
#   separately by neighborhood classes listed in 'n.vars'
neb.fits <- data.frame()
for(i in 1:length(factors)) {
    new <- make.subset(factors[[i]], dat)
    n.fits <- data.frame()
    for(j in 1:length(n.vars)) {
        n <- split.pvals(new, n.vars[j])
        n <- cbind(n, neighbor=rep(n.vars[j],nrow(n)))
        if(ncol(n.fits)>1){
            n.fits <- rbind.fill(n.fits,n)
        } else { n.fits <- n }
    }
    fact.cols <- make.cols(factors[[i]],nrow(n.fits))
    n.fits <- cbind(fact.cols,n.fits)
    if(ncol(neb.fits)>0) {
        neb.fits <- rbind.fill(neb.fits,n.fits)
    } else { neb.fits <- n.fits }
}

# alternative using new allsubs function
dat <- read.csv("long-complete-cases-sr2.csv")
abba <- subset(dat, spec=="ABBA" & stat== "ALIVE")
dat <- abba

# function to apply to all subsets
func <- function(x) {
    if(nrow(x) > 3) {
        c(summary(lm(x$ht~x$dbh,2))$coefficients[3,][4],
          sign(summary(lm(x$ht~x$dbh,2))$coefficients[3,][4]))
    }
}

results <- data.frame()
for(i in 1:length(vars)) {
    var1 <- c(env.var,vars[i])
    new <- allsubs(dat, var1, func = func)
    results <- c(results, new)
}




# helper function with dlallsubs()
#  -returns full poly fits
polys <- function(x) { if(nrow(x) > 3) {
    lm(x$ht ~ poly(x$dbh, 2)) }
else NA }

test <- ldply(results)
m <- modelstats(results)
# compare the two, neb.fits and test
c(env.var, vars[1])
new <- dlallsubs(dat, c(env.var, vars[1]), polys, out.name = polys)
m <- modelstats(new)
test <- ldply(new)

nrow(test)
nrow(neb.fits)

neb1 <- data.frame()
for(i in 1:length(n.vars)) {




a <- subset(dat, elevcl=="L" & spec=="ABBA" & aspcl=="E")
test <- dlallsubs(a, vars="nmaxbacl", polys)
modelstats(test[!is.na(test)])

#write.csv(neb.fits, "neighbor-fits.csv")



