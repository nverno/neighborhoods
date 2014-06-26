################################################################################
##
##                      Test mnm aggregation functions
##
################################################################################
source("~/work/neighborhoods/surround/functions.R")
source("~/work/functions/functions-neighborhood.R")
dat <- read.csv("~/work/data/moose/moose-long.csv")

## Specific plot to test
plot <- 4

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

## define targets and neighbors
targets <- subset(dat, bqudx < (12-sr) & bqudx > (-1 + sr) & bqudy < (12 - sr) &
                  bqudy > (-1 + sr) & stat=="ALIVE")
neighbors <- subset(dat, bqudx < 11 & bqudx > 0 & bqudy < 11 &
                    bqudy > 0 & stat=="ALIVE")

## remove trees that dont satisfy certain conditions
grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0)
abbas <- targets[grew,]

################################################################################
##
##                                 mnm_plot
##
################################################################################
## Replaces default 0 values with NAs in neighbor matrices and forms a list
##  of neighbor matrices for all plots
cat("Testing: mnm_plot()\n")

## - Check that neighbor matrices formed by mnm_plot() same as mnm()
targs <- abbas[abbas$pplot == 4, ]
nebs <- neighbors[neighbors$pplot == 4, ]
mnm1 <- mnm(targs, nebs, sr, ind.var)              # mnm() output
mnmlist <- mnm_plot(abbas, neighbors, sr, ind.var) # mnm_plot() list of plot mnms
mnm2 <- mnmlist[[toString(plot)]]                  # mnm_plot() output corresponding to "plot"

## Dimension check
cat("\tChecking dimensions... \t\t\t\t")
numN1 <- max(mnm1$number_neighbors)
numN2 <- dim(mnm2[[1]])[2]
if (numN1 == numN2) {
    cat("Passed\n")
} else
    cat("Failed\n")

## Matrix values check
cat("\tChecking values in matrices... \t\t\t\t")
inds <- match(names(mnm1)[!names(mnm1) %in% c("id", "yr")], names(mnm2))
inds <- inds[!is.na(inds)]
valChecks <- sapply(names(mnm2)[inds], FUN = function(mat) {
    mat1 <- mnm1[[mat]]
    mat2 <- mnm2[[mat]]
    ## Check some random rows
    rows <- sample(nrow(mat1), round(nrow(mat1)/2))
    tests <- c()
    for (i in rows) {
        nnebs <- mnm1$number_neighbors[i]
        if (nnebs == 0 &
            all(is.na(mat2[i,])) &
            all(mat1[i,] == 0)) {
            tests <- c(tests, TRUE)
        } else {
            tests <- c(tests, all(mat1[i, 1:nnebs] == mat2[i, 1:nnebs]))
        }
    }
    all(tests)
})

if(all(valChecks)){
    cat("Passed\n")
} else {
    cat("Failed\n")
}

## Check id and yr variables (non-matrices)
nonMats <- c("id", "yr")
cat(sprintf("\tChecking non-matrices: %s... \t\t\t\t", toString(nonMats)))
valChecks <- sapply(nonMats, FUN = function(name) {
    mat1 <- mnm1[[name]]
    mat2 <- mnm2[[name]]
    all(mat1 == mat2)
})

if(all(valChecks)){
    cat("Passed\n")
} else {
    cat("Failed\n")
}

## Check additional columns
addedCols <- c("aspcl", "elevcl", "plot")
cat("\tChecking %s...\t\t\t\t")
valChecks <- sapply(names(mnmlist), FUN = function(pname) {
    ## Check plot matches list labels
    pnum <- as.numeric(pname)
    pcheck <- (pnum == unique(mnmlist[[pname]][["plot"]]))

    ## Check aspcl and elevcl
    asp1 <- unique(dat[dat$pplot == pnum, "aspcl"])
    asp2 <- unique(mnmlist[[pname]][["aspcl"]])
    elev1 <- unique(dat[dat$pplot == pnum, "elevcl"])
    elev2 <- unique(mnmlist[[pname]][["elevcl"]])

    all(c(pcheck, asp1 == asp2, elev1 == elev2))
})

if(all(valChecks)){
    cat("Passed\n")
} else {
    cat("Failed\n")
}


################################################################################
##
##                                  mnm_agg
##
################################################################################
## Aggregates results of mnm_plot() into single set of neighbor matrices
cat("Testing: mnm_agg()...\n")
cat("\t** Note ** checking against results from mnm_plot\n")
mnmlist <- mnm_plot(abbas, neighbors, sr, ind.var) # mnm_plot() list of plot mnms
mnmagg <- mnm_agg(abbas, neighbors, sr, ind.var)   # aggregated neighborhood values
matNames <- ("direction_x", "direction_y", "direction_z", "distances",
             "variable", "species", "neighbor_id")

## Check values in matrices with those from mnm_plot()
## Matrix values check
cat("\tChecking values in matrices... \t\t\t\t")
inds <- match(names(mnm1)[!names(mnm1) %in% c("id", "yr")], names(mnm2))
inds <- inds[!is.na(inds)]
valChecks <- sapply(names(mnm2)[inds], FUN = function(mat) {
    mat1 <- mnm1[[mat]]
    mat2 <- mnm2[[mat]]
    ## Check some random rows
    rows <- sample(nrow(mat1), round(nrow(mat1)/2))
    tests <- c()
    for (i in rows) {
        nnebs <- mnm1$number_neighbors[i]
        if (nnebs == 0 &
            all(is.na(mat2[i,])) &
            all(mat1[i,] == 0)) {
            tests <- c(tests, TRUE)
        } else {
            tests <- c(tests, all(mat1[i, 1:nnebs] == mat2[i, 1:nnebs]))
        }
    }
    all(tests)
})

if(all(valChecks)){
    cat("Passed\n")
} else {
    cat("Failed\n")
}


