################################################################################
##
##            Estimate Surround Parameters with Basal Area Growth
##
################################################################################

source("~/work/neighborhoods/surround/functions.R")
source("~/work/functions/functions-neighborhood.R")

## Neighborhood variables
## - nsize is 9,25, etc
## - alpha, beta are neighborhood parameters (distance, size)
## - theta is direction, slope params
## - C is size of connected components
## - dep.var is neighbor size variable
## - ind.var is comparison variable between target and neighbor
##   (if only looking a neighbors larger than target, this variable determines
##    whether a neighbor is included in the neighborhood analysis)
## - spec: species of targets we are interested in (all species are used as neighbors)
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

###############################################################################
##
## Real data:
##
dat <- read.csv("~/work/data/moose/moose-long.csv")

## define targets and neighbors
targets <- subset(dat, bqudx < (12-sr) & bqudx > (-1 + sr) & bqudy < (12 - sr) &
                  bqudy > (-1 + sr) & stat=="ALIVE")
neighbors <- subset(dat, bqudx < 11 & bqudx > 0 & bqudy < 11 &
                    bqudy > 0 & stat=="ALIVE")
## remove trees that dont satisfy certain conditions
grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0)
abbas <- targets[grew,]

## make neighbor matrices using square radius (i.e bqudx,bqudy)
## abba_mats <- mnm(abbas, neighbors, sr, ind.var=ind.var)

## compute nsi
## i <- 1
## num_nebs <- abba_mats$number_neighbors[i]
## nbrs <- data.frame(x=abba_mats$direction_x[i:num_nebs],
##                    y=abba_mats$direction_y[i:num_nebs],
##                    distance=abba_mats$distances[i:num_nebs],
##                    size=abba_mats$variable[i:num_nebs],
##                    z=abba_mats$direction_z[i:num_nebs])

## nsi(nbrs=nbrs, C=C, alpha = alpha, beta = beta, theta = theta, nsize = 9)

## Create neighborhood matrices by plot
## Returns a list of neighborhood matrices for each plot
## '0' values in the matrices have been replaced by 'NA' where there are no neighbors
mnm_plot<- function(targets, neighbors, sr, ind.var = "ba") {
    ## Create list of neighborhoods for each plot
    matlst <- lapply(unique(targets$pplot), FUN = function(plot) {
        targs <- targets[targets$pplot == plot, ]
        nebs <- neighbors[neighbors$pplot == plot, ]
        NM <- mnm(targs, nebs, sr=sr, ind.var = ind.var)

        ## Create a 'key' matrix to transform '0' to NA where this is no neighbor
        ## key has '1' at indices of values to keep, 'NA' at indices to remove
        key <- matrix(1, nrow(NM$distances), ncol(NM$distances))
        for (i in seq_along(NM$number_neighbors)) {
            numnebs <- NM$number_neighbors[i]
            if (numnebs > 0 && numnebs < ncol(key)) { # there are nebs and 0's
                key[i, (numnebs+1):ncol(key)] <- NA
            } else {                                  # No nebs, set entire row to NA
                key[i, ] <- NA
            }
        }

        ## Trim extra columns from matrices
        maxneb <- max(NM$number_neighbors)
        mats <- lapply(NM[c("direction_x", "direction_y", "direction_z",
                            "distances", "variable", "species", "neighbor_id")],
                       FUN = function(mat) {
                           (mat * key)[, 1:maxneb]
                       })

        ## Add target id, plot id, elevcl, aspcl, and year columns
        mats$id <- NM$id
        mats$plot <- rep(plot, length(NM$id))
        mats$yr <- NM$yr
        mats$aspcl <- rep(unique(targs$aspcl), length(NM$id))
        mats$elevcl <- rep(unique(targs$elevcl), length(NM$id))

        ## Return the new matrix
        mats
    })

    names(matlst) <- unique(targets$pplot)
    matlst
}


## Create list neighborhood matrices, combined across all plots
## The main work is done by mnm_plot
mnm_agg <- function(targets, neighbors, sr, ind.var = "ba") {
    mnmlst <- mnm_plot(targets, neighbors, sr, ind.var)

    ## Find max number of neighbors across all plots
    maxneb <- max( sapply(mnmlst, FUN = function(pp) dim(pp[["distances"]])[[2]] ) )

    ## Fill matrices with NAs to be same dimensions
    matinds <- which(names(mnmlst[[1]]) %in%
                     c("direction_x", "direction_y", "direction_z",
                       "distances", "variable", "species", "neighbor_id"))
    matNames <- names(mnmlst[[1]][matinds])
    padMat <- lapply(mnmlst, FUN = function(NM) {
        numPad <- maxneb - dim(NM[["distances"]])[2]
        pads <- matrix(NA,
                       nrow = dim(NM[["distances"]])[1],
                       ncol = numPad)
        padded <- lapply(matNames, FUN = function(mat) {
            cbind(NM[[mat]], pads)
        })
        names(padded) <- matNames
        return ( padded )
    })

    ## Combine padded matrices into single list of neighbor matrices
    combMat <- lapply(matNames, FUN = function(mat) {
        mats <- lapply(padMat, FUN = function(plt) {
            plt[[mat]]
        })
        do.call(rbind, mats)
    })
    names(combMat) <- matNames

    ## Combine other neighborhood variables
    otherVars <- names(mnmlst[[1]])[!names(mnmlst[[1]]) %in% matNames]
    combVars <- lapply(otherVars, FUN = function(var) {
        lst <- lapply(mnmlst, FUN = function(plt) {
            plt[[var]]
        })
        do.call(c, lst)
    })
    names(combVars) <- otherVars

    ## Return list combining matrice variables and vector variables
    return ( c(combMat, combVars) )
}


# helper functions to retrieve model name/ind.var from global environment
#  for internal calls
get.model <- function() { mod <- get("currentmodel"); mod }
get.ind.var <- function() { get("ind.var") }

# log.likelihood function
normNLL <- function(params, x, currentmodel=NULL) {
    if(missing(currentmodel)) { currentmodel <- get.model() }
    sd = params[["sd"]]
    ind.var <- get.ind.var()
    mu = do.call(currentmodel, list(params,ind.var))
    -sum(dnorm(x, mean = mu, sd = sd, log = TRUE))
}

# size logistic nci model
slnm <- function(ps, ind.var="priorba")
{
    PG = ps[["PG"]]
    sizeX0 = ps[["sizeX0"]]
    sizeXb = ps[["sizeXb"]]
    alpha = ps[["alpha"]]
    beta = ps[["beta"]]
    C = ps[["C"]]
    D = ps[["D"]]
    size.effect <- exp(-0.5*(log(targets[,ind.var]/sizeX0)/sizeXb)^2)
    nsi <-
    nci <- rowSums(((bas ^ alpha)/(distances ^ beta)), na.rm=TRUE)
    competition.effect <- exp(-(C) * nci^D)
    PG * size.effect * competition.effect
}

surround_growth <- function(ps, ind.var = "priorba") {

}


### Automated fitting of neighborhood models by MLE
fit_MLE <- function(dat, sr, spec="ABBA", ind.var="ba", dep.var, models=NULL, bigger=TRUE,
                    method="Nelder-Mead", maxit=1000,
                    savefits="~/work/neighborhoods/surround/parameters/growth/currentfits.Rda",
                    realdist = FALSE) {
    srt <- max(sr) # if multiple sr, targets are those in all neighborhoods
    fits <- c()
    if(realdist == FALSE) {
        neighbors <<- subset(dat, bqudx < 11 & bqudx > 0 & bqudy < 11 &
                             bqudy > 0 & stat=="ALIVE")
        targets <<- subset(dat, bqudx < (12-srt) & bqudx > (-1 + srt) &
                           bqudy < (12 - srt) & bqudy > (-1 + srt) & stat=="ALIVE")
    }
    if(realdist == TRUE) {
        targets <<- subset(dat, abs(x) < (11-sr) & abs(y) < (11-sr) & stat=="ALIVE")
        neighbors <<- subset(dat, abs(x) <= 11 & abs(y) <= 11 & stat=="ALIVE")
    }
    ## remove trees that dont satisfy certain conditions
    grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0 &
                  targets[,ind.var]>0)
    targets <<- targets[grew,]
    for(i in sr) {  # make neighbor matrices
        print(paste("Making neighbor matrices for *", i, "* sized neighborhoods..."))
        make.neighbor.matrices(targets, neighbors, i, ind.var=ind.var, bigger=bigger,
                               realdist = realdist)
                                        # assign matrices in global for later access
        assign(paste0("species",i), species, envir = .GlobalEnv)
        assign(paste0("bas",i), bas, envir = .GlobalEnv)
        assign(paste0("distances",i), distances, envir = .GlobalEnv)
                                        # fit models
        if(!missing(models)) {
            print(paste("Fitting models with sr =", i))
            fits1 <- sapply(models, FUN=function(d) {
                print(paste("Model:", d))
                currentmodel <<- d
                ps <- get.params(sr = i, spec, ind.var, dep.var, d)
                print("Starting Parameters:"); print(unlist(ps,recursive = FALSE))
                parnames(normNLL) <<- c(names(ps))
                fit2 <- mle2(normNLL,
                             start = unlist(ps,recursive = FALSE),
                             data = list(x = targets[,dep.var]),
                             method = method,
                             control = list(maxit = maxit))
                add.params(sr=i, spec, ind.var, dep.var, newpars = coef(fit2), d)
                                        # add fit to current fits saved file
                tmp.env <- new.env() # environment to save fits in
                load(savefits, envir = tmp.env)
                assign(paste(d,sr,spec,sep = "."),fit2,envir=tmp.env)
                save(list=ls(all.names=TRUE, pos=tmp.env),
                     envir=tmp.env, file=savefits)
                rm(tmp.env)
                fit2
            })
            names(fits1) <- paste0(models,i)
            fits <- c(fits, fits1)
        }
    }
    fits
}


## Testing platform for neighborhood MLE models
source("~/work/functions/functions.R")
source("~/work/functions/functions-growth.R")
source("~/work/neighborhoods/neighborhood-models.R")

## Data
dat = read.csv("~/work/data/data/long-bc-derived.csv")

## Parameters
sr = c(6)
spec = "FD"
ind.var = "priorbv" ## will be used in neighborhood calculations
dep.var = "rgrsisdp"
currentmodel = "simplest"
method = "Nelder-Mead"
maxit = 100000
realdist = TRUE ## TRUE for BC data where exact coordinates are known

## Make sure starting parameters are defined in parameters.csv
## If necessary, create new starting parameters:
## pars <- read.csv("parameters.csv")
## rowcopy = pars[83:86,]
## rowcopy$model <- "simplest"
## rowcopy$mdep.var <- "rgrplot"
## pars <- rbind(pars, rowcopy)
## write.csv(pars, "parameters.csv", row.names =FALSE)
pars <- read.csv("~/work/data/data/parameters/parameters.csv")
ps <- get.params(sr,spec,ind.var,dep.var,currentmodel = currentmodel)

fit <- fit.MLE.models(dat=dat,sr=sr,spec=spec,ind.var=ind.var,dep.var=dep.var,
                      models=currentmodel,method=method,
                      realdist = realdist, maxit = maxit)
