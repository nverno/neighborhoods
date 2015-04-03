# Various neighborhood models
#  params is passed a vector of named parameters to be assigned internally
#  in function environment.

### Fit using MLE
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

# size power model
spm <- function(ps,ind.var="priorba") {
    PG = ps[["PG"]]
    b = ps[["b"]]
    PG * targets[,ind.var] ^ b
}

# size power nci model
spnm <- function(ps, ind.var="priorba") {
    PG = ps[["PG"]]
    b = ps[["b"]]
    alpha = ps[["alpha"]]
    beta = ps[["beta"]]
    D = ps[["D"]]
    size.effect = PG*targets[,ind.var]^b
    nci = rowSums((bas^alpha)/(distances^beta), na.rm = TRUE)
    competition.effect = nci^D
    size.effect * competition.effect
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
    nci <- rowSums(((bas ^ alpha)/(distances ^ beta)), na.rm=TRUE)
    competition.effect <- exp(-(C) * nci^D)
    PG * size.effect * competition.effect
}

# size logistic exponential nci model
slenm <- function(ps, ind.var="priorba")
{
    PG = ps[["PG"]]
    alpha = ps[["alpha"]]
    beta = ps[["beta"]]
    C = ps[["C"]]
    D = ps[["D"]]
    b = ps[["b"]]
    sizeX0 = ps[["sizeX0"]]
    sizeXb = ps[["sizeXb"]]
    size.effect <- exp(-0.5 * (log(targets[,ind.var]/sizeX0)/sizeXb)^b)
    nci <- rowSums(((bas ^ alpha)/(distances ^ beta)), na.rm=TRUE)
    competition.effect <- exp(-(C) * nci^D)
    PG * size.effect * competition.effect
}

# size power exponential nci model
spenm <- function(ps, ind.var="priorba")
{
    PG = ps[["PG"]]
    alpha = ps[["alpha"]]
    beta = ps[["beta"]]
    C = ps[["C"]]
    D = ps[["D"]]
    b = ps[["b"]]
    size.effect <- targets[,ind.var]^b
    nci <- rowSums(((bas ^ alpha)/(distances ^ beta)), na.rm=TRUE)
    competition.effect <- exp(-(C) * nci^D)
    PG * size.effect * competition.effect
}

# size logistic density nci model
sldnm <- function(ps, ind.var="priorba")
{
    PG = ps[["PG"]]
    sizeX0 = ps[["sizeX0"]]
    sizeXb = ps[["sizeXb"]]
    C = ps[["C"]]
    D = ps[["D"]]
    size.effect <- exp(-0.5*(log(targets[,ind.var]/sizeX0)/sizeXb)^2)
    nci <- rowSums(bas>0,na.rm=TRUE)
    competition.effect <- exp(-(C) * nci^D)
    PG * size.effect * competition.effect
}

density.nci.model <- function(PG, C, D)
{
  nci <- rowSums(bas>0,na.rm=TRUE)
  competition.effect <- exp(-(C) * nci^D)
  PG * competition.effect
}

size.logistic.nci.lambda.model <- function(PG, sizeX0, sizeXb, alpha, beta, lambda, C, D)
{
  size.effect <- exp(-0.5*(log(targets$PRIORBA/sizeX0)/sizeXb)^2)
  #lambda.vals <- ifelse(neigh.lambda.indexes==0,0,lambda[neigh.lambda.indexes])
  lambda.vals <- lambda[neigh.lambda.indexes]
  dim(lambda.vals) <- dim(neigh.lambda.indexes)
  nci <- rowSums((lambda.vals * (bas ^ alpha)/(distances ^ beta)), na.rm=TRUE)
  competition.effect <- exp(-(C) * nci^D)
  PG * size.effect * competition.effect
}

# Simple neighbor model as predictor of rgr, no size
simplest <- function(ps, ind.var = "priorbv")
{
    PG = ps[["PG"]]
    alpha = ps[["alpha"]]
    beta = ps[["beta"]]
    C = ps[["C"]]
    nci <- rowSums(((bas ^ alpha)/(distances ^ beta)), na.rm=TRUE)
    competition.effect <- exp(-C*nci)
    PG * competition.effect
}
