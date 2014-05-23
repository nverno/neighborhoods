### Fit model using maximum likelihood estimation
#source("make-neighbor-matrices.R") # create neighbor matrices

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

# get starting params from previous runs, if model has new params create a list
#  manually.., define current model
currentmodel <- "simplest"
#ps <- list(alpha = 1, beta = 1, D = 1, PG = 1, b = 1, sd = 0.002)
ps <- get.params(sr=sr, spec=spec, ind.var=ind.var, dep.var=dep.var,
           currentmodel = currentmodel)


## ps <- lapply(ps, function(x) x <- 1)
## ps[["b"]] <- 2
## ps <- ps[!names(ps) %in% c("sizeX0", "sizeXb")]

 # fit using optim ~ 30 seconds for standard methods, ~ 15 mins for SANN (returned
#  same result with default maxiter and starting params from previous fit)
stime <- Sys.time()
fit <- optim(fn = normNLL,
             par = unlist(ps, recursive = FALSE),
             x = targets[,dep.var], method = "Nelder-Mead")
optim.time <- Sys.time() - stime

# fit using mle2 wrapper for optim
# use parnames to attach parameter names to normNLL function to pass parameters
#  inside a vector to mle2
parnames(normNLL) <- c(names(ps))
fit2 <- mle2(normNLL,
             start = unlist(ps,recursive = FALSE),
             data = list(x = targets[,dep.var]),
             method = "SANN",
             control = list(maxit = 5000))

# add newly fitted parameters to parameters file
add.params(sr=sr, spec=spec, ind.var=ind.var, dep.var=dep.var, newpars = coef(fit2),
           currentmodel = currentmodel)

pars <- read.csv("parameters.csv"); pars


