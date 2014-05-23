# comparisons between nls, mle, glm
# make neighbor matrices beforehand...
sr <- 2
ind.var <- "priordbh"
dep.var <- "dbhgrowth"
spec = "ABBA"

fit.MLE.models(long,sr,spec, ind.var, dep.var)

### No neighborhoods
# Simple linear model, i.e dbhgrowth ~ priordbh
# least squares fit
formula <- as.formula(paste(dep.var,"~",ind.var))
lm.fit <- lm(formula, data=targets)

with(targets, plot(ind.var, dep.var))
abline(lm.fit)

ggplot(targets, aes_string(ind.var, dep.var)) + geom_point() +
    geom_smooth(method="lm", formula = y ~ x)

# nls fit
formula <- paste(dep.var, "~ a *", ind.var, "+ b")
nls.fit <- nls(as.formula(formula), data = targets,
               start = list(a = 1, b=0))

# glm fit
glm.fit <- glm(formula, family = gaussian, data=targets)

# MLE fit
ps <- c(a = 1, b = 1, sd = 1)
linear.model <- function(ps, ind.var) {
    a = ps[["a"]]
    b = ps[["b"]]
    a * targets[,ind.var] + b
}
parnames(normNLL) <- c("a","b","sd")
currentmodel <- "linear.model"
mle.fit.linear <- mle2(normNLL,
                start = unlist(ps, recursive = FALSE),
                method = "Nelder-Mead",
                data = list(x = targets[,dep.var]))

### More complicated model using neighborhoods
neighbor.model <- function(ps, ind.var) {
    a = ps[["a"]]
    b = ps[["b"]]
    alpha = ps[["alpha"]]
    beta = ps[["beta"]]
    C = ps[["C"]]
    D = ps[["D"]]
    size.effect = a * targets[,ind.var] ^ b
    nci = rowSums(bas^alpha/distances^beta, na.rm = TRUE)
    competition.effect = exp(C*nci ^ D)
    size.effect * competition.effect
}

ps <- c(a = 1, b = 1, alpha = 1, beta = 1, C = 1, D = 1, sd = 1)

# nls fit
formula <- as.formula(
    paste(dep.var,"~ a *",ind.var,"* exp(-C * (rowSums((bas ^ alpha)/(distances ^ beta),
na.rm = TRUE)) ^ D)"))

nls.fit <- nls(formula, data = targets,
               start = as.list(ps[!names(ps) %in% c("sd")]))

# glm fit
glm.fit <- glm(formula, start = as.list(ps), family = gaussian, data=targets)

# MLE fit
parnames(normNLL) <- names(ps)
currentmodel <- "neighbor.model"
mle.fit.neighbor <- mle2(normNLL,
                         start = unlist(ps, recursive = FALSE),
                         method = "SANN",
                         data = list(x = targets[,dep.var]),
                         control = list(maxit = 1000))


"slnm", "sldnm", "spnm", "spenm",

pred.neighbor <- do.call("slnm", list(coef(mle.fit.neighbor), ind.var))
pred.linear <- do.call(linear.model, list(coef(mle.fit.linear), ind.var))
    observed <- targets[,dep.var]
pred.lm <- predict(lm.fit)
xvar <- targets[,ind.var]
dat <- data.frame(pred.neighbor = pred.neighbor, pred.linear = pred.linear,
                  observed = observed, xvar = xvar, pred.lm = pred.lm)
datlong <- melt(dat, id.vars = "xvar")
names(datlong) <- c("xvar","model","depvar")

p1 <- ggplot(datlong, aes(xvar, depvar, color=model)) +
    geom_point(alpha=0.3, size=2)
p1



ggplot(targets, aes(priorba, htgrowth)) + geom_point() +
    geom_smooth()

