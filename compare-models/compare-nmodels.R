# Neighborhood Model Comparison, confidence intervals, parameter profiles, slices,
#  etc...
long <- read.csv("long.csv")

# Make choices
sr <- c(2)
spec <- "ABBA"
ind.var <- "priorba"
dep.var <- "bagrowth"
models <- c("slnm", "sldnm", "spnm", "spenm", "slenm", "spm")

# get MLE fits, before running make sure parameters are initialized in
#  parameters.csv
fits <- fit.MLE.models(long, sr, spec, ind.var, dep.var, models, method = "SANN",
                       maxit = 10000)







# likelihood profiles
profile.slnm <- profile(fits[["slnm"]])

# likelihood ratio test
lapply(fits, anova)
anova(fits[[1]],fits[[2]],fits[[3]],fits[[4]])

# AIC
AICtab(fits[[1]],fits[[2]],fits[[3]],fits[[4]])

# confidence intervals
p <- confint(fits[[1]], "C")
plot(profile(fits[[3]]))

plot(confint(fits[[1]], "C"))

# plotting slices through parameter space
library(emdbook)
plot(calcslice(fits[[3]],fits[[2]]))

# covariance
cov2cor(vcov(fits[[3]]))
