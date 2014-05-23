# Use size classes fit individually to each plot,
#  fit polynomials, ht ~ poly(dbh, 2) by neighborhood size class and plot,
#  Sum the AIC values for each neighborhood type to compare...
dat <- read.csv("long-ABBA-classes-byplot.csv")

# subset with just ABBA and stat=="ALIVE" and complete cases for dbh and ht
dat <- subset(dat, spec=="ABBA" & stat=="ALIVE")
dat <- dat[complete.cases(dat$dbh, dat$ht),]

# log-transform, remove 0 values
dat[,c("dbh","ht")] <- apply(dat[,c("dbh","ht")], 1, log)
dat <- subset(dat, dbh>1 & ht<2)

# get list of all polynomial fits..
clcols <- intersect(grep("^n",names(dat)), grep("cl$", names(dat)))

# create list of models for each plot/size class, extract AIC values + MSE, return sums
#  for each neighborhood calculation method...
tt <- sapply(names(dat)[clcols], function (x) {
    models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
        d[,x] <- as.factor(as.character(d[,x]))
        dlply(d, x, .fun = function(l) {
            if(nrow(l)>3) {
                lm(l$ht ~ poly(l$dbh, 2))
            }
        })
    }), recursive = FALSE)
    c(sum(getAIC(models_list[!is.null(models_list)])$aic2),
      sum(getMSE(models_list)$mse/length(models_list[!is.null(models_list)])))
})

tt

nonebs <- lm(ht ~ poly(dbh, 2), data = dat)
mean(residuals(nonebs)^2)
extractAIC(nonebs)

################################
# nsumbabig.1
models_nsumbabig <- unlist(dlply(dat, .(pplot), .fun = function(d) {
    d$nsumbabig.3cl <- as.factor(as.character(d$nsumbabig.3cl))
#    print(paste("levels",length(levels(d$nsumbaclq))))
    dlply(d, .(d$nsumbabig.3cl), .fun = function(x) {
        if(nrow(x) > 3) {
            lm(x$ht ~ poly(x$dbh, 2))
        }
    })
}), recursive = FALSE)

##############################
# some graphs
east <- subset(dat, pplot<16)
windows()
ggplot(east, aes(dbh,ht, color = factor(nsumbabigclq))) + geom_point() +
    facet_wrap(~pplot) + geom_smooth(method="lm",formula = y~ poly(x,2),se=FALSE, lwd=1)

windows()
ggplot(subset(dat, elevcl=="L"), aes(dbh,ht, color = factor(nsumbabigclq))) +
    geom_point() +
    facet_wrap(~pplot) + geom_smooth(lwd=1, se=FALSE)


# nsumba5clq #
windows()
ggplot(subset(dat, elevcl=="L"), aes(dbh,ht, color = factor(nsumba5clq))) +
    geom_point() +
    facet_wrap(~pplot) + geom_smooth(lwd=1, se=FALSE)

windows()
ggplot(subset(dat, elevcl=="M"), aes(dbh,ht, color = factor(nsumba5clq))) +
    geom_point() +
    facet_wrap(~pplot) + geom_smooth(lwd=1, se=FALSE)

windows()
ggplot(subset(dat, elevcl=="H"), aes(dbh,ht, color = factor(nsumba5clq))) +
    geom_point() +
    facet_wrap(~pplot) + geom_smooth(lwd=1, se=FALSE)

###
windows()
ggplot(subset(dat, elevcl=="L"), aes(dbh,ht, color=factor(nsumbabig.1cl))) +
    geom_point(alpha=0.3) +
    facet_wrap(~pplot) + geom_smooth(lwd=1, se=FALSE)

