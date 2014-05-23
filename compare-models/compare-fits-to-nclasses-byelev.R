# Use size classes fit individually to each plot,
#  fit polynomials, ht ~ poly(dbh, 2) by neighborhood size class and plot,
#  Sum the AIC values for each neighborhood type to compare...
dat <- read.csv("long-ABBA-classes-byelev.csv")

# subset with just ABBA and stat=="ALIVE" and complete cases for dbh and ht
dat <- subset(dat, spec=="ABBA" & stat=="ALIVE")
dat <- dat[complete.cases(dat$dbh, dat$ht),]

# log-transform, remove 0 values
#dat[,c("dbh","ht")] <- apply(dat[,c("dbh","ht")], 1, log)
dat <- subset(dat, dbh>0 & ht>0)

# get list of all polynomial fits..
clcols <- intersect(grep("^n",names(dat)), grep("clq$", names(dat)))

# create list of models for each elevcl/size class, extract AIC values + MSE, return sums
#  for each neighborhood calculation method...
tt <- sapply(names(dat)[clcols], function (x) {
    models_list <- unlist(dlply(dat, .(elevcl), .fun = function(d) {
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

##############################
# some graphs
ggplot(subset(dat, elevcl=="L"), aes(dbh,ht, color=factor(nsumbabig.3clq2))) +
    geom_point(alpha=0.3) +
    facet_wrap(~pplot) + geom_smooth(lwd=1, se=FALSE)


###
