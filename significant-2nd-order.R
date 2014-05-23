### search data for significant 2nd order parameters in polynomial fits
# 2nd order polynomials will be fit to log-transformed ht vs dbh for each species
#
#  This fit will be successively divided by environmental categorical
#   variables: 'aspcl', 'elevcl', 'soilcl', 'slopcl'
#
comp <- read.csv("long-complete-cases-sr2.csv")

# using only ABBA, log-transform
dat <- subset(comp, spec=="ABBA" & stat=="ALIVE")
dat[,c("dbh","ht")] <- log(dat[,c("dbh","ht")])

# DBH-based neighborhood class columns
clscols <- intersect( grep("^n",names(dat)), grep("cl$",names(dat)) )
clscols <- intersect( grep("ba|dbh", names(dat)), clscols)

# get all possible environmental factor combinations
env.var <- c("aspcl","elevcl","soilcl","slopcl")
allcombs.env <- multicombn(env.var, length(env.var))

# make dataset with significance values for each combination of environmental
# factors (without neighbors)
pvals <- data.frame()
for(i in 1:length(allcombs.env)) {
    new <- ddply(dat, allcombs.env[[i]], function(x) {
        c(summary(lm(x[["ht"]]~poly(x[["dbh"]],2)))$coefficients[3,][4],
         sign = sign(summary(lm(x[["ht"]]~poly(x[["dbh"]],2)))$coefficients[3,][1]))
    })
    pvals <- rbind.fill(new,pvals)
}

