### look through neighbor fits and determine the most significant
# neighborhood to use
nebs <- read.csv("neighbor-fits.csv")

dat <- read.csv("long-complete-cases-sr2.csv")
dat <- subset(dat, spec=="ABBA" & stat=="ALIVE")
dat[,c("dbh","ht")] <- apply(dat[,c("dbh","ht")], 2, log)

# subset with cl classes only
grep("cl$",levels(nebs$neighbor))
nebscl <- subset(nebs, neighbor %in% levels(neighbor)[grep("cl$",levels(neighbor))])

test <- ddply(nebscl, .(aspcl, elevcl, soilcl, slopcl, neighbor),
              function(x) {
                  data.frame(psum = sum(x$pval, na.rm = TRUE),
                         sign = sum(x$sign, na.rm = TRUE)) })

# sorted by summing pvals across size classes,
# nmaxbacl by far the most significant, then nsumbabigcl
bypval <- test[order(test$psum),]
table(head(bypval, n=40)$neighbor)

# sorted by sign differences from 0
# negative signs:
bysign <- test[order(test$sign),]
table(head(bysign, n=20)$neighbor)

# positive signs
bysign2 <- test[order(test$sign, decreasing = TRUE),]
table(head(bysign2, n=20)$neighbor)

# rename neighbor classes uniformly (i.e 1:8)
levels(dat$nmaxbacl) <- c(2:8,1)
levels(dat$nmaxbacl) <- sort(levels(dat$nmaxbacl))
# nmaxbacl across elevcl and aspcl
sampsizes <- ddply(dat, .(aspcl,nmaxbacl), function(x) nrow(x))
todrop <- with(dat,
               which(aspcl=="E" & nmaxbacl %in% c(6:8) |
                     aspcl=="W" & nmaxbacl == 6))

ggplot(dat, aes(dbh,ht,color=nmaxbacl)) + geom_point(alpha=0.3, size=2) +
    geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE,lwd=1,
                data = dat[-todrop,]) + facet_wrap(~aspcl + elevcl)

# make just two bins
dat$twobin <- cut(as.numeric(dat$nmaxbacl), breaks = c(seq(0,8,4)))
ggplot(dat, aes(dbh,ht,color=twobin)) + geom_point(alpha=0.3, size=2) +
    geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE,lwd=1,) +
    facet_wrap(~aspcl + elevcl)

# faceted by plots 4:15
ggplot(subset(dat, pplot<16), aes(dbh,ht,color=twobin)) + geom_point(alpha=0.3, size=2) +
    geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE,lwd=1,) +
    facet_wrap(~pplot)

# make two bins with different breaks for each plot determined by max height
# for each plot
plots <- levels(factor(dat$pplot))

ggplot(dat, aes(dbh,ht,color=twobin)) + geom_point(alpha=0.3, size=2) +
    geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE,lwd=1,) +
    facet_wrap(~dbhcl)

# nsumht98cl signs change with neighbor height
s1 <- subset(dat, elevcl == "M" & time==1998)
make.poly(s1, "dbh","ht", fit.by = "nsumht98cl")

# number of sign changes, ignoring those with nonsignificant pvalues.
siglevel <- 0.05
test1 <- ddply(nebscl, .(aspcl, elevcl, soilcl, slopcl, neighbor),
              function(x) {
                  with(x,
                       data.frame(sig = sum(pval<=siglevel, na.rm = TRUE),
                                  nonsig = sum(pval>siglevel,na.rm=TRUE),
                                  sump = sum(pval[pval<=siglevel],na.rm = TRUE),
                                  negsign = sum(sign[pval<siglevel]<0,na.rm = TRUE),
                                  possign = sum(sign[pval<siglevel]>0,na.rm = TRUE))) })

test1$signchanges <- (test1$possign + test1$negsign)

# sort by pvals
bypval <- with(test1, test1[ sig > 0,])
bypval <- with(test1, test1[order(sump/sig),])

# visualize most significant by pvalue
d <- subset(dat, elevcl == "M")
ggplot(d, aes(dbh, ht, color = nmaxbacl)) + geom_point(alpha=.4, size=2) +
    geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE,lwd=1,) +
    facet_wrap(~soilcl + slopcl)


test <- subset(bypval, is.na(aspcl) & is.na(elevcl) & is.na(soilcl))

ggplot(dat, aes(dbh, ht, color = nsumbacl)) + geom_point(alpha=.4, size=2) +
    geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE,lwd=1,) +
    facet_wrap(~slopcl)



# by sign changes
bysign <- with(test1, test1[order(negsign, decreasing = TRUE),])


ggplot(dat, aes(dbh, ht, color = dbhcl)) + geom_point(alpha=.4, size=2) +
    geom_smooth(method="lm", formula = y ~ poly(x, 2), se=FALSE,lwd=1,) +
    facet_wrap(~slopcl)

