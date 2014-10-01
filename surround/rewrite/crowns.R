################################################################################
##
##                  Estimating crown dimensions and shapes
##
## - Ellipsoids for deciduous trees
## - Cones for conifers
##
################################################################################
## Master data has crown dimensions from 86/87
if (Sys.info()['sysname'] == "Linux") {
    pp <- read.csv("~/Dropbox/Shared/Data/pptreemas10bv.csv")
} else
    pp <- read.csv("C:/Users/noah/Dropbox/Shared/Data/pptreemas10bv.csv")

## Combine data from 86/87 for modeling.
## Note: some trees have measurements in both years.
## Columns to create/combine:
##  crown area, crown depth, basal area, and height
##
## Just using data from 86/87 where crown dimensions were measured
names(pp) <- tolower(names(pp))
dat <- pp[((!is.na(pp$cperp86) & !is.na(pp$clong86))
          | (!is.na(pp$cperp87) & !is.na(pp$clong87)))
          & !(!is.na(pp$cperp86) & !is.na(pp$cperp87)), ]  # remove repeated measurements
nrow(dat[!is.na(dat$cperp86) & !is.na(dat$cperp87), ])     # 0
nrow(dat[!is.na(dat$clong86) & !is.na(dat$clong87), ])     # 0
nrow(dat[is.na(dat$ht86) & is.na(dat$ht87)])


## Crown area: pi * a * b,
## 'a' and 'b' are long and short axes of ellipse
dat$crarea <-



################################################################################
##
##                                  Models
##
## - Crown area: function of BA/HT
## - Crown depth: function of HT
##
################################################################################
