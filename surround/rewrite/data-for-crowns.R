## Master data
if (Sys.info()['sysname'] == "Linux") {
    pp <- read.csv("~/Dropbox/Shared/Data/pptreemas10bv.csv")
} else
    pp <- read.csv("C:/Users/noah/Dropbox/Shared/Data/pptreemas10bv.csv")


## Data pertaining to crown shapes:
## - Data from years 86/87 for plots in H/M/L
## - Only data from EAST
## - Mostly ABBA, BECO, PIRU, a few BEAL, BEPA, SOAM
## - Length along two perpendicular axes
## - Direction of long axis
## -
p86 <- !is.na(pp$CLONG86)
p87 <- !is.na(pp$CLONG87)
dat <- pp[p86 | p87, ]

## Total number of trees with crown data from both years by plot
table(pp[!is.na(pp$CLONG86) | !is.na(pp$CLONG87), "PPLOT"]) # Long axis data
table(pp[!is.na(pp$CPERP86) | !is.na(pp$CPERP87), "PPLOT"]) # Perp axis data
table(pp[!is.na(pp$CAZLNG86) | !is.na(pp$CAZLNG87), "PPLOT"]) # Long axis direction

## Number of trees in each plot by year
table(pp[p86, "PPLOT"]) # 1986
table(pp[p87, "PPLOT"]) # 1987

## Number of trees by elevation (both years)
table(pp[p86 | p87, "ELEVCL"])

## Number of trees by aspect (both years)
table(pp[p86 | p87, "ASPCL"]) # Only east side

## Species
table(dat$SPEC)
