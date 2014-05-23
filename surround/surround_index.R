## Examine the effect of crowdedness on bagrowth/htgrowth by species in the mid-east plots
source("~/work/functions/functions-neighborhood.R")

library(ggplot2)
library(grid)
library(plyr)

pp <- read.csv("~/work/data/data/dynamicallometry/moose-long-canopies.csv")
## mid-east
dat <- subset(pp, pplot %in% c(8,9,11))


