### Look for meaning in neighborhood fits
source("functions.R")
fits <- read.csv("neighbor-fits.csv")

### Which fits are the most significant???
# filter out 98 ht classes
dropnebs <- grep("98", levels(fits$neighbor))
dfits <- fits[

subset(
