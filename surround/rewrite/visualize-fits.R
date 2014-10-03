################################################################################
##
##                     Visualize Crown Dimension Models
##
################################################################################
source("~/work/ecodatascripts/vars/crowns/model/fit-models.R")

################################################################################
##
##                         Examine Crown Area Models
##
################################################################################
## ABBA, PIRU, BECO/BEAL/BEPA fits
par(mfrow = c(3,1))
elev <- c("L", "M", "H")
spec <- c("ABBA", "PIRU", "BECO")
for (spp in spec) {
    dev.new()
    par(mfrow = c(3,1))
    for (ee in elev) {
        dd <- dat[dat$elevcl == ee & dat$spec == spp, ]
        plot(dd$ba, dd$crarea, main = paste("Crown Area Prediction:", spp, "at", ee, "Elevation"))
        points(dd$ba, predict(crwnAreaMods[[ee]][[tolower(spp)]],
                              newdata = dd), col = "red")
    }
}

## For hardwoods in general
par(mfrow = c(3,1))
for (ee in elev) {
    dd <- droplevels(dat[dat$elevcl == ee & dat$spec %in% hardwoods, ])
    dd$specint <- dd$spec
    levels(dd$specint) <- c(1:length(levels(dd$specint)))
    plot(dd$ba, dd$crarea, main = paste("Crown Area Prediction: Hardwoods at", ee, "Elevation"),
         pch = as.integer(dd$specint))
    points(dd$ba, predict(crwnAreaMods[[ee]][["hard"]], newdata = dd), col = "red",
           pch = as.integer(dd$specint))
    legend("bottomright", legend = paste0(hardwoods, ": ", table(dd$spec)),
           pch = as.integer(unique(levels(dd$specint))))
}

################################################################################
##
##                        Examine Crown Depth Models
##
################################################################################
## ABBA, PIRU, BECO/BEAL/BEPA fits
par(mfrow = c(3,1))
elev <- c("L", "M", "H")
spec <- c("ABBA", "PIRU", "BECO")
for (spp in spec) {
    dev.new()
    par(mfrow = c(3,1))
    for (ee in elev) {
        dd <- dat[dat$elevcl == ee & dat$spec == spp, ]
        plot(dd$ba, dd$crdepth, main = paste("Crown Depth Prediction:", spp, "at", ee, "Elevation"))
        points(dd$ba, predict(crwnDepthMods[[ee]][[tolower(spp)]],
                              newdata = dd), col = "red")
    }
}

## For hardwoods in general
par(mfrow = c(3,1))
for (ee in elev) {
    dd <- droplevels(dat[dat$elevcl == ee & dat$spec %in% hardwoods, ])
    dd$specint <- dd$spec
    levels(dd$specint) <- c(1:length(levels(dd$specint)))
    plot(dd$ba, dd$crdepth, main = paste("Crown Depth Prediction: Hardwoods at", ee, "Elevation"),
         pch = as.integer(dd$specint))
    points(dd$ba, predict(crwnDepthMods[[ee]][["hard"]], newdata = dd), col = "red",
           pch = as.integer(dd$specint))
    legend("bottomright", legend = paste0(hardwoods, ": ", table(dd$spec)),
           pch = as.integer(unique(levels(dd$specint))))
}
