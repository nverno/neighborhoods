mnm <-
function(targets, neighbors, sr, ind.var = "ba") {
    maxn <- maxnebs_disc(targets = targets$id, neighbors = neighbors$id,
                         plots = neighbors$pplot, stat = neighbors$stat,
                         bqudx = neighbors$bqudx, bqudy = neighbors$bqudy,
                         time = neighbors$time, sr = sr)

    neighbors$spec <- as.integer(neighbors$spec)
    slp <- unique(neighbors[!is.na(neighbors$slope), "slope"])
    asp <- unique(neighbors[!is.na(neighbors$asp), "asp"])
    matrices <- mnmRcpp(targets = targets$id, targtimes = targets$time,
                        neighbors = neighbors$id,
                        plots = neighbors$pplot, spp = neighbors$spec,
                        stat = neighbors$stat, bqudx = neighbors$bqudx,
                        bqudy = neighbors$bqudy, time = neighbors$time,
                        variable = neighbors[,ind.var], sr = sr, maxn = maxn,
                        z = neighbors$z, slp = slp, asp = asp)

    return ( matrices )
}
