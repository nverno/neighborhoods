### Create neighbor values
### Values to create:
#
# DBH-based: max neighbor DBH (nmaxdbh), max neighbor BA (nmaxba), total sum of
#  neighbor BA >= 0 (NSUMBA), sum of neighbor BA where DBH>=5 cm (nsumba5.),
#  sum neighbor BA where neighbors >= target (nsumbabig)
#
# Height values only for 98 trees due to incompleteness of other year ht sampling
# Height-based: max neighbor height (nmaxht), total sum neighbor heights (nsumht),
#  sum neighbor heights taller than target (nsumhtbig)

# calculate values for 6 quadrat sized neighborhoods (sr = 2)
# DBH-based:
yrs <- c(86,87,98,10)
sr <- 2
pp <- subset(pp, CLASS=="P" & PPLOT > 3)
targs <- subset(pp, BQUDX < 10 & BQUDX > 1 & BQUDY > 1 & BQUDY < 10)
for(i in yrs) {
# max neighbor dbh (single value)
    cols.yr <- c(paste("DBH",i,sep=""),paste("BA",i,sep=""),paste("STAT",i,sep=""))
    pp.yr <- subset(pp, pp[,cols.yr[3]]=="ALIVE")
    newcol <- apply(targs[targs[cols.yr[3]]=="ALIVE",c("PPLOT","BQUDX","BQUDY","TAG",
                    cols.yr[1])],1, function(x) {
                        nebs <- subset(pp.yr, x["PPLOT"]==PPLOT & x["BQUDX"]<BQUDX+sr &
                                       x["BQUDX"]>BQUDX-sr & x["BQUDY"]<BQUDY+sr &
                                       x["BQUDY"]>BQUDY-sr & x["TAG"]!=TAG)
                        ifelse(nrow(nebs)==0,0,ifelse(max(nebs[,cols.yr[1]], na.rm=T)<=0,
                                       0,max(nebs[,cols.yr[1]],na.rm=T))) })
    targs[,paste("NMAXDBH",i,sep="")] <- NA
    targs[targs[cols.yr[3]]=="ALIVE",paste("NMAXDBH",i,sep="")] <- newcol
                                        # max neighbor BA (single value)
    newcol <- apply(targs[targs[cols.yr[3]]=="ALIVE",c("PPLOT","BQUDX","BQUDY","TAG",
                    cols.yr[2])],1, function(x) {
                        nebs <- subset(pp.yr, x["PPLOT"]==PPLOT & x["BQUDX"]<BQUDX+sr &
                                       x["BQUDX"]>BQUDX-sr & x["BQUDY"]<BQUDY+sr &
                                       x["BQUDY"]>BQUDY-sr & x["TAG"]!=TAG)
                        ifelse(nrow(nebs)==0,0,ifelse(max(nebs[,cols.yr[2]], na.rm=T)<=0,
                                       0,max(nebs[,cols.yr[2]],na.rm=T))) })
    targs[,paste("NMAXBA",i,sep="")] <- NA
    targs[targs[cols.yr[3]]=="ALIVE",paste("NMAXBA",i,sep="")] <- newcol
                                        # summed neighbor BA (all inclusive >= 0)
    newcol <- apply(targs[targs[cols.yr[3]]=="ALIVE",c("PPLOT","BQUDX","BQUDY","TAG",
                    cols.yr[2])],1, function(x) {
                        nebs <- subset(pp.yr, x["PPLOT"]==PPLOT & x["BQUDX"]<BQUDX+sr &
                                       x["BQUDX"]>BQUDX-sr & x["BQUDY"]<BQUDY+sr &
                                       x["BQUDY"]>BQUDY-sr & x["TAG"]!=TAG)
                        ifelse(nrow(nebs)==0,0,ifelse(sum(nebs[,cols.yr[2]], na.rm=T)<=0,
                                       0,sum(nebs[,cols.yr[2]],na.rm=T))) })
    targs[,paste("NSUMBA",i,sep="")] <- NA
    targs[targs[cols.yr[3]]=="ALIVE",paste("NSUMBA",i,sep="")] <- newcol
                                        # summed neighbor BA (all inclusive >= 5)
    newcol <- apply(targs[targs[cols.yr[3]]=="ALIVE",c("PPLOT","BQUDX","BQUDY","TAG",
                    cols.yr[2],cols.yr[1])],1, function(x) {
                        nebs <- subset(pp.yr, x["PPLOT"]==PPLOT & x["BQUDX"]<BQUDX+sr &
                                       x["BQUDX"]>BQUDX-sr & x["BQUDY"]<BQUDY+sr &
                                       x["BQUDY"]>BQUDY-sr & get(cols.yr[1])>=5 &
                                       x["TAG"]!=TAG)
                        ifelse(nrow(nebs)==0,0,ifelse(sum(nebs[,cols.yr[2]], na.rm=T)<=0,
                                       0,sum(nebs[,cols.yr[2]],na.rm=T))) })
    targs[,paste("NSUMBA5.",i,sep="")] <- NA
    targs[targs[cols.yr[3]]=="ALIVE",paste("NSUMBA5.",i,sep="")] <- newcol
                                        # summed neighbor BA (neighbors > target by DBH)
    newcol <- apply(targs[targs[cols.yr[3]]=="ALIVE",c("PPLOT","BQUDX","BQUDY","TAG",
                    cols.yr[2],cols.yr[1])],1, function(x) {
                        nebs <- subset(pp.yr, x["PPLOT"]==PPLOT & x["BQUDX"]<BQUDX+sr &
                                       x["BQUDX"]>BQUDX-sr & x["BQUDY"]<BQUDY+sr &
                                       x["BQUDY"]>BQUDY-sr &
                                       get(cols.yr[1])>=x[cols.yr[1]] & x["TAG"]!=TAG)
                        ifelse(nrow(nebs)==0,0,ifelse(sum(nebs[,cols.yr[2]], na.rm=T)<=0,
                                       0,sum(nebs[,cols.yr[2]],na.rm=T))) })
    targs[,paste("NSUMBABIG",i,sep="")] <- NA
    targs[targs[cols.yr[3]]=="ALIVE",paste("NSUMBABIG",i,sep="")] <- newcol
}

#######################################################################################
#
#  Height-based calculations
#
yrs <- c(98)
for(i in yrs) {
                                        # max neighbor height (single value)
    cols.yr <- c(paste("HT",i,sep=""),paste("STAT",i,sep=""))
    pp.yr <- subset(pp, get(cols.yr[2])=="ALIVE")
    newcol <- apply(subset(targs, get(cols.yr[2])=="ALIVE",
                    select = c("PPLOT","BQUDX","BQUDY","TAG",cols.yr[1])) , 1,
                    function(x) {
                        nebs <- subset(pp.yr, x["PPLOT"]==PPLOT & x["BQUDX"]<BQUDX+sr &
                                       x["BQUDX"]>BQUDX-sr & x["BQUDY"]<BQUDY+sr &
                                       x["BQUDY"]>BQUDY-sr & x["TAG"]!=TAG)
                        ifelse(nrow(nebs)==0,0,ifelse(max(nebs[,cols.yr[1]], na.rm=T)<=0,
                                       0,max(nebs[,cols.yr[1]],na.rm=T))) })
    targs[,paste("NMAXHT",i,sep="")] <- NA
    targs[targs[,cols.yr[2]]=="ALIVE",paste("NMAXHT",i,sep="")] <- newcol
                                        # sum all neighbor heights
    newcol <- apply(subset(targs, get(cols.yr[2])=="ALIVE",
                    select = c("PPLOT","BQUDX","BQUDY","TAG",cols.yr[1])) , 1,
                    function(x) {
                        nebs <- subset(pp.yr, x["PPLOT"]==PPLOT & x["BQUDX"]<BQUDX+sr &
                                       x["BQUDX"]>BQUDX-sr & x["BQUDY"]<BQUDY+sr &
                                       x["BQUDY"]>BQUDY-sr & x["TAG"]!=TAG)
                        ifelse(nrow(nebs)==0,0,ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
                                       0,sum(nebs[,cols.yr[1]],na.rm=T))) })
    targs[,paste("NSUMHT",i,sep="")] <- NA
    targs[targs[,cols.yr[2]]=="ALIVE",paste("NSUMHT",i,sep="")] <- newcol
                                        # sum neighbor heights >= target
    newcol <- apply(subset(targs, get(cols.yr[2])=="ALIVE",
                    select = c("PPLOT","BQUDX","BQUDY","TAG",cols.yr[1])) , 1,
                    function(x) {
                        nebs <- subset(pp.yr, x["PPLOT"]==PPLOT & x["BQUDX"]<BQUDX+sr &
                                       x["BQUDX"]>BQUDX-sr & x["BQUDY"]<BQUDY+sr &
                                       x["BQUDY"]>BQUDY-sr & get(cols.yr[1])>=
                                       x[cols.yr[1]] & x["TAG"]!=TAG)
                        ifelse(nrow(nebs)==0,0,ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
                                       0,sum(nebs[,cols.yr[1]],na.rm=T))) })
    targs[,paste("NSUMHTBIG",i,sep="")] <- NA
    targs[targs[,cols.yr[2]]=="ALIVE",paste("NSUMHTBIG",i,sep="")] <- newcol
}

## check values
# if alive with dbh, should have DBH based n-value>=0
lapply(subset(targs, !is.na(DBH98) & STAT98=="ALIVE",
              select = c("NMAXDBH98","NMAXBA98","NSUMBA98","NSUMBA5.98",
                  "NSUMBABIG98")), function(x) length(x[is.na(x)]))

## write
names(targs)
## new <- targs[ -c(1) ]
write.csv(targs, 'targs-sr2.csv')
