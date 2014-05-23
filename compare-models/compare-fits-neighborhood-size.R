
R version 3.0.1 (2013-05-16) -- "Good Sport"
Copyright (C) 2013 The R Foundation for Statistical Computing
Platform: i386-w64-mingw32/i386 (32-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> > options(chmhelp=FALSE, help_type="text")
> options(STERM='iESS', str.dendrogram.last="'", editor='emacsclient.exe', show.error.locations=TRUE)
> source("allometry.R")
Loading required package: MASS
Loading required package: outliers
------------------------------------------
If you use this package, please cite the recent paper containing description of this software:
Komsta, L. Chemometric and statistical evaluation of calibration curves in pharmaceutical analysis 
           - a short review on trends and recommendations. J. AOAC Int. 2012, 95, 3, 669-672.
------------------------------------------
Loading required package: mgcv
This is mgcv 1.7-22. For overview type 'help("mgcv-package")'.
Loading required package: deldir
deldir 0.0-22
spatstat 1.31-2 
Type 'help(spatstat)' for an overview of spatstat 
     'latest.news()' for news on latest version 
     'licence.polygons()' for licence information on polygon calculations
Loading required package: XML
Loading required package: zoo

Attaching package: 'zoo'

The following object is masked from 'package:base':

    as.Date, as.Date.numeric

> pp <- subset(pp, CLASS=="P" & PPLOT > 3)
> srs <- c(1,2,3)
> yrs <- c(86,87,98,10)
> stime <- Sys.time()
> # make different neighborhood columns for each combination of yr, sr, and
> #  neighborhood summing method
> #  current runtime: ~ 4 minutes.  Could be faster if neigborhood summing operations
> #   were condensed into one apply function
> stime <- Sys.time()
> makecolumuns <- lapply(srs, function(a) {
+     lapply(yrs, function(b) {
+         cols.yr <- c(paste0("BA",b),paste0("STAT",b),paste0("DBH",b))
+                                         # define targets
+         targs <- with(pp, which(BQUDX < (12-a) & BQUDX > (-1+a) &
+                                 BQUDY > (-1+a) & BQUDY < (12-a) &
+                                 pp[,cols.yr[2]]=="ALIVE"))
+                                         # define neighbors total set of neighbors
+         neighbors <- pp[pp[,cols.yr[2]]=="ALIVE",]
+                                         # Neighborhood method 1: total neighbor BA
+         pp[,paste("NSUMBA",a,b,sep=".")] <<- rep(NA, nrow(pp))
+         pp[targs,paste("NSUMBA",a,b,sep=".")] <<-
+             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors,
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
+                                         # Neighborhood method 2: sum neighbor BA > 5 dbh
+         pp[,paste("NSUMBA5",a,b,sep=".")] <<- rep(NA, nrow(pp))
+         pp[targs,paste("NSUMBA5",a,b,sep=".")] <<-
+             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors,
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        get(cols.yr[3])>=5 & x[["BQUDY"]]>BQUDY-a &
+                                        x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
+                                         # Neighborhood method 3: sum neighbor BA > focal
+         pp[,paste("NSUMBABIG",a,b,sep=".")] <<- rep(NA, nrow(pp))
+         pp[targs,paste("NSUMBABIG",a,b,sep=".")] <<-
+             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors,
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        cols.yr[3]>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
+     })
+ })
> etime <- Sys.time()
> etime-stime # ~4 minute runtime
Time difference of 16.2847 mins
> ## check values
> # if alive with dbh, should have DBH based n-value>=0
> targs <- with(pp, pp[which(BQUDX < (11) & BQUDX > (0) &
+                            BQUDY > (0) & BQUDY < (11)),])
> lapply(subset(targs, !is.na(DBH98) & STAT98=="ALIVE",
+               select = names(targs)[
+               grep("^N[[:print:]]*98$",names(targs))]),
+        function(x) length(x[is.na(x)]))
$NOTE98
[1] 0

$NSUMBA.1.98
[1] 0

$NSUMBA5.1.98
[1] 0

$NSUMBABIG.1.98
[1] 0

$NSUMBA.2.98
[1] 1799

$NSUMBA5.2.98
[1] 1799

$NSUMBABIG.2.98
[1] 1799

$NSUMBA.3.98
[1] 3152

$NSUMBA5.3.98
[1] 3152

$NSUMBABIG.3.98
[1] 3152

> 
> nrow(subset(targs, !is.na(DBH98) & !is.na(NSUMBABIG.3.98)))
[1] 1764
> range(targs$NSUMBABIG.3.98, na.rm=T)
[1] 0 0
> range(targs$NSUMBA5.3.98, na.rm=T)
[1] 0.05774418 0.78597020
> a
Error: object 'a' not found
> b
Error: object 'b' not found
> a <- 1
> b <- 86
> 
>         cols.yr <- c(paste0("BA",b),paste0("STAT",b),paste0("DBH",b))
> targs <- with(pp, which(BQUDX < (12-a) & BQUDX > (-1+a) &
+                                 BQUDY > (-1+a) & BQUDY < (12-a) &
+                                 pp[,cols.yr[2]]=="ALIVE"))
>                                         # define neighbors total set of neighbors
>         neighbors <- pp[pp[,cols.yr[2]]=="ALIVE",]
> cols.yr
[1] "BA86"   "STAT86" "DBH86" 
> nrow(neighbors)
[1] 1364
> x <- pp[targs,][1,]
> class(x)
[1] "data.frame"
> x[["PPLOT"]]
[1] 4
> nebs <- with(neighbors,
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        cols.yr[3]>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
Error in `[.data.frame`(neighbors, PPLOT == x[["PPLOT"]] & x[["BQUDX"]] <  : 
  dims [product 1] do not match the length of object [1364]
> with(neighbors, nebs <- 
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        cols.yr[3]>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
Error in `[.data.frame`(neighbors, PPLOT == x[["PPLOT"]] & x[["BQUDX"]] <  : 
  dims [product 1] do not match the length of object [1364]
> a
[1] 1
> apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
+                                         # define neighbors for individuals
+                 with(neighbors, nebs <- 
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        cols.yr[3]>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+ ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
Error in nrow(nebs) (from #8) : object 'nebs' not found
> test <-             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors, 
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        cols.yr[3]>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
> length(test)
[1] 1362
> head(test)
386 388 390 391 392 489 
  0   0   0   0   0   0 
> test <-             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors, 
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        get(cols.yr[3])>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
> head(test)
386 388 390 391 392 489 
  0   0   0   0   0   0 
> test <-             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors, 
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        targs[,cols.yr[3]]>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
Error in targs[, cols.yr[3]] (from #3) : incorrect number of dimensions
> test <-             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG","DBH")], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors, 
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        targs[,cols.yr[3]]>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
Error in `[.data.frame`(pp, targs, c("PPLOT", "BQUDX", "BQUDY", "TAG",  : 
  undefined columns selected
> test <-             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG",cols.yr[3])], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors, 
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        targs[,cols.yr[3]]>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
Error in targs[, cols.yr[3]] (from #3) : incorrect number of dimensions
> test <-             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG",cols.yr[3])], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors, 
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        get(cols.yr[3])>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
> head(test)
       386        388        390        391        392        489 
0.02269806 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 
> head(test,20)
       386        388        390        391        392        489        490 
0.02269806 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 
       491        492        493        494        564        565        566 
0.00000000 0.00000000 0.00000000 0.00000000 0.03048059 0.00000000 0.00000000 
       567        568        569        571        572        573 
0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 
> head(test,40)
        386         388         390         391         392         489 
0.022698060 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 
        490         491         492         493         494         564 
0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.030480589 
        565         566         567         568         569         571 
0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 
        572         573         630         631         632         633 
0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 
        635         662         665         668         669         670 
0.000000000 0.000000000 0.000000000 0.011309760 0.000000000 0.000000000 
        673         674         676         677         679         680 
0.070686000 0.000000000 0.015639670 0.000000000 0.000000000 0.000000000 
        681         682         683         684 
0.008992045 0.000000000 0.000000000 0.000000000 
> head(test,100)
        386         388         390         391         392         489 
0.022698060 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 
        490         491         492         493         494         564 
0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.030480589 
        565         566         567         568         569         571 
0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 
        572         573         630         631         632         633 
0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 
        635         662         665         668         669         670 
0.000000000 0.000000000 0.000000000 0.011309760 0.000000000 0.000000000 
        673         674         676         677         679         680 
0.070686000 0.000000000 0.015639670 0.000000000 0.000000000 0.000000000 
        681         682         683         684         762         764 
0.008992045 0.000000000 0.000000000 0.000000000 0.015393840 0.000000000 
        765         766         767         768         769         770 
0.013684810 0.000000000 0.000000000 0.021614993 0.000000000 0.000000000 
        772         775         776         777         778         779 
0.039603795 0.000000000 0.000000000 0.034966793 0.022167130 0.000000000 
        780         782         828         830         831         832 
0.000000000 0.000000000 0.000000000 0.013478249 0.000000000 0.007088235 
        833         834         835         836         837         838 
0.000000000 0.000000000 0.000000000 0.032047462 0.000000000 0.000000000 
        839         840         889         890         891         894 
0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 
        895         896         897         898         899         900 
0.000000000 0.000000000 0.000000000 0.071037859 0.074056937 0.000000000 
        901         902         976         977         978         979 
0.000000000 0.066966346 0.000000000 0.000000000 0.036305115 0.000000000 
        980         981         982         983         984         986 
0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 
       1018        1019        1020        1021        1022        1024 
0.000000000 0.027759178 0.041547660 0.000000000 0.000000000 0.000000000 
       1025        1057        1059        1100 
0.000000000 0.000000000 0.000000000 0.000000000 
> length(test[test>0])
[1] 642
> a
[1] 1
> a<-3
> test <-             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG",cols.yr[3])], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors, 
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        get(cols.yr[3])>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
> head(test)
      386       388       390       391       392       489 
0.2292975 0.1720214 0.2086219 0.1989018 0.0000000 0.0000000 
> head(test,20)
       386        388        390        391        392        489        490 
0.22929753 0.17202145 0.20862188 0.19890176 0.00000000 0.00000000 0.00000000 
       491        492        493        494        564        565        566 
0.05725566 0.00000000 0.20121163 0.32489171 0.22776521 0.00000000 0.23082199 
       567        568        569        571        572        573 
0.21747019 0.33094557 0.38520493 0.12990280 0.12793616 0.14398424 
> nrow(pp)
[1] 11369
> names(pp)
  [1] "X"              "PPLOT"          "SPLOT"          "TAG"           
  [5] "SPEC"           "CLASS"          "YRTAG"          "YRREC"         
  [9] "YRMORT"         "STAT86"         "STAT87"         "STAT88"        
 [13] "STAT89"         "STAT98"         "STAT10"         "STAT11"        
 [17] "DBH86"          "DECM86"         "HT86"           "CRHT86"        
 [21] "CLONG86"        "CAZLNG86"       "CPERP86"        "DBH87"         
 [25] "HT87"           "CRHT87"         "CPOS87"         "DECM87"        
 [29] "DECW87"         "CLONG87"        "CAZLNG87"       "CPERP87"       
 [33] "CLITOP87"       "HT88"           "CPOS88"         "DECM88"        
 [37] "DECW88"         "WD89OL"         "WD89AV"         "EXWI89"        
 [41] "LVBD89"         "DECM89"         "DBH98"          "HT98"          
 [45] "DECM98"         "DECW98"         "CPOS98"         "COND98"        
 [49] "SNHT98"         "DECAY98"        "AOL98"          "SNAZ98"        
 [53] "HTBCR98"        "HTTCR98"        "CRBASE98"       "WD98"          
 [57] "ICE98"          "DATE98"         "NOTE98"         "DBH10"         
 [61] "HT10"           "DECM10"         "CPOS10"         "COND10"        
 [65] "DECAY10"        "AOL10"          "NOTE10"         "DBH11"         
 [69] "DAH11"          "HT11"           "CII11"          "NOTE11"        
 [73] "ELEV"           "ELEVCL"         "ASPCL"          "ASP"           
 [77] "SOILCL"         "SLOPE8687"      "SLOPCL"         "LAND"          
 [81] "MICR"           "BQUDX"          "BQUDY"          "BQCRDX"        
 [85] "BQCRDY"         "BPCRDX"         "BPCRDY"         "CSAP8788"      
 [89] "CSAP98"         "EX87"           "EX86"           "EX85"          
 [93] "EX84"           "EX83"           "OUT"            "CQUDX86"       
 [97] "CQUDY86"        "CQCRDX86"       "CQCRDY86"       "FLAG"          
[101] "CH98"           "RES98"          "RATIO98"        "RATCAN98"      
[105] "CH10"           "RES10"          "RATIO10"        "RATCAN10"      
[109] "CH87"           "RES87"          "RATIO87"        "RATCAN87"      
[113] "CH86"           "RATIO86"        "RATCAN86"       "MEAS87"        
[117] "BA86"           "BA87"           "BA98"           "BA10"          
[121] "BAGROWTH98"     "BAGROWTH10"     "DBHGROWTH98"    "DBHGROWTH10"   
[125] "HTGROWTH98"     "HTGROWTH10"     "p98dbh"         "p98ht"         
[129] "p10dbh"         "p10ht"          "NSUMBA.1.86"    "NSUMBA5.1.86"  
[133] "NSUMBABIG.1.86" "NSUMBA.1.87"    "NSUMBA5.1.87"   "NSUMBABIG.1.87"
[137] "NSUMBA.1.98"    "NSUMBA5.1.98"   "NSUMBABIG.1.98" "NSUMBA.1.10"   
[141] "NSUMBA5.1.10"   "NSUMBABIG.1.10" "NSUMBA.2.86"    "NSUMBA5.2.86"  
[145] "NSUMBABIG.2.86" "NSUMBA.2.87"    "NSUMBA5.2.87"   "NSUMBABIG.2.87"
[149] "NSUMBA.2.98"    "NSUMBA5.2.98"   "NSUMBABIG.2.98" "NSUMBA.2.10"   
[153] "NSUMBA5.2.10"   "NSUMBABIG.2.10" "NSUMBA.3.86"    "NSUMBA5.3.86"  
[157] "NSUMBABIG.3.86" "NSUMBA.3.87"    "NSUMBA5.3.87"   "NSUMBABIG.3.87"
[161] "NSUMBA.3.98"    "NSUMBA5.3.98"   "NSUMBABIG.3.98" "NSUMBA.3.10"   
[165] "NSUMBA5.3.10"   "NSUMBABIG.3.10"
> srs <- c(1,2,3)
> yrs <- c(86,87,98,10)
> # make different neighborhood columns for each combination of yr, sr, and
> #  neighborhood summing method
> #  current runtime: ~ 16 minutes.  Could be faster if neigborhood summing operations
> #   were condensed into one apply function
> stime <- Sys.time()
> makecolumuns <- lapply(srs, function(a) {
+     lapply(yrs, function(b) {
+         cols.yr <- c(paste0("BA",b),paste0("STAT",b),paste0("DBH",b))
+                                         # define targets
+         targs <- with(pp, which(BQUDX < (12-a) & BQUDX > (-1+a) &
+                                 BQUDY > (-1+a) & BQUDY < (12-a) &
+                                 pp[,cols.yr[2]]=="ALIVE"))
+                                         # define neighbors total set of neighbors
+         neighbors <- pp[pp[,cols.yr[2]]=="ALIVE",]
+                                         # Neighborhood method 1: total neighbor BA
+         pp[,paste("NSUMBA",a,b,sep=".")] <<- rep(NA, nrow(pp))
+         pp[targs,paste("NSUMBA",a,b,sep=".")] <<-
+             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors,
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
+                                         # Neighborhood method 2: sum neighbor BA > 5 dbh
+         pp[,paste("NSUMBA5",a,b,sep=".")] <<- rep(NA, nrow(pp))
+         pp[targs,paste("NSUMBA5",a,b,sep=".")] <<-
+             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors,
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        get(cols.yr[3])>=5 & x[["BQUDY"]]>BQUDY-a &
+                                        x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
+                                         # Neighborhood method 3: sum neighbor BA > focal
+         pp[,paste("NSUMBABIG",a,b,sep=".")] <<- rep(NA, nrow(pp))
+         pp[targs,paste("NSUMBABIG",a,b,sep=".")] <<-
+             apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG",cols.yr[3])], 1, function(x) {
+                                         # define neighbors for individuals
+                 nebs <- with(neighbors,
+                              neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
+                                        x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
+                                        get(cols.yr[3])>=x[cols.yr[3]] &
+                                        x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
+                                         # Sum neighbor BA, 0 if no neighbors
+                 ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
+                            0,sum(nebs[,cols.yr[1]],na.rm=T)))
+             })
+     })
+ })
> etime <- Sys.time()
> etime-stime
Time difference of 19.36799 mins
> range(long$NSUMBABIG.1.98, na.rm=T)
Error: object 'long' not found
> range(pp$NSUMBABIG.1.98, na.rm=T)
[1] 0.0000000 0.1849028
> range(pp$NSUMBABIG.3.98, na.rm=T)
[1] 0.0000000 0.7552469
> range(pp$NSUMBABIG.2.98, na.rm=T)
[1] 0.0000000 0.3795948
> ## check values
> # if alive with dbh, should have DBH based n-value>=0
> targs <- with(pp, pp[which(BQUDX < (11) & BQUDX > (0) &
+                            BQUDY > (0) & BQUDY < (11)),])
> lapply(subset(targs, !is.na(DBH98) & STAT98=="ALIVE",
+               select = names(targs)[
+               grep("^N[[:print:]]*98$",names(targs))]),
+        function(x) length(x[is.na(x)]))
$NOTE98
[1] 0

$NSUMBA.1.98
[1] 0

$NSUMBA5.1.98
[1] 0

$NSUMBABIG.1.98
[1] 0

$NSUMBA.2.98
[1] 1799

$NSUMBA5.2.98
[1] 1799

$NSUMBABIG.2.98
[1] 1799

$NSUMBA.3.98
[1] 3152

$NSUMBA5.3.98
[1] 3152

$NSUMBABIG.3.98
[1] 3152

> nrow(subset(targs, !is.na(DBH98) & !is.na(NSUMBABIG.3.98)))
[1] 1764
> write.csv(pp, 'targs-sr2.csv')
> targs <- read.csv("targs-sr2.csv")
> targs <- subset(targs, PPLOT>3 & ASPCL!="")
> names(targs) <- tolower(names(targs))
> 
> # make some dummy columns
> targs$dbhgrowth86 <- NA
> targs$dbhgrowth87 <- NA
> targs$bagrowth86 <- NA
> targs$bagrowth87 <- NA
> targs$htgrowth86 <- NA
> targs$htgrowth87 <- NA
> targs$cpos86 <- NA
> # reshape
> long <- reshape(targs, v.names = c("nsumba.1"), direction = "long")
Error in reshape(targs, v.names = c("nsumba.1"), direction = "long") : 
  no 'reshapeWide' attribute, must specify 'varying'
> long <- reshape(targs,varying=
+                 list(
+                     c(grep("nsumba.1",names(targs))),
+                     c(grep("nsumba.2",names(targs))),
+                     c(grep("nsumba.3",names(targs))),
+                     c(grep("nsumba5.1",names(targs))),
+                     c(grep("nsumba5.2",names(targs))),
+                     c(grep("nsumba5.3",names(targs))),
+                     c(grep("nsumbabig.1",names(targs))),
+                     c(grep("nsumbabig.2",names(targs))),
+                     c(grep("nsumbabig.3",names(targs))),
+                     c("dbh86","dbh87","dbh98","dbh10"),
+                     c("ht86","ht87","ht98","ht10"),
+                     c("dbhgrowth86","dbhgrowth87", "dbhgrowth98","dbhgrowth10"),
+                     c("bagrowth86","bagrowth87","bagrowth98","bagrowth10"),
+                     c("htgrowth86","htgrowth87","htgrowth98","htgrowth10"),
+                     c("stat86","stat87","stat98","stat10"),
+                     c("cpos86","cpos87","cpos98","cpos10"),
+                     c("ch86","ch87","ch98","ch10"),
+                     c("ba86","ba87","ba98","ba10")),
+                 times=c(1986,1987,1998,2010),
+                 direction = "long",
+                 v.names=c("nsumba.1","nsumba.2","nsumba.3","nsumba5.1",
+                 "nsumba5.2","nsumba5.3","nsumbabig.1","nsumbabig.2","nsumbabig.3",
+                 "dbh","ht", "dbhgrowth","bagrowth","htgrowth","stat","cpos","ch","ba"))
> 
> # remove some unnecessary columns
> indices <- match( c("pplot","splot","tag","spec","yrmort","elev","elevcl",
+                     "aspcl","asp","soilcl","slopcl","bqudx","bqudy"), names(long))
> fromhere <- match("meas87",names(long))
> toend <- length(names(long))
> tokeep <- c(indices, fromhere:toend)
> long <- subset(long, select = tokeep)
> 
> # remove rows that have no dbh or ht data
> long <- subset(long, !is.na(dbh) | !is.na(ht))
> 
> # mortality column
> long$yrmort <- factor(long$yrmort)
> long$mort <- !is.na(long$yrmort)
> names(long)
 [1] "pplot"       "splot"       "tag"         "spec"        "yrmort"     
 [6] "elev"        "elevcl"      "aspcl"       "asp"         "soilcl"     
[11] "slopcl"      "bqudx"       "bqudy"       "meas87"      "p98dbh"     
[16] "p98ht"       "p10dbh"      "p10ht"       "time"        "nsumba.1"   
[21] "nsumba.2"    "nsumba.3"    "nsumba5.1"   "nsumba5.2"   "nsumba5.3"  
[26] "nsumbabig.1" "nsumbabig.2" "nsumbabig.3" "dbh"         "ht"         
[31] "dbhgrowth"   "bagrowth"    "htgrowth"    "stat"        "cpos"       
[36] "ch"          "ba"          "id"          "mort"       
> range(long$nsumbabig.3,na.rm=T)
[1] 0.0000000 0.7552469
> write.csv(long, "long-sr2.csv")
> long <- read.csv("long-sr2.csv")
> 
> ### make size classes for all neighborhood values, dbh, ht, and ba
> # columns to make classes from:
> base3 <- which(names(long)=="dbh" | names(long)=="ht" | names(long)=="ba")
> 
> # drop previously made class columns
> drops <- "^n[[:alpha:]]*[[:digit:]]*cl$|clq$|growthcl"
> todrop <- grep(drops, names(long))
> long <- subset(long, select = !names(long)%in%names(long)[todrop])
> 
> pattern <- "^n|growth"
> classcols <- grep(pattern, names(long))
> classcols <- classcols[!classcols %in%
+                        c(grep("98",names(long)), grep("htgrowth", names(long)))]
> 
> ### QUANTILE-based size classes, excluding trees with 0 dbh/ht/ba
> # exclude 0 from classes for base3 (ht, dbh, ba)
> # choose number of breaks, species, plots
> nbreaks <- 3
> plots <- 4:27
> long <- subset(long, spec=="ABBA" & stat=="ALIVE")
> # order long by pplot
> long <- long[order(long$pplot),]
> long <- long[complete.cases(long$dbh, long$ht),] # check for missing ht/dbh
> nrow(long)
[1] 6476
> tt <- sapply(names(long)[classcols], function(x) {
+     newname <- paste0(x, "clq2")
+     long[,newname] <<- unlist(
+         sapply(plots, function(d) {
+             sub <- long[long$pplot==d,x]
+             as.character(quantclass(sub,nbreaks))
+         }))
+ })
Error in cut.default(var, breaks = quantile(var, probs = seq(0, 1, 1/numbreaks),  (from functions.R#61) : 
  'breaks' are not unique
> # classes by set class width (as a fraction of maximum value)
> tt <- sapply(names(long)[classcols], function(x) {
+     newname <- paste0(x, "cl")
+     long[,newname] <<- unlist(
+         sapply(plots, function(d) {
+             sub <- long[long$pplot==d,x]
+             as.character(sizeclasses(sub,nbreaks))
+         }))
+ })
> ### classes by set class width (as a fraction of maximum value)
> tt <- sapply(names(long)[classcols], function(x) {
+     newname <- paste0(x, "cl2")
+     long[,newname] <<- unlist(
+         sapply(plots, function(d) {
+             sub <- long[long$pplot==d,x]
+             as.character(sizeclasses(sub,nbreaks))
+         }))
+ })
> 
> ### make sclasses with labels 1:3
> tt <- sapply(names(long)[classcols], function(x) {
+     newname <- paste0(x, "cl")
+     long[,newname] <<- unlist(
+         sapply(plots, function(d) {
+             sub <- long[long$pplot==d,x]
+             as.numeric(sizeclasses(sub,nbreaks))
+         }))
+ })
> names(long)
 [1] "X"              "pplot"          "splot"          "tag"           
 [5] "spec"           "yrmort"         "elev"           "elevcl"        
 [9] "aspcl"          "asp"            "soilcl"         "slopcl"        
[13] "bqudx"          "bqudy"          "meas87"         "p98dbh"        
[17] "p98ht"          "p10dbh"         "p10ht"          "time"          
[21] "nsumba.1"       "nsumba.2"       "nsumba.3"       "nsumba5.1"     
[25] "nsumba5.2"      "nsumba5.3"      "nsumbabig.1"    "nsumbabig.2"   
[29] "nsumbabig.3"    "dbh"            "ht"             "dbhgrowth"     
[33] "bagrowth"       "htgrowth"       "stat"           "cpos"          
[37] "ch"             "ba"             "id"             "mort"          
[41] "nsumba.1cl"     "nsumba.2cl"     "nsumba.3cl"     "nsumba5.1cl"   
[45] "nsumba5.2cl"    "nsumba5.3cl"    "nsumbabig.1cl"  "nsumbabig.2cl" 
[49] "nsumbabig.3cl"  "dbhgrowthcl"    "bagrowthcl"     "nsumba.1cl2"   
[53] "nsumba.2cl2"    "nsumba.3cl2"    "nsumba5.1cl2"   "nsumba5.2cl2"  
[57] "nsumba5.3cl2"   "nsumbabig.1cl2" "nsumbabig.2cl2" "nsumbabig.3cl2"
[61] "dbhgrowthcl2"   "bagrowthcl2"   
> write.csv(long,"long-ABBA-classes-byplot.csv")
> dat <- read.csv("long-ABBA-classes-byplot.csv")
> dat <- subset(dat, spec=="ABBA" & stat=="ALIVE")
> dat <- dat[complete.cases(dat$dbh, dat$ht),]
> nrow(dat)
[1] 6476
> clqcols <- grep("cl$", names(dat))
> # function to extract model AICs
> getAIC <- function(list_of_models) {
+     ldply(list_of_models,
+           function(model) {
+               c(
+                   aic = extractAIC(model)
+                   )
+           })
+ }
> 
> # function to get mse
> getMSE <- function(list_of_models) {
+     ldply(list_of_models,
+           function(model) {
+               c(
+                   mse = mean(residuals(model)^2)
+                   )
+           })
+ }
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             lm(l$ht ~ poly(l$dbh, 2))
+         })
+     }), recursive = FALSE)
+     c(assign(x, sum(getAIC(models_list)$aic2), envir=parent.frame()),
+       assign(x, sum(getMSE(models_list)$mse/72)))
+ })
Error in poly(l$dbh, 2) (from #5) : 
  'degree' must be less than number of unique points
> 
> tt
        nsumba.1 nsumba.2 nsumba.3 nsumba5.1 nsumba5.2 nsumba5.3 nsumbabig.1
   [1,]        1       NA       NA         1        NA        NA           1
   [2,]        1        1        2         1         1         2           1
   [3,]        1        1       NA         1         1        NA           1
   [4,]        1       NA       NA         1        NA        NA           1
   [5,]        1        1        2         1         1         2           1
   [6,]        1       NA       NA         1        NA        NA           1
   [7,]        1        1        2         1         1         2           1
   [8,]        1        2        3         1         2         3           1
   [9,]        1        2       NA         1         2        NA           1
  [10,]        1        2        2         1         2         2           1
  [11,]        1        1        2         1         1         2           1
  [12,]        1        2       NA         1         2        NA           1
  [13,]        1        1        2         1         1         2           1
  [14,]        1       NA       NA         1        NA        NA           1
  [15,]        1       NA       NA         1        NA        NA           1
  [16,]        1       NA       NA         1        NA        NA           1
  [17,]        1       NA       NA         1        NA        NA           1
  [18,]        1       NA       NA         1        NA        NA           1
  [19,]        1       NA       NA         1        NA        NA           1
  [20,]        1        1        3         1         1         3           1
  [21,]        1        1       NA         1         1        NA           1
  [22,]        1       NA       NA         1        NA        NA           1
  [23,]        1       NA       NA         1        NA        NA           1
  [24,]        1       NA       NA         1        NA        NA           1
  [25,]        3       NA       NA         3        NA        NA           3
  [26,]        1       NA       NA         1        NA        NA           1
  [27,]        1       NA       NA         1        NA        NA           1
  [28,]        1       NA       NA         1        NA        NA           1
  [29,]        1       NA       NA         1        NA        NA           1
  [30,]        1        2       NA         1         2        NA           1
  [31,]        3       NA       NA         3        NA        NA           3
  [32,]        1       NA       NA         1        NA        NA           1
  [33,]        3       NA       NA         3        NA        NA           3
  [34,]        1        1        3         1         1         3           1
  [35,]        1        1        1         1         1         1           1
  [36,]        1        3       NA         1         3        NA           1
  [37,]        1        1       NA         1         1        NA           1
  [38,]        1        1        3         1         1         3           1
  [39,]        1        1       NA         1         1        NA           1
  [40,]        1       NA       NA         1        NA        NA           1
  [41,]        1        1        3         1         1         3           1
  [42,]        1        1        2         1         1         2           1
  [43,]        1        1        1         1         1         1           1
  [44,]        2       NA       NA         2        NA        NA           2
  [45,]        1       NA       NA         1        NA        NA           1
  [46,]        2        2       NA         2         2        NA           2
  [47,]        2        2       NA         2         2        NA           2
  [48,]        2        2       NA         2         2        NA           2
  [49,]        1       NA       NA         1        NA        NA           1
  [50,]        2        2       NA         2         2        NA           2
  [51,]        1        2        3         1         2         3           1
  [52,]        1        2        3         1         2         3           1
  [53,]        1       NA       NA         1        NA        NA           1
  [54,]        1        2        3         1         2         2           1
  [55,]        1        2        3         1         2         3           1
  [56,]        2        2       NA         2         2        NA           2
  [57,]        1       NA       NA         1        NA        NA           1
  [58,]        2       NA       NA         2        NA        NA           2
  [59,]        1        2       NA         1         2        NA           1
  [60,]        1       NA       NA         1        NA        NA           1
  [61,]        2        2       NA         2         2        NA           2
  [62,]        2        2       NA         2         2        NA           2
  [63,]        1       NA       NA         1        NA        NA           1
  [64,]        1       NA       NA         1        NA        NA           1
  [65,]        1       NA       NA         1        NA        NA           1
  [66,]        1        2       NA         1         2        NA           1
  [67,]        1        1        2         1         1         2           1
  [68,]        1        2       NA         1         2        NA           1
  [69,]        1       NA       NA         1        NA        NA           1
  [70,]        1       NA       NA         1        NA        NA           1
  [71,]        1        2        3         1         2         3           1
  [72,]        1       NA       NA         1        NA        NA           1
  [73,]        1       NA       NA         1        NA        NA           1
  [74,]        1       NA       NA         1        NA        NA           1
  [75,]        1        1        3         1         1         3           1
  [76,]        1       NA       NA         1        NA        NA           1
  [77,]        3       NA       NA         3        NA        NA           3
  [78,]        1       NA       NA         1        NA        NA           1
  [79,]        1        1       NA         1         1        NA           1
  [80,]        1       NA       NA         1        NA        NA           1
  [81,]        1        1       NA         1         1        NA           1
  [82,]        1        1       NA         1         1        NA           1
  [83,]        1       NA       NA         1        NA        NA           1
  [84,]        1       NA       NA         1        NA        NA           1
  [85,]        1        1       NA         1         1        NA           1
  [86,]        1        1        3         1         1         3           1
  [87,]        1       NA       NA         1        NA        NA           1
  [88,]        1       NA       NA         1        NA        NA           1
  [89,]        1       NA       NA         1        NA        NA           1
  [90,]        1        1        3         1         1         3           1
  [91,]        1        1       NA         1         1        NA           1
  [92,]        1       NA       NA         1        NA        NA           1
  [93,]        1        1        3         1         1         3           1
  [94,]        1        1        2         1         1         2           1
  [95,]        1       NA       NA         1        NA        NA           1
  [96,]        1       NA       NA         1        NA        NA           1
  [97,]        1        1       NA         1         1        NA           1
  [98,]        1        1        2         1         1         2           1
  [99,]        1       NA       NA         1        NA        NA           1
 [100,]        1       NA       NA         1        NA        NA           1
 [101,]        1       NA       NA         1        NA        NA           1
 [102,]        1       NA       NA         1        NA        NA           1
 [103,]        1        1        1         1         1         1           1
 [104,]        1        3       NA         1         3        NA           1
 [105,]        2        1        3         2         1         3           2
 [106,]        1        1       NA         1         1        NA           1
 [107,]        1       NA       NA         1        NA        NA           1
 [108,]        2        1        3         2         1         3           2
 [109,]        1        1        2         1         1         2           1
 [110,]        1        1        1         1         1         1           1
 [111,]        2        2       NA         2         2        NA           2
 [112,]        2        2       NA         2         2        NA           2
 [113,]        2        2       NA         2         2        NA           2
 [114,]        1        1        3         1         1         3           1
 [115,]        1       NA       NA         1        NA        NA           1
 [116,]        1       NA       NA         1        NA        NA           1
 [117,]        2        2       NA         2         2        NA           2
 [118,]        1        2       NA         1         2        NA           1
 [119,]        1       NA       NA         1        NA        NA           1
 [120,]        2        2       NA         2         2        NA           2
 [121,]        2        2       NA         2         2        NA           2
 [122,]        1       NA       NA         1        NA        NA           1
 [123,]        1       NA       NA         1        NA        NA           1
 [124,]        1       NA       NA         1        NA        NA           1
 [125,]        1        2       NA         1         2        NA           1
 [126,]        1        1        2         1         1         2           1
 [127,]        1        2       NA         1         2        NA           1
 [128,]        1       NA       NA         1        NA        NA           1
 [129,]        1       NA       NA         1        NA        NA           1
 [130,]        1        2        3         1         2         3           1
 [131,]        1       NA       NA         1        NA        NA           1
 [132,]        1       NA       NA         1        NA        NA           1
 [133,]        1        1        3         1         1         3           1
 [134,]        1       NA       NA         1        NA        NA           1
 [135,]        1        1        2         1         1         2           1
 [136,]        1        1       NA         1         2        NA           1
 [137,]        1       NA       NA         1        NA        NA           1
 [138,]        1       NA       NA         1        NA        NA           1
 [139,]        1        2       NA         1         2        NA           1
 [140,]        1        2       NA         1         2        NA           1
 [141,]        1        2       NA         1         2        NA           1
 [142,]        1        2       NA         1         2        NA           1
 [143,]        1        2        2         1         2         2           1
 [144,]        1        2       NA         1         2        NA           1
 [145,]        1        2        2         1         2         2           1
 [146,]        1        1        2         1         1         2           1
 [147,]        1        1        2         1         1         2           1
 [148,]        1       NA       NA         1        NA        NA           1
 [149,]        1        1        2         1         1         2           1
 [150,]        1        2        2         1         2         2           1
 [151,]        1        1       NA         1         1        NA           1
 [152,]        1       NA       NA         1        NA        NA           1
 [153,]        1       NA       NA         1        NA        NA           1
 [154,]        1        1        2         1         1         2           1
 [155,]        1        1        2         1         1         2           1
 [156,]        1        1        2         1         1         2           1
 [157,]        1        1        2         1         1         2           1
 [158,]        1        1        1         1         1         2           1
 [159,]        1       NA       NA         1        NA        NA           1
 [160,]        1        2        2         1         2         2           1
 [161,]        1       NA       NA         1        NA        NA           1
 [162,]        1        1       NA         1         1        NA           1
 [163,]        1        1        3         1         1         3           1
 [164,]        2        1        2         2         1         2           2
 [165,]        1        2       NA         1         2        NA           1
 [166,]        2        1        2         2         1         2           2
 [167,]        1        2       NA         1         2        NA           1
 [168,]        1        2       NA         1         2        NA           1
 [169,]        1        2       NA         1         2        NA           1
 [170,]        1        2        2         1         2         2           1
 [171,]        1        2       NA         1         2        NA           1
 [172,]        1        2        2         1         2         2           1
 [173,]        1        3        3         1         3         3           1
 [174,]        2        2        3         2         2         3           2
 [175,]        2        2        3         2         2         3           2
 [176,]        2        2        3         2         2         3           2
 [177,]        1        2       NA         1         2        NA           1
 [178,]        1        2       NA         1         2        NA           1
 [179,]        1        2       NA         1         2        NA           1
 [180,]        1        2       NA         1         2        NA           1
 [181,]        1        2       NA         1         2        NA           1
 [182,]        1        2       NA         1         2        NA           1
 [183,]        1        2        3         1         2         3           1
 [184,]        1        1       NA         1         1        NA           1
 [185,]        1        2        3         1         2         3           1
 [186,]        1        1       NA         1         1        NA           1
 [187,]        1        1       NA         1         1        NA           1
 [188,]        1        2       NA         1         2        NA           1
 [189,]        2        2        3         2         2         3           2
 [190,]        1        3        3         1         3         2           1
 [191,]        1        2       NA         1         2        NA           1
 [192,]        1        1       NA         1         1        NA           1
 [193,]        1        3        2         1         3         2           1
 [194,]        1        3        3         1         3         2           1
 [195,]        1       NA       NA         1        NA        NA           1
 [196,]        1       NA       NA         1        NA        NA           1
 [197,]        2        2        3         2         2         3           2
 [198,]        1       NA       NA         1        NA        NA           1
 [199,]        1       NA       NA         1        NA        NA           1
 [200,]        1       NA       NA         1        NA        NA           1
 [201,]        1        3        3         1         3         3           1
 [202,]        2        2        3         2         2         3           2
 [203,]        1        2       NA         1         2        NA           1
 [204,]        1       NA       NA         1        NA        NA           1
 [205,]        1       NA       NA         1        NA        NA           1
 [206,]        1       NA       NA         1        NA        NA           1
 [207,]        1        2       NA         1         2        NA           1
 [208,]        1       NA       NA         1        NA        NA           1
 [209,]        1       NA       NA         1        NA        NA           1
 [210,]        3        2       NA         3         2        NA           3
 [211,]        1        1        2         1         1         2           1
 [212,]        1        1        2         1         1         2           1
 [213,]        1       NA       NA         1        NA        NA           1
 [214,]        1        2        2         1         2         2           1
 [215,]        1       NA       NA         1        NA        NA           1
 [216,]        2        3       NA         2         3        NA           2
 [217,]        1       NA       NA         1        NA        NA           1
 [218,]        1       NA       NA         1        NA        NA           1
 [219,]        1        2       NA         1         2        NA           1
 [220,]        1       NA       NA         1        NA        NA           1
 [221,]        1       NA       NA         1        NA        NA           1
 [222,]        1       NA       NA         1        NA        NA           1
 [223,]        1       NA       NA         1        NA        NA           1
 [224,]        1        1        3         1         1         2           1
 [225,]        1       NA       NA         1        NA        NA           1
 [226,]        1       NA       NA         1        NA        NA           1
 [227,]        1        2       NA         1         2        NA           1
 [228,]        1       NA       NA         1        NA        NA           1
 [229,]        1        1       NA         1         1        NA           1
 [230,]        1        2       NA         1         2        NA           1
 [231,]        1        1        3         1         1         2           1
 [232,]        1        1       NA         1         1        NA           1
 [233,]        1        2       NA         1         2        NA           1
 [234,]        1       NA       NA         1        NA        NA           1
 [235,]        1       NA       NA         1        NA        NA           1
 [236,]        1       NA       NA         1        NA        NA           1
 [237,]        1        2       NA         1         2        NA           1
 [238,]        1       NA       NA         1        NA        NA           1
 [239,]        2        3       NA         2         3        NA           2
 [240,]        1       NA       NA         1        NA        NA           1
 [241,]        2        2        3         2         2         3           2
 [242,]        1        2        3         1         2         3           1
 [243,]        1        2       NA         1         2        NA           1
 [244,]        1       NA       NA         1        NA        NA           1
 [245,]        1       NA       NA         1        NA        NA           1
 [246,]        1        2        3         1         2         2           1
 [247,]        1        1        2         1         1         2           1
 [248,]        1        1        2         1         1         2           1
 [249,]        1        2        2         1         2         2           1
 [250,]        1       NA       NA         1        NA        NA           1
 [251,]        1        1       NA         1         1        NA           1
 [252,]        2        2       NA         2         2        NA           2
 [253,]        2        2       NA         2         2        NA           2
 [254,]        1        1       NA         1         1        NA           1
 [255,]        2        2       NA         2         2        NA           2
 [256,]        1        2       NA         1         2        NA           1
 [257,]        1        2        2         1         2         2           1
 [258,]        2        2       NA         2         2        NA           2
 [259,]        1        2        3         1         2         3           1
 [260,]        1        2       NA         1         2        NA           1
 [261,]        1        2        2         1         2         2           1
 [262,]        1        2        2         1         2         2           1
 [263,]        1        2       NA         1         2        NA           1
 [264,]        1       NA       NA         1        NA        NA           1
 [265,]        1       NA       NA         1        NA        NA           1
 [266,]        1       NA       NA         1        NA        NA           1
 [267,]        1       NA       NA         1        NA        NA           1
 [268,]        1       NA       NA         1        NA        NA           1
 [269,]        1       NA       NA         1        NA        NA           1
 [270,]        1        2        3         1         2         3           1
 [271,]        1       NA       NA         1        NA        NA           1
 [272,]        1       NA       NA         1        NA        NA           1
 [273,]        1       NA       NA         1        NA        NA           1
 [274,]        1       NA       NA         1        NA        NA           1
 [275,]        1        1       NA         1         1        NA           1
 [276,]        1       NA       NA         1        NA        NA           1
 [277,]        2        2       NA         2         2        NA           2
 [278,]        1       NA       NA         1        NA        NA           1
 [279,]        2        2        2         2         2         2           2
 [280,]        1        2        2         1         2         2           1
 [281,]        1        2        2         1         2         2           1
 [282,]        1       NA       NA         1        NA        NA           1
 [283,]        1        2       NA         1         2        NA           1
 [284,]        1        2        3         1         2         3           1
 [285,]        2        2        3         2         2         2           2
 [286,]        1        2       NA         1         2        NA           1
 [287,]        2        2        2         2         2         2           2
 [288,]        1       NA       NA         1        NA        NA           1
 [289,]        1       NA       NA         1        NA        NA           1
 [290,]        1       NA       NA         1        NA        NA           1
 [291,]        1       NA       NA         1        NA        NA           1
 [292,]        1       NA       NA         1        NA        NA           1
 [293,]        2        2        3         2         2         2           2
 [294,]        1        1       NA         1         1        NA           1
 [295,]        1       NA       NA         1        NA        NA           1
 [296,]        1        2       NA         1         2        NA           1
 [297,]        1       NA       NA         1        NA        NA           1
 [298,]        1       NA       NA         1        NA        NA           1
 [299,]        2        2        3         2         2         2           2
 [300,]        1       NA       NA         1        NA        NA           1
 [301,]        1        3        2         1         2         2           1
 [302,]        1        2        3         1         2         3           1
 [303,]        3        2       NA         3         2        NA           3
 [304,]        1        2        2         1         2         2           1
 [305,]        1       NA       NA         1        NA        NA           1
 [306,]        1        2        3         1         2         2           1
 [307,]        1        1       NA         1         1        NA           1
 [308,]        1       NA       NA         1        NA        NA           1
 [309,]        1       NA       NA         1        NA        NA           1
 [310,]        1       NA       NA         1        NA        NA           1
 [311,]        1       NA       NA         1        NA        NA           1
 [312,]        1        3        3         1         3         3           1
 [313,]        1        2        2         1         2         2           1
 [314,]        1        2       NA         1         2        NA           1
 [315,]        1       NA       NA         1        NA        NA           1
 [316,]        1        2       NA         1         2        NA           1
 [317,]        1        1       NA         1         1        NA           1
 [318,]        1       NA       NA         1        NA        NA           1
 [319,]        1        2       NA         1         2        NA           1
 [320,]        1        2       NA         1         2        NA           1
 [321,]        1        2       NA         1         2        NA           1
 [322,]        1        2        3         1         2         3           1
 [323,]        1        2       NA         1         2        NA           1
 [324,]        1        2        3         1         2         3           1
 [325,]        2        2        3         2         2         3           2
 [326,]        1        3       NA         1         3        NA           1
 [327,]        1        3       NA         1         3        NA           1
 [328,]        1        2        3         1         2         3           1
 [329,]        1        1       NA         1         1        NA           1
 [330,]        1        1       NA         1         1        NA           1
 [331,]        1        3        3         1         3         3           1
 [332,]        1        1       NA         1         1        NA           1
 [333,]        1        3        2         1         3         2           1
 [334,]        1        2       NA         1         2        NA           1
 [335,]        2        2        3         2         2         3           2
 [336,]        1       NA       NA         1        NA        NA           1
 [337,]        1       NA       NA         1        NA        NA           1
 [338,]        2        2        3         2         2         3           2
 [339,]        1       NA       NA         1        NA        NA           1
 [340,]        1       NA       NA         1        NA        NA           1
 [341,]        1        3       NA         1         3        NA           1
 [342,]        1       NA       NA         1        NA        NA           1
 [343,]        1        3       NA         1         3        NA           1
 [344,]        1        1        2         1         1         2           1
 [345,]        1        1        2         1         1         2           1
 [346,]        1       NA       NA         1        NA        NA           1
 [347,]        1        2        2         1         2         2           1
 [348,]        1       NA       NA         1        NA        NA           1
 [349,]        1       NA       NA         1        NA        NA           1
 [350,]        2       NA       NA         2        NA        NA           2
 [351,]        1        2       NA         1         2        NA           1
 [352,]        1       NA       NA         1        NA        NA           1
 [353,]        2       NA       NA         2        NA        NA           2
 [354,]        1        1        3         1         1         3           1
 [355,]        2       NA       NA         2        NA        NA           2
 [356,]        1        1       NA         1         1        NA           1
 [357,]        1        1        3         1         1         3           1
 [358,]        1        1       NA         1         1        NA           1
 [359,]        1        3       NA         1         3        NA           1
 [360,]        1       NA       NA         1        NA        NA           1
 [361,]        2        2       NA         2         2        NA           2
 [362,]        2        2        2         2         2         2           2
 [363,]        1        1        2         1         1         2           1
 [364,]        1        2       NA         1         2        NA           1
 [365,]        1       NA       NA         1        NA        NA           1
 [366,]        1       NA       NA         1        NA        NA           1
 [367,]        1        1        2         1         1         2           1
 [368,]        1        1        2         1         1         2           1
 [369,]        1        2        2         1         2         2           1
 [370,]        1       NA       NA         1        NA        NA           1
 [371,]        1        1       NA         1         1        NA           1
 [372,]        1        2       NA         1         2        NA           1
 [373,]        1        2       NA         1         2        NA           1
 [374,]        1        1        2         1         1         2           1
 [375,]        1       NA       NA         1        NA        NA           1
 [376,]        1        1       NA         1         1        NA           1
 [377,]        1       NA       NA         1        NA        NA           1
 [378,]        2        2       NA         2         2        NA           2
 [379,]        1       NA       NA         1        NA        NA           1
 [380,]        1        1        2         1         1         2           1
 [381,]        1        2        2         1         2         2           1
 [382,]        1        3        3         1         3         3           1
 [383,]        1       NA       NA         1        NA        NA           1
 [384,]        1        2       NA         1         2        NA           1
 [385,]        1        2        3         1         2         3           1
 [386,]        2        2        3         2         2         3           2
 [387,]        1        2       NA         1         2        NA           1
 [388,]        2        2        3         2         2         3           2
 [389,]        3        3       NA         3         3        NA           3
 [390,]        1       NA       NA         1        NA        NA           1
 [391,]        1       NA       NA         1        NA        NA           1
 [392,]        1       NA       NA         1        NA        NA           1
 [393,]        1        2       NA         1         2        NA           1
 [394,]        1       NA       NA         1        NA        NA           1
 [395,]        1       NA       NA         1        NA        NA           1
 [396,]        1        3        3         1         3         3           1
 [397,]        1        2        3         1         2         3           1
 [398,]        1       NA       NA         1        NA        NA           1
 [399,]        1        3        3         1         3         3           1
 [400,]        1        2        3         1         2         3           1
 [401,]        1        2       NA         1         2        NA           1
 [402,]        1        2       NA         1         2        NA           1
 [403,]        1        1       NA         1         1        NA           1
 [404,]        1       NA       NA         1        NA        NA           1
 [405,]        1       NA       NA         1        NA        NA           1
 [406,]        1       NA       NA         1        NA        NA           1
 [407,]        1        3        3         1         3         3           1
 [408,]        1        1        2         1         1         2           1
 [409,]        1        2        3         1         2         3           1
 [410,]        1        2       NA         1         2        NA           1
 [411,]        1       NA       NA         1        NA        NA           1
 [412,]        1       NA       NA         1        NA        NA           1
 [413,]        1        1       NA         1         1        NA           1
 [414,]        1        1       NA         1         1        NA           1
 [415,]        1        1       NA         1         1        NA           1
 [416,]        1        2        3         1         2         3           1
 [417,]        1       NA       NA         1        NA        NA           1
 [418,]        1       NA       NA         1        NA        NA           1
 [419,]        1       NA       NA         1        NA        NA           1
 [420,]        1       NA       NA         1        NA        NA           1
 [421,]        1       NA       NA         1        NA        NA           1
 [422,]        1       NA       NA         1        NA        NA           1
 [423,]        1        3        3         1         3         3           1
 [424,]        1        1        2         1         1         2           1
 [425,]        1        1        3         1         2         3           1
 [426,]        1       NA       NA         1        NA        NA           1
 [427,]        3        2        3         3         2         3           3
 [428,]        1        2        3         1         2         3           1
 [429,]        1        2       NA         1         2        NA           1
 [430,]        1       NA       NA         1        NA        NA           1
 [431,]        1        2        3         1         2         3           1
 [432,]        1        2        3         1         2         3           1
 [433,]        1        2       NA         1         2        NA           1
 [434,]        1       NA       NA         1        NA        NA           1
 [435,]        2       NA       NA         2        NA        NA           2
 [436,]        1        1       NA         1         1        NA           1
 [437,]        1        3        2         1         3         2           1
 [438,]        1        3        2         1         3         2           1
 [439,]        1        3        2         1         3         2           1
 [440,]        1        1        2         1         1         2           1
 [441,]        1        2       NA         1         2        NA           1
 [442,]        1        2       NA         1         2        NA           1
 [443,]        1        1        1         1         1         1           1
 [444,]        1        1        2         1         1         2           1
 [445,]        1        1       NA         1         1        NA           1
 [446,]        2        1       NA         2         1        NA           2
 [447,]        1        1       NA         1         1        NA           1
 [448,]        1       NA       NA         1        NA        NA           1
 [449,]        1        1        1         1         1         1           1
 [450,]        1        1       NA         1         1        NA           1
 [451,]        1       NA       NA         1        NA        NA           1
 [452,]        1        1       NA         1         1        NA           1
 [453,]        1       NA       NA         1        NA        NA           1
 [454,]        1       NA       NA         1        NA        NA           1
 [455,]        1       NA       NA         1        NA        NA           1
 [456,]        1       NA       NA         1        NA        NA           1
 [457,]        1       NA       NA         1        NA        NA           1
 [458,]        1        1        3         1         1         3           1
 [459,]        1        1        3         1         1         3           1
 [460,]        1        1        1         1         1         1           1
 [461,]        1        1        3         1         1         3           1
 [462,]        1       NA       NA         1        NA        NA           1
 [463,]        1       NA       NA         1        NA        NA           1
 [464,]        1        1        1         1         1         1           1
 [465,]        1       NA       NA         1        NA        NA           1
 [466,]        1        1        3         1         1         3           1
 [467,]        1        3        3         1         3         3           1
 [468,]        1       NA       NA         1        NA        NA           1
 [469,]        1       NA       NA         1        NA        NA           1
 [470,]        1       NA       NA         1        NA        NA           1
 [471,]        1       NA       NA         1        NA        NA           1
 [472,]        1        3        3         1         3         3           1
 [473,]        1        1        3         1         1         3           1
 [474,]        1        2        3         1         2         3           1
 [475,]        1       NA       NA         1        NA        NA           1
 [476,]        2       NA       NA         2        NA        NA           2
 [477,]        1        1       NA         1         1        NA           1
 [478,]        1        1       NA         1         1        NA           1
 [479,]        2        2       NA         2         2        NA           2
 [480,]        1        2       NA         1         2        NA           1
 [481,]        1        2       NA         1         2        NA           1
 [482,]        1       NA       NA         1        NA        NA           1
 [483,]        1       NA       NA         1        NA        NA           1
 [484,]        1       NA       NA         1        NA        NA           1
 [485,]        1        1        3         1         1         3           1
 [486,]        1        1        3         1         1         3           1
 [487,]        1        2       NA         1         2        NA           1
 [488,]        1       NA       NA         1        NA        NA           1
 [489,]        1       NA       NA         1        NA        NA           1
 [490,]        1        1        3         1         1         3           1
 [491,]        1        2        3         1         2         3           1
 [492,]        1        2        3         1         2         3           1
 [493,]        1        1        2         1         1         3           1
 [494,]        1        2        2         1         2         2           1
 [495,]        1        2        2         1         2         2           1
 [496,]        1        2       NA         1         2        NA           1
 [497,]        1        3       NA         1         3        NA           1
 [498,]        2        3       NA         2         3        NA           2
 [499,]        1        2        2         1         2         2           1
 [500,]        1       NA       NA         1        NA        NA           1
 [501,]        1        3       NA         1         3        NA           1
 [502,]        1       NA       NA         1        NA        NA           1
 [503,]        1        2       NA         1         2        NA           1
 [504,]        3       NA       NA         3        NA        NA           3
 [505,]        1       NA       NA         1        NA        NA           1
 [506,]        1        2        2         1         2         2           1
 [507,]        1       NA       NA         1        NA        NA           1
 [508,]        1        2        2         1         2         3           1
 [509,]        1        2       NA         1         2        NA           1
 [510,]        1       NA       NA         1        NA        NA           1
 [511,]        1        2       NA         1         2        NA           1
 [512,]        1       NA       NA         1        NA        NA           1
 [513,]        1       NA       NA         1        NA        NA           1
 [514,]        1        2        2         1         2         2           1
 [515,]        1        1        2         1         1         2           1
 [516,]        1        2        2         1         2         2           1
 [517,]        2        2        3         2         2         3           2
 [518,]        1       NA       NA         1        NA        NA           1
 [519,]        1       NA       NA         1        NA        NA           1
 [520,]        1        1       NA         1         1        NA           1
 [521,]        1        1        2         1         1         2           1
 [522,]        2       NA       NA         2        NA        NA           2
 [523,]        1       NA       NA         1        NA        NA           1
 [524,]        1        2       NA         1         2        NA           1
 [525,]        1        1       NA         1         1        NA           1
 [526,]        1       NA       NA         1        NA        NA           1
 [527,]        1        1        2         1         1         2           1
 [528,]        1       NA       NA         1        NA        NA           1
 [529,]        1        1       NA         1         1        NA           1
 [530,]        1       NA       NA         1        NA        NA           1
 [531,]        1        1       NA         1         1        NA           1
 [532,]        1       NA       NA         1        NA        NA           1
 [533,]        1       NA       NA         1        NA        NA           1
 [534,]        1       NA       NA         1        NA        NA           1
 [535,]        1        2        2         1         2         2           1
 [536,]        1        1       NA         1         1        NA           1
 [537,]        1       NA       NA         1        NA        NA           1
 [538,]        1       NA       NA         1        NA        NA           1
 [539,]        1        1        2         1         1         2           1
 [540,]        3       NA       NA         3        NA        NA           3
 [541,]        1       NA       NA         1        NA        NA           1
 [542,]        2        2        3         2         2         3           2
 [543,]        1        2        3         1         2         3           1
 [544,]        1        2        3         1         2         3           1
 [545,]        2        3        3         2         3         3           2
 [546,]        1        2        2         1         2         2           1
 [547,]        1        2       NA         1         2        NA           1
 [548,]        1        2       NA         1         2        NA           1
 [549,]        2        3        3         2         3         3           1
 [550,]        1        2        3         1         2         3           1
 [551,]        2        3        3         2         2         3           2
 [552,]        2        3        3         2         3         3           2
 [553,]        1        2        3         1         2         3           1
 [554,]        1        2        3         1         2         3           1
 [555,]        2        3        3         2         3         3           2
 [556,]        1        2        3         1         2         3           1
 [557,]        1        2        3         1         2         3           1
 [558,]        1        2        3         1         2         3           1
 [559,]        2        2        3         2         2         3           2
 [560,]        2        3        3         2         2         3           2
 [561,]        2        3        3         2         2         3           2
 [562,]        1        2       NA         1         2        NA           1
 [563,]        1        2       NA         1         2        NA           1
 [564,]        2        3       NA         2         2        NA           2
 [565,]        2        3       NA         2         2        NA           2
 [566,]        2        3       NA         2         2        NA           2
 [567,]        2        3        3         2         2         3           2
 [568,]        2        3       NA         2         2        NA           2
 [569,]        2        3       NA         2         2        NA           2
 [570,]        1       NA       NA         1        NA        NA           1
 [571,]        1       NA       NA         1        NA        NA           1
 [572,]        2        3       NA         2         3        NA           2
 [573,]        1       NA       NA         1        NA        NA           1
 [574,]        1       NA       NA         1        NA        NA           1
 [575,]        2        3       NA         2         3        NA           2
 [576,]        3       NA       NA         3        NA        NA           3
 [577,]        3       NA       NA         3        NA        NA           3
 [578,]        3       NA       NA         3        NA        NA           3
 [579,]        3       NA       NA         3        NA        NA           3
 [580,]        2       NA       NA         2        NA        NA           2
 [581,]        2       NA       NA         2        NA        NA           2
 [582,]        2        3       NA         2         2        NA           2
 [583,]        1       NA       NA         1        NA        NA           1
 [584,]        2       NA       NA         2        NA        NA           2
 [585,]        2       NA       NA         2        NA        NA           2
 [586,]        2       NA       NA         2        NA        NA           2
 [587,]        2       NA       NA         2        NA        NA           2
 [588,]        1       NA       NA         1        NA        NA           1
 [589,]        3       NA       NA         3        NA        NA           3
 [590,]        1       NA       NA         1        NA        NA           1
 [591,]        1       NA       NA         1        NA        NA           1
 [592,]        1       NA       NA         1        NA        NA           1
 [593,]        1        2        3         1         2         3           1
 [594,]        2       NA       NA         2        NA        NA           2
 [595,]        1        2       NA         1         2        NA           1
 [596,]        1       NA       NA         1        NA        NA           1
 [597,]        1       NA       NA         1        NA        NA           1
 [598,]        1       NA       NA         1        NA        NA           1
 [599,]        2       NA       NA         2        NA        NA           2
 [600,]        2        2        3         2         2         3           2
 [601,]        1        2        3         1         2         3           1
 [602,]        1       NA       NA         1        NA        NA           1
 [603,]        3       NA       NA         3        NA        NA           3
 [604,]        1        2       NA         1         2        NA           1
 [605,]        1        2        3         1         2         3           1
 [606,]        2        2       NA         2         2        NA           2
 [607,]        1       NA       NA         1        NA        NA           1
 [608,]        1        2        3         1         2         3           1
 [609,]        1        2       NA         1         2        NA           1
 [610,]        1       NA       NA         1        NA        NA           1
 [611,]        1        2       NA         1         2        NA           1
 [612,]        1       NA       NA         1        NA        NA           1
 [613,]        1       NA       NA         1        NA        NA           1
 [614,]        1        2        3         1         2         3           1
 [615,]        1        1        2         1         1         2           1
 [616,]        1        2        2         1         2         2           1
 [617,]        2        2        3         2         2         3           2
 [618,]        3        2        2         3         2         2           3
 [619,]        1        3       NA         1         2        NA           1
 [620,]        1        2        2         1         2         2           1
 [621,]        1        3       NA         1         2        NA           1
 [622,]        3       NA       NA         3        NA        NA           3
 [623,]        1       NA       NA         1        NA        NA           1
 [624,]        1        2       NA         1         2        NA           1
 [625,]        1        2        2         1         2         2           1
 [626,]        1        2        2         1         2         2           1
 [627,]        1        2        2         1         2         2           1
 [628,]        2        2       NA         2         2        NA           2
 [629,]        3        2        2         3         2         2           3
 [630,]        1        2        2         1         2         2           1
 [631,]        1        2       NA         1         2        NA           1
 [632,]        2        2        3         2         2         3           2
 [633,]        1        2       NA         1         2        NA           1
 [634,]        2        2       NA         2         2        NA           2
 [635,]        2        2       NA         2         2        NA           2
 [636,]        3       NA       NA         3        NA        NA           3
 [637,]        1        2        2         1         2         2           1
 [638,]        3        2        2         3         2         2           3
 [639,]        2        2        2         2         2         2           2
 [640,]        1        2        2         1         2         2           1
 [641,]        1        2       NA         1         2        NA           1
 [642,]        2        2       NA         2         2        NA           2
 [643,]        1        2        2         1         2         2           1
 [644,]        1        3       NA         1         2        NA           1
 [645,]        1       NA       NA         1        NA        NA           1
 [646,]        1        1       NA         1         1        NA           1
 [647,]        1       NA       NA         1        NA        NA           1
 [648,]        1        1       NA         1         1        NA           1
 [649,]        1        2        2         1         2         2           1
 [650,]        1        1        2         1         1         2           1
 [651,]        1       NA       NA         1        NA        NA           1
 [652,]        1       NA       NA         1        NA        NA           1
 [653,]        1       NA       NA         1        NA        NA           1
 [654,]        1        2        2         1         2         2           1
 [655,]        1        2        2         1         2         2           1
 [656,]        1        2        2         1         2         2           1
 [657,]        1        1        2         1         1         2           1
 [658,]        1       NA       NA         1        NA        NA           1
 [659,]        1       NA       NA         1        NA        NA           1
 [660,]        3        2       NA         3         2        NA           3
 [661,]        3        2       NA         3         2        NA           3
 [662,]        3        2       NA         3         2        NA           3
 [663,]        1        2        2         1         2         2           1
 [664,]        1        2        2         1         2         2           1
 [665,]        1        2        2         1         2         2           1
 [666,]        1        2        2         1         2         2           1
 [667,]        1        2        2         1         2         2           1
 [668,]        2        1        2         1         1         2           2
 [669,]        1        2        2         1         2         2           1
 [670,]        1        2        2         1         2         2           1
 [671,]        1        1        2         1         1         2           1
 [672,]        1        2        2         1         2         2           1
 [673,]        1        1        2         1         1         2           1
 [674,]        1        2        2         1         2         2           1
 [675,]        1        2        2         1         2         2           1
 [676,]        1        2        2         1         2         2           1
 [677,]        2        1        2         1         1         2           2
 [678,]        1        1        2         1         1         2           1
 [679,]        2        1        2         1         1         2           2
 [680,]        2        1        2         1         1         2           2
 [681,]        2        1        2         1         1         2           2
 [682,]        2        1        2         1         1         2           1
 [683,]        2        1        2         1         1         2           2
 [684,]        1       NA       NA         1        NA        NA           1
 [685,]        1        2       NA         1         2        NA           1
 [686,]        1       NA       NA         1        NA        NA           1
 [687,]        1        2       NA         1         2        NA           1
 [688,]        1       NA       NA         1        NA        NA           1
 [689,]        1       NA       NA         1        NA        NA           1
 [690,]        1       NA       NA         1        NA        NA           1
 [691,]        1        2        2         1         2         2           1
 [692,]        1        1       NA         1         1        NA           1
 [693,]        1       NA       NA         1        NA        NA           1
 [694,]        1       NA       NA         1        NA        NA           1
 [695,]        3       NA       NA         3        NA        NA           3
 [696,]        1        2        2         1         2         2           1
 [697,]        1        2        2         1         2         2           1
 [698,]        1        2        2         1         2         2           1
 [699,]        1        2        2         1         2         2           1
 [700,]        1        2        2         1         2         2           1
 [701,]        1        2        2         1         2         2           1
 [702,]        1        2       NA         1         2        NA           1
 [703,]        1        2       NA         1         2        NA           1
 [704,]        1        1        2         1         1         2           1
 [705,]        1        1        2         1         1         2           1
 [706,]        1        1        2         1         1         2           1
 [707,]        1        1        2         1         1         2           1
 [708,]        1        1        2         1         1         2           1
 [709,]        1        1        2         1         1         2           1
 [710,]        1        1        2         1         1         2           1
 [711,]        1        1        2         1         1         2           1
 [712,]        1        1        2         1         1         3           1
 [713,]        1        1        2         1         1         2           1
 [714,]        1        1        2         1         1         2           1
 [715,]        1        2        2         1         2         2           1
 [716,]        1        1        2         1         1         2           1
 [717,]        1        2       NA         1         2        NA           1
 [718,]        1        1        2         1         1         3           1
 [719,]        1        1        2         1         1         2           1
 [720,]        1        2       NA         1         2        NA           1
 [721,]        1        2        2         1         2         2           1
 [722,]        1        2        2         1         2         2           1
 [723,]        1        2        2         1         2         2           1
 [724,]        2        2        3         2         2         3           2
 [725,]        2        2        3         2         2         3           1
 [726,]        1        2        3         1         2         3           1
 [727,]        2        3        3         2         3         3           1
 [728,]        1        2        3         1         2         3           1
 [729,]        1        2       NA         1         2        NA           1
 [730,]        1        2       NA         1         2        NA           1
 [731,]        2        3        3         2         3         3           2
 [732,]        1        3        3         1         3         3           1
 [733,]        2        3        3         2         3         3           2
 [734,]        2        3        3         2         3         3           2
 [735,]        1        3        3         1         3         3           1
 [736,]        2        3        3         2         3         3           2
 [737,]        1        2        3         1         2         3           1
 [738,]        1        2        3         1         2         3           1
 [739,]        2        2        3         2         2         3           2
 [740,]        2        3        3         2         3         3           2
 [741,]        2        3        3         2         3         3           2
 [742,]        1        2       NA         1         2        NA           1
 [743,]        1        2       NA         1         2        NA           1
 [744,]        2        2       NA         2         2        NA           2
 [745,]        2        2       NA         2         2        NA           2
 [746,]        2        2       NA         2         2        NA           2
 [747,]        2        3        3         2         3         3           2
 [748,]        2        2       NA         2         2        NA           2
 [749,]        2        2       NA         2         2        NA           2
 [750,]        1       NA       NA         1        NA        NA           1
 [751,]        2        3       NA         2         3        NA           2
 [752,]        1        3       NA         1         3        NA           1
 [753,]        3       NA       NA         3        NA        NA           3
 [754,]        3       NA       NA         3        NA        NA           3
 [755,]        3       NA       NA         3        NA        NA           3
 [756,]        2       NA       NA         2        NA        NA           2
 [757,]        2       NA       NA         2        NA        NA           2
 [758,]        2        2       NA         2         2        NA           2
 [759,]        1       NA       NA         1        NA        NA           1
 [760,]        1       NA       NA         1        NA        NA           1
 [761,]        1       NA       NA         1        NA        NA           1
 [762,]        1       NA       NA         1        NA        NA           1
 [763,]        1        2       NA         1         2        NA           1
 [764,]        2       NA       NA         2        NA        NA           2
 [765,]        1       NA       NA         1        NA        NA           1
 [766,]        1       NA       NA         1        NA        NA           1
 [767,]        3       NA       NA         3        NA        NA           3
 [768,]        1       NA       NA         1        NA        NA           1
 [769,]        3       NA       NA         3        NA        NA           3
 [770,]        3       NA       NA         3        NA        NA           3
 [771,]        1        3       NA         1         2        NA           1
 [772,]        3       NA       NA         3        NA        NA           3
 [773,]        3       NA       NA         3        NA        NA           3
 [774,]        2        3        3         2         3         3           2
 [775,]        1        2       NA         1         2        NA           1
 [776,]        2       NA       NA         2        NA        NA           2
 [777,]        1        2       NA         1         2        NA           1
 [778,]        1       NA       NA         1        NA        NA           1
 [779,]        1       NA       NA         1        NA        NA           1
 [780,]        1       NA       NA         1        NA        NA           1
 [781,]        1       NA       NA         1        NA        NA           1
 [782,]        2        2        3         2         2         3           2
 [783,]        1        3        3         1         3         3           1
 [784,]        1       NA       NA         1        NA        NA           1
 [785,]        3       NA       NA         3        NA        NA           3
 [786,]        1        1        2         1         1         2           1
 [787,]        1        2        2         1         2         2           1
 [788,]        1       NA       NA         1        NA        NA           1
 [789,]        1       NA       NA         1        NA        NA           1
 [790,]        1        2        2         1         2         2           1
 [791,]        1        2        2         1         2         2           1
 [792,]        1        2        2         1         2         2           1
 [793,]        1        2       NA         1         2        NA           1
 [794,]        1        2       NA         1         2        NA           1
 [795,]        1        2       NA         1         2        NA           1
 [796,]        1       NA       NA         1        NA        NA           1
 [797,]        1       NA       NA         1        NA        NA           1
 [798,]        1       NA       NA         1        NA        NA           1
 [799,]        1       NA       NA         1        NA        NA           1
 [800,]        1       NA       NA         1        NA        NA           1
 [801,]        1       NA       NA         1        NA        NA           1
 [802,]        1       NA       NA         1        NA        NA           1
 [803,]        1       NA       NA         1        NA        NA           1
 [804,]        1       NA       NA         1        NA        NA           1
 [805,]        2        2       NA         2         2        NA           2
 [806,]        1        2        2         1         2         2           1
 [807,]        1        2        2         1         2         2           1
 [808,]        1        2        2         1         2         2           1
 [809,]        1       NA       NA         1        NA        NA           1
 [810,]        1       NA       NA         1        NA        NA           1
 [811,]        1        2       NA         1         2        NA           1
 [812,]        1       NA       NA         1        NA        NA           1
 [813,]        1        2       NA         1         2        NA           1
 [814,]        1       NA       NA         1        NA        NA           1
 [815,]        1        2        2         1         2         2           1
 [816,]        2        2       NA         2         2        NA           2
 [817,]        1       NA       NA         1        NA        NA           1
 [818,]        1        2        2         1         2         3           1
 [819,]        1       NA       NA         1        NA        NA           1
 [820,]        1        1        2         1         1         2           1
 [821,]        1       NA       NA         1        NA        NA           1
 [822,]        1       NA       NA         1        NA        NA           1
 [823,]        1       NA       NA         1        NA        NA           1
 [824,]        1        2        3         1         2         3           1
 [825,]        1        2       NA         1         2        NA           1
 [826,]        1        3       NA         1         3        NA           1
 [827,]        1        3       NA         1         3        NA           1
 [828,]        1       NA       NA         1        NA        NA           1
 [829,]        1        3       NA         1         3        NA           1
 [830,]        1        2       NA         1         2        NA           1
 [831,]        1        2       NA         1         2        NA           1
 [832,]        1       NA       NA         1        NA        NA           1
 [833,]        1        2       NA         1         2        NA           1
 [834,]        1       NA       NA         1        NA        NA           1
 [835,]        1       NA       NA         1        NA        NA           1
 [836,]        1        2        2         1         2         2           1
 [837,]        2        2       NA         2         2        NA           2
 [838,]        1       NA       NA         1        NA        NA           1
 [839,]        2       NA       NA         2        NA        NA           1
 [840,]        1        1       NA         1         1        NA           1
 [841,]        2       NA       NA         2        NA        NA           2
 [842,]        1       NA       NA         1        NA        NA           1
 [843,]        1       NA       NA         1        NA        NA           1
 [844,]        1        1       NA         1         1        NA           1
 [845,]        1       NA       NA         1        NA        NA           1
 [846,]        1       NA       NA         1        NA        NA           1
 [847,]        1       NA       NA         1        NA        NA           1
 [848,]        1       NA       NA         1        NA        NA           1
 [849,]        1        1        2         1         1         2           1
 [850,]        1        1        2         1         1         2           1
 [851,]        1        1        2         1         1         2           1
 [852,]        1        1        2         1         1         2           1
 [853,]        1       NA       NA         1        NA        NA           1
 [854,]        2        2       NA         2         2        NA           2
 [855,]        2        2       NA         2         2        NA           1
 [856,]        1       NA       NA         1        NA        NA           1
 [857,]        1        1        2         1         1         2           1
 [858,]        1        2        2         1         2         2           1
 [859,]        1       NA       NA         1        NA        NA           1
 [860,]        1        2        2         1         2         2           1
 [861,]        1        2        2         1         2         2           1
 [862,]        1        2        2         1         2         2           1
 [863,]        1        2       NA         1         2        NA           1
 [864,]        1        2       NA         1         2        NA           1
 [865,]        1        2       NA         1         2        NA           1
 [866,]        1       NA       NA         1        NA        NA           1
 [867,]        1       NA       NA         1        NA        NA           1
 [868,]        1       NA       NA         1        NA        NA           1
 [869,]        1       NA       NA         1        NA        NA           1
 [870,]        1       NA       NA         1        NA        NA           1
 [871,]        1       NA       NA         1        NA        NA           1
 [872,]        1       NA       NA         1        NA        NA           1
 [873,]        1       NA       NA         1        NA        NA           1
 [874,]        1       NA       NA         1        NA        NA           1
 [875,]        1       NA       NA         1        NA        NA           1
 [876,]        1        1       NA         1         1        NA           1
 [877,]        1       NA       NA         1        NA        NA           1
 [878,]        1       NA       NA         1        NA        NA           1
 [879,]        1        2       NA         1         1        NA           1
 [880,]        1        2       NA         1         1        NA           1
 [881,]        1        2       NA         1         2        NA           1
 [882,]        1       NA       NA         1        NA        NA           1
 [883,]        1       NA       NA         1        NA        NA           1
 [884,]        1       NA       NA         1        NA        NA           1
 [885,]        1        2        3         1         2         3           1
 [886,]        1        3        3         1         3         3           1
 [887,]        3        3       NA         3         3        NA           3
 [888,]        1       NA       NA         1        NA        NA           1
 [889,]        2        3       NA         2         3        NA           2
 [890,]        1        2        2         1         2         2           1
 [891,]        1        2        2         1         2         2           1
 [892,]        1        2        2         1         2         2           1
 [893,]        1       NA       NA         1        NA        NA           1
 [894,]        1        2       NA         1         2        NA           1
 [895,]        1       NA       NA         1        NA        NA           1
 [896,]        1        2       NA         1         2        NA           1
 [897,]        1       NA       NA         1        NA        NA           1
 [898,]        1        2        2         1         2         2           1
 [899,]        1       NA       NA         1        NA        NA           1
 [900,]        1        2        3         1         2         3           1
 [901,]        1        1        2         1         1         2           1
 [902,]        1       NA       NA         1        NA        NA           1
 [903,]        1        2        3         1         2         3           1
 [904,]        1        3        2         1         3         2           1
 [905,]        1       NA       NA         1        NA        NA           1
 [906,]        2       NA       NA         2        NA        NA           2
 [907,]        1        2        3         1         2         3           1
 [908,]        1        3        3         1         3         3           1
 [909,]        1        3        2         1         3         2           1
 [910,]        1        3        2         1         3         2           1
 [911,]        1        3        2         1         3         2           1
 [912,]        3        3        3         3         3         3           3
 [913,]        3        3        3         3         3         3           3
 [914,]        1        3        3         1         3         3           1
 [915,]        1        2       NA         1         2        NA           1
 [916,]        1        3        2         1         3         2           1
 [917,]        1        2        3         1         2         3           1
 [918,]        1        2        3         1         2         3           1
 [919,]        1        2       NA         1         2        NA           1
 [920,]        1        2        3         1         2         3           1
 [921,]        1        2        3         1         2         3           1
 [922,]        1        2        3         1         2         3           1
 [923,]        1       NA       NA         1        NA        NA           1
 [924,]        1        2        3         1         2         3           1
 [925,]        1        2       NA         1         2        NA           1
 [926,]        1        3       NA         1         3        NA           1
 [927,]        1        3       NA         1         3        NA           1
 [928,]        1       NA       NA         1        NA        NA           1
 [929,]        1        2       NA         1         2        NA           1
 [930,]        1        3       NA         1         3        NA           1
 [931,]        1        3       NA         1         3        NA           1
 [932,]        1        3       NA         1         3        NA           1
 [933,]        1       NA       NA         1        NA        NA           1
 [934,]        1        3       NA         1         3        NA           1
 [935,]        1       NA       NA         1        NA        NA           1
 [936,]        1       NA       NA         1        NA        NA           1
 [937,]        2        3        2         2         3         2           2
 [938,]        2        3        2         2         3         2           2
 [939,]        2        3        2         2         3         2           2
 [940,]        2        3        2         2         3         2           2
 [941,]        2        3        2         2         3         2           2
 [942,]        2        3        2         2         3         2           2
 [943,]        1        2        3         1         2         3           1
 [944,]        1        2        3         1         2         3           1
 [945,]        1        2        3         1         2         3           1
 [946,]        2        3        2         2         3         2           2
 [947,]        1        3        3         1         3         3           1
 [948,]        1        2        3         1         2         3           1
 [949,]        1        3        3         1         3         3           1
 [950,]        1        2        3         1         2         3           1
 [951,]        1        2        3         1         2         3           1
 [952,]        1        2        3         1         2         3           1
 [953,]        1        3        3         1         3         3           1
 [954,]        1        3        3         1         3         3           1
 [955,]        1        3        3         1         3         3           1
 [956,]        1        3        2         1         3         2           1
 [957,]        1        3        2         1         2         2           1
 [958,]        1        2       NA         1         2        NA           1
 [959,]        1       NA       NA         1        NA        NA           1
 [960,]        1       NA       NA         1        NA        NA           1
 [961,]        2       NA       NA         2        NA        NA           2
 [962,]        1        3        2         1         3         2           1
 [963,]        1       NA       NA         1        NA        NA           1
 [964,]        1        2        3         1         2         3           1
 [965,]        1       NA       NA         1        NA        NA           1
 [966,]        1       NA       NA         1        NA        NA           1
 [967,]        1        2       NA         1         2        NA           1
 [968,]        2        2       NA         2         2        NA           2
 [969,]        1       NA       NA         1        NA        NA           1
 [970,]        2       NA       NA         2        NA        NA           1
 [971,]        1        1       NA         1         1        NA           1
 [972,]        2       NA       NA         2        NA        NA           2
 [973,]        1       NA       NA         1        NA        NA           1
 [974,]        1       NA       NA         1        NA        NA           1
 [975,]        1        2       NA         1         1        NA           1
 [976,]        1       NA       NA         1        NA        NA           1
 [977,]        1       NA       NA         1        NA        NA           1
 [978,]        1       NA       NA         1        NA        NA           1
 [979,]        1       NA       NA         1        NA        NA           1
 [980,]        1        1        2         1         1         2           1
 [981,]        1        2        2         1         2         2           1
 [982,]        1        1        2         1         1         2           1
 [983,]        1        1        3         1         1         3           1
 [984,]        1       NA       NA         1        NA        NA           1
 [985,]        2        2       NA         2         2        NA           2
 [986,]        2        2       NA         2         2        NA           1
 [987,]        1       NA       NA         1        NA        NA           1
 [988,]        1       NA       NA         1        NA        NA           1
 [989,]        1       NA       NA         1        NA        NA           1
 [990,]        2       NA       NA         2        NA        NA           2
 [991,]        2       NA       NA         2        NA        NA           2
 [992,]        2       NA       NA         2        NA        NA           2
 [993,]        1        2       NA         1         2        NA           1
 [994,]        1        1        2         1         1         2           1
 [995,]        1       NA       NA         1        NA        NA           1
 [996,]        1        1        3         1         1         3           1
 [997,]        1        2        2         1         2         2           1
 [998,]        1        2        3         1         2         2           1
 [999,]        1        2        2         1         2         2           1
[1000,]        1        2        3         1         2         2           1
[1001,]        1       NA       NA         1        NA        NA           1
[1002,]        1        2        2         1         2         2           1
[1003,]        1        2        2         1         2         2           1
[1004,]        1        1        3         1         1         3           1
[1005,]        1        1        3         1         1         3           1
[1006,]        1       NA       NA         1        NA        NA           1
[1007,]        1        2        3         1         2         3           1
[1008,]        1        3        3         1         3         3           1
[1009,]        3        3        3         3         2         3           3
[1010,]        3        3        3         3         2         3           3
[1011,]        1        2        3         1         2         3           1
[1012,]        1        2       NA         1         2        NA           1
[1013,]        1        2        3         1         2         3           1
[1014,]        1       NA       NA         1        NA        NA           1
[1015,]        1        2        3         1         2         3           1
[1016,]        1        3       NA         1         3        NA           1
[1017,]        1       NA       NA         1        NA        NA           1
[1018,]        1        3       NA         1         3        NA           1
[1019,]        1        3       NA         1         3        NA           1
[1020,]        2       NA       NA         2        NA        NA           2
[1021,]        1        3       NA         1         3        NA           1
[1022,]        1       NA       NA         1        NA        NA           1
[1023,]        1        2        2         1         2         2           1
[1024,]        1        1        3         1         1         3           1
[1025,]        1        1        3         1         1         3           1
[1026,]        1        1        3         1         1         3           1
[1027,]        1        2        3         1         2         3           1
[1028,]        1        2        2         1         2         2           1
[1029,]        1       NA       NA         1        NA        NA           1
[1030,]        1       NA       NA         1        NA        NA           1
[1031,]        2        3       NA         2         2        NA           2
[1032,]        1        3        2         1         3         2           1
[1033,]        1        1       NA         1         1        NA           1
[1034,]        1        3       NA         1         3        NA           1
[1035,]        1        3        2         1         3         2           1
[1036,]        1       NA       NA         1        NA        NA           1
[1037,]        2        2        2         2         2         2           2
[1038,]        1        1       NA         1         1        NA           1
[1039,]        1        2        2         1         2         2           1
[1040,]        1        2        3         1         2         3           1
[1041,]        2        2        2         2         2         2           2
[1042,]        1        2        2         1         2         2           1
[1043,]        1       NA       NA         1        NA        NA           1
[1044,]        1        2        2         1         2         2           1
[1045,]        1       NA       NA         1        NA        NA           1
[1046,]        1       NA       NA         1        NA        NA           1
[1047,]        1        2        2         1         2         2           1
[1048,]        2        2        2         2         2         2           2
[1049,]        1        2        2         1         2         2           1
[1050,]        1        2        2         1         2         2           1
[1051,]        1        2       NA         1         2        NA           1
[1052,]        1       NA       NA         1        NA        NA           1
[1053,]        1        2       NA         1         2        NA           1
[1054,]        1       NA       NA         1        NA        NA           1
[1055,]        1        2        2         1         2         2           1
[1056,]        1        2        2         1         2         2           1
[1057,]        3       NA       NA         3        NA        NA           3
[1058,]        1       NA       NA         1        NA        NA           1
[1059,]        1        2        2         1         2         2           1
[1060,]        1        2        2         1         2         2           1
[1061,]        1        2        2         1         2         2           1
[1062,]        1        1       NA         1         1        NA           1
[1063,]        1        1       NA         1         1        NA           1
[1064,]        1        2        2         1         2         2           1
[1065,]        1        2        2         1         2         2           1
[1066,]        1       NA       NA         1        NA        NA           1
[1067,]        3        2        2         3         2         2           3
[1068,]        1        2        1         1         2         1           1
[1069,]        1        3        2         1         3         2           1
[1070,]        1        2       NA         1         2        NA           1
[1071,]        1        2       NA         1         2        NA           1
[1072,]        1        2        2         1         3         2           1
[1073,]        1       NA       NA         1        NA        NA           1
[1074,]        1       NA       NA         1        NA        NA           1
[1075,]        1        2        2         1         2         2           1
[1076,]        1       NA       NA         1        NA        NA           1
[1077,]        1        2        2         1         2         2           1
[1078,]        1        1       NA         1         1        NA           1
[1079,]        1        2       NA         1         2        NA           1
[1080,]        1        2        3         1         2         3           1
[1081,]        1        1        2         1         1         2           1
[1082,]        1       NA       NA         1        NA        NA           1
[1083,]        1        2        2         1         3         2           1
[1084,]        1        2        2         1         2         2           1
[1085,]        1       NA       NA         1        NA        NA           1
[1086,]        1        1        2         1         1         2           1
[1087,]        1        1        2         1         1         2           1
[1088,]        1        2       NA         1         2        NA           1
[1089,]        1        1       NA         1         1        NA           1
[1090,]        1        1        2         1         1         2           1
[1091,]        1        1       NA         1         1        NA           1
[1092,]        1        2       NA         1         2        NA           1
[1093,]        1        2        2         1         2         2           1
[1094,]        1       NA       NA         1        NA        NA           1
[1095,]        1        2        2         1         2         2           1
[1096,]        1       NA       NA         1        NA        NA           1
[1097,]        1        1       NA         1         1        NA           1
[1098,]        1       NA       NA         1        NA        NA           1
[1099,]        1       NA       NA         1        NA        NA           1
[1100,]        1       NA       NA         1        NA        NA           1
[1101,]        1        2       NA         1         2        NA           1
[1102,]        1       NA       NA         1        NA        NA           1
[1103,]        1       NA       NA         1        NA        NA           1
[1104,]        1        2        2         1         2         2           1
[1105,]        1        1        2         1         1         2           1
[1106,]        1        1       NA         1         1        NA           1
[1107,]        1       NA       NA         1        NA        NA           1
[1108,]        1        2        2         1         2         2           1
[1109,]        1        1       NA         1         1        NA           1
[1110,]        1        3        3         1         3         3           1
[1111,]        1        2       NA         1         2        NA           1
[1112,]        1        3       NA         1         3        NA           1
[1113,]        1        3        3         1         3         3           1
[1114,]        1       NA       NA         1        NA        NA           1
[1115,]        2        3        3         2         3         3           2
[1116,]        1        2       NA         1         2        NA           1
[1117,]        1        3        3         1         3         3           1
[1118,]        1        3        3         1         3         3           1
[1119,]        2        2        3         2         2         3           2
[1120,]        1       NA       NA         1        NA        NA           1
[1121,]        1        2        3         1         2         3           1
[1122,]        1       NA       NA         1        NA        NA           1
[1123,]        1       NA       NA         1        NA        NA           1
[1124,]        1        2        2         1         2         2           1
[1125,]        2        3        3         2         3         3           2
[1126,]        1        3        3         1         3         3           1
[1127,]        1        3        3         1         3         3           1
[1128,]        1        3       NA         1         3        NA           1
[1129,]        2       NA       NA         2        NA        NA           2
[1130,]        1        3       NA         1         3        NA           1
[1131,]        1       NA       NA         1        NA        NA           1
[1132,]        1        3        2         1         3         2           1
[1133,]        1        2        3         1         2         2           1
[1134,]        3       NA       NA         3        NA        NA           3
[1135,]        1        3        2         1         3         2           1
[1136,]        1       NA       NA         1        NA        NA           1
[1137,]        1        3        2         1         3         2           1
[1138,]        2        3        2         2         3         2           2
[1139,]        1        3        2         1         3         2           1
[1140,]        3        3        2         3         3         2           3
[1141,]        3        3        2         3         3         2           3
[1142,]        3        3        2         3         3         2           3
[1143,]        1        2       NA         1         1        NA           1
[1144,]        2        3        2         2         3         2           2
[1145,]        1        3        2         1         3         2           1
[1146,]        1        2        2         1         2         2           1
[1147,]        1        2        2         1         2         2           1
[1148,]        1        2        2         1         2         2           1
[1149,]        1        1       NA         1         1        NA           1
[1150,]        1        2        2         1         2         2           1
[1151,]        1        2        2         1         2         2           1
[1152,]        1        2        2         1         2         2           1
[1153,]        1        2       NA         1         2        NA           1
[1154,]        1        2        2         1         2         2           1
[1155,]        1        2        2         1         2         2           1
[1156,]        1        1       NA         1         1        NA           1
[1157,]        1        2       NA         1         2        NA           1
[1158,]        1        2        2         1         2         2           1
[1159,]        1        2       NA         1         1        NA           1
[1160,]        1        2       NA         1         1        NA           1
[1161,]        1        3        2         1         3         2           1
[1162,]        1        3        2         1         3         2           1
[1163,]        1        2       NA         1         2        NA           1
[1164,]        1        2       NA         1         2        NA           1
[1165,]        1       NA       NA         1        NA        NA           1
[1166,]        1       NA       NA         1        NA        NA           1
[1167,]        1        2       NA         1         2        NA           1
[1168,]        1        1       NA         1         1        NA           1
[1169,]        1        2       NA         1         2        NA           1
[1170,]        1        2       NA         1         2        NA           1
[1171,]        1        2       NA         1         2        NA           1
[1172,]        1        2       NA         1         2        NA           1
[1173,]        1        2       NA         1         2        NA           1
[1174,]        1       NA       NA         1        NA        NA           1
[1175,]        1       NA       NA         1        NA        NA           1
[1176,]        2       NA       NA         2        NA        NA           2
[1177,]        1       NA       NA         1        NA        NA           1
[1178,]        2       NA       NA         2        NA        NA           2
[1179,]        2       NA       NA         2        NA        NA           2
[1180,]        2       NA       NA         2        NA        NA           2
[1181,]        1       NA       NA         1        NA        NA           1
[1182,]        1        2       NA         1         2        NA           1
[1183,]        1        1       NA         1         1        NA           1
[1184,]        1        2       NA         1         2        NA           1
[1185,]        1        2       NA         1         2        NA           1
[1186,]        2       NA       NA         2        NA        NA           2
[1187,]        1        2       NA         1         2        NA           1
[1188,]        1       NA       NA         1        NA        NA           1
[1189,]        1        3        2         1         3         2           1
[1190,]        1        2        2         1         2         2           1
[1191,]        1       NA       NA         1        NA        NA           1
[1192,]        2        1       NA         2         1        NA           2
[1193,]        2        1       NA         2         1        NA           2
[1194,]        2        1       NA         2         1        NA           2
[1195,]        1       NA       NA         1        NA        NA           1
[1196,]        1       NA       NA         1        NA        NA           1
[1197,]        1       NA       NA         1        NA        NA           1
[1198,]        1       NA       NA         1        NA        NA           1
[1199,]        1       NA       NA         1        NA        NA           1
[1200,]        1       NA       NA         1        NA        NA           1
[1201,]        1       NA       NA         1        NA        NA           1
[1202,]        1       NA       NA         1        NA        NA           1
[1203,]        1       NA       NA         1        NA        NA           1
[1204,]        1        2       NA         1         1        NA           1
[1205,]        1       NA       NA         1        NA        NA           1
[1206,]        1        2       NA         1         1        NA           1
[1207,]        1        2        2         1         2         2           1
[1208,]        1        2        2         1         2         2           1
[1209,]        1        1       NA         1         1        NA           1
[1210,]        3        3        2         3         3         2           3
[1211,]        1       NA       NA         1        NA        NA           1
[1212,]        2        1       NA         2         1        NA           2
[1213,]        1        2        2         1         2         2           1
[1214,]        1       NA       NA         1        NA        NA           1
[1215,]        1        3        2         1         3         2           1
[1216,]        1        2        2         1         2         2           1
[1217,]        1       NA       NA         1        NA        NA           1
[1218,]        1        2       NA         1         1        NA           1
[1219,]        2        1       NA         2         1        NA           2
[1220,]        1       NA       NA         1        NA        NA           1
[1221,]        1       NA       NA         1        NA        NA           1
[1222,]        1       NA       NA         1        NA        NA           1
[1223,]        3        2        2         3         2         2           3
[1224,]        1        3        2         1         3         2           1
[1225,]        1        2       NA         1         2        NA           1
[1226,]        1        3       NA         1         3        NA           1
[1227,]        1        3        3         1         3         3           1
[1228,]        1       NA       NA         1        NA        NA           1
[1229,]        1       NA       NA         1        NA        NA           1
[1230,]        1        2        3         1         2         3           1
[1231,]        1        2        2         1         2         2           1
[1232,]        1        2        2         1         2         2           1
[1233,]        1       NA       NA         1        NA        NA           1
[1234,]        1        2       NA         1         2        NA           1
[1235,]        1        2       NA         1         2        NA           1
[1236,]        1        2       NA         1         2        NA           1
[1237,]        1        1       NA         1         1        NA           1
[1238,]        1        1       NA         1         1        NA           1
[1239,]        1       NA       NA         1        NA        NA           1
[1240,]        1        2       NA         1         2        NA           1
[1241,]        1        2        3         1         2         3           1
[1242,]        1        3        2         1         3         2           1
[1243,]        1       NA       NA         1        NA        NA           1
[1244,]        1        3        2         1         3         2           1
[1245,]        1       NA       NA         1        NA        NA           1
[1246,]        1        1       NA         1         1        NA           1
[1247,]        1        2        3         1         2         3           1
[1248,]        1        2        2         1         2         2           1
[1249,]        1       NA       NA         1        NA        NA           1
[1250,]        1        3        3         1         3         3           1
[1251,]        1        2        3         1         2         3           1
[1252,]        1       NA       NA         1        NA        NA           1
[1253,]        1        2        2         1         2         2           1
[1254,]        1        2        2         2         2         2           1
[1255,]        1        2       NA         1         2        NA           1
[1256,]        1        1       NA         1         1        NA           1
[1257,]        1        1        2         1         1         2           1
[1258,]        1        2       NA         1         2        NA           1
[1259,]        1       NA       NA         1        NA        NA           1
[1260,]        1        2        2         1         2         2           1
[1261,]        1       NA       NA         1        NA        NA           1
[1262,]        1        1       NA         1         1        NA           1
[1263,]        1       NA       NA         1        NA        NA           1
[1264,]        1       NA       NA         1        NA        NA           1
[1265,]        1       NA       NA         1        NA        NA           1
[1266,]        1        3       NA         1         3        NA           1
[1267,]        1       NA       NA         1        NA        NA           1
[1268,]        1        2        2         1         2         2           1
[1269,]        1        1        2         1         1         2           1
[1270,]        1       NA       NA         1        NA        NA           1
[1271,]        1        2        2         1         2         2           1
[1272,]        1        2       NA         1         2        NA           1
[1273,]        1       NA       NA         1        NA        NA           1
[1274,]        1        2        2         1         2         2           1
[1275,]        1        2        2         1         2         2           1
[1276,]        1        2       NA         1         2        NA           1
[1277,]        1        2        2         1         2         2           1
[1278,]        1        2        2         1         2         2           1
[1279,]        1        2        2         1         2         2           1
[1280,]        1        2        2         1         2         2           1
[1281,]        1        3        2         1         3         2           1
[1282,]        1        2        2         1         2         2           1
[1283,]        1        3        2         1         3         2           1
[1284,]        1        2        2         1         2         2           1
[1285,]        1        2        2         1         2         2           1
[1286,]        1        2       NA         1         2        NA           1
[1287,]        1       NA       NA         1        NA        NA           1
[1288,]        1        2       NA         1         2        NA           1
[1289,]        1        2       NA         1         2        NA           1
[1290,]        1        2       NA         1         2        NA           1
[1291,]        1        2       NA         1         2        NA           1
[1292,]        1        2       NA         1         2        NA           1
[1293,]        1        3       NA         1         3        NA           1
[1294,]        1       NA       NA         1        NA        NA           1
[1295,]        1       NA       NA         1        NA        NA           1
[1296,]        1       NA       NA         1        NA        NA           1
[1297,]        1       NA       NA         1        NA        NA           1
[1298,]        1        2       NA         1         2        NA           1
[1299,]        1        3       NA         1         3        NA           1
[1300,]        1        3       NA         1         3        NA           1
[1301,]        1       NA       NA         1        NA        NA           1
[1302,]        1       NA       NA         1        NA        NA           1
[1303,]        1        2        2         1         2         2           1
[1304,]        1        2        2         1         2         2           1
[1305,]        1        3        2         1         2         2           1
[1306,]        1        2       NA         1         2        NA           1
[1307,]        1        1       NA         1         1        NA           1
[1308,]        1       NA       NA         1        NA        NA           1
[1309,]        1       NA       NA         1        NA        NA           1
[1310,]        1       NA       NA         1        NA        NA           1
[1311,]        1       NA       NA         1        NA        NA           1
[1312,]        1        1       NA         1         1        NA           1
[1313,]        1        2        2         1         2         2           1
[1314,]        1        2        2         1         2         2           1
[1315,]        1       NA       NA         1        NA        NA           1
[1316,]        1       NA       NA         1        NA        NA           1
[1317,]        1        1        2         1         1         2           1
[1318,]        1        2        2         1         2         2           1
[1319,]        1        1       NA         1         1        NA           1
[1320,]        1        1        2         1         1         2           1
[1321,]        1       NA       NA         1        NA        NA           1
[1322,]        1       NA       NA         1        NA        NA           1
[1323,]        1       NA       NA         1        NA        NA           1
[1324,]        1       NA       NA         1        NA        NA           1
[1325,]        1        1       NA         1         1        NA           1
[1326,]        1        2       NA         1         2        NA           1
[1327,]        1        1       NA         1         1        NA           1
[1328,]        1        2       NA         1         2        NA           1
[1329,]        2        3        3         3         3         3           2
[1330,]        1        1       NA         1         1        NA           1
[1331,]        1        3        3         1         3         3           1
[1332,]        1       NA       NA         1        NA        NA           1
[1333,]        1        2        2         1         2         2           1
[1334,]        1        2        2         1         2         2           1
[1335,]        1       NA       NA         1        NA        NA           1
[1336,]        1        3        3         1         3         3           1
[1337,]        1       NA       NA         1        NA        NA           1
[1338,]        1        3        3         1         3         3           1
[1339,]        1        1       NA         1         1        NA           1
[1340,]        1       NA       NA         1        NA        NA           1
[1341,]        1       NA       NA         1        NA        NA           1
[1342,]        1       NA       NA         1        NA        NA           1
[1343,]        1       NA       NA         1        NA        NA           1
[1344,]        2        3       NA         2         3        NA           2
[1345,]        2        3       NA         2         3        NA           1
[1346,]        3        3       NA         3         3        NA           3
[1347,]        2        3       NA         2         3        NA           2
[1348,]        1        1        3         1         1         3           1
[1349,]        1       NA       NA         1        NA        NA           1
[1350,]        1        2       NA         1         2        NA           1
[1351,]        1        2        3         1         2         3           1
[1352,]        3        3       NA         3         3        NA           3
[1353,]        1        2        3         1         2         3           1
[1354,]        1       NA       NA         1        NA        NA           1
[1355,]        1        3        2         1         3         2           1
[1356,]        1       NA       NA         1        NA        NA           1
[1357,]        1       NA       NA         1        NA        NA           1
[1358,]        1       NA       NA         1        NA        NA           1
[1359,]        1       NA       NA         1        NA        NA           1
[1360,]        1        3        3         1         3         3           1
[1361,]        1        3       NA         1         3        NA           1
[1362,]        1       NA       NA         1        NA        NA           1
[1363,]        2        2        3         2         2         3           2
[1364,]        1        3        3         1         3         3           1
[1365,]        1        2       NA         1         2        NA           1
[1366,]        1        3        3         1         3         3           1
[1367,]        1        3        3         1         3         3           1
[1368,]        1       NA       NA         1        NA        NA           1
[1369,]        1       NA       NA         1        NA        NA           1
[1370,]        2        2        3         2         2         3           2
[1371,]        1       NA       NA         1        NA        NA           1
[1372,]        1        1        3         1         1         3           1
[1373,]        1        1        3         1         1         3           1
[1374,]        1       NA       NA         1        NA        NA           1
[1375,]        1        2       NA         1         2        NA           1
[1376,]        1        2       NA         1         2        NA           1
[1377,]        1       NA       NA         1        NA        NA           1
[1378,]        1        1        2         1         1         2           1
[1379,]        1        1       NA         1         1        NA           1
[1380,]        1       NA       NA         1        NA        NA           1
[1381,]        1       NA       NA         1        NA        NA           1
[1382,]        1       NA       NA         1        NA        NA           1
[1383,]        1        1       NA         1         1        NA           1
[1384,]        1        2       NA         1         2        NA           1
[1385,]        1        1        2         1         1         2           1
[1386,]        1        1       NA         1         1        NA           1
[1387,]        1        2        2         1         2         2           1
[1388,]        1        2        2         1         2         2           1
[1389,]        1        2        2         1         2         2           1
[1390,]        1        2       NA         1         2        NA           1
[1391,]        1        2       NA         1         2        NA           1
[1392,]        1        1        2         1         1         2           1
[1393,]        1        1        2         1         1         2           1
[1394,]        1        2        2         1         2         2           1
[1395,]        1        2        2         1         2         2           1
[1396,]        1        2        2         1         2         2           1
[1397,]        1        1        2         1         1         2           1
[1398,]        1        1        2         1         1         2           1
[1399,]        1       NA       NA         1        NA        NA           1
[1400,]        1       NA       NA         1        NA        NA           1
[1401,]        1       NA       NA         1        NA        NA           1
[1402,]        1        1       NA         1         1        NA           1
[1403,]        1       NA       NA         1        NA        NA           1
[1404,]        1       NA       NA         1        NA        NA           1
[1405,]        1       NA       NA         1        NA        NA           1
[1406,]        1       NA       NA         1        NA        NA           1
[1407,]        1        3        3         1         3         3           1
[1408,]        1        1       NA         1         1        NA           1
[1409,]        1        2        3         1         2         3           1
[1410,]        1       NA       NA         1        NA        NA           1
[1411,]        1        2        2         1         2         2           1
[1412,]        1        2        2         1         2         2           1
[1413,]        1       NA       NA         1        NA        NA           1
[1414,]        1        3        3         1         3         3           1
[1415,]        1       NA       NA         1        NA        NA           1
[1416,]        1        2        3         1         2         3           1
[1417,]        1        1       NA         1         1        NA           1
[1418,]        1       NA       NA         1        NA        NA           1
[1419,]        1       NA       NA         1        NA        NA           1
[1420,]        1       NA       NA         1        NA        NA           1
[1421,]        1        2        3         1         2         3           1
[1422,]        1        2        2         1         2         2           1
[1423,]        1       NA       NA         1        NA        NA           1
[1424,]        1        3        3         1         3         3           1
[1425,]        1        2        2         1         2         2           1
[1426,]        1       NA       NA         1        NA        NA           1
[1427,]        2        3       NA         2         3        NA           2
[1428,]        2        3       NA         2         3        NA           2
[1429,]        3        3       NA         3         3        NA           3
[1430,]        1        1        3         1         1         3           1
[1431,]        1       NA       NA         1        NA        NA           1
[1432,]        1        2       NA         1         2        NA           1
[1433,]        1        3        3         1         3         3           1
[1434,]        3        3       NA         3         3        NA           3
[1435,]        1        3        3         1         3         3           1
[1436,]        1       NA       NA         1        NA        NA           1
[1437,]        1        3        3         1         3         3           1
[1438,]        1       NA       NA         1        NA        NA           1
[1439,]        1       NA       NA         1        NA        NA           1
[1440,]        1       NA       NA         1        NA        NA           1
[1441,]        1        3        3         1         3         3           1
[1442,]        1        2        3         1         2         3           1
[1443,]        1        2        3         1         2         3           1
[1444,]        1        2        3         1         2         3           1
[1445,]        1        2        3         1         2         3           1
[1446,]        1        3        3         1         3         3           1
[1447,]        1        2       NA         1         2        NA           1
[1448,]        1        3        3         1         3         3           1
[1449,]        1       NA       NA         1        NA        NA           1
[1450,]        1       NA       NA         1        NA        NA           1
[1451,]        1       NA       NA         1        NA        NA           1
[1452,]        1       NA       NA         1        NA        NA           1
[1453,]        1       NA       NA         1        NA        NA           1
[1454,]        1       NA       NA         1        NA        NA           1
[1455,]        1       NA       NA         1        NA        NA           1
[1456,]        1        2        3         1         2         3           1
[1457,]        1        3        3         1         3         3           1
[1458,]        1        2        3         1         2         3           1
[1459,]        1        2        3         1         2         3           1
[1460,]        3        3       NA         3         3        NA           3
[1461,]        1       NA       NA         1        NA        NA           1
[1462,]        1        2       NA         1         2        NA           1
[1463,]        1        2        3         1         2         3           1
[1464,]        1        2        3         1         2         3           1
[1465,]        1        3        3         1         3         3           1
[1466,]        1        3        3         1         3         3           1
[1467,]        1        3       NA         1         3        NA           1
[1468,]        1       NA       NA         1        NA        NA           1
[1469,]        2        2        3         2         2         3           2
[1470,]        1        3        3         1         3         3           1
[1471,]        2        2       NA         2         2        NA           1
[1472,]        1        3        3         1         3         3           1
[1473,]        1        3        3         1         3         3           1
[1474,]        1       NA       NA         1        NA        NA           1
[1475,]        1       NA       NA         1        NA        NA           1
[1476,]        1       NA       NA         1        NA        NA           1
[1477,]        1        2        3         1         2         3           1
[1478,]        1        2        3         1         2         3           1
[1479,]        1        2       NA         1         2        NA           1
[1480,]        1        2       NA         1         2        NA           1
[1481,]        1       NA       NA         1        NA        NA           1
[1482,]        1       NA       NA         1        NA        NA           1
[1483,]        2        3       NA         2         3        NA           2
[1484,]        2        3       NA         2         3        NA           1
[1485,]        3        3       NA         3         3        NA           3
[1486,]        1        2        3         1         2         3           1
[1487,]        1       NA       NA         1        NA        NA           1
[1488,]        1        2       NA         1         2        NA           1
[1489,]        1        3        3         1         3         3           1
[1490,]        1        3        3         1         3         3           1
[1491,]        1       NA       NA         1        NA        NA           1
[1492,]        1        3        3         1         3         3           1
[1493,]        1       NA       NA         1        NA        NA           1
[1494,]        1       NA       NA         1        NA        NA           1
[1495,]        1       NA       NA         1        NA        NA           1
[1496,]        1        2        3         1         1         3           1
[1497,]        1        3        3         1         3         3           1
[1498,]        1        3        3         1         3         3           1
[1499,]        1       NA       NA         1        NA        NA           1
[1500,]        1       NA       NA         1        NA        NA           1
[1501,]        1        3        3         1         3         3           1
[1502,]        1        3        3         1         3         3           1
[1503,]        3        3       NA         3         3        NA           3
[1504,]        3        3       NA         3         3        NA           3
[1505,]        1       NA       NA         1        NA        NA           1
[1506,]        1       NA       NA         1        NA        NA           1
[1507,]        1        3       NA         1         3        NA           1
[1508,]        1       NA       NA         1        NA        NA           1
[1509,]        1        2       NA         1         2        NA           1
[1510,]        1        3        3         1         3         3           1
[1511,]        1        3        2         1         3         2           1
[1512,]        1        2        3         1         2         3           1
[1513,]        1        2       NA         1         2        NA           1
[1514,]        1        1        2         1         1         2           1
[1515,]        2        2       NA         2         2        NA           1
[1516,]        1       NA       NA         1        NA        NA           1
[1517,]        1        3        2         1         3         2           1
[1518,]        2        2        2         2         2         2           1
[1519,]        3        2       NA         3         2        NA           2
[1520,]        3        2       NA         3         2        NA           3
[1521,]        1        1       NA         1         1        NA           1
[1522,]        1        2       NA         1         2        NA           1
[1523,]        1       NA       NA         1        NA        NA           1
[1524,]        1       NA       NA         1        NA        NA           1
[1525,]        1        2       NA         1         2        NA           1
[1526,]        1        2       NA         1         2        NA           1
[1527,]        1        2        2         1         2         2           1
[1528,]        1       NA       NA         1        NA        NA           1
[1529,]        1        1        2         1         1         2           1
[1530,]        1        1        2         1         1         2           1
[1531,]        1        2        2         1         2         2           1
[1532,]        1       NA       NA         1        NA        NA           1
[1533,]        1        1        2         1         1         2           1
[1534,]        2       NA       NA         2        NA        NA           2
[1535,]        1        1       NA         1         1        NA           1
[1536,]        1       NA       NA         1        NA        NA           1
[1537,]        2        2        2         2         2         3           2
[1538,]        2        1       NA         2         1        NA           1
[1539,]        1       NA       NA         1        NA        NA           1
[1540,]        1       NA       NA         1        NA        NA           1
[1541,]        1        2        2         1         2         2           1
[1542,]        2       NA       NA         2        NA        NA           2
[1543,]        2        2       NA         2         2        NA           2
[1544,]        1        1        2         1         1         2           1
[1545,]        1        2       NA         1         2        NA           1
[1546,]        1       NA       NA         1        NA        NA           1
[1547,]        1        2       NA         1         2        NA           1
[1548,]        1       NA       NA         1        NA        NA           1
[1549,]        2        3        2         2         3         2           2
[1550,]        1       NA       NA         1        NA        NA           1
[1551,]        1       NA       NA         1        NA        NA           1
[1552,]        3        2       NA         3         2        NA           3
[1553,]        1       NA       NA         1        NA        NA           1
[1554,]        1        2        3         1         2         3           1
[1555,]        1       NA       NA         1        NA        NA           1
[1556,]        1        3       NA         1         3        NA           1
[1557,]        2        3       NA         2         3        NA           2
[1558,]        1        3        3         1         3         3           1
[1559,]        1       NA       NA         1        NA        NA           1
[1560,]        2        3       NA         2         3        NA           1
[1561,]        1        3        3         1         3         3           1
[1562,]        1       NA       NA         1        NA        NA           1
[1563,]        1        3       NA         1         3        NA           1
[1564,]        1       NA       NA         1        NA        NA           1
[1565,]        1       NA       NA         1        NA        NA           1
[1566,]        1        2        2         1         2         2           1
[1567,]        2        2       NA         2         2        NA           2
[1568,]        1        2        2         1         2         2           1
[1569,]        2        2       NA         2         2        NA           2
[1570,]        1        2        2         1         2         1           1
[1571,]        1        1        2         1         1         2           1
[1572,]        2        3        2         1         2         1           2
[1573,]        2        3        2         1         2         1           2
[1574,]        1        2        2         1         2         2           1
[1575,]        1        2        2         1         2         2           1
[1576,]        1        2        2         1         2         2           1
[1577,]        3        3        2         2         2         2           3
[1578,]        3        3        2         2         2         2           3
[1579,]        2        3        2         1         2         1           1
[1580,]        2        2       NA         2         2        NA           2
[1581,]        2        2       NA         2         2        NA           2
[1582,]        2        3        2         1         2         1           2
[1583,]        1        3        2         1         2         1           1
[1584,]        2        2       NA         2         2        NA           2
[1585,]        1        2       NA         1         2        NA           1
[1586,]        2        2       NA         2         2        NA           2
[1587,]        1       NA       NA         1        NA        NA           1
[1588,]        2        2       NA         2         2        NA           2
[1589,]        2        3        2         1         2         1           2
[1590,]        1        2       NA         1         1        NA           1
[1591,]        1        2       NA         1         1        NA           1
[1592,]        1        2       NA         1         1        NA           1
[1593,]        1        2       NA         1         1        NA           1
[1594,]        1        2        2         1         1         2           1
[1595,]        1        2        2         1         1         2           1
[1596,]        1        1        2         1         1         2           1
[1597,]        1        2        3         1         1         2           1
[1598,]        1        2        3         1         1         2           1
[1599,]        1        1        2         1         1         2           1
[1600,]        1        1        2         1         1         2           1
[1601,]        1        1        2         1         1         2           1
[1602,]        1        1        2         1         1         2           1
[1603,]        1        2        2         1         2         2           1
[1604,]        1        1        2         1         1         2           1
[1605,]        1        2        2         1         2         1           1
[1606,]        1        2       NA         1         2        NA           1
[1607,]        1        2       NA         1         2        NA           1
[1608,]        1       NA       NA         1        NA        NA           1
[1609,]        1        2       NA         1         2        NA           1
[1610,]        1        2       NA         1         2        NA           1
[1611,]        1       NA       NA         1        NA        NA           1
[1612,]        1       NA       NA         1        NA        NA           1
[1613,]        1       NA       NA         1        NA        NA           1
[1614,]        1       NA       NA         1        NA        NA           1
[1615,]        1       NA       NA         1        NA        NA           1
[1616,]        1       NA       NA         1        NA        NA           1
[1617,]        1        2       NA         1         2        NA           1
[1618,]        1       NA       NA         1        NA        NA           1
[1619,]        1       NA       NA         1        NA        NA           1
[1620,]        1       NA       NA         1        NA        NA           1
[1621,]        1       NA       NA         1        NA        NA           1
[1622,]        1        2       NA         1         2        NA           1
[1623,]        1       NA       NA         1        NA        NA           1
[1624,]        2        2       NA         2         2        NA           2
[1625,]        1        2       NA         1         1        NA           1
[1626,]        1       NA       NA         1        NA        NA           1
[1627,]        1       NA       NA         1        NA        NA           1
[1628,]        2        2       NA         2         2        NA           2
[1629,]        1       NA       NA         1        NA        NA           1
[1630,]        1       NA       NA         1        NA        NA           1
[1631,]        1       NA       NA         1        NA        NA           1
[1632,]        1       NA       NA         1        NA        NA           1
[1633,]        1       NA       NA         1        NA        NA           1
[1634,]        1       NA       NA         1        NA        NA           1
[1635,]        1       NA       NA         1        NA        NA           1
[1636,]        1       NA       NA         1        NA        NA           1
[1637,]        1       NA       NA         1        NA        NA           1
[1638,]        1        2       NA         1         1        NA           1
[1639,]        1       NA       NA         1        NA        NA           1
[1640,]        1        1       NA         1         1        NA           1
[1641,]        1        1       NA         1         1        NA           1
[1642,]        1        2       NA         1         1        NA           1
[1643,]        1        1       NA         1         1        NA           1
[1644,]        1        2       NA         1         1        NA           1
[1645,]        1       NA       NA         1        NA        NA           1
[1646,]        1       NA       NA         1        NA        NA           1
[1647,]        1       NA       NA         1        NA        NA           1
[1648,]        1       NA       NA         1        NA        NA           1
[1649,]        1       NA       NA         1        NA        NA           1
[1650,]        1       NA       NA         1        NA        NA           1
[1651,]        1       NA       NA         1        NA        NA           1
[1652,]        1       NA       NA         1        NA        NA           1
[1653,]        1       NA       NA         1        NA        NA           1
[1654,]        1       NA       NA         1        NA        NA           1
[1655,]        1       NA       NA         1        NA        NA           1
[1656,]        1       NA       NA         1        NA        NA           1
[1657,]        1       NA       NA         1        NA        NA           1
[1658,]        1       NA       NA         1        NA        NA           1
[1659,]        1        2       NA         1         2        NA           1
[1660,]        1        1        2         1         1         2           1
[1661,]        1        1        2         1         1         2           1
[1662,]        1        2        2         1         2         2           1
[1663,]        1        1        2         1         1         2           1
[1664,]        1        1        2         1         1         2           1
[1665,]        1        2        2         1         1         2           1
[1666,]        1        2        2         1         1         2           1
[1667,]        1        1       NA         1         1        NA           1
[1668,]        1        2        2         1         1         2           1
[1669,]        1        1        2         1         1         2           1
[1670,]        1        2        2         1         1         2           1
[1671,]        1        1        2         1         1         2           1
[1672,]        1        1        2         1         1         2           1
[1673,]        1        2        2         1         1         2           1
[1674,]        1        1       NA         1         1        NA           1
[1675,]        1        1       NA         1         1        NA           1
[1676,]        1        2        2         1         1         2           1
[1677,]        1        2        2         1         1         2           1
[1678,]        1        2        2         1         2         2           1
[1679,]        1        2        2         1         2         2           1
[1680,]        1        1        2         1         1         2           1
[1681,]        1        2        2         1         1         2           1
[1682,]        1        2        2         1         2         2           1
[1683,]        1        2        3         1         1         2           1
[1684,]        1        2       NA         1         1        NA           1
[1685,]        1        2        2         1         1         2           1
[1686,]        2        2        2         2         2         2           2
[1687,]        2        2        2         2         2         2           2
[1688,]        2        2        2         2         2         2           2
[1689,]        2        2        2         2         2         2           2
[1690,]        2        2        2         2         2         2           2
[1691,]        2        2        2         2         2         2           2
[1692,]        2        2        2         2         2         2           2
[1693,]        3        3        2         2         2         2           3
[1694,]        1        2        2         1         2         2           1
[1695,]        1        1        2         1         1         2           1
[1696,]        1        2        2         1         1         2           1
[1697,]        1        2        2         1         2         2           1
[1698,]        1        2        2         1         2         2           1
[1699,]        1        1        2         1         1         2           1
[1700,]        2        2        2         2         2         2           2
[1701,]        1        1        2         1         1         2           1
[1702,]        3        3        2         2         2         2           3
[1703,]        3        3        2         2         2         2           3
[1704,]        3        3        2         2         2         2           3
[1705,]        3        3        2         2         2         2           2
[1706,]        3        3        2         2         2         2           2
[1707,]        3        3        2         2         2         2           2
[1708,]        3        3        2         2         2         2           3
[1709,]        1       NA       NA         1        NA        NA           1
[1710,]        1       NA       NA         1        NA        NA           1
[1711,]        1       NA       NA         1        NA        NA           1
[1712,]        1       NA       NA         1        NA        NA           1
[1713,]        1       NA       NA         1        NA        NA           1
[1714,]        1       NA       NA         1        NA        NA           1
[1715,]        1       NA       NA         1        NA        NA           1
[1716,]        1        1       NA         1         1        NA           1
[1717,]        1        1       NA         1         1        NA           1
[1718,]        1        1        2         1         1         2           1
[1719,]        1       NA       NA         1        NA        NA           1
[1720,]        1       NA       NA         1        NA        NA           1
[1721,]        1       NA       NA         1        NA        NA           1
[1722,]        1       NA       NA         1        NA        NA           1
[1723,]        1       NA       NA         1        NA        NA           1
[1724,]        1        1       NA         1         1        NA           1
[1725,]        1        1       NA         1         1        NA           1
[1726,]        1       NA       NA         1        NA        NA           1
[1727,]        1       NA       NA         1        NA        NA           1
[1728,]        1        1       NA         1         1        NA           1
[1729,]        1        1       NA         1         1        NA           1
[1730,]        1       NA       NA         1        NA        NA           1
[1731,]        1       NA       NA         1        NA        NA           1
[1732,]        1       NA       NA         1        NA        NA           1
[1733,]        1       NA       NA         1        NA        NA           1
[1734,]        1       NA       NA         1        NA        NA           1
[1735,]        1       NA       NA         1        NA        NA           1
[1736,]        1       NA       NA         1        NA        NA           1
[1737,]        2       NA       NA         2        NA        NA           2
[1738,]        2       NA       NA         2        NA        NA           2
[1739,]        2       NA       NA         2        NA        NA           2
[1740,]        2       NA       NA         2        NA        NA           2
[1741,]        2       NA       NA         2        NA        NA           2
[1742,]        1       NA       NA         1        NA        NA           1
[1743,]        1       NA       NA         1        NA        NA           1
[1744,]        1       NA       NA         1        NA        NA           1
[1745,]        1       NA       NA         1        NA        NA           1
[1746,]        1       NA       NA         1        NA        NA           1
[1747,]        1       NA       NA         1        NA        NA           1
[1748,]        1       NA       NA         1        NA        NA           1
[1749,]        1        2       NA         1         2        NA           1
[1750,]        1        2       NA         1         2        NA           1
[1751,]        1        2       NA         1         2        NA           1
[1752,]        1        2       NA         1         2        NA           1
[1753,]        1        2       NA         1         1        NA           1
[1754,]        1       NA       NA         1        NA        NA           1
[1755,]        1       NA       NA         1        NA        NA           1
[1756,]        1       NA       NA         1        NA        NA           1
[1757,]        1       NA       NA         1        NA        NA           1
[1758,]        1       NA       NA         1        NA        NA           1
[1759,]        1        2       NA         1         1        NA           1
[1760,]        1        2       NA         1         1        NA           1
[1761,]        1        2       NA         1         1        NA           1
[1762,]        1        1       NA         1         1        NA           1
[1763,]        1       NA       NA         1        NA        NA           1
[1764,]        1        2       NA         1         1        NA           1
[1765,]        1       NA       NA         1        NA        NA           1
[1766,]        1        1        2         1         1         2           1
[1767,]        1       NA       NA         1        NA        NA           1
[1768,]        1       NA       NA         1        NA        NA           1
[1769,]        1       NA       NA         1        NA        NA           1
[1770,]        1       NA       NA         1        NA        NA           1
[1771,]        1       NA       NA         1        NA        NA           1
[1772,]        2        3        2         1         2         1           1
[1773,]        2        3        2         1         2         1           1
[1774,]        2        3        2         1         2         1           2
[1775,]        2        2       NA         2         2        NA           2
[1776,]        1        2       NA         1         2        NA           1
[1777,]        2        3        2         1         2         1           1
[1778,]        2        2       NA         2         2        NA           2
[1779,]        1        2       NA         1         1        NA           1
[1780,]        1        2        2         1         1         2           1
[1781,]        1        2        2         1         1         2           1
[1782,]        1        2       NA         1         1        NA           1
[1783,]        1        1       NA         1         1        NA           1
[1784,]        1        2        2         1         1         2           1
[1785,]        1        1        2         1         1         2           1
[1786,]        1        2        3         1         1         2           1
[1787,]        1        2        3         1         1         2           1
[1788,]        1        1        2         1         1         2           1
[1789,]        1        2        3         1         1         2           1
[1790,]        1        2        2         1         2         2           1
[1791,]        1        1        2         1         1         2           1
[1792,]        1        1        2         1         1         2           1
[1793,]        1       NA       NA         1        NA        NA           1
[1794,]        1        1        2         1         1         2           1
[1795,]        1        1        2         1         1         2           1
[1796,]        2        2        2         2         2         2           2
[1797,]        1        2        2         1         2         2           1
[1798,]        1        1        2         1         1         2           1
[1799,]        3        3        2         2         2         2           3
[1800,]        3        3        2         2         2         2           3
[1801,]        3        3        2         2         2         2           3
[1802,]        3        3        2         2         2         2           3
[1803,]        1       NA       NA         1        NA        NA           1
[1804,]        2       NA       NA         2        NA        NA           2
[1805,]        1        2       NA         1         2        NA           1
[1806,]        1        1        2         1         1         2           1
[1807,]        1        2        2         1         1         2           1
[1808,]        1        2       NA         1         2        NA           1
[1809,]        1       NA       NA         1        NA        NA           1
[1810,]        1        2       NA         1         1        NA           1
[1811,]        1        2       NA         1         2        NA           1
[1812,]        1        2       NA         1         2        NA           1
[1813,]        1        2       NA         1         2        NA           1
[1814,]        1        2       NA         1         2        NA           1
[1815,]        1        2       NA         1         1        NA           1
[1816,]        1        2       NA         1         1        NA           1
[1817,]        1        2        2         1         2         2           1
[1818,]        1        2        2         1         2         2           1
[1819,]        1        2        2         1         2         2           1
[1820,]        1        1        2         1         1         2           1
[1821,]        1        2        2         1         2         2           1
[1822,]        2        2        2         2         2         2           2
[1823,]        2        2        2         2         2         2           2
[1824,]        2        2        2         2         2         2           2
[1825,]        3        3        2         2         2         2           3
[1826,]        3        3        2         2         2         2           3
[1827,]        3        3        2         2         2         2           3
[1828,]        2        2       NA         2         2        NA           2
[1829,]        1        1       NA         1         1        NA           1
[1830,]        2        2       NA         2         2        NA           2
[1831,]        1       NA       NA         1        NA        NA           1
[1832,]        1       NA       NA         1        NA        NA           1
[1833,]        1       NA       NA         1        NA        NA           1
[1834,]        1       NA       NA         1        NA        NA           1
[1835,]        1        2       NA         1         2        NA           1
[1836,]        1       NA       NA         1        NA        NA           1
[1837,]        1       NA       NA         1        NA        NA           1
[1838,]        1       NA       NA         1        NA        NA           1
[1839,]        1        2       NA         1         2        NA           1
[1840,]        1        2       NA         1         2        NA           1
[1841,]        1       NA       NA         1        NA        NA           1
[1842,]        1        1       NA         1         1        NA           1
[1843,]        2        3        2         1         2         1           2
[1844,]        2        3        2         1         2         1           2
[1845,]        2        3        2         1         2         1           2
[1846,]        1        2        2         1         1         2           1
[1847,]        1        1       NA         1         1        NA           1
[1848,]        1       NA       NA         1        NA        NA           1
[1849,]        2        3        2         1         2         1           2
[1850,]        2        2       NA         2         2        NA           2
[1851,]        1       NA       NA         1        NA        NA           1
[1852,]        1       NA       NA         1        NA        NA           1
[1853,]        3        3        2         2         2         2           3
[1854,]        1        2        2         1         1         2           1
[1855,]        1        1       NA         1         1        NA           1
[1856,]        1       NA       NA         1        NA        NA           1
[1857,]        1        2        2         1         2         2           1
[1858,]        2        2        2         2         2         2           2
[1859,]        1        2       NA         1         1        NA           1
[1860,]        1       NA       NA         1        NA        NA           1
[1861,]        1        2        3         1         1         2           1
[1862,]        2        2       NA         2         2        NA           2
[1863,]        1        1        2         1         1         2           1
[1864,]        1        1        2         1         1         2           1
[1865,]        1        1        2         1         1         2           1
[1866,]        1        1        2         1         1         2           1
[1867,]        1        2        2         1         1         2           1
[1868,]        1        2        2         1         1         2           1
[1869,]        1        2       NA         1         2        NA           1
[1870,]        1       NA       NA         1        NA        NA           1
[1871,]        1       NA       NA         1        NA        NA           1
[1872,]        2        2       NA         2         2        NA           2
[1873,]        1       NA       NA         1        NA        NA           1
[1874,]        1        2        2         1         1         2           1
[1875,]        1       NA       NA         1        NA        NA           1
[1876,]        1       NA       NA         1        NA        NA           1
[1877,]        1       NA       NA         1        NA        NA           1
[1878,]        1       NA       NA         1        NA        NA           1
[1879,]        1       NA       NA         1        NA        NA           1
[1880,]        1       NA       NA         1        NA        NA           1
[1881,]        3        3        2         2         2         2           3
[1882,]        1       NA       NA         1        NA        NA           1
[1883,]        2       NA       NA         2        NA        NA           2
[1884,]        1        2       NA         1         2        NA           1
[1885,]        1       NA       NA         1        NA        NA           1
[1886,]        1       NA       NA         1        NA        NA           1
[1887,]        1        2       NA         1         2        NA           1
[1888,]        1        2        2         1         2         2           1
[1889,]        1       NA       NA         1        NA        NA           1
[1890,]        2        1        2         2         1         2           2
[1891,]        1        1        2         1         1         2           1
[1892,]        1       NA       NA         1        NA        NA           1
[1893,]        1        2        2         1         2         2           1
[1894,]        1        2        2         1         2         2           1
[1895,]        1       NA       NA         1        NA        NA           1
[1896,]        2        2        2         2         2         2           2
[1897,]        1        2        2         1         2         2           1
[1898,]        1        2        2         1         2         2           1
[1899,]        1        2        2         1         2         2           1
[1900,]        1        2        2         1         2         2           1
[1901,]        1        2        2         1         2         2           1
[1902,]        1        2        2         1         2         2           1
[1903,]        2        2        2         2         2         2           2
[1904,]        2        2        2         2         2         2           2
[1905,]        1        1        2         1         1         2           1
[1906,]        1       NA       NA         1        NA        NA           1
[1907,]        2       NA       NA         2        NA        NA           2
[1908,]        1        2        2         1         2         2           1
[1909,]        1       NA       NA         1        NA        NA           1
[1910,]        1        2        2         1         2         2           1
[1911,]        1        2       NA         1         2        NA           1
[1912,]        2        2        3         2         2         3           2
[1913,]        1       NA       NA         1        NA        NA           1
[1914,]        1       NA       NA         1        NA        NA           1
[1915,]        1        2        2         1         2         2           1
[1916,]        2       NA       NA         2        NA        NA           2
[1917,]        2        2       NA         2         2        NA           2
[1918,]        1        2        2         1         2         2           1
[1919,]        2        2       NA         2         2        NA           1
[1920,]        2       NA       NA         2        NA        NA           2
[1921,]        1        2       NA         1         2        NA           1
[1922,]        1       NA       NA         1        NA        NA           1
[1923,]        3        3        3         3         3         3           3
[1924,]        1        2        3         1         2         3           1
[1925,]        1        2        2         1         2         3           1
[1926,]        1       NA       NA         1        NA        NA           1
[1927,]        2        3        3         2         3         3           2
[1928,]        1       NA       NA         1        NA        NA           1
[1929,]        1       NA       NA         1        NA        NA           1
[1930,]        3        3       NA         3         2        NA           3
[1931,]        1       NA       NA         1        NA        NA           1
[1932,]        1        3        3         1         3         3           1
[1933,]        1       NA       NA         1        NA        NA           1
[1934,]        1        3       NA         1         3        NA           1
[1935,]        2        3       NA         2         3        NA           2
[1936,]        1        3        3         1         3         3           1
[1937,]        1       NA       NA         1        NA        NA           1
[1938,]        2        3       NA         2         3        NA           1
[1939,]        1        3        3         1         3         3           1
[1940,]        1       NA       NA         1        NA        NA           1
[1941,]        1        3       NA         1         3        NA           1
[1942,]        2       NA       NA         1        NA        NA           2
[1943,]        1       NA       NA         1        NA        NA           1
[1944,]        2       NA       NA         1        NA        NA           2
[1945,]        2       NA       NA         1        NA        NA           2
[1946,]        1       NA       NA         1        NA        NA           1
[1947,]        1       NA       NA         1        NA        NA           1
[1948,]        2        2        3         2         2         3           2
[1949,]        1        2       NA         1         2        NA           1
[1950,]        1        2        2         1         2         2           1
[1951,]        2        3       NA         2         3        NA           2
[1952,]        1        1        3         1         1         3           1
[1953,]        1        2        2         1         2         2           1
[1954,]        1        3       NA         1         2        NA           1
[1955,]        1        3        3         1         3         3           1
[1956,]        1        3        3         1         3         3           1
[1957,]        1        3        3         1         3         3           1
[1958,]        1        3        3         1         3         3           1
[1959,]        1        2       NA         1         2        NA           1
[1960,]        2        2       NA         1         2        NA           2
[1961,]        2        2       NA         1         2        NA           2
[1962,]        2        2       NA         1         2        NA           1
[1963,]        1        2        3         1         2         3           1
[1964,]        1        2        3         1         2         3           1
[1965,]        1        2        3         1         2         3           1
[1966,]        1        2        3         1         2         3           1
[1967,]        1        2        3         1         2         3           1
[1968,]        1        2       NA         1         2        NA           1
[1969,]        1        2       NA         1         2        NA           1
[1970,]        1       NA       NA         1        NA        NA           1
[1971,]        1       NA       NA         1        NA        NA           1
[1972,]        1       NA       NA         1        NA        NA           1
[1973,]        1       NA       NA         1        NA        NA           1
[1974,]        2        2       NA         1         2        NA           1
[1975,]        1       NA       NA         1        NA        NA           1
[1976,]        1        3       NA         1         2        NA           1
[1977,]        1       NA       NA         1        NA        NA           1
[1978,]        1       NA       NA         1        NA        NA           1
[1979,]        1       NA       NA         1        NA        NA           1
[1980,]        1       NA       NA         1        NA        NA           1
[1981,]        1        2       NA         1         2        NA           1
[1982,]        1        2       NA         1         2        NA           1
[1983,]        1       NA       NA         1        NA        NA           1
[1984,]        1       NA       NA         1        NA        NA           1
[1985,]        1       NA       NA         1        NA        NA           1
[1986,]        2       NA       NA         2        NA        NA           1
[1987,]        1       NA       NA         1        NA        NA           1
[1988,]        1        3        3         1         3         3           1
[1989,]        2        2        3         2         2         3           1
[1990,]        1        2        3         1         2         3           1
[1991,]        1        2        3         1         2         3           1
[1992,]        2        2        3         2         2         3           1
[1993,]        1        2        3         1         2         3           1
[1994,]        1        2        3         1         2         3           1
[1995,]        1        2        3         1         2         3           1
[1996,]        1        2       NA         1         2        NA           1
[1997,]        1        2        3         1         2         3           1
[1998,]        1        3        3         1         3         3           1
[1999,]        1        3        3         1         3         3           1
[2000,]        1        2        3         1         2         3           1
[2001,]        2        3        3         2         3         3           2
[2002,]        2        3        3         2         3         3           2
[2003,]        3        3        3         3         3         3           3
[2004,]        3        3        3         3         3         3           3
[2005,]        1        3        3         1         3         3           1
[2006,]        1        2        3         1         2         3           1
[2007,]        1        2        3         1         2         3           1
[2008,]        3        3        3         3         3         3           3
[2009,]        3        3        3         3         3         3           3
[2010,]        3        3        3         3         3         3           3
[2011,]        3        3        3         3         3         3           3
[2012,]        3        3        3         3         3         3           3
[2013,]        3        3        3         3         3         3           3
[2014,]        2       NA       NA         2        NA        NA           2
[2015,]        2       NA       NA         2        NA        NA           2
[2016,]        1        2        3         1         2         3           1
[2017,]        2       NA       NA         2        NA        NA           1
[2018,]        1       NA       NA         1        NA        NA           1
[2019,]        1       NA       NA         1        NA        NA           1
[2020,]        1        2       NA         1         2        NA           1
[2021,]        1       NA       NA         1        NA        NA           1
[2022,]        1       NA       NA         1        NA        NA           1
[2023,]        3       NA       NA         3        NA        NA           2
[2024,]        3       NA       NA         3        NA        NA           3
[2025,]        3       NA       NA         3        NA        NA           3
[2026,]        1       NA       NA         1        NA        NA           1
[2027,]        1       NA       NA         1        NA        NA           1
[2028,]        1       NA       NA         1        NA        NA           1
[2029,]        1        3       NA         1         3        NA           1
[2030,]        1        3       NA         1         3        NA           1
[2031,]        1       NA       NA         1        NA        NA           1
[2032,]        2       NA       NA         2        NA        NA           2
[2033,]        1        2       NA         1         2        NA           1
[2034,]        1       NA       NA         1        NA        NA           1
[2035,]        1       NA       NA         1        NA        NA           1
[2036,]        1       NA       NA         1        NA        NA           1
[2037,]        1       NA       NA         1        NA        NA           1
[2038,]        1       NA       NA         1        NA        NA           1
[2039,]        1        3        3         1         3         3           1
[2040,]        1        3        3         1         3         3           1
[2041,]        2        2       NA         1         2        NA           1
[2042,]        1        2        3         1         2         3           1
[2043,]        2        2       NA         1         2        NA           1
[2044,]        1        2        3         1         2        NA           1
[2045,]        1        2        3         1         2         3           1
[2046,]        2        2        3         2         2         3           2
[2047,]        2        2        3         2         2         3           1
[2048,]        1        2        3         1         2         3           1
[2049,]        1       NA       NA         1        NA        NA           1
[2050,]        1       NA       NA         1        NA        NA           1
[2051,]        1       NA       NA         1        NA        NA           1
[2052,]        1        2        3         1         2         3           1
[2053,]        1        2       NA         1         2        NA           1
[2054,]        1       NA       NA         1        NA        NA           1
[2055,]        1       NA       NA         1        NA        NA           1
[2056,]        1       NA       NA         1        NA        NA           1
[2057,]        1       NA       NA         1        NA        NA           1
[2058,]        1        2        3         1         2         3           1
[2059,]        1        2        3         1         2         3           1
[2060,]        3       NA       NA         2        NA        NA           2
[2061,]        3       NA       NA         3        NA        NA           2
[2062,]        1        3       NA         1         2        NA           1
[2063,]        3        3        3         3         3         3           3
[2064,]        1       NA       NA         1        NA        NA           1
[2065,]        1       NA       NA         1        NA        NA           1
[2066,]        1       NA       NA         1        NA        NA           1
[2067,]        1        2        3         1         2         3           1
[2068,]        1        2        2         1         2         2           1
[2069,]        2        2        2         2         2         2           1
[2070,]        1        2        2         1         2         2           1
[2071,]        1       NA       NA         1        NA        NA           1
[2072,]        1        2        2         1         2         2           1
[2073,]        1        2       NA         1         2        NA           1
[2074,]        3       NA       NA         3        NA        NA           3
[2075,]        1       NA       NA         1        NA        NA           1
[2076,]        1        2        3         1         2         3           1
[2077,]        1        2        3         1         2         3           1
[2078,]        1        3        3         1         3         3           1
[2079,]        3       NA       NA         3        NA        NA           3
[2080,]        1        3       NA         1         3        NA           1
[2081,]        3       NA       NA         3        NA        NA           3
[2082,]        1       NA       NA         1        NA        NA           1
[2083,]        1        2       NA         1         2        NA           1
[2084,]        1        3        3         1         3         3           1
[2085,]        2        2        3         2         2         3           2
[2086,]        1        2       NA         1         2        NA           1
[2087,]        1        2        3         1         2         3           1
[2088,]        1        3       NA         1         3        NA           1
[2089,]        2        3       NA         2         3        NA           2
[2090,]        1        3        3         1         3         3           1
[2091,]        2        3        3         2         3         3           2
[2092,]        1       NA       NA         1        NA        NA           1
[2093,]        1        2       NA         1         2        NA           1
[2094,]        1       NA       NA         1        NA        NA           1
[2095,]        1        2       NA         1         2        NA           1
[2096,]        2       NA       NA         2        NA        NA           1
[2097,]        1        3       NA         1         3        NA           1
[2098,]        1        3       NA         1         3        NA           1
[2099,]        1        3       NA         1         3        NA           1
[2100,]        1       NA       NA         1        NA        NA           1
[2101,]        1        3        3         1         3         3           1
[2102,]        1        3        3         1         3         3           1
[2103,]        2       NA       NA         2        NA        NA           2
[2104,]        2       NA       NA         2        NA        NA           2
[2105,]        2       NA       NA         2        NA        NA           2
[2106,]        2       NA       NA         2        NA        NA           2
[2107,]        1       NA       NA         1        NA        NA           1
[2108,]        2        3        3         2         3         3           2
[2109,]        1       NA       NA         1        NA        NA           1
[2110,]        1        2        3         1         2         3           1
[2111,]        1        3        3         1         3         3           1
[2112,]        1        3       NA         1         3        NA           1
[2113,]        1       NA       NA         1        NA        NA           1
[2114,]        1        2        3         1         2         3           1
[2115,]        1        2        2         1         2         2           1
[2116,]        1        1        2         1         1         2           1
[2117,]        1        1        2         1         1         2           1
[2118,]        1        2       NA         1         2        NA           1
[2119,]        1        1        2         1         1         2           1
[2120,]        1        1        2         1         1         2           1
[2121,]        1        2       NA         1         2        NA           1
[2122,]        1       NA       NA         1        NA        NA           1
[2123,]        1        1       NA         1         1        NA           1
[2124,]        1        2        2         1         2         2           1
[2125,]        1        2        2         1         2         2           1
[2126,]        1        2        2         1         2         2           1
[2127,]        1       NA       NA         1        NA        NA           1
[2128,]        1        2       NA         1         2        NA           1
[2129,]        1       NA       NA         1        NA        NA           1
[2130,]        3       NA       NA         3        NA        NA           3
[2131,]        1       NA       NA         1        NA        NA           1
[2132,]        1        2        2         1         2         2           1
[2133,]        1        2        3         1         2         3           1
[2134,]        1        2        3         1         2         3           1
[2135,]        3       NA       NA         3        NA        NA           3
[2136,]        1        3       NA         1         3        NA           1
[2137,]        1       NA       NA         1        NA        NA           1
[2138,]        1        2       NA         1         2        NA           1
[2139,]        2        2        3         2         2         3           2
[2140,]        1        2        3         1         2         3           1
[2141,]        1        3       NA         1         3        NA           1
[2142,]        1       NA       NA         1        NA        NA           1
[2143,]        1        2       NA         1         2        NA           1
[2144,]        1       NA       NA         1        NA        NA           1
[2145,]        1        2       NA         1         2        NA           1
[2146,]        3       NA       NA         3        NA        NA           3
[2147,]        1        3       NA         1         3        NA           1
[2148,]        1        3       NA         1         3        NA           1
[2149,]        1        3        3         1         3         3           1
[2150,]        1        3        3         1         3         3           1
[2151,]        1        3       NA         1         3        NA           1
[2152,]        1        3       NA         1         3        NA           1
[2153,]        1        3       NA         1         3        NA           1
[2154,]        2       NA       NA         2        NA        NA           1
[2155,]        2        3       NA         2         3        NA           2
[2156,]        2        3       NA         2         3        NA           2
[2157,]        1        3        3         1         3         3           1
[2158,]        1        3        3         1         3         3           1
[2159,]        1        3       NA         1         3        NA           1
[2160,]        1        2       NA         1         2        NA           1
[2161,]        1        2       NA         1         2        NA           1
[2162,]        1       NA       NA         1        NA        NA           1
[2163,]        2        2        3         2         2         3           1
[2164,]        2        2        3         2         2         3           1
[2165,]        3       NA       NA         3        NA        NA           1
[2166,]        3       NA       NA         3        NA        NA           3
[2167,]        3       NA       NA         3        NA        NA           2
[2168,]        1        2        3         1         2         3           1
[2169,]        1       NA       NA         1        NA        NA           1
[2170,]        1        3        3         1         3         3           1
[2171,]        1        3       NA         1         3        NA           1
[2172,]        1       NA       NA         1        NA        NA           1
[2173,]        1        3        3         1         3         3           1
[2174,]        1       NA       NA         1        NA        NA           1
[2175,]        1        3        3         1         3         3           1
[2176,]        1        3        3         1         3         3           1
[2177,]        1        2        3         1         2         3           1
[2178,]        1        2        3         1         2         3           1
[2179,]        1        2        2         1         2         2           1
[2180,]        1        1        2         1         1         2           1
[2181,]        1        2        2         1         2         2           1
[2182,]        1        2       NA         1         2        NA           1
[2183,]        1        1        2         1         1         2           1
[2184,]        1        2        2         1         2         2           1
[2185,]        1        2       NA         1         2        NA           1
[2186,]        3        2       NA         3         2        NA           3
[2187,]        1       NA       NA         1        NA        NA           1
[2188,]        1       NA       NA         1        NA        NA           1
[2189,]        1       NA       NA         1        NA        NA           1
[2190,]        1       NA       NA         1        NA        NA           1
[2191,]        1        3       NA         1         3        NA           1
[2192,]        1        3        3         1         3         3           1
[2193,]        1        3        3         1         3         3           1
[2194,]        1        3       NA         1         3        NA           1
[2195,]        1        3       NA         1         3        NA           1
[2196,]        2       NA       NA         2        NA        NA           1
[2197,]        2       NA       NA         2        NA        NA           2
[2198,]        2        3       NA         2         3        NA           2
[2199,]        1        2        3         1         2         3           1
[2200,]        1        3       NA         1         3        NA           1
[2201,]        1        2       NA         1         2        NA           1
[2202,]        1       NA       NA         1        NA        NA           1
[2203,]        2        2        2         2         2         2           1
[2204,]        2        2        2         2         2         2           1
[2205,]        3       NA       NA         3        NA        NA           2
[2206,]        3       NA       NA         3        NA        NA           3
[2207,]        3       NA       NA         3        NA        NA           3
[2208,]        1        2        3         1         2         3           1
[2209,]        1        3        3         1         3         3           1
[2210,]        3        3        3         3         3         3           3
[2211,]        2        2        3         2         2         3           2
[2212,]        2        2        3         2         2         3           2
[2213,]        1        2        3         1         2         3           1
[2214,]        1        2        3         1         2         3           1
[2215,]        1        3       NA         1         3        NA           1
[2216,]        2        2        3         2         2         3           2
[2217,]        3        3        3         3         3         3           3
[2218,]        1        2        3         1         2         3           1
[2219,]        3        3        3         3         3         3           3
[2220,]        1        2        3         1         2         3           1
[2221,]        1        2        3         1         2         2           1
[2222,]        1        3       NA         1         3        NA           1
[2223,]        1       NA       NA         1        NA        NA           1
[2224,]        1        3        3         1         2         3           1
[2225,]        1        3        3         1         3         3           1
[2226,]        1       NA       NA         1        NA        NA           1
[2227,]        1        2        2         1         2         2           1
[2228,]        1       NA       NA         1        NA        NA           1
[2229,]        1        1       NA         1         2        NA           1
[2230,]        1        2       NA         1         2        NA           1
[2231,]        1        2        2         1         2         2           1
[2232,]        1        2       NA         1         2        NA           1
[2233,]        1       NA       NA         1        NA        NA           1
[2234,]        1       NA       NA         1        NA        NA           1
[2235,]        1       NA       NA         1        NA        NA           1
[2236,]        1        2        2         1         2         2           1
[2237,]        2       NA       NA         2        NA        NA           2
[2238,]        1        2        2         1         2         3           1
[2239,]        2       NA       NA         2        NA        NA           2
[2240,]        1       NA       NA         1        NA        NA           1
[2241,]        1       NA       NA         1        NA        NA           1
[2242,]        1       NA       NA         1        NA        NA           1
[2243,]        1        2        2         1         3         2           1
[2244,]        1        2        2         1         2         3           1
[2245,]        2        2       NA         2         2        NA           2
[2246,]        1        2        2         1         2         2           1
[2247,]        1       NA       NA         1        NA        NA           1
[2248,]        1        2       NA         1         2        NA           1
[2249,]        1        2       NA         1         2        NA           1
[2250,]        1        2       NA         1         2        NA           1
[2251,]        1        2       NA         1         2        NA           1
[2252,]        1        2        2         1         2         3           1
[2253,]        1        2        2         1         2         2           1
[2254,]        1       NA       NA         1        NA        NA           1
[2255,]        1        2       NA         1         2        NA           1
[2256,]        1       NA       NA         1        NA        NA           1
[2257,]        1        2       NA         1         2        NA           1
[2258,]        1        1       NA         1         2        NA           1
[2259,]        1        2        2         2         2         3           1
[2260,]        1        2       NA         1         2        NA           1
[2261,]        1       NA       NA         1        NA        NA           1
[2262,]        1        2        2         1         2         3           1
[2263,]        1       NA       NA         1        NA        NA           1
[2264,]        1       NA       NA         1        NA        NA           1
[2265,]        1        2       NA         1         2        NA           1
[2266,]        1        2       NA         1         2        NA           1
[2267,]        1       NA       NA         1        NA        NA           1
[2268,]        1        2       NA         1         2        NA           1
[2269,]        1        2       NA         1         2        NA           1
[2270,]        1        2       NA         1         2        NA           1
[2271,]        1        2        2         2         2         3           1
[2272,]        1       NA       NA         1        NA        NA           1
[2273,]        1       NA       NA         1        NA        NA           1
[2274,]        2       NA       NA         2        NA        NA           2
[2275,]        1       NA       NA         1        NA        NA           1
[2276,]        1        2       NA         1         2        NA           1
[2277,]        1        2       NA         1         2        NA           1
[2278,]        1        2       NA         1         2        NA           1
[2279,]        1       NA       NA         1        NA        NA           1
[2280,]        1       NA       NA         1        NA        NA           1
[2281,]        1        2        2         1         2         2           1
[2282,]        1        2        2         1         2         2           1
[2283,]        1        2        2         1         2         2           1
[2284,]        1        2       NA         1         2        NA           1
[2285,]        1       NA       NA         1        NA        NA           1
[2286,]        1        2       NA         1         2        NA           1
[2287,]        1       NA       NA         1        NA        NA           1
[2288,]        1       NA       NA         1        NA        NA           1
[2289,]        1        2       NA         1         2        NA           1
[2290,]        1       NA       NA         1        NA        NA           1
[2291,]        1        2        2         1         2         2           1
[2292,]        2       NA       NA         2        NA        NA           1
[2293,]        1        2        2         1         2         2           1
[2294,]        2        2        2         2         2         3           1
[2295,]        2        2        2         2         2         3           2
[2296,]        1       NA       NA         1        NA        NA           1
[2297,]        1        2        2         1         2         2           1
[2298,]        1        2        2         1         2         2           1
[2299,]        1        2        2         1         2         2           1
[2300,]        1        2        2         1         2         2           1
[2301,]        1        2        2         1         2         2           1
[2302,]        1        2       NA         1         2        NA           1
[2303,]        1        2        2         1         2         2           1
[2304,]        1       NA       NA         1        NA        NA           1
[2305,]        1       NA       NA         1        NA        NA           1
[2306,]        1       NA       NA         1        NA        NA           1
[2307,]        1       NA       NA         1        NA        NA           1
[2308,]        2       NA       NA         2        NA        NA           2
[2309,]        1       NA       NA         1        NA        NA           1
[2310,]        1       NA       NA         2        NA        NA           1
[2311,]        1       NA       NA         1        NA        NA           1
[2312,]        1        2        2         1         2         2           1
[2313,]        2        2       NA         2         2        NA           2
[2314,]        1       NA       NA         1        NA        NA           1
[2315,]        1        2       NA         1         2        NA           1
[2316,]        2       NA       NA         2        NA        NA           2
[2317,]        2        2       NA         2         2        NA           2
[2318,]        1        2       NA         1         2        NA           1
[2319,]        1       NA       NA         1        NA        NA           1
[2320,]        1       NA       NA         1        NA        NA           1
[2321,]        1       NA       NA         1        NA        NA           1
[2322,]        1        2       NA         1         2        NA           1
[2323,]        1       NA       NA         1        NA        NA           1
[2324,]        1        2       NA         1         2        NA           1
[2325,]        1        2        2         1         2         2           1
[2326,]        1       NA       NA         1        NA        NA           1
[2327,]        1       NA       NA         1        NA        NA           1
[2328,]        1        2       NA         1         2        NA           1
[2329,]        1        2       NA         1         2        NA           1
[2330,]        2        2       NA         2         2        NA           1
[2331,]        1        2        2         1         2         2           1
[2332,]        1        2        2         1         2         2           1
[2333,]        1        2       NA         1         3        NA           1
[2334,]        2        2       NA         2         2        NA           1
[2335,]        1       NA       NA         1        NA        NA           1
[2336,]        1       NA       NA         1        NA        NA           1
[2337,]        1        2        2         1         2         2           1
[2338,]        1        2       NA         1         2        NA           1
[2339,]        2        1        2         2         1         2           2
[2340,]        1       NA       NA         1        NA        NA           1
[2341,]        1        2       NA         1         2        NA           1
[2342,]        1       NA       NA         1        NA        NA           1
[2343,]        1        2        2         1         2         3           1
[2344,]        1        2        2         1         2         3           1
[2345,]        2        2       NA         2         2        NA           2
[2346,]        1        2        2         1         2         3           1
[2347,]        2        2       NA         2         2        NA           2
[2348,]        1       NA       NA         1        NA        NA           1
[2349,]        1        2        2         1         2         3           1
[2350,]        1       NA       NA         1        NA        NA           1
[2351,]        1        2       NA         1         2        NA           1
[2352,]        1        2       NA         1         2        NA           1
[2353,]        3        2       NA         3         2        NA           2
[2354,]        1       NA       NA         1        NA        NA           1
[2355,]        1        2        2         1         2         2           1
[2356,]        1        1        2         1         1         2           1
[2357,]        3        2       NA         3         2        NA           3
[2358,]        1       NA       NA         1        NA        NA           1
[2359,]        1        2        2         1         2         2           1
[2360,]        1       NA       NA         1        NA        NA           1
[2361,]        1       NA       NA         1        NA        NA           1
[2362,]        1        2        2         1         2         2           1
[2363,]        1        2        2         1         2         2           1
[2364,]        1        2        2         1         2         2           1
[2365,]        1        2        2         1         2         3           1
[2366,]        2        2       NA         2         2        NA           2
[2367,]        1        2        3         1         2         3           1
[2368,]        1        2       NA         1         2        NA           1
[2369,]        3        2       NA         3         2        NA           2
[2370,]        1        2        2         1         2         3           1
[2371,]        1       NA       NA         1        NA        NA           1
[2372,]        1        2        2         1         2         3           1
[2373,]        1       NA       NA         1        NA        NA           1
[2374,]        1        2        2         1         2         2           1
[2375,]        1       NA       NA         1        NA        NA           1
[2376,]        1       NA       NA         1        NA        NA           1
[2377,]        1        2        2         1         2         2           1
[2378,]        1       NA       NA         1        NA        NA           1
[2379,]        1       NA       NA         1        NA        NA           1
[2380,]        2        2       NA         2         2        NA           2
[2381,]        1       NA       NA         1        NA        NA           1
[2382,]        1       NA       NA         1        NA        NA           1
[2383,]        1        2       NA         1         2        NA           1
[2384,]        1        2       NA         1         2        NA           1
[2385,]        1       NA       NA         1        NA        NA           1
[2386,]        1       NA       NA         1        NA        NA           1
[2387,]        1       NA       NA         1        NA        NA           1
[2388,]        1       NA       NA         1        NA        NA           1
[2389,]        1        3        3         1         3         3           1
[2390,]        1       NA       NA         1        NA        NA           1
[2391,]        1        2       NA         1         2        NA           1
[2392,]        1        2       NA         1         2        NA           1
[2393,]        2        2       NA         2         2        NA           1
[2394,]        1       NA       NA         1        NA        NA           1
[2395,]        1       NA       NA         1        NA        NA           1
[2396,]        1       NA       NA         1        NA        NA           1
[2397,]        1        2        3         1         2         3           1
[2398,]        1       NA       NA         2        NA        NA           1
[2399,]        1        2        3         1         2         3           1
[2400,]        2       NA       NA         2        NA        NA           2
[2401,]        1       NA       NA         1        NA        NA           1
[2402,]        1       NA       NA         1        NA        NA           1
[2403,]        1       NA       NA         1        NA        NA           1
[2404,]        1        3        3         1         3         3           1
[2405,]        1        2        3         1         2         3           1
[2406,]        2        3       NA         2         3        NA           1
[2407,]        1        3        3         1         3         3           1
[2408,]        1       NA       NA         1        NA        NA           1
[2409,]        2        2       NA         2         2        NA           2
[2410,]        1        3       NA         1         3        NA           1
[2411,]        2        2       NA         2         2        NA           2
[2412,]        2        2       NA         2         2        NA           1
[2413,]        1        2        3         1         2         3           1
[2414,]        1        2        3         1         2         3           1
[2415,]        1       NA       NA         1        NA        NA           1
[2416,]        1        2       NA         1         2        NA           1
[2417,]        1       NA       NA         1        NA        NA           1
[2418,]        2        2       NA         2         2        NA           1
[2419,]        1        2       NA         1         2        NA           1
[2420,]        1       NA       NA         1        NA        NA           1
[2421,]        1        2        3         1         2         3           1
[2422,]        1       NA       NA         1        NA        NA           1
[2423,]        2        2       NA         2         2        NA           1
[2424,]        1        3       NA         1         3        NA           1
[2425,]        1       NA       NA         1        NA        NA           1
[2426,]        1        3       NA         2         3        NA           1
[2427,]        1        2       NA         1         2        NA           1
[2428,]        1        2       NA         1         2        NA           1
[2429,]        1        3        3         1         3         3           1
[2430,]        1        2       NA         1         2        NA           1
[2431,]        1        2       NA         1         2        NA           1
[2432,]        2        2       NA         2         2        NA           2
[2433,]        1        2        3         1         3         3           1
[2434,]        2        3       NA         2         3        NA           2
[2435,]        1        3        3         1         3         3           1
[2436,]        1       NA       NA         1        NA        NA           1
[2437,]        2        2       NA         2         2        NA           2
[2438,]        2        2        3         2         2         3           1
[2439,]        1       NA       NA         1        NA        NA           1
[2440,]        1       NA       NA         1        NA        NA           1
[2441,]        2       NA       NA         2        NA        NA           2
[2442,]        1       NA       NA         1        NA        NA           1
[2443,]        1        2       NA         1         2        NA           1
[2444,]        1        3       NA         1         3        NA           1
[2445,]        1        3       NA         1         2        NA           1
[2446,]        1       NA       NA         1        NA        NA           1
[2447,]        1       NA       NA         1        NA        NA           1
[2448,]        1        2        3         1         2         2           1
[2449,]        1        2        3         1         2         3           1
[2450,]        1        2        3         1         2         2           1
[2451,]        1        3       NA         1         3        NA           1
[2452,]        1       NA       NA         1        NA        NA           1
[2453,]        1       NA       NA         1        NA        NA           1
[2454,]        1        3       NA         1         3        NA           1
[2455,]        1       NA       NA         1        NA        NA           1
[2456,]        1        3        3         1         3         3           1
[2457,]        1        3        3         1         3         3           1
[2458,]        2        3        3         2         2         3           2
[2459,]        2        3        3         2         2         3           2
[2460,]        2        3        3         2         2         3           2
[2461,]        1        3        3         1         3         3           1
[2462,]        3        3        3         3         3         3           3
[2463,]        1        3       NA         1         3        NA           1
[2464,]        1        3       NA         1         3        NA           1
[2465,]        2        3        3         2         3         3           2
[2466,]        1        2        3         1         2         3           1
[2467,]        1        2        3         1         2         3           1
[2468,]        2        3        3         2         2         3           2
[2469,]        1       NA       NA         1        NA        NA           1
[2470,]        1       NA       NA         1        NA        NA           1
[2471,]        1       NA       NA         1        NA        NA           1
[2472,]        1        2       NA         1         2        NA           1
[2473,]        1        2       NA         1         2        NA           1
[2474,]        1       NA       NA         1        NA        NA           1
[2475,]        1       NA       NA         1        NA        NA           1
[2476,]        1        3       NA         1         3        NA           1
[2477,]        3        3        3         3         3         3           3
[2478,]        1        2       NA         1         2        NA           1
[2479,]        1        2       NA         1         2        NA           1
[2480,]        1        2       NA         1         2        NA           1
[2481,]        1       NA       NA         1        NA        NA           1
[2482,]        1        2       NA         1         2        NA           1
[2483,]        1       NA       NA         1        NA        NA           1
[2484,]        1        2        3         1         2         3           1
[2485,]        3        3        3         3         3         3           3
[2486,]        3        3        3         3         3         3           3
[2487,]        1        3       NA         1         3        NA           1
[2488,]        2        3        3         2         3         3           2
[2489,]        2        3        3         2         3         3           2
[2490,]        1        3        3         1         3         3           1
[2491,]        1        2       NA         1         2        NA           1
[2492,]        1       NA       NA         1        NA        NA           1
[2493,]        1       NA       NA         1        NA        NA           1
[2494,]        1        2        3         1         2         3           1
[2495,]        1        2       NA         1         2        NA           1
[2496,]        2        2       NA         2         2        NA           2
[2497,]        1        2       NA         1         2        NA           1
[2498,]        1        2       NA         1         2        NA           1
[2499,]        1        2       NA         1         2        NA           1
[2500,]        1        3        3         1         3         3           1
[2501,]        1        2        3         1         2         3           1
[2502,]        2        2        3         3         2         3           2
[2503,]        2       NA       NA         2        NA        NA           1
[2504,]        1        2        3         1         2         3           1
[2505,]        2        2        3         2         2         3           1
[2506,]        2        2        3         2         2         3           2
[2507,]        1       NA       NA         1        NA        NA           1
[2508,]        1       NA       NA         1        NA        NA           1
[2509,]        1       NA       NA         1        NA        NA           1
[2510,]        1       NA       NA         1        NA        NA           1
[2511,]        1       NA       NA         1        NA        NA           1
[2512,]        1       NA       NA         1        NA        NA           1
[2513,]        1       NA       NA         1        NA        NA           1
[2514,]        1        2       NA         1         2        NA           1
[2515,]        1        2       NA         1         2        NA           1
[2516,]        1       NA       NA         1        NA        NA           1
[2517,]        1       NA       NA         1        NA        NA           1
[2518,]        1        2       NA         1         2        NA           1
[2519,]        1        2       NA         1         2        NA           1
[2520,]        2        3        3         2         2         3           2
[2521,]        2        2       NA         2         2        NA           2
[2522,]        2        2       NA         2         2        NA           2
[2523,]        2        2       NA         2         2        NA           2
[2524,]        1        2       NA         1         2        NA           1
[2525,]        2        2       NA         2         2        NA           2
[2526,]        1       NA       NA         1        NA        NA           1
[2527,]        1       NA       NA         1        NA        NA           1
[2528,]        2        3       NA         1         3        NA           2
[2529,]        1        2        3         1         2         3           1
[2530,]        1       NA       NA         1        NA        NA           1
[2531,]        1       NA       NA         1        NA        NA           1
[2532,]        1       NA       NA         1        NA        NA           1
[2533,]        1       NA       NA         1        NA        NA           1
[2534,]        1        3        3         1         3         3           1
[2535,]        3        2        3         3         2         3           3
[2536,]        1        2       NA         1         2        NA           1
[2537,]        2       NA       NA         2        NA        NA           2
[2538,]        2       NA       NA         2        NA        NA           2
[2539,]        2       NA       NA         2        NA        NA           2
[2540,]        2       NA       NA         2        NA        NA           2
[2541,]        2       NA       NA         2        NA        NA           2
[2542,]        1        2       NA         1         2        NA           1
[2543,]        1       NA       NA         1        NA        NA           1
[2544,]        1       NA       NA         1        NA        NA           1
[2545,]        1       NA       NA         1        NA        NA           1
[2546,]        1       NA       NA         1        NA        NA           1
[2547,]        1       NA       NA         1        NA        NA           1
[2548,]        1        2        3         1         2         3           1
[2549,]        1        3        3         1         3         3           1
[2550,]        1        2        3         1         3         3           1
[2551,]        1        2        3         1         2         3           1
[2552,]        1        2       NA         1         3        NA           1
[2553,]        1        2        3         1         2         3           1
[2554,]        1       NA       NA         1        NA        NA           1
[2555,]        1       NA       NA         1        NA        NA           1
[2556,]        1       NA       NA         1        NA        NA           1
[2557,]        1       NA       NA         1        NA        NA           1
[2558,]        2       NA       NA         2        NA        NA           2
[2559,]        1       NA       NA         1        NA        NA           1
[2560,]        1       NA       NA         1        NA        NA           1
[2561,]        1        2        2         1         2         3           1
[2562,]        2        3       NA         2         3        NA           2
[2563,]        1       NA       NA         1        NA        NA           1
[2564,]        1        2       NA         1         3        NA           1
[2565,]        2       NA       NA         2        NA        NA           2
[2566,]        2        2       NA         2         2        NA           2
[2567,]        1        2       NA         1         2        NA           1
[2568,]        1       NA       NA         1        NA        NA           1
[2569,]        1       NA       NA         1        NA        NA           1
[2570,]        1       NA       NA         1        NA        NA           1
[2571,]        1        2       NA         1         2        NA           1
[2572,]        1       NA       NA         1        NA        NA           1
[2573,]        1        3       NA         1         3        NA           1
[2574,]        1        3        2         1         3         3           1
[2575,]        1       NA       NA         1        NA        NA           1
[2576,]        1       NA       NA         1        NA        NA           1
[2577,]        1        3       NA         1         3        NA           1
[2578,]        1        2       NA         1         2        NA           1
[2579,]        2        3       NA         2         3        NA           2
[2580,]        1        2        3         1         3         3           1
[2581,]        1        2        3         1         2         3           1
[2582,]        1        3       NA         2         3        NA           1
[2583,]        1       NA       NA         1        NA        NA           1
[2584,]        1       NA       NA         1        NA        NA           1
[2585,]        1       NA       NA         1        NA        NA           1
[2586,]        1       NA       NA         1        NA        NA           1
[2587,]        1       NA       NA         1        NA        NA           1
[2588,]        1       NA       NA         1        NA        NA           1
[2589,]        1        3       NA         1         3        NA           1
[2590,]        1        2        3         1         2         3           1
[2591,]        2        3       NA         2         3        NA           1
[2592,]        1       NA       NA         1        NA        NA           1
[2593,]        1       NA       NA         1        NA        NA           1
[2594,]        1        2        3         1         2         3           1
[2595,]        1        3       NA         1         3        NA           1
[2596,]        1       NA       NA         1        NA        NA           1
[2597,]        1        3       NA         2         3        NA           1
[2598,]        1       NA       NA         1        NA        NA           1
[2599,]        1        2        3         1         3         3           1
[2600,]        1        2        3         1         2         3           1
[2601,]        2        3       NA         3         3        NA           2
[2602,]        1        2        3         1         3         3           1
[2603,]        2        3       NA         2         3        NA           2
[2604,]        1        2        3         1         2         3           1
[2605,]        1       NA       NA         1        NA        NA           1
[2606,]        1        2       NA         1         2        NA           1
[2607,]        3        3       NA         3         3        NA           2
[2608,]        2       NA       NA         2        NA        NA           1
[2609,]        1        2        2         1         2         2           1
[2610,]        1        1        2         1         1         3           1
[2611,]        3        3       NA         3         3        NA           3
[2612,]        1       NA       NA         1        NA        NA           1
[2613,]        1        3        3         1         3         3           1
[2614,]        2       NA       NA         2        NA        NA           1
[2615,]        1       NA       NA         1        NA        NA           1
[2616,]        1        2        2         1         2         3           1
[2617,]        1        2        3         1         2         3           1
[2618,]        1        2        3         1         2         3           1
[2619,]        2        2       NA         2         2        NA           2
[2620,]        1        3        3         1         3         3           1
[2621,]        1        2       NA         1         2        NA           1
[2622,]        3        3       NA         3         3        NA           3
[2623,]        1        2        3         1         2         3           1
[2624,]        1       NA       NA         1        NA        NA           1
[2625,]        1        2        3         1         2         3           1
[2626,]        2       NA       NA         2        NA        NA           1
[2627,]        1        3        3         1         3         3           1
[2628,]        1       NA       NA         1        NA        NA           1
[2629,]        2       NA       NA         2        NA        NA           1
[2630,]        1       NA       NA         1        NA        NA           1
[2631,]        2       NA       NA         2        NA        NA           2
[2632,]        3        2       NA         3         3        NA           3
[2633,]        2       NA       NA         2        NA        NA           2
[2634,]        2       NA       NA         2        NA        NA           1
[2635,]        1        3       NA         1         3        NA           1
[2636,]        1       NA       NA         1        NA        NA           1
[2637,]        1       NA       NA         1        NA        NA           1
[2638,]        2       NA       NA         2        NA        NA           1
[2639,]        3        2       NA         3         3        NA           3
[2640,]        1        2       NA         1         2        NA           1
[2641,]        1        2        3         1         2         3           1
[2642,]        2       NA       NA         2        NA        NA           2
[2643,]        1       NA       NA         1        NA        NA           1
[2644,]        2       NA       NA         2        NA        NA           2
[2645,]        2       NA       NA         2        NA        NA           2
[2646,]        2       NA       NA         2        NA        NA           1
[2647,]        3        2       NA         3         2        NA           3
[2648,]        2        2        3         2         2         3           1
[2649,]        1       NA       NA         1        NA        NA           1
[2650,]        1       NA       NA         1        NA        NA           1
[2651,]        1       NA       NA         1        NA        NA           1
[2652,]        1        3       NA         1         3        NA           1
[2653,]        1        3       NA         1         3        NA           1
[2654,]        1       NA       NA         1        NA        NA           1
[2655,]        2       NA       NA         2        NA        NA           2
[2656,]        1        3        3         1         3         3           1
[2657,]        2        3        3         1         3         3           1
[2658,]        1        3        3         1         3         3           1
[2659,]        1        3       NA         1         3        NA           1
[2660,]        1       NA       NA         1        NA        NA           1
[2661,]        1       NA       NA         1        NA        NA           1
[2662,]        1        3       NA         1         3        NA           1
[2663,]        1       NA       NA         1        NA        NA           1
[2664,]        1        3        3         1         3         3           1
[2665,]        1        3        3         1         3         3           1
[2666,]        1        3        3         1         3         3           1
[2667,]        2        3        3         2         3         3           2
[2668,]        1        3        3         1        NA         3           1
[2669,]        1        3       NA         1         3        NA           1
[2670,]        1        3       NA         1         3        NA           1
[2671,]        3        3        3         3         3         3           3
[2672,]        3        3        3         3         3         3           3
[2673,]        1        3        3         1         3         3           1
[2674,]        2        3        3         2         3         3           2
[2675,]        2       NA       NA         2        NA        NA           2
[2676,]        1       NA       NA         1        NA        NA           1
[2677,]        1        3       NA         1         3        NA           1
[2678,]        2       NA       NA         2        NA        NA           2
[2679,]        3        3        3         3         3         3           3
[2680,]        1        3       NA         1         3        NA           1
[2681,]        1        3       NA         1         3        NA           1
[2682,]        1       NA       NA         1        NA        NA           1
[2683,]        1        3       NA         1         3        NA           1
[2684,]        1       NA       NA         1        NA        NA           1
[2685,]        1        3        3         1         3         3           1
[2686,]        3        3        3         3         3         3           3
[2687,]        2        3       NA         2         3        NA           2
[2688,]        3        3        3         3         3         3           3
[2689,]        1        3       NA         1         2        NA           1
[2690,]        1       NA       NA         1        NA        NA           1
[2691,]        1        2       NA         1         2        NA           1
[2692,]        3        2       NA         3         2        NA           3
[2693,]        1        2       NA         1         2        NA           1
[2694,]        1        3        3         1         3         3           1
[2695,]        3        3        3         3         3         3           3
[2696,]        2       NA       NA         2        NA        NA           1
[2697,]        1        2        3         1         2         3           1
[2698,]        2        3        3         2         3         3           2
[2699,]        2        3        3         2         3         3           1
[2700,]        1       NA       NA         1        NA        NA           1
[2701,]        1       NA       NA         1        NA        NA           1
[2702,]        1       NA       NA         1        NA        NA           1
[2703,]        2       NA       NA         2        NA        NA           2
[2704,]        2       NA       NA         2        NA        NA           2
[2705,]        1       NA       NA         1        NA        NA           1
[2706,]        1       NA       NA         1        NA        NA           1
[2707,]        1        3       NA         1         2        NA           1
[2708,]        1        3       NA         1         2        NA           1
[2709,]        1       NA       NA         1        NA        NA           1
[2710,]        1        3       NA         1         3        NA           1
[2711,]        1        3       NA         1         3        NA           1
[2712,]        2        3        3         2         3         3           2
[2713,]        3        2       NA         3         2        NA           3
[2714,]        2        2       NA         2         2        NA           2
[2715,]        3        2       NA         3         2        NA           3
[2716,]        1        3       NA         1         2        NA           1
[2717,]        3        2       NA         3         2        NA           3
[2718,]        1       NA       NA         1        NA        NA           1
[2719,]        2       NA       NA         1        NA        NA           2
[2720,]        1        3        3         1         3         3           1
[2721,]        1       NA       NA         1        NA        NA           1
[2722,]        1       NA       NA         1        NA        NA           1
[2723,]        1        3        3         1         3         3           1
[2724,]        3        3        3         3         3         3           3
[2725,]        1        3       NA         1         3        NA           1
[2726,]        2       NA       NA         2        NA        NA           2
[2727,]        2       NA       NA         2        NA        NA           2
[2728,]        2       NA       NA         2        NA        NA           2
[2729,]        2       NA       NA         2        NA        NA           2
[2730,]        1        3       NA         1         3        NA           1
[2731,]        1       NA       NA         1        NA        NA           1
[2732,]        1       NA       NA         1        NA        NA           1
[2733,]        2       NA       NA         2        NA        NA           2
[2734,]        2        2        3         2         2         3           2
[2735,]        2        2        3         2         2         3           2
[2736,]        1        3       NA         1         3        NA           1
[2737,]        2        2        3         2         2         3           2
[2738,]        2        3        3         2         3         3           2
[2739,]        1        2        3         1         2         3           1
[2740,]        1        2       NA         1         2        NA           1
[2741,]        1        2       NA         1         2        NA           1
[2742,]        1        2       NA         1         2        NA           1
[2743,]        1        3       NA         1         3        NA           1
[2744,]        2        2        3         2         2         3           2
[2745,]        1       NA       NA         1        NA        NA           1
[2746,]        1       NA       NA         1        NA        NA           1
[2747,]        1        3       NA         1         3        NA           1
[2748,]        2        3        2         2         3         3           2
[2749,]        1        2       NA         1         2        NA           1
[2750,]        1       NA       NA         1        NA        NA           1
[2751,]        1        2       NA         1         2        NA           1
[2752,]        1       NA       NA         1        NA        NA           1
[2753,]        1       NA       NA         1        NA        NA           1
[2754,]        1        2       NA         1         2        NA           1
[2755,]        1       NA       NA         1        NA        NA           1
[2756,]        1        3       NA         1         3        NA           1
[2757,]        1       NA       NA         1        NA        NA           1
[2758,]        1       NA       NA         1        NA        NA           1
[2759,]        1        3        3         1         3         3           1
[2760,]        1       NA       NA         1        NA        NA           1
[2761,]        1       NA       NA         1        NA        NA           1
[2762,]        1       NA       NA         1        NA        NA           1
[2763,]        3        3        3         3         3         3           2
[2764,]        1        2       NA         1         2        NA           1
[2765,]        2       NA       NA         2        NA        NA           1
[2766,]        2       NA       NA         2        NA        NA           2
[2767,]        1        3        3         1         3         3           1
[2768,]        1        3       NA         1         3        NA           1
[2769,]        1        2        3         1         2         3           1
[2770,]        3        3        3         3         3         3           3
[2771,]        1        3        3         1         3         3           1
[2772,]        1       NA       NA         1        NA        NA           1
[2773,]        1        2       NA         1         2        NA           1
[2774,]        3        3        3         3         3         3           1
[2775,]        3        3        3         3         3         3           2
[2776,]        1        2        3         1         2         3           1
[2777,]        1       NA       NA         1        NA        NA           1
[2778,]        2        3        2         2         3         3           2
[2779,]        1        2       NA         1         2        NA           1
[2780,]        1        2        2         1         2         3           1
[2781,]        1       NA       NA         1        NA        NA           1
[2782,]        1        3        3         1         3         3           1
[2783,]        1        3        3         1         3         3           1
[2784,]        1        3        3         1         3         3           1
[2785,]        1        2        2         1         2         2           1
[2786,]        1        2        3         1         2         3           1
[2787,]        2        2        2         2         2         2           2
[2788,]        1        3        3         1         3         3           1
[2789,]        1        3        3         1         3         3           1
[2790,]        1        2       NA         1         2        NA           1
[2791,]        1       NA       NA         1        NA        NA           1
[2792,]        2       NA       NA         2        NA        NA           2
[2793,]        1        2        2         1         2         2           1
[2794,]        3        2        2         3         2         2           3
[2795,]        3        2        2         3         2         2           3
[2796,]        2       NA       NA         2        NA        NA           2
[2797,]        2        2        3         2         2         3           2
[2798,]        1        2       NA         1         2        NA           1
[2799,]        1       NA       NA         1        NA        NA           1
[2800,]        1        2       NA         1         2        NA           1
[2801,]        1        2       NA         1         2        NA           1
[2802,]        1       NA       NA         1        NA        NA           1
[2803,]        2        3        3         2         3         3           2
[2804,]        2       NA       NA         2        NA        NA           2
[2805,]        2       NA       NA         2        NA        NA           2
[2806,]        3        3        3         3         3         3           3
[2807,]        1        3        3         1         3         3           1
[2808,]        1        3        3         1         3         3           1
[2809,]        2        3        3         2         3         3           2
[2810,]        1        3        3         1         3         3           1
[2811,]        1        3        3         1         3         3           1
[2812,]        1        3       NA         1         2        NA           1
[2813,]        1        3       NA         1         2        NA           1
[2814,]        1        3       NA         1         2        NA           1
[2815,]        1       NA       NA         1        NA        NA           1
[2816,]        1        3       NA         1         2        NA           1
[2817,]        1       NA       NA         1        NA        NA           1
[2818,]        1       NA       NA         1        NA        NA           1
[2819,]        2        3       NA         2         3        NA           2
[2820,]        1       NA       NA         1        NA        NA           1
[2821,]        3       NA       NA         3        NA        NA           3
[2822,]        3       NA       NA         3        NA        NA           3
[2823,]        1        3        3         1         3         3           1
[2824,]        1        3       NA         1         3        NA           1
[2825,]        1        3       NA         1         3        NA           1
[2826,]        1        3       NA         1         3        NA           1
[2827,]        2        3       NA         1         3        NA           1
[2828,]        2        3        3         2         3         3           2
[2829,]        1       NA       NA         1        NA        NA           1
[2830,]        1       NA       NA         1        NA        NA           1
[2831,]        2        3       NA         2         3        NA           1
[2832,]        1        3       NA         1         3        NA           1
[2833,]        2       NA       NA         2        NA        NA           2
[2834,]        1        2       NA         1         2        NA           1
[2835,]        1       NA       NA         1        NA        NA           1
[2836,]        1       NA       NA         1        NA        NA           1
[2837,]        2        3       NA         2         3        NA           2
[2838,]        2       NA       NA         1        NA        NA           1
[2839,]        2       NA       NA         2        NA        NA           1
[2840,]        1        3        3         1         3         3           1
[2841,]        1       NA       NA         1        NA        NA           1
[2842,]        2       NA       NA         2        NA        NA           1
[2843,]        1       NA       NA         1        NA        NA           1
[2844,]        2        3        3         2         3         3           1
[2845,]        1        3       NA         1         3        NA           1
[2846,]        2       NA       NA         2        NA        NA           2
[2847,]        1        3        3         1         3         3           1
[2848,]        2        3       NA         2         3        NA           1
[2849,]        2        3        3         1         3         3           1
[2850,]        3        3        3         3         3         3           3
[2851,]        1        3        3         1         3         3           1
[2852,]        2       NA       NA         1        NA        NA           1
[2853,]        1        3       NA         1         3        NA           1
[2854,]        2        3        3         2         3         3           1
[2855,]        3        3        3         3         3         3           2
[2856,]        2        3       NA         2         3        NA           2
[2857,]        2       NA       NA         2        NA        NA           2
[2858,]        2        3       NA         2         3        NA           2
[2859,]        2        3       NA         2         3        NA           2
[2860,]        2       NA       NA         2        NA        NA           2
[2861,]        2       NA       NA         2        NA        NA           2
[2862,]        2       NA       NA         2        NA        NA           2
[2863,]        1        3       NA         1         3        NA           1
[2864,]        1        3       NA         1         3        NA           1
[2865,]        2        3        3         2         3         3           1
[2866,]        3        3        3         3         3         3           3
[2867,]        3        3        3         3         3         3           3
[2868,]        3        3        3         3         3         3           3
[2869,]        3        3        3         3         3         3           3
[2870,]        3        3        3         3         3         3           3
[2871,]        1       NA       NA         1        NA        NA           1
[2872,]        1        2       NA         1         2        NA           1
[2873,]        2        2       NA         1         2        NA           2
[2874,]        2        2       NA         1         2        NA           1
[2875,]        1        3        3         1         3         3           1
[2876,]        2        3       NA         2         3        NA           2
[2877,]        2        2       NA         1         2        NA           2
[2878,]        3        3        3         3         3         3           3
[2879,]        1       NA       NA         1        NA        NA           1
[2880,]        2        3        3         2         3         3           2
[2881,]        2       NA       NA         2        NA        NA           2
[2882,]        2       NA       NA         2        NA        NA           2
[2883,]        1        3       NA         1         3        NA           1
[2884,]        2       NA       NA         2        NA        NA           2
[2885,]        3       NA       NA         3        NA        NA           3
[2886,]        1       NA       NA         1        NA        NA           1
[2887,]        1       NA       NA         1        NA        NA           1
[2888,]        1        3       NA         1         2        NA           1
[2889,]        2        2       NA         1         2        NA           2
[2890,]        1        3       NA         1         3        NA           1
[2891,]        1        3       NA         1         3        NA           1
[2892,]        1       NA       NA         1        NA        NA           1
[2893,]        3        3        3         3         3         3           3
[2894,]        2        3       NA         2         3        NA           2
[2895,]        1        3        3         1         3         3           1
[2896,]        1        3       NA         1         3        NA           1
[2897,]        2       NA       NA         2        NA        NA           2
[2898,]        3       NA       NA         3        NA        NA           3
[2899,]        1        2        3         1         2         3           1
[2900,]        2        3       NA         2         3        NA           1
[2901,]        3        3       NA         3         3        NA           3
[2902,]        1        2        3         1         3         3           1
[2903,]        1        2        3         1         2         3           1
[2904,]        3        3       NA         3         3        NA           2
[2905,]        1        2        3         1         2         3           1
[2906,]        1        2        3         1         2         3           1
[2907,]        2        3       NA         2         3        NA           1
[2908,]        1       NA       NA         1        NA        NA           1
[2909,]        1        3       NA         1         3        NA           1
[2910,]        2       NA       NA         2        NA        NA           1
[2911,]        1        2        3         1         2         3           1
[2912,]        1        3        3         1         3         3           1
[2913,]        1        3       NA         1         3        NA           1
[2914,]        1        2        3         1         3         3           1
[2915,]        1       NA       NA         1        NA        NA           1
[2916,]        1        3       NA         1         3        NA           1
[2917,]        1       NA       NA         1        NA        NA           1
[2918,]        1       NA       NA         1        NA        NA           1
[2919,]        2        3        3         2         3         3           2
[2920,]        2       NA       NA         2        NA        NA           1
[2921,]        1        3       NA         1         3        NA           1
[2922,]        2        3        3         2         3         3           1
[2923,]        1       NA       NA         1        NA        NA           1
[2924,]        1        3        3         1         3         3           1
[2925,]        2       NA       NA         2        NA        NA           1
[2926,]        1       NA       NA         1        NA        NA           1
[2927,]        2       NA       NA         2        NA        NA           2
[2928,]        1       NA       NA         1        NA        NA           1
[2929,]        1       NA       NA         1        NA        NA           1
[2930,]        2       NA       NA         2        NA        NA           2
[2931,]        2       NA       NA         2        NA        NA           2
[2932,]        1       NA       NA         1        NA        NA           1
[2933,]        1       NA       NA         1        NA        NA           1
[2934,]        1        3       NA         1         3        NA           1
[2935,]        1       NA       NA         1        NA        NA           1
[2936,]        2       NA       NA         2        NA        NA           1
[2937,]        2        3       NA         2         3        NA           2
[2938,]        1       NA       NA         1        NA        NA           1
[2939,]        1       NA       NA         1        NA        NA           1
[2940,]        2        3       NA         2         3        NA           2
[2941,]        1        3       NA         1         3        NA           1
[2942,]        1       NA       NA         1        NA        NA           1
[2943,]        1        3        3         1         3         3           1
[2944,]        1       NA       NA         1        NA        NA           1
[2945,]        1        3       NA         1         3        NA           1
[2946,]        1        3       NA         1         3        NA           1
[2947,]        1        2        3         1         2         3           1
[2948,]        1        2        3         1         2         3           1
[2949,]        1        2       NA         1         2        NA           1
[2950,]        1       NA       NA         1        NA        NA           1
[2951,]        1       NA       NA         1        NA        NA           1
[2952,]        1        3        3         1         3         3           1
[2953,]        1        2        3         1         3         3           1
[2954,]        2        3       NA         2         3        NA           2
[2955,]        2        3        3         2         3         3           1
[2956,]        1        3       NA         1         3        NA           1
[2957,]        2        3        3         2         3         3           2
[2958,]        2       NA       NA         2        NA        NA           2
[2959,]        1       NA       NA         1        NA        NA           1
[2960,]        2       NA       NA         2        NA        NA           1
[2961,]        1        2       NA         1         2        NA           1
[2962,]        1       NA       NA         1        NA        NA           1
[2963,]        1        2       NA         1         2        NA           1
[2964,]        1        3       NA         1         3        NA           1
[2965,]        1       NA       NA         1        NA        NA           1
[2966,]        1       NA       NA         1        NA        NA           1
[2967,]        1        2        3         1         2         3           1
[2968,]        1        3        3         1         3         3           1
[2969,]        3        3        3         3         3         3           3
[2970,]        1        3        3         1         3         3           1
[2971,]        1        3        3         1         3         3           1
[2972,]        1        3        3         1         3         3           1
[2973,]        1        3        3         1         3         3           1
[2974,]        1        3        3         1         3         3           1
[2975,]        2        2       NA         1         2        NA           2
[2976,]        1        2       NA         1         2        NA           1
[2977,]        1        2       NA         1         2        NA           1
[2978,]        1       NA       NA         1        NA        NA           1
[2979,]        1        2       NA         1         2        NA           1
[2980,]        1       NA       NA         1        NA        NA           1
[2981,]        2       NA       NA         1        NA        NA           1
[2982,]        2        3       NA         2         3        NA           2
[2983,]        2       NA       NA         2        NA        NA           2
[2984,]        3       NA       NA         3        NA        NA           3
[2985,]        3       NA       NA         3        NA        NA           3
[2986,]        1        3       NA         1         3        NA           1
[2987,]        2        3       NA         2         3        NA           1
[2988,]        1        3       NA         1         3        NA           1
[2989,]        1        3       NA         1         3        NA           1
[2990,]        2        3        3         2         3         3           2
[2991,]        1       NA       NA         1        NA        NA           1
[2992,]        1       NA       NA         1        NA        NA           1
[2993,]        1        2       NA         1         2        NA           1
[2994,]        2       NA       NA         2        NA        NA           2
[2995,]        1        2       NA         1         2        NA           1
[2996,]        1       NA       NA         1        NA        NA           1
[2997,]        2        3       NA         2         3        NA           2
[2998,]        1       NA       NA         1        NA        NA           1
[2999,]        1       NA       NA         1        NA        NA           1
[3000,]        1       NA       NA         1        NA        NA           1
[3001,]        1       NA       NA         1        NA        NA           1
[3002,]        2        3        3         2         3         3           1
[3003,]        1        2       NA         1         2        NA           1
[3004,]        1       NA       NA         1        NA        NA           1
[3005,]        2        3       NA         2         3        NA           1
[3006,]        2        3        3         2         3         3           1
[3007,]        3        3        3         3         3         3           3
[3008,]        1        3        3         1         3         3           1
[3009,]        2       NA       NA         2        NA        NA           1
[3010,]        1        3       NA         1         3        NA           1
[3011,]        2        3        3         2         3         3           1
[3012,]        3        3        3         2         3         3           2
[3013,]        2        3       NA         2         3        NA           2
[3014,]        2       NA       NA         2        NA        NA           2
[3015,]        2        2       NA         2         2        NA           2
[3016,]        1        3        3         1         3         3           1
[3017,]        1        3        3         1         3         3           1
[3018,]        3        3        3         3         3         3           3
[3019,]        1        3        3         1         3         3           1
[3020,]        1        3        3         1         3         3           1
[3021,]        2       NA       NA         2        NA        NA           2
[3022,]        2       NA       NA         2        NA        NA           2
[3023,]        2        3        3         2         3         3           2
[3024,]        2       NA       NA         2        NA        NA           2
[3025,]        1        3       NA         1         3        NA           1
[3026,]        1        3        3         1         3         3           1
[3027,]        3        3        3         3         3         3           3
[3028,]        3        3        3         3         3         3           3
[3029,]        3        3        3         3         3         3           3
[3030,]        3        3        3         3         3         3           3
[3031,]        3        3        3         3         3         3           3
[3032,]        2        2       NA         2         2        NA           2
[3033,]        2        2       NA         2         2        NA           2
[3034,]        1        3        3         1         3         3           1
[3035,]        2        3       NA         2         3        NA           2
[3036,]        2        2       NA         2         2        NA           2
[3037,]        3        3        3         3         3         3           3
[3038,]        2        3        3         2         3         3           2
[3039,]        2        3       NA         2         3        NA           2
[3040,]        1       NA       NA         1        NA        NA           1
[3041,]        3       NA       NA         3        NA        NA           3
[3042,]        2       NA       NA         2        NA        NA           2
[3043,]        2        2       NA         1         2        NA           2
[3044,]        2        2       NA         2         2        NA           2
[3045,]        1        3       NA         1         3        NA           1
[3046,]        2       NA       NA         2        NA        NA           2
[3047,]        2       NA       NA         2        NA        NA           2
[3048,]        1       NA       NA         1        NA        NA           1
[3049,]        1       NA       NA         1        NA        NA           1
[3050,]        1       NA       NA         1        NA        NA           1
[3051,]        1       NA       NA         1        NA        NA           1
[3052,]        1       NA       NA         1        NA        NA           1
[3053,]        2       NA       NA         2        NA        NA           2
[3054,]        2       NA       NA         2        NA        NA           2
[3055,]        1        3       NA         1         3        NA           1
[3056,]        1       NA       NA         1        NA        NA           1
[3057,]        2       NA       NA         2        NA        NA           2
[3058,]        1       NA       NA         1        NA        NA           1
[3059,]        3       NA       NA         3        NA        NA           3
[3060,]        2       NA       NA         2        NA        NA           2
[3061,]        1       NA       NA         1        NA        NA           1
[3062,]        1        2        2         1         2         2           1
[3063,]        2       NA       NA         2        NA        NA           1
[3064,]        1        1        2         1         1         2           1
[3065,]        1       NA       NA         1        NA        NA           1
[3066,]        1        2       NA         1         2        NA           1
[3067,]        1        1        2         1         2         3           1
[3068,]        1       NA       NA         1        NA        NA           1
[3069,]        1        2        3         1         2         3           1
[3070,]        2        2       NA         2         2        NA           2
[3071,]        1        1        2         1         1         2           1
[3072,]        1        1        2         1         1         2           1
[3073,]        1        2        2         1         2         2           1
[3074,]        2        2       NA         2         2        NA           2
[3075,]        1        1        2         1         1         2           1
[3076,]        1        1        2         1         1         2           1
[3077,]        1       NA       NA         1        NA        NA           1
[3078,]        1       NA       NA         1        NA        NA           1
[3079,]        2       NA       NA         2        NA        NA           1
[3080,]        1       NA       NA         1        NA        NA           1
[3081,]        1        1        2         1         1         2           1
[3082,]        1        2       NA         1         2        NA           1
[3083,]        1        1       NA         1         1        NA           1
[3084,]        1        1        2         1         1         2           1
[3085,]        1        2       NA         1         3        NA           1
[3086,]        1        1        2         1         1         2           1
[3087,]        1        2       NA         1         2        NA           1
[3088,]        2       NA       NA         2        NA        NA           2
[3089,]        1       NA       NA         1        NA        NA           1
[3090,]        1       NA       NA         1        NA        NA           1
[3091,]        1       NA       NA         1        NA        NA           1
[3092,]        1        1        2         1         2         2           1
[3093,]        1       NA       NA         1        NA        NA           1
[3094,]        1        1        2         1         1         2           1
[3095,]        1        1        2         1         1         2           1
[3096,]        1       NA       NA         1        NA        NA           1
[3097,]        1       NA       NA         1        NA        NA           1
[3098,]        1        3        3         1         3         3           1
[3099,]        1        3       NA         1         3        NA           1
[3100,]        1        3       NA         1         3        NA           1
[3101,]        1        2       NA         1         2        NA           1
[3102,]        1        2       NA         1         2        NA           1
[3103,]        1        2       NA         1         2        NA           1
[3104,]        1        2        3         1         2         3           1
[3105,]        1       NA       NA         1        NA        NA           1
[3106,]        1        3       NA         1         3        NA           1
[3107,]        1       NA       NA         1        NA        NA           1
[3108,]        1        3        3         1         3         3           1
[3109,]        1       NA       NA         1        NA        NA           1
[3110,]        1        2        3         1         2         3           1
[3111,]        1       NA       NA         1        NA        NA           1
[3112,]        1       NA       NA         1        NA        NA           1
[3113,]        1       NA       NA         1        NA        NA           1
[3114,]        1        2       NA         1         2        NA           1
[3115,]        1        2        3         1         2         3           1
[3116,]        1        2        3         1         2         3           1
[3117,]        1       NA       NA         1        NA        NA           1
[3118,]        1        1        3         1         1         3           1
[3119,]        1       NA       NA         1        NA        NA           1
[3120,]        1        3       NA         1         3        NA           1
[3121,]        1       NA       NA         1        NA        NA           1
[3122,]        1        2        3         1         2         3           1
[3123,]        1        3       NA         1         3        NA           1
[3124,]        1       NA       NA         1        NA        NA           1
[3125,]        1       NA       NA         1        NA        NA           1
[3126,]        1       NA       NA         1        NA        NA           1
[3127,]        1       NA       NA         1        NA        NA           1
[3128,]        1       NA       NA         1        NA        NA           1
[3129,]        1        3       NA         1         3        NA           1
[3130,]        1        1        3         1         1         3           1
[3131,]        1        1        3         1         1         3           1
[3132,]        1        1        3         1         1         3           1
[3133,]        3        3        2         3         3         3           3
[3134,]        1        2        3         1         2         3           1
[3135,]        1       NA       NA         1        NA        NA           1
[3136,]        1       NA       NA         1        NA        NA           1
[3137,]        1        2       NA         1         2        NA           1
[3138,]        1        3       NA         1         3        NA           1
[3139,]        1        1        2         1         1         2           1
[3140,]        3       NA       NA         3        NA        NA           3
[3141,]        3       NA       NA         3        NA        NA           3
[3142,]        3       NA       NA         3        NA        NA           3
[3143,]        3       NA       NA         3        NA        NA           3
[3144,]        1       NA       NA         1        NA        NA           1
[3145,]        1       NA       NA         1        NA        NA           1
[3146,]        1       NA       NA         1        NA        NA           1
[3147,]        1        2        3         1         2         3           1
[3148,]        1        2        2         1         2         2           1
[3149,]        1        2        2         1         2         2           1
[3150,]        1        2        2         1         2         2           1
[3151,]        1        2        3         1         2         3           1
[3152,]        1        2        3         1         2         3           1
[3153,]        1        2        3         1         2         3           1
[3154,]        1        2        3         1         2         3           1
[3155,]        1        2       NA         1         2        NA           1
[3156,]        1        2       NA         1         2        NA           1
[3157,]        1        2       NA         1         2        NA           1
[3158,]        2       NA       NA         1        NA        NA           2
[3159,]        2       NA       NA         1        NA        NA           2
[3160,]        1       NA       NA         1        NA        NA           1
[3161,]        1        2        3         1         2         2           1
[3162,]        1       NA       NA         1        NA        NA           1
[3163,]        1        2        3         1         1         3           1
[3164,]        2       NA       NA         2        NA        NA           2
[3165,]        1        2       NA         1         2        NA           1
[3166,]        1        2        3         1         2         3           1
[3167,]        1        2        3         1         2         3           1
[3168,]        2        2       NA         2         2        NA           2
[3169,]        1        1        3         1         1         3           1
[3170,]        1        2        2         1         2         2           1
[3171,]        1        1        2         1         1         2           1
[3172,]        1        2        2         1         2         2           1
[3173,]        1       NA       NA         1        NA        NA           1
[3174,]        2       NA       NA         1        NA        NA           1
[3175,]        1       NA       NA         1        NA        NA           1
[3176,]        1        1        2         1         1         2           1
[3177,]        1        2       NA         1         2        NA           1
[3178,]        1        2        2         1         2         2           1
[3179,]        1        2       NA         1         2        NA           1
[3180,]        1        2        2         1         1         2           1
[3181,]        1        3       NA         1         3        NA           1
[3182,]        1        2       NA         1         2        NA           1
[3183,]        1        2        2         1         2         2           1
[3184,]        2       NA       NA         1        NA        NA           2
[3185,]        2       NA       NA         1        NA        NA           1
[3186,]        2       NA       NA         1        NA        NA           1
[3187,]        1       NA       NA         1        NA        NA           1
[3188,]        1        2       NA         1         2        NA           1
[3189,]        2        2       NA         2         2        NA           2
[3190,]        1        3       NA         1         3        NA           1
[3191,]        1        2        3         1         2         3           1
[3192,]       NA       NA       NA        NA        NA        NA          NA
[3193,]        2       NA       NA         2        NA        NA           2
[3194,]        1       NA       NA         1        NA        NA           1
[3195,]        1       NA       NA         1        NA        NA           1
[3196,]        1        3       NA         1         3        NA           1
[3197,]        1        2        3         1         1         3           1
[3198,]        1        2       NA         1         2        NA           1
[3199,]        1        2        3         1         1         3           1
[3200,]        1        3       NA         1         3        NA           1
[3201,]        1       NA       NA         1        NA        NA           1
[3202,]        3       NA       NA         3        NA        NA           3
[3203,]        1       NA       NA         1        NA        NA           1
[3204,]        1       NA       NA         1        NA        NA           1
[3205,]        1       NA       NA         1        NA        NA           1
[3206,]        1       NA       NA         1        NA        NA           1
[3207,]        1       NA       NA         1        NA        NA           1
[3208,]        2        2       NA         2         2        NA           2
[3209,]        1        3       NA         1         3        NA           1
[3210,]        1        3       NA         1         3        NA           1
[3211,]        1        2        3         1         1         3           1
[3212,]        1        2        3         1         1         3           1
[3213,]        1        2       NA         1         2        NA           1
[3214,]        1        1        2         1         1         2           1
[3215,]        1        1        2         1         1         2           1
[3216,]        1        1        2         1         1         2           1
[3217,]        1        1        2         1         1         2           1
[3218,]        1        2        3         1         1         3           1
[3219,]        1        2        2         1         2         2           1
[3220,]        1        2        3         1         2         2           1
[3221,]        1        2        3         1         2         2           1
[3222,]        1        2        3         1         2         2           1
[3223,]        1       NA       NA         1        NA        NA           1
[3224,]        1       NA       NA         1        NA        NA           1
[3225,]        2       NA       NA         1        NA        NA           1
[3226,]        1        2        2         1         2         2           1
[3227,]        1       NA       NA         1        NA        NA           1
[3228,]        1        1        2         1         1         2           1
[3229,]        1       NA       NA         1        NA        NA           1
[3230,]        1       NA       NA         1        NA        NA           1
[3231,]        3        2       NA         2         2        NA           3
[3232,]        1        2        3         1         2         2           1
[3233,]        2        2       NA         2         2        NA           2
[3234,]        1        2        2         1         2         2           1
[3235,]        1       NA       NA         1        NA        NA           1
[3236,]        1        2       NA         1         2        NA           1
[3237,]        1        3       NA         1         3        NA           1
[3238,]        1        3       NA         1         3        NA           1
[3239,]        1        2       NA         1         2        NA           1
[3240,]        2        2        3         2         2         3           2
[3241,]        1       NA       NA         1        NA        NA           1
[3242,]        1        2        3         1         2         3           1
[3243,]        1        3        3         1         3         3           1
[3244,]        1       NA       NA         1        NA        NA           1
[3245,]        1        2        3         1         2         3           1
[3246,]        2       NA       NA         2        NA        NA           1
[3247,]        1       NA       NA         1        NA        NA           1
[3248,]        1        3       NA         1         3        NA           1
[3249,]        1       NA       NA         1        NA        NA           1
[3250,]        1        3       NA         1         3        NA           1
[3251,]        1        2        3         1         2         3           1
[3252,]        1        3        3         1         3         3           1
[3253,]        1       NA       NA         1        NA        NA           1
[3254,]        1       NA       NA         1        NA        NA           1
[3255,]        1        2       NA         1         2        NA           1
[3256,]        1        2        3         1         2         3           1
[3257,]        1        2       NA         1         2        NA           1
[3258,]        1        2       NA         1         2        NA           1
[3259,]        1        2        3         1         2         3           1
[3260,]        1        2       NA         1         2        NA           1
[3261,]        1        2        3         1         2         3           1
[3262,]        2       NA       NA         2        NA        NA           2
[3263,]        1       NA       NA         1        NA        NA           1
[3264,]        3        2        3         3         2         3           3
[3265,]        2       NA       NA         2        NA        NA           2
[3266,]        1       NA       NA         1        NA        NA           1
[3267,]        1       NA       NA         1        NA        NA           1
[3268,]        1        3       NA         1         3        NA           1
[3269,]        1        2       NA         1         2        NA           1
[3270,]        1        2        3         1         2         3           1
[3271,]        3        2        2         3         2         3           2
[3272,]        1       NA       NA         1        NA        NA           1
[3273,]        1       NA       NA         1        NA        NA           1
[3274,]        1       NA       NA         1        NA        NA           1
[3275,]        1       NA       NA         1        NA        NA           1
[3276,]        2       NA       NA         2        NA        NA           2
[3277,]        2       NA       NA         2        NA        NA           2
[3278,]        1        2       NA         1         2        NA           1
[3279,]        1        2       NA         1         2        NA           1
[3280,]        1        2       NA         1         2        NA           1
[3281,]        1        3       NA         1         3        NA           1
[3282,]        3        2        3         3         2         3           3
[3283,]        1        3        3         1         3         3           1
[3284,]        1        2       NA         1         2        NA           1
[3285,]        2        3        3         2         3         3           2
[3286,]        1        3        3         1         3         3           1
[3287,]        1        3        3         1         3         3           1
[3288,]        1       NA       NA         1        NA        NA           1
[3289,]        1        2       NA         1         2        NA           1
[3290,]        1        3        3         1         3         3           1
[3291,]        1        2       NA         1         2        NA           1
[3292,]        1       NA       NA         1        NA        NA           1
[3293,]        1        3        3         1         3         3           1
[3294,]        2       NA       NA         2        NA        NA           2
[3295,]        1        3        3         1         3         3           1
[3296,]        1        2        3         1         3         3           1
[3297,]        1       NA       NA         1        NA        NA           1
[3298,]        1        2        3         1         2         3           1
[3299,]        1        2       NA         1         2        NA           1
[3300,]        1        2       NA         1         2        NA           1
[3301,]        1        3        3         1         3         3           1
[3302,]        1        3        3         1         3         3           1
[3303,]        1        2       NA         1         2        NA           1
[3304,]        1        2       NA         1         2        NA           1
[3305,]        1       NA       NA         1        NA        NA           1
[3306,]        1       NA       NA         1        NA        NA           1
[3307,]        1        2        3         1         3         3           1
[3308,]        1        2        3         1         3         3           1
[3309,]        1        3        3         1         3         3           1
[3310,]        2        3        3         2         3         3           2
[3311,]        1        2        3         1         3         3           1
[3312,]        1        2       NA         1         2        NA           1
[3313,]        1        2       NA         1         2        NA           1
[3314,]        1        2       NA         1         2        NA           1
[3315,]        1        2       NA         1         2        NA           1
[3316,]        1        2       NA         1         2        NA           1
[3317,]        1        2       NA         1         2        NA           1
[3318,]        1       NA       NA         1        NA        NA           1
[3319,]        1       NA       NA         1        NA        NA           1
[3320,]        2       NA       NA         2        NA        NA           2
[3321,]        2       NA       NA         2        NA        NA           2
[3322,]        1       NA       NA         1        NA        NA           1
[3323,]        1       NA       NA         1        NA        NA           1
[3324,]        1       NA       NA         1        NA        NA           1
[3325,]        1       NA       NA         1        NA        NA           1
[3326,]        1        2       NA         1         2        NA           1
[3327,]        1        2       NA         1         3        NA           1
[3328,]        1        3        3         1         3         3           1
[3329,]        1        2       NA         1         3        NA           1
[3330,]        1       NA       NA         1        NA        NA           1
[3331,]        1       NA       NA         1        NA        NA           1
[3332,]        1        2       NA         1         2        NA           1
[3333,]        2        3        3         2         3         3           2
[3334,]        3        3        3         3         3         3           3
[3335,]        3        3        3         3         3         3           3
[3336,]        3        3        3         3         3         3           3
[3337,]        1        3        3         1         3         3           1
[3338,]        1        3       NA         1         3        NA           1
[3339,]        1        3       NA         1         3        NA           1
[3340,]        1        1        3         1         1         3           1
[3341,]        3       NA       NA         3        NA        NA           3
[3342,]        3       NA       NA         3        NA        NA           3
[3343,]        1       NA       NA         1        NA        NA           1
[3344,]        1        2        3         1         2         3           1
[3345,]        1        3       NA         1         2        NA           1
[3346,]        1        3       NA         1         2        NA           1
[3347,]        2       NA       NA         2        NA        NA           2
[3348,]        2       NA       NA         2        NA        NA           2
[3349,]        1       NA       NA         1        NA        NA           1
[3350,]        1        2        3         1         2         3           1
[3351,]        1       NA       NA         1        NA        NA           1
[3352,]        1        2        3         1         2         3           1
[3353,]        2       NA       NA         2        NA        NA           2
[3354,]        1        3       NA         1         2        NA           1
[3355,]        1        2        3         1         2         3           1
[3356,]        3        3       NA         3         3        NA           3
[3357,]        1        1        3         1         1         3           1
[3358,]        1        2        3         1         2         3           1
[3359,]        1       NA       NA         1        NA        NA           1
[3360,]        2       NA       NA         2        NA        NA           2
[3361,]        1       NA       NA         1        NA        NA           1
[3362,]        1        1        3         1         1         3           1
[3363,]        1        3       NA         1         3        NA           1
[3364,]        1        2       NA         1         2        NA           1
[3365,]        1        2        3         1         2         3           1
[3366,]        1        3       NA         1         3        NA           1
[3367,]        1        3       NA         1         3        NA           1
[3368,]        2       NA       NA         2        NA        NA           2
[3369,]        2       NA       NA         2        NA        NA           2
[3370,]        2       NA       NA         2        NA        NA           2
[3371,]        1        3       NA         1         3        NA           1
[3372,]        3       NA       NA         3        NA        NA           3
[3373,]        1       NA       NA         1        NA        NA           1
[3374,]        1        3       NA         1         3        NA           1
[3375,]        1        2        3         1         2         3           1
[3376,]        3       NA       NA         3        NA        NA           3
[3377,]        1       NA       NA         1        NA        NA           1
[3378,]        1       NA       NA         1        NA        NA           1
[3379,]        1       NA       NA         1        NA        NA           1
[3380,]        1       NA       NA         1        NA        NA           1
[3381,]        1        3       NA         1         3        NA           1
[3382,]        1        3       NA         1         3        NA           1
[3383,]        1        3       NA         1         3        NA           1
[3384,]        1        1        3         1         1         3           1
[3385,]        1        2        3         1         2         3           1
[3386,]        1        2        3         1         2         3           1
[3387,]        1        3        3         1         2         3           1
[3388,]        1       NA       NA         1        NA        NA           1
[3389,]        1       NA       NA         1        NA        NA           1
[3390,]        2       NA       NA         2        NA        NA           1
[3391,]        1       NA       NA         1        NA        NA           1
[3392,]        1        1        3         1         1         3           1
[3393,]        1       NA       NA         1        NA        NA           1
[3394,]        1       NA       NA         1        NA        NA           1
[3395,]        1        3        3         1         2         3           1
[3396,]        3        3       NA         3         3        NA           3
[3397,]        1        3       NA         1         3        NA           1
[3398,]        1        3       NA         1         3        NA           1
[3399,]        1       NA       NA         1        NA        NA           1
[3400,]        3       NA       NA         3        NA        NA           3
[3401,]        3       NA       NA         3        NA        NA           3
[3402,]        3       NA       NA         3        NA        NA           3
[3403,]        2       NA       NA         2        NA        NA           2
[3404,]        3       NA       NA         3        NA        NA           3
[3405,]        1       NA       NA         1        NA        NA           1
[3406,]        1       NA       NA         1        NA        NA           1
[3407,]        1       NA       NA         1        NA        NA           1
[3408,]        1       NA       NA         1        NA        NA           1
[3409,]        1       NA       NA         1        NA        NA           1
[3410,]        1        1       NA         1         1        NA           1
[3411,]        1        2       NA         1         2        NA           1
[3412,]        1        2       NA         1         2        NA           1
[3413,]        1        2        3         1         2         3           1
[3414,]        1        1        2         1         1         2           1
[3415,]        1        1        2         1         1         2           1
[3416,]        1        1        2         1         1         2           1
[3417,]        1        2        2         1         2         2           1
[3418,]        1       NA       NA         1        NA        NA           1
[3419,]        1       NA       NA         1        NA        NA           1
[3420,]        1        1        2         1         1         2           1
[3421,]        1       NA       NA         1        NA        NA           1
[3422,]        1       NA       NA         1        NA        NA           1
[3423,]        1       NA       NA         1        NA        NA           1
[3424,]        1        1        2         1         1         2           1
[3425,]        1       NA       NA         1        NA        NA           1
[3426,]        1       NA       NA         1        NA        NA           1
[3427,]        1        1        3         1         1         3           1
[3428,]        1        2       NA         1         2        NA           1
[3429,]        1        3        3         1         3         3           1
[3430,]        1       NA       NA         1        NA        NA           1
[3431,]        1       NA       NA         1        NA        NA           1
[3432,]        1        2       NA         1         2        NA           1
[3433,]        1        1       NA         1         1        NA           1
[3434,]        2        2        3         2         2         3           2
[3435,]        2        2        3         2         2         3           2
[3436,]        1       NA       NA         1        NA        NA           1
[3437,]        1        2       NA         1         2        NA           1
[3438,]        1        1       NA         1         1        NA           1
[3439,]        1        2       NA         1         2        NA           1
[3440,]        1        2        3         1         2         3           1
[3441,]        1        2        3         1         2         3           1
[3442,]        1        2        2         1         2         2           1
[3443,]        1        2        2         1         2         2           1
[3444,]        1       NA       NA         1        NA        NA           1
[3445,]        1       NA       NA         1        NA        NA           1
[3446,]        1       NA       NA         1        NA        NA           1
[3447,]        1       NA       NA         1        NA        NA           1
[3448,]        1       NA       NA         1        NA        NA           1
[3449,]        1        2       NA         1         2        NA           1
[3450,]        1        2       NA         1         2        NA           1
[3451,]        1        2       NA         1         2        NA           1
[3452,]        1        2       NA         1         2        NA           1
[3453,]        2        2        3         2         2         3           2
[3454,]        3        2        3         3         2         3           3
[3455,]        3        2        3         3         2         3           3
[3456,]        1        3        3         1         3         3           1
[3457,]        1        3        3         1         3         3           1
[3458,]        2       NA       NA         2        NA        NA           2
[3459,]        1        3       NA         1         3        NA           1
[3460,]        1        2        2         1         2         2           1
[3461,]        1        2        2         1         2         2           1
[3462,]        1        1        2         1         1         2           1
[3463,]        1       NA       NA         1        NA        NA           1
[3464,]        1       NA       NA         1        NA        NA           1
[3465,]        1       NA       NA         1        NA        NA           1
[3466,]        2        2        2         2         2         2           2
[3467,]        1        2        3         1         2         3           1
[3468,]        1       NA       NA         1        NA        NA           1
[3469,]        3       NA       NA         3        NA        NA           3
[3470,]        1        2       NA         1         2        NA           1
[3471,]        1        2        3         1         2         3           1
[3472,]        1       NA       NA         1        NA        NA           1
[3473,]        2       NA       NA         2        NA        NA           2
[3474,]        1        2       NA         1         2        NA           1
[3475,]        1        2       NA         1         2        NA           1
[3476,]        3       NA       NA         3        NA        NA           3
[3477,]        1       NA       NA         1        NA        NA           1
[3478,]        1        2        3         1         2         3           1
[3479,]        1       NA       NA         1        NA        NA           1
[3480,]        1        3        2         1         3         2           1
[3481,]        1       NA       NA         1        NA        NA           1
[3482,]        1        2       NA         1         2        NA           1
[3483,]        1        1       NA         1         1        NA           1
[3484,]        1        2        2         1         2         2           1
[3485,]        2        2        3         2         2         3           2
[3486,]        1        2       NA         1         2        NA           1
[3487,]        1        1       NA         1         1        NA           1
[3488,]        1        2        3         1         2         3           1
[3489,]        1       NA       NA         1        NA        NA           1
[3490,]        1       NA       NA         1        NA        NA           1
[3491,]        1        3        3         1         3         3           1
[3492,]        1       NA       NA         1        NA        NA           1
[3493,]        1       NA       NA         1        NA        NA           1
[3494,]        2        2        3         2         2         3           2
[3495,]        1        2        3         1         2         3           1
[3496,]        1        2       NA         1         2        NA           1
[3497,]        1        1       NA         1         1        NA           1
[3498,]        1        2       NA         1         2        NA           1
[3499,]        1        2        3         1         2         2           1
[3500,]        1        2        3         1         2         2           1
[3501,]        1        2        3         1         2         3           1
[3502,]        1        2        3         1         2         3           1
[3503,]        1        2        3         1         2         2           1
[3504,]        1        2        3         1         2         2           1
[3505,]        1       NA       NA         1        NA        NA           1
[3506,]        1       NA       NA         1        NA        NA           1
[3507,]        1       NA       NA         1        NA        NA           1
[3508,]        1       NA       NA         1        NA        NA           1
[3509,]        1       NA       NA         1        NA        NA           1
[3510,]        1        2       NA         1         2        NA           1
[3511,]        1        2       NA         1         2        NA           1
[3512,]        3        2        3         3         2         3           3
[3513,]        2        2        3         2         2         3           2
[3514,]        1        3        3         1         3         3           1
[3515,]        1        3       NA         1         3        NA           1
[3516,]        1        2        3         1         2         2           1
[3517,]        2        2        2         2         2         2           2
[3518,]        1       NA       NA         1        NA        NA           1
[3519,]        1       NA       NA         1        NA        NA           1
[3520,]        1       NA       NA         1        NA        NA           1
[3521,]        1        1        2         1         1         2           1
[3522,]        1        1       NA         1         1        NA           1
[3523,]        2        2        2         2         2         2           2
[3524,]        1       NA       NA         1        NA        NA           1
[3525,]        1        1        2         1         1         2           1
[3526,]        2        1        2         2         1         2           1
[3527,]        1        1        2         1         1         2           1
[3528,]        1       NA       NA         1        NA        NA           1
[3529,]        1       NA       NA         1        NA        NA           1
[3530,]        1        1        2         1         1         2           1
[3531,]        1       NA       NA         1        NA        NA           1
[3532,]        1       NA       NA         1        NA        NA           1
[3533,]        2        1       NA         2         1        NA           2
[3534,]        1        3        2         1         3         2           1
[3535,]        1        2        2         1         2         2           1
[3536,]        1        3       NA         1         3        NA           1
[3537,]        1       NA       NA         1        NA        NA           1
[3538,]        1        3        2         1         3         2           1
[3539,]        1       NA       NA         1        NA        NA           1
[3540,]        3       NA       NA         3        NA        NA           3
[3541,]        1       NA       NA         1        NA        NA           1
[3542,]        1       NA       NA         1        NA        NA           1
[3543,]        1       NA       NA         1        NA        NA           1
[3544,]        1       NA       NA         1        NA        NA           1
[3545,]        1       NA       NA         1        NA        NA           1
[3546,]        1        2       NA         1         2        NA           1
[3547,]        1        2       NA         1         2        NA           1
[3548,]        1        2       NA         1         2        NA           1
[3549,]        1        2       NA         1         2        NA           1
[3550,]        2        2       NA         2         2        NA           2
[3551,]        2        2       NA         2         2        NA           2
[3552,]        2        2        2         2         2         2           2
[3553,]        1        1        3         1         1         3           1
[3554,]        1        3        3         1         3         3           1
[3555,]        1        3        2         1         3         2           1
[3556,]        1        2        2         1         2         2           1
[3557,]        1        2        2         1         2         2           1
[3558,]        1        2        2         1         2         2           1
[3559,]        1        2        2         1         2         2           1
[3560,]        2       NA       NA         2        NA        NA           2
[3561,]        1        1        2         1         1         2           1
[3562,]        1       NA       NA         1        NA        NA           1
[3563,]        1        1        3         1         1         3           1
[3564,]        1        1        2         1         1         2           1
[3565,]        1        1       NA         1         1        NA           1
[3566,]        1        2        3         1         2         3           1
[3567,]        1        1        3         1         1         3           1
[3568,]        1       NA       NA         1        NA        NA           1
[3569,]        2        1        3         1         1         3           2
[3570,]        1        1        3         1         1         3           1
[3571,]        2        1        3         1         1         3           2
[3572,]        2        1        3         1         1         3           2
[3573,]        1       NA       NA         1        NA        NA           1
[3574,]        1       NA       NA         1        NA        NA           1
[3575,]        1        3        3         1         3         3           1
[3576,]        1        3        3         1         3         3           1
[3577,]        1        3        3         1         3         3           1
[3578,]        1        3        3         1         3         3           1
[3579,]        1       NA       NA         1        NA        NA           1
[3580,]        2       NA       NA         2        NA        NA           2
[3581,]        2       NA       NA         2        NA        NA           2
[3582,]        1       NA       NA         1        NA        NA           1
[3583,]        1       NA       NA         1        NA        NA           1
[3584,]        1       NA       NA         1        NA        NA           1
[3585,]        3       NA       NA         3        NA        NA           3
[3586,]        3       NA       NA         3        NA        NA           3
[3587,]        1       NA       NA         1        NA        NA           1
[3588,]        2        1        3         1         1         3           2
[3589,]        2        1        3         1         1         3           2
[3590,]        2        1        3         1         1         3           2
[3591,]        1        1        2         1         1         2           1
[3592,]        2       NA       NA         2        NA        NA           2
[3593,]        2       NA       NA         2        NA        NA           2
[3594,]        1        1        2         1         1         2           1
[3595,]        1       NA       NA         1        NA        NA           1
[3596,]        1        1        3         1         1         3           1
[3597,]        1        1       NA         1         1        NA           1
[3598,]        1        1        3         1         1         3           1
[3599,]        2        1        3         1         1         3           1
[3600,]        1       NA       NA         1        NA        NA           1
[3601,]        1       NA       NA         1        NA        NA           1
[3602,]        1        3       NA         1         3        NA           1
[3603,]        2        2        2         2         2         2           2
[3604,]        1       NA       NA         1        NA        NA           1
[3605,]        3       NA       NA         3        NA        NA           3
[3606,]        3        3        3         3         3         3           3
[3607,]        1        2        3         1         2         3           1
[3608,]        1       NA       NA         1        NA        NA           1
[3609,]        1        2        3         1         2         3           1
[3610,]        1        2        2         1         2         2           1
[3611,]        1        2        2         1         2         2           1
[3612,]        1       NA       NA         1        NA        NA           1
[3613,]        1       NA       NA         1        NA        NA           1
[3614,]        1        2       NA         1         2        NA           1
[3615,]        1        2       NA         1         2        NA           1
[3616,]        2        2        3         2         2         3           2
[3617,]        1        2       NA         1         2        NA           1
[3618,]        1        3       NA         1         3        NA           1
[3619,]        1        3        3         1         3         3           1
[3620,]        1        2       NA         1         2        NA           1
[3621,]        1        3        3         1         3         3           1
[3622,]        1        2        3         1         2         3           1
[3623,]        1        2       NA         1         2        NA           1
[3624,]        1        2        2         1         2         2           1
[3625,]        1        2       NA         1         2        NA           1
[3626,]        1       NA       NA         1        NA        NA           1
[3627,]        1        2        3         1         2         3           1
[3628,]        1       NA       NA         1        NA        NA           1
[3629,]        2       NA       NA         2        NA        NA           2
[3630,]        1        3       NA         1         3        NA           1
[3631,]        3       NA       NA         3        NA        NA           3
[3632,]        1       NA       NA         1        NA        NA           1
[3633,]        1        2        3         1         2         3           1
[3634,]        1        2        2         1         2         2           1
[3635,]        2       NA       NA         2        NA        NA           2
[3636,]        1        1        3         1         1         3           1
[3637,]        1        1        3         1         1         3           1
[3638,]        1       NA       NA         1        NA        NA           1
[3639,]        1        3       NA         1         3        NA           1
[3640,]        1        2       NA         1         2        NA           1
[3641,]        2        2        3         2         2         2           2
[3642,]        1        2        3         1         2         3           1
[3643,]        1        2        3         1         2         3           1
[3644,]        1        2        3         1         2         3           1
[3645,]        1        2        3         1         2         3           1
[3646,]        1        2        3         1         2         3           1
[3647,]        1        2        3         1         2         3           1
[3648,]        1        1        3         1         1         3           1
[3649,]        1        1        3         1         1         3           1
[3650,]        1        1        3         1         1         3           1
[3651,]        3        2       NA         3         2        NA           3
[3652,]        1        1       NA         1         1        NA           1
[3653,]        1        1        3         1         1         3           1
[3654,]        1       NA       NA         1        NA        NA           1
[3655,]        1       NA       NA         1        NA        NA           1
[3656,]        1        2       NA         1         2        NA           1
[3657,]        1        1        3         1         1         3           1
[3658,]        1        1        3         1         1         3           1
[3659,]        1        3        2         1         3         2           1
[3660,]        1        2        2         1         2         2           1
[3661,]        1        2        2         1         2         2           1
[3662,]        1       NA       NA         1        NA        NA           1
[3663,]        1        2       NA         1         2        NA           1
[3664,]        1        2       NA         1         2        NA           1
[3665,]        1       NA       NA         1        NA        NA           1
[3666,]        1        3        3         1         3         3           1
[3667,]        1       NA       NA         1        NA        NA           1
[3668,]        1        2       NA         1         2        NA           1
[3669,]        1        2        2         1         2         2           1
[3670,]        3        3        3         3         3         3           3
[3671,]        1        2       NA         1         2        NA           1
[3672,]        1       NA       NA         1        NA        NA           1
[3673,]        3       NA       NA         3        NA        NA           3
[3674,]        1        2        2         1         2         2           1
[3675,]        3       NA       NA         3        NA        NA           3
[3676,]        2       NA       NA         2        NA        NA           1
[3677,]        1        1        2         1         1         2           1
[3678,]        1        2       NA         1         2        NA           1
[3679,]        1        3       NA         1         3        NA           1
[3680,]        1        3       NA         1         3        NA           1
[3681,]        1        2        2         1         2         2           1
[3682,]        2       NA       NA         2        NA        NA           1
[3683,]        1       NA       NA         1        NA        NA           1
[3684,]        1        3       NA         1         3        NA           1
[3685,]        1        2       NA         1         2         3           1
[3686,]        1        3       NA         1         3        NA           1
[3687,]        1        3       NA         1         3        NA           1
[3688,]        1        3       NA         1         3        NA           1
[3689,]        1        2       NA         1         2        NA           1
[3690,]        1        2       NA         1         2        NA           1
[3691,]        1       NA       NA         1        NA        NA           1
[3692,]        1        2       NA         1         2        NA           1
[3693,]        1       NA       NA         1        NA        NA           1
[3694,]        1        2       NA         1         2        NA           1
[3695,]        2       NA       NA         2        NA        NA           2
[3696,]        1        3        3         1         3         3           1
[3697,]        1       NA       NA         1        NA        NA           1
[3698,]        3       NA       NA         3        NA        NA           2
[3699,]        1        2       NA         1         2        NA           1
[3700,]        3       NA       NA         3        NA        NA           3
[3701,]        1        2        3         1         2         3           1
[3702,]        1        2        2         1         2         2           1
[3703,]        1       NA       NA         1        NA        NA           1
[3704,]        1        2        3         1         2         2           1
[3705,]        1        2        3         1         2         2           1
[3706,]        1        3       NA         1         3        NA           1
[3707,]        1        2       NA         1         2        NA           1
[3708,]        1       NA       NA         1        NA        NA           1
[3709,]        1        2       NA         1         2        NA           1
[3710,]        1        2       NA         1         2        NA           1
[3711,]        1        2       NA         1         2        NA           1
[3712,]        1        3        3         1         3         3           1
[3713,]        1        3        3         1         3         3           1
[3714,]        1        3        3         1         3         3           1
[3715,]        1        2        3         1         2         2           1
[3716,]        1        2        3         1         2         2           1
[3717,]        3        2        2         3         2         2           3
[3718,]        3        2        2         3         2         2           3
[3719,]        3        2        2         3         2         2           3
[3720,]        1        2        3         1         2         2           1
[3721,]        1        2        3         1         2         2           1
[3722,]        1        2        3         1         2         2           1
[3723,]        1        2        3         1         2         2           1
[3724,]        1        2        3         1         2         2           1
[3725,]        1        2        3         1         2         3           1
[3726,]        1        2        3         1         2         2           1
[3727,]        1        2        3         1         2         2           1
[3728,]        1        2       NA         1         2        NA           1
[3729,]        1        2       NA         1         2        NA           1
[3730,]        1       NA       NA         1        NA        NA           1
[3731,]        3       NA       NA         3        NA        NA           3
[3732,]        1       NA       NA         1        NA        NA           1
[3733,]        1       NA       NA         1        NA        NA           1
[3734,]        2        3       NA         2         3        NA           2
[3735,]        2       NA       NA         2        NA        NA           2
[3736,]        1       NA       NA         1        NA        NA           1
[3737,]        1       NA       NA         1        NA        NA           1
[3738,]        2       NA       NA         2        NA        NA           2
[3739,]        1       NA       NA         1        NA        NA           1
[3740,]        3       NA       NA         3        NA        NA           3
[3741,]        1       NA       NA         1        NA        NA           1
[3742,]        3       NA       NA         3        NA        NA           3
[3743,]        2        1       NA         2         1        NA           2
[3744,]        1        1        2         1         1         2           1
[3745,]        1       NA       NA         1        NA        NA           1
[3746,]        1        1        2         1         1         2           1
[3747,]        1        1       NA         1         1        NA           1
[3748,]        1        1       NA         1         1        NA           1
[3749,]        1       NA       NA         1        NA        NA           1
[3750,]        1        1       NA         1         1        NA           1
[3751,]        1       NA       NA         1        NA        NA           1
[3752,]        1        2        2         1         2         2           1
[3753,]        1        2        2         1         2         2           1
[3754,]        1        2        2         1         2         2           1
[3755,]        2        3       NA         2         3        NA           2
[3756,]        1       NA       NA         1        NA        NA           1
[3757,]        1        1        2         1         1         2           1
[3758,]        1        3       NA         1         3        NA           1
[3759,]        3       NA       NA         3        NA        NA           3
[3760,]        1       NA       NA         1        NA        NA           1
[3761,]        1        2       NA         1         2        NA           1
[3762,]        1        2        3         1         2         3           1
[3763,]        1        3        3         1         3         3           1
[3764,]        1        2        3         1         2         3           1
[3765,]        1       NA       NA         1        NA        NA           1
[3766,]        1       NA       NA         1        NA        NA           1
[3767,]        2       NA       NA         2        NA        NA           2
[3768,]        1       NA       NA         1        NA        NA           1
[3769,]        1        3        3         1         3         3           1
[3770,]        1        3        3         1         3         3           1
[3771,]        1        3        3         1         3         3           1
[3772,]        1        3        3         1         3         3           1
[3773,]        1       NA       NA         1        NA        NA           1
[3774,]        2       NA       NA         2        NA        NA           1
[3775,]        1        1       NA         1         1        NA           1
[3776,]        2        3        3         2         3         3           1
[3777,]        1       NA       NA         1        NA        NA           1
[3778,]        1       NA       NA         1        NA        NA           1
[3779,]        1       NA       NA         1        NA        NA           1
[3780,]        3        3        3         3         3         3           3
[3781,]        1        3       NA         1         3        NA           1
[3782,]        1       NA       NA         1        NA        NA           1
[3783,]        1        2       NA         1         2        NA           1
[3784,]        1        3        3         1         3         3           1
[3785,]        1        3       NA         1         3        NA           1
[3786,]        1        1       NA         1         1        NA           1
[3787,]        1       NA       NA         1        NA        NA           1
[3788,]        1       NA       NA         1        NA        NA           1
[3789,]        1       NA       NA         1        NA        NA           1
[3790,]        2        3       NA         2         3        NA           2
[3791,]        2        2        3         2         2         3           2
[3792,]        1        3        3         1         3         3           1
[3793,]        2        2        3         2         2         3           2
[3794,]        1        3        3         1         3         3           1
[3795,]        3        2        3         3         2         3           3
[3796,]        3        2        3         3         2         3           3
[3797,]        3        2        3         3         2         3           3
[3798,]        3        2        3         3         2         3           3
[3799,]        1        1        2         1         1         2           1
[3800,]        2        2        3         2         2         3           2
[3801,]        1        3        2         1         3         2           1
[3802,]        1        3        2         1         3         2           1
[3803,]        1        1        2         1         1         2           1
[3804,]        1        3        2         1         3         2           1
[3805,]        2        2        3         2         2         3           2
[3806,]        2        2        3         2         2         3           2
[3807,]        1       NA       NA         1        NA        NA           1
[3808,]        1       NA       NA         1        NA        NA           1
[3809,]        1        3       NA         1         3        NA           1
[3810,]        1       NA       NA         1        NA        NA           1
[3811,]        1        3       NA         1         3        NA           1
[3812,]        1        1        2         1         1         2           1
[3813,]        2        3       NA         2         3        NA           2
[3814,]        1        1       NA         1         1        NA           1
[3815,]        1        1        2         1         1         2           1
[3816,]        2        3       NA         2         3        NA           2
[3817,]        1       NA       NA         1        NA        NA           1
[3818,]        2       NA       NA         2        NA        NA           2
[3819,]        2       NA       NA         2        NA        NA           2
[3820,]        2       NA       NA         2        NA        NA           2
[3821,]        1        1        2         1         1         2           1
[3822,]        1        1        2         1         1         2           1
[3823,]        3       NA       NA         3        NA        NA           3
[3824,]        3       NA       NA         3        NA        NA           3
[3825,]        1       NA       NA         1        NA        NA           1
[3826,]        2        3       NA         2         3        NA           2
[3827,]        1       NA       NA         1        NA        NA           1
[3828,]        1        3       NA         1         3        NA           1
[3829,]        2        3       NA         2         3        NA           2
[3830,]        2       NA       NA         2        NA        NA           2
[3831,]        1        1        2         1         1         2           1
[3832,]        1        3       NA         1         3        NA           1
[3833,]        3       NA       NA         3        NA        NA           3
[3834,]        1       NA       NA         1        NA        NA           1
[3835,]        1        1        2         1         1         2           1
[3836,]        1        2        3         1         2         3           1
[3837,]        1        1        3         1         1         3           1
[3838,]        1       NA       NA         1        NA        NA           1
[3839,]        1       NA       NA         1        NA        NA           1
[3840,]        1        2       NA         1         2        NA           1
[3841,]        1        2       NA         1         2        NA           1
[3842,]        1        2        2         1         2         2           1
[3843,]        1        2        2         1         2         2           1
[3844,]        3        2        3         3         2         3           3
[3845,]        3        2        3         3         2         3           3
[3846,]        3        2        3         3         2         3           3
[3847,]        3        1       NA         3         1        NA           3
[3848,]        3        1       NA         3         1        NA           3
[3849,]        3        2       NA         3         2        NA           3
[3850,]        1       NA       NA         1        NA        NA           1
[3851,]        1       NA       NA         1        NA        NA           1
[3852,]        3       NA       NA         3        NA        NA           3
[3853,]        3       NA       NA         3        NA        NA           3
[3854,]        3       NA       NA         3        NA        NA           3
[3855,]        3       NA       NA         3        NA        NA           3
[3856,]        3       NA       NA         3        NA        NA           3
[3857,]        3       NA       NA         3        NA        NA           3
[3858,]        3       NA       NA         3        NA        NA           3
[3859,]        1       NA       NA         1        NA        NA           1
[3860,]        1        2       NA         1         2        NA           1
[3861,]        1       NA       NA         1        NA        NA           1
[3862,]        1       NA       NA         1        NA        NA           1
[3863,]        1       NA       NA         1        NA        NA           1
[3864,]        1        3        2         1         3         2           1
[3865,]        1        2        3         1         2         3           1
[3866,]        1        3        3         1         3         3           1
[3867,]        1        3        3         1         3         3           1
[3868,]        1        2       NA         1         2        NA           1
[3869,]        1       NA       NA         1        NA        NA           1
[3870,]        1        2        3         1         2         3           1
[3871,]        1        2        3         1         2         3           1
[3872,]        3        2       NA         3         2        NA           3
[3873,]        3        2       NA         3         2        NA           3
[3874,]        3        2       NA         3         2        NA           3
[3875,]        1       NA       NA         1        NA        NA           1
[3876,]        1        3        2         1         3         2           1
[3877,]        2        2        3         2         2         3           2
[3878,]        1        2        3         1         3         3           1
[3879,]        2        2        3         2         2         3           2
[3880,]        1        3        3         1         3         3           1
[3881,]        2        2       NA         2         2        NA           2
[3882,]        2        2        3         2         2         3           2
[3883,]        3        2       NA         3         2        NA           3
[3884,]        1        3        2         1         3         2           1
[3885,]        2       NA       NA         2        NA        NA           2
[3886,]        1        2        3         1         2         3           1
[3887,]        1        2        3         1         2         3           1
[3888,]        1        2       NA         1         2        NA           1
[3889,]        1       NA       NA         1        NA        NA           1
[3890,]        1        2        3         1         2         3           1
[3891,]        1        2       NA         1         2        NA           1
[3892,]        1       NA       NA         1        NA        NA           1
[3893,]        2        2       NA         2         2        NA           2
[3894,]        1       NA       NA         1        NA        NA           1
[3895,]        1        2       NA         1         2        NA           1
[3896,]        1        2       NA         1         2        NA           1
[3897,]        1        2       NA         1         2        NA           1
[3898,]        1        2        3         1         2         3           1
[3899,]        2        3        3         2         3         3           2
[3900,]        1       NA       NA         1        NA        NA           1
[3901,]        2        2       NA         2         2        NA           2
[3902,]        3        2        2         3         2         2           3
[3903,]        1        2       NA         1         2        NA           1
[3904,]        1        2       NA         1         2        NA           1
[3905,]        2       NA       NA         1        NA        NA           2
[3906,]        1       NA       NA         1        NA        NA           1
[3907,]        1        2        2         1         2         2           1
[3908,]        3        2        3         3         2         3           3
[3909,]        3        2        3         3         2         3           3
[3910,]        3        2        3         3         2         3           3
[3911,]        3        2       NA         3         2        NA           3
[3912,]        1        1       NA         1         1        NA           1
[3913,]        1       NA       NA         1        NA        NA           1
[3914,]        3       NA       NA         3        NA        NA           3
[3915,]        3       NA       NA         3        NA        NA           3
[3916,]        3       NA       NA         3        NA        NA           3
[3917,]        3       NA       NA         3        NA        NA           3
[3918,]        3       NA       NA         3        NA        NA           3
[3919,]        3       NA       NA         3        NA        NA           3
[3920,]        3       NA       NA         3        NA        NA           3
[3921,]        1        3       NA         1         2        NA           1
[3922,]        1        2        1         1         2         1           1
[3923,]        1        1       NA         1         1        NA           1
[3924,]        1       NA       NA         1        NA        NA           1
[3925,]        1       NA       NA         1        NA        NA           1
[3926,]        1        2       NA         1         2        NA           1
[3927,]        1        2        2         1         2         2           1
[3928,]        1        2       NA         1         2        NA           1
[3929,]        1        3        3         1         3         3           1
[3930,]        1        2       NA         1         2        NA           1
[3931,]        1        2       NA         1         2        NA           1
[3932,]        1        2       NA         1         2        NA           1
[3933,]        1        3       NA         1         3        NA           1
[3934,]        1       NA       NA         1        NA        NA           1
[3935,]        1       NA       NA         1        NA        NA           1
[3936,]        2       NA       NA         2        NA        NA           2
[3937,]        1       NA       NA         1        NA        NA           1
[3938,]        1        3        2  # create list of models for each plot/size class, extract AIC values + MSE, return sums
       1         3         2           1
[3939,]        1        3        3         1         3         3           1
[3940,]        1        3        3         1         3         3           1
[3941,]        1        2       NA         1         2        NA           1
[3942,]        1       NA       NA         1        NA        NA           1
[3943,]        1        3        3         1         3         3           1
[3944,]        1        3        3         1         3         3           1
[3945,]        3        3       NA         3         3        NA           3
[3946,]        3        3       NA         3         3        NA           3
[3947,]        3        3       NA         3         3        NA           3
[3948,]        1       NA       NA         1        NA        NA           1
[3949,]        1        3        3         1         3         3           1
[3950,]        2        2        3         2         2         3           2
[3951,]        3        3        3         3         3         3           3
[3952,]        1        3        3         1         3         3           1
[3953,]        2        2       NA         2         2        NA           2
[3954,]        3        3        3         3         3         3           3
[3955,]        3        3       NA         3         3        NA           3
[3956,]        1        3        3         1         3         3           1
[3957,]        1        2        2         1         2         2           1
[3958,]        1        3       NA         1         3        NA           1
[3959,]        1        2        3         1         2         3           1
[3960,]        1       NA       NA         1        NA        NA           1
[3961,]        1        2       NA         1         2        NA           1
[3962,]        1       NA       NA         1        NA        NA           1
[3963,]        2        2       NA         2         2        NA           2
[3964,]        1        2       NA         1         2        NA           1
[3965,]        1        2       NA         1         2        NA           1
[3966,]        1       NA       NA         2        NA        NA           1
[3967,]        1        2       NA         1         2        NA           1
[3968,]        1        2        3         1         2         3           1
[3969,]        1       NA       NA         1        NA        NA           1
[3970,]        2       NA       NA         2        NA        NA           2
[3971,]        1        2        3         1         2         3           1
[3972,]        1        2        3         1         2         3           1
[3973,]        1        2       NA         1         2        NA           1
[3974,]        1       NA       NA         1        NA        NA           1
[3975,]        1       NA       NA         1        NA        NA           1
[3976,]        1       NA       NA         1        NA        NA           1
[3977,]        1       NA       NA         1        NA        NA           1
[3978,]        1       NA       NA         1        NA        NA           1
[3979,]        1       NA       NA         1        NA        NA           1
[3980,]        2       NA       NA         2        NA        NA           2
[3981,]        1       NA       NA         1        NA        NA           1
[3982,]        1       NA       NA         1        NA        NA           1
[3983,]        2       NA       NA         2        NA        NA           1
[3984,]        3       NA       NA         3        NA        NA           3
[3985,]        1       NA       NA         1        NA        NA           1
[3986,]        1       NA       NA         1        NA        NA           1
[3987,]        1       NA       NA         1        NA        NA           1
[3988,]        1        2        2         1         2         2           1
[3989,]        2        1       NA         2         1        NA           2
[3990,]        2        3       NA         2         3        NA           2
[3991,]        1       NA       NA         1        NA        NA           1
[3992,]        1        1       NA         1         1        NA           1
[3993,]        1        1       NA         1         1        NA           1
[3994,]        1        1       NA         1         1        NA           1
[3995,]        1        2       NA         1         2        NA           1
[3996,]        1        1       NA         1         1        NA           1
[3997,]        1        1        2         1         1         2           1
[3998,]        1        2        3         1         2         3           1
[3999,]        1       NA       NA         1        NA        NA           1
[4000,]        2        3       NA         2         3        NA           2
[4001,]        1        2        2         1         2         2           1
[4002,]        1        1        2         1         1         2           1
[4003,]        1        1       NA         1         1        NA           1
[4004,]        1        1       NA         1         1        NA           1
[4005,]        1        1       NA         1         1        NA           1
[4006,]        1        1        2         1         1         2           1
[4007,]        1        1        2         1         1         2           1
[4008,]        1        1        2         1         1         2           1
[4009,]        1        1        2         1         1         2           1
[4010,]        1        1        2         1         1         2           1
[4011,]        1        1        2         1         1         2           1
[4012,]        1        1        2         1         1         2           1
[4013,]        1        2        2         2         2         2           1
[4014,]        1        2        2         2         2         2           1
[4015,]        1        2        2         2         2         2           1
[4016,]        1        2        2         2         2         2           1
[4017,]        1        2        2         2         2         2           1
[4018,]        1        2        2         2         2         2           1
[4019,]        1        2        2         2         2         2           1
[4020,]        1        2        2         2         2         2           1
[4021,]        1        2        2         2         2         2           1
[4022,]        1        2        2         2         2         2           1
[4023,]        1        2        2         2         2         2           1
[4024,]        1        2        2         2         2         2           1
[4025,]        1        2        2         2         2         2           1
[4026,]        1        1        2         1         1         2           1
[4027,]        1        1        2         1         1         2           1
[4028,]        1        1        2         1         1         2           1
[4029,]        1        1        2         1         1         2           1
[4030,]        1        1        2         1         1         2           1
[4031,]        1        1        2         1         1         2           1
[4032,]        1        1        2         1         1         2           1
[4033,]        1        1        2         1         1         2           1
[4034,]        1        1        2         1         1         2           1
[4035,]        1        2       NA         1         2        NA           1
[4036,]        1        2       NA         1         2        NA           1
[4037,]        1        1        2         1         1         2           1
[4038,]        1        1        2         1         1         2           1
[4039,]        1        1        2         1         1         2           1
[4040,]        1        1        2         1         1         2           1
[4041,]        1        1        2         1         1         2           1
[4042,]        1        1        2         1         1         2           1
[4043,]        1        1        2         1         1         2           1
[4044,]        1        1        2         1         1         2           1
[4045,]        1        1        2         1         1         2           1
[4046,]        1        1        2         1         1         2           1
[4047,]        1        2       NA         1         2        NA           1
[4048,]        1        2       NA         1         2        NA           1
[4049,]        2       NA       NA         2        NA        NA           2
[4050,]        2       NA       NA         2        NA        NA           2
[4051,]        2       NA       NA         2        NA        NA           2
[4052,]        1       NA       NA         1        NA        NA           1
[4053,]        1        1       NA         1         1        NA           1
[4054,]        1        1        2         1         1         2           1
[4055,]        1        2        2         1         2         2           1
[4056,]        1        1        2         1         1         2           1
[4057,]        1        3       NA         1         3        NA           1
[4058,]        1        1        2         1         1         2           1
[4059,]        1        1        2         1         1         2           1
[4060,]        1        3       NA         1         3        NA           1
[4061,]        1        1        2         1         1         2           1
[4062,]        1        1        2         1         1         2           1
[4063,]        1       NA       NA         1        NA        NA           1
[4064,]        1       NA       NA         1        NA        NA           1
[4065,]        1       NA       NA         1        NA        NA           1
[4066,]        1       NA       NA         1        NA        NA           1
[4067,]        1       NA       NA         1        NA        NA           1
[4068,]        1       NA       NA         1        NA        NA           1
[4069,]        1       NA       NA         1        NA        NA           1
[4070,]        1       NA       NA         2        NA        NA           1
[4071,]        1       NA       NA         2        NA        NA           1
[4072,]        1        2       NA         1         2        NA           1
[4073,]        1        2       NA         1         2        NA           1
[4074,]        1        2       NA         1         2        NA           1
[4075,]        1        2       NA         1         2        NA           1
[4076,]        1        2       NA         1         2        NA           1
[4077,]        1        2       NA         1         2        NA           1
[4078,]        1        2       NA         1         2        NA           1
[4079,]        1        2       NA         1         2        NA           1
[4080,]        1        2       NA         1         2        NA           1
[4081,]        1        2       NA         1         2        NA           1
[4082,]        1        2       NA         1         2        NA           1
[4083,]        1        2       NA         1         2        NA           1
[4084,]        1        2       NA         1         2        NA           1
[4085,]        1        2       NA         1         2        NA           1
[4086,]        2        2       NA         2         2        NA           2
[4087,]        2        2       NA         2         2        NA           2
[4088,]        2        2       NA         2         2        NA           2
[4089,]        2        2       NA         2         2        NA           2
[4090,]        2        2       NA         2         2        NA           2
[4091,]        1        2       NA         1         2        NA           1
[4092,]        1        2       NA         1         2        NA           1
[4093,]        1        2       NA         1         2        NA           1
[4094,]        2       NA       NA         2        NA        NA           2
[4095,]        2       NA       NA         2        NA        NA           2
[4096,]        2       NA       NA         2        NA        NA           2
[4097,]        2       NA       NA         2        NA        NA           2
[4098,]        2       NA       NA         2        NA        NA           2
[4099,]        2       NA       NA         2        NA        NA           2
[4100,]        2       NA       NA         2        NA        NA           2
[4101,]        2       NA       NA         2        NA        NA           2
[4102,]        1        3       NA         1         3        NA           1
[4103,]        1        3       NA         1         3        NA           1
[4104,]        1        3       NA         1         3        NA           1
[4105,]        1        2        2         1         1         2           1
[4106,]        1       NA       NA         1        NA        NA           1
[4107,]        1       NA       NA         1        NA        NA           1
[4108,]        2        2       NA         2         2        NA           2
[4109,]        1        1       NA         1         1        NA           1
[4110,]        2       NA       NA         2        NA        NA           2
[4111,]        1       NA       NA         1        NA        NA           1
[4112,]        2       NA       NA         2        NA        NA           2
[4113,]        1        1        2         1         1         2           1
[4114,]        1        2       NA         1         2        NA           1
[4115,]        1       NA       NA         1        NA        NA           1
[4116,]        1       NA       NA         1        NA        NA           1
[4117,]        1       NA       NA         1        NA        NA           1
[4118,]        1       NA       NA         1        NA        NA           1
[4119,]        1       NA       NA         1        NA        NA           1
[4120,]        1        2       NA         1         2        NA           1
[4121,]        1       NA       NA         1        NA        NA           1
[4122,]        1       NA       NA         1        NA        NA           1
[4123,]        1       NA       NA         1        NA        NA           1
[4124,]        1       NA       NA         1        NA        NA           1
[4125,]        1       NA       NA         1        NA        NA           1
[4126,]        1       NA       NA         1        NA        NA           1
[4127,]        1        3        2         1         3         2           1
[4128,]        1        2       NA         1         2        NA           1
[4129,]        1        3        2         1         3         2           1
[4130,]        1        2       NA         1         2        NA           1
[4131,]        1        3        3         1         3         3           1
[4132,]        3        3       NA         3         3        NA           3
[4133,]        1       NA       NA         1        NA        NA           1
[4134,]        1        2        3         1         2         3           1
[4135,]        1        2       NA         1         2        NA           1
[4136,]        1        2        3         1         2         3           1
[4137,]        1       NA        3         1        NA         3           1
[4138,]        1        1        2         1         1         2           1
[4139,]        3        2       NA         3         2        NA           3
[4140,]        1        2        3         1         2         3           1
[4141,]        1        3        3         1         3         3           1
[4142,]        1        2       NA         1         2        NA           1
[4143,]        1       NA       NA         1        NA        NA           1
[4144,]        2        1        2         2         1         2           1
[4145,]        2        1        2         2         1         2           2
[4146,]        1        1       NA         1         1        NA           1
[4147,]        1       NA       NA         1        NA        NA           1
[4148,]        1        1        3         1         1         3           1
[4149,]        1        2        2         1         2         2           1
[4150,]        1        2       NA         1         2        NA           1
[4151,]        1       NA       NA         1        NA        NA           1
[4152,]        1       NA       NA         1        NA        NA           1
[4153,]        1        2       NA         1         2        NA           1
[4154,]        1       NA       NA         1        NA        NA           1
[4155,]        1       NA       NA         1        NA        NA           1
[4156,]        1       NA       NA         1        NA        NA           1
[4157,]        1       NA       NA         1        NA        NA           1
[4158,]        1        3        2         1         3         2           1
[4159,]        1        2       NA         1         2        NA           1
[4160,]        1        2       NA         1         2        NA           1
[4161,]        1        3        2         1         3         2           1
[4162,]        3        3       NA         3         3        NA           3
[4163,]        1        2        2         1         2         2           1
[4164,]        1        2        2         1         2         2           1
[4165,]        1        1       NA         1         1        NA           1
[4166,]        1       NA       NA         1        NA        NA           1
[4167,]        3        3       NA         3         3        NA           3
[4168,]        1       NA       NA         1        NA        NA           1
[4169,]        2        2        2         2         2         2           1
[4170,]        1       NA       NA         1        NA        NA           1
[4171,]        1       NA       NA         1        NA        NA           1
[4172,]        1        3       NA         1         3        NA           1
[4173,]        3        2        2         3         2         2           3
[4174,]        1        3        3         1         3         3           1
[4175,]        1        3        3         1         3         3           1
[4176,]        1        2       NA         1         2        NA           1
[4177,]        1        3        2         1         3         2           1
[4178,]        1        3        2         1         3         2           1
[4179,]        1       NA       NA         1        NA        NA           1
[4180,]        1        1       NA         1         1        NA           1
[4181,]        1       NA       NA         1        NA        NA           1
[4182,]        1        1        3         1         1         3           1
[4183,]        1       NA       NA         1        NA        NA           1
[4184,]        2       NA       NA         2        NA        NA           2
[4185,]        1        2       NA         1         2        NA           1
[4186,]        1        1        2         1         1         2           1
[4187,]        1       NA       NA         1        NA        NA           1
[4188,]        1       NA       NA         1        NA        NA           1
[4189,]        1        1       NA         1         1        NA           1
[4190,]        1        2       NA         1         2        NA           1
[4191,]        1        2        2         1         2         2           1
[4192,]        1        3        3         1         3         3           1
[4193,]        1        2       NA         1         2        NA           1
[4194,]        1       NA       NA         1        NA        NA           1
[4195,]        1        1        2         1         1         2           1
[4196,]        1        1        2         1         1         2           1
[4197,]        1        2       NA         1         2        NA           1
[4198,]        1        1       NA         1         1        NA           1
[4199,]        1        3        3         1         3         3           1
[4200,]        1       NA       NA         1        NA        NA           1
[4201,]        1       NA       NA         1        NA        NA           1
[4202,]        1       NA       NA         1        NA        NA           1
[4203,]        1       NA       NA         1        NA        NA           1
[4204,]        1       NA       NA         1        NA        NA           1
[4205,]        1       NA       NA         1        NA        NA           1
[4206,]        1       NA       NA         1        NA        NA           1
[4207,]        1        3       NA         1         3        NA           1
[4208,]        1        2        2         1         2         2           1
[4209,]        1        2        2         1         2         2           1
[4210,]        1       NA       NA         1        NA        NA           1
[4211,]        2       NA       NA         2        NA        NA           2
[4212,]        1        2       NA         1         2        NA           1
[4213,]        1        1        2         1         1         2           1
[4214,]        1       NA       NA         1        NA        NA           1
[4215,]        1       NA       NA         1        NA        NA           1
[4216,]        1       NA       NA         1        NA        NA           1
[4217,]        1       NA       NA         1        NA        NA           1
[4218,]        1       NA       NA         1        NA        NA           1
[4219,]        1       NA       NA         1        NA        NA           1
[4220,]        1       NA       NA         1        NA        NA           1
[4221,]        1       NA       NA         1        NA        NA           1
[4222,]        1       NA       NA         1        NA        NA           1
[4223,]        1        1       NA         1         1        NA           1
[4224,]        1        1       NA         1         1        NA           1
[4225,]        1       NA       NA         1        NA        NA           1
[4226,]        1       NA       NA         1        NA        NA           1
[4227,]        1       NA       NA         1        NA        NA           1
[4228,]        1       NA       NA         1        NA        NA           1
[4229,]        1       NA       NA         1        NA        NA           1
[4230,]        1       NA       NA         1        NA        NA           1
[4231,]        1       NA       NA         1        NA        NA           1
[4232,]        1       NA       NA         1        NA        NA           1
[4233,]        1       NA       NA         1        NA        NA           1
[4234,]        1        1        1         1         1         1           1
[4235,]        1        1       NA         1         1        NA           1
[4236,]        1        1       NA         1         1        NA           1
[4237,]        1        1       NA         1         1        NA           1
[4238,]        1        1       NA         1         1        NA           1
[4239,]        1        2       NA         1         2        NA           1
[4240,]        1       NA       NA         1        NA        NA           1
[4241,]        1        1       NA         1         1        NA           1
[4242,]        1        1       NA         1         1        NA           1
[4243,]        1        1       NA         1         1        NA           1
[4244,]        1        1       NA         1         1        NA           1
[4245,]        1       NA       NA         1        NA        NA           1
[4246,]        1        1       NA         1         1        NA           1
[4247,]        1       NA       NA         1        NA        NA           1
[4248,]        3       NA       NA         3        NA        NA           3
[4249,]        1        2       NA         1         2        NA           1
[4250,]        1        2       NA         1         2        NA           1
[4251,]        1        2       NA         1         2        NA           1
[4252,]        1        1        2         1         1         2           1
[4253,]        1        1        2         1         1         2           1
[4254,]        1        2       NA         1         2        NA           1
[4255,]        1        2       NA         1         2        NA           1
[4256,]        1        1        2         1         1         2           1
[4257,]        1        1        2         1         1         2           1
[4258,]        1        2       NA         1         2        NA           1
[4259,]        1        1        1         1         1         1           1
[4260,]        1        1        1         1         1         1           1
[4261,]        1       NA       NA         1        NA        NA           1
[4262,]        1        3       NA         1         3        NA           1
[4263,]        1        1        3         1         1         3           1
[4264,]        1        2       NA         1         2        NA           1
[4265,]        1        3       NA         1         3        NA           1
[4266,]        1       NA       NA         1        NA        NA           1
[4267,]        1        2       NA         1         2        NA           1
[4268,]        1        2        3         1         2         3           1
[4269,]        1        2        3         1         2         3           1
[4270,]        1        2       NA         1         2        NA           1
[4271,]        1        2       NA         1         2        NA           1
[4272,]        1        2        2         1         2         2           1
[4273,]        1        2        2         1         2         2           1
[4274,]        1        2        3         1         2         3           1
[4275,]        1        2        3         1         2         3           1
[4276,]        1        2        2         1         2         2           1
[4277,]        1        1       NA         1         1        NA           1
[4278,]        1        1       NA         1         1        NA           1
[4279,]        1        1        2         1         1         2           1
[4280,]        1       NA       NA         1        NA        NA           1
[4281,]        1       NA       NA         1        NA        NA           1
[4282,]        1        2       NA         1         2        NA           1
[4283,]        1       NA       NA         1        NA        NA           1
[4284,]        2       NA       NA         2        NA        NA           2
[4285,]        1       NA       NA         1        NA        NA           1
[4286,]        1       NA       NA         1        NA        NA           1
[4287,]        2       NA       NA         2        NA        NA           2
[4288,]        1        2       NA         1         2        NA           1
[4289,]        1        1        2         1         1         2           1
[4290,]        1        2        2         1         2         2           1
[4291,]        1        2        2         1         2         2           1
[4292,]        1        1        2         1         1         2           1
[4293,]        3        2       NA         3         2        NA           3
[4294,]        1        2        2         1         2         2           1
[4295,]        1        2        2         1         2         2           1
[4296,]        1        1        2         1         1         2           1
[4297,]        1        2       NA         1         2        NA           1
[4298,]        1        1        2         1         1         2           1
[4299,]        1        2       NA         1         2        NA           1
[4300,]        1        2       NA         1         2        NA           1
[4301,]        1       NA       NA         1        NA        NA           1
[4302,]        1       NA       NA         1        NA        NA           1
[4303,]        1        2       NA         1         2        NA           1
[4304,]        1        2       NA         1         2        NA           1
[4305,]        1        2       NA         1         2        NA           1
[4306,]        1        1        2         1         1         2           1
[4307,]        1        1        2         1         1         2           1
[4308,]        1        2        2         1         2         2           1
[4309,]        1        1        2         1         1         2           1
[4310,]        1        2        3         1         2         3           1
[4311,]        1       NA       NA         1        NA        NA           1
[4312,]        1        2        2         1         2         2           1
[4313,]        1        2       NA         1         2        NA           1
[4314,]        1        2        3         1         2         3           1
[4315,]        1       NA       NA         1        NA        NA           1
[4316,]        1        1       NA         1         1        NA           1
[4317,]        1       NA       NA         1        NA        NA           1
[4318,]        1       NA       NA         1        NA        NA           1
[4319,]        1       NA       NA         1        NA        NA           1
[4320,]        1       NA       NA         1        NA        NA           1
[4321,]        1        2       NA         1         2        NA           1
[4322,]        1        2       NA         1         2        NA           1
[4323,]        1       NA       NA         1        NA        NA           1
[4324,]        1        1        2         1         1         2           1
[4325,]        1        1        2         1         1         2           1
[4326,]        1       NA       NA         1        NA        NA           1
[4327,]        1        1        2         1         1         2           1
[4328,]        1        2       NA         1         2        NA           1
[4329,]        1        2       NA         1         2        NA           1
[4330,]        1        2       NA         1         2        NA           1
[4331,]        1        1        2         1         1         2           1
[4332,]        1       NA       NA         1        NA        NA           1
[4333,]        1       NA       NA         1        NA        NA           1
[4334,]        1        2       NA         1         2        NA           1
[4335,]        1        1        2         1         1         2           1
[4336,]        1        1        2         1         1         2           1
[4337,]        1        1        2         1         1         2           1
[4338,]        1        2        2         1         2         2           1
[4339,]        1        2       NA         1         2        NA           1
[4340,]        1        1       NA         1         1        NA           1
[4341,]        1       NA       NA         1        NA        NA           1
[4342,]        1       NA       NA         1        NA        NA           1
[4343,]        1        1       NA         1         1        NA           1
[4344,]        1        1       NA         1         1        NA           1
[4345,]        1        2       NA         1         2        NA           1
[4346,]        1        2        2         1         2         2           1
[4347,]        1        2        2         1         2         2           1
[4348,]        1        1        3         1         1         3           1
[4349,]        1        1        3         1         1         3           1
[4350,]        1        1        2         1         1         2           1
[4351,]        1        1       NA         1         1        NA           1
[4352,]        1        1       NA         1         1        NA           1
[4353,]        1       NA       NA         1        NA        NA           1
[4354,]        1       NA       NA         1        NA        NA           1
[4355,]        1       NA       NA         1        NA        NA           1
[4356,]        1       NA       NA         1        NA        NA           1
[4357,]        1        2       NA         1         2        NA           1
[4358,]        1        2       NA         1         2        NA           1
[4359,]        1        2       NA         1         2        NA           1
[4360,]        1        2       NA         1         2        NA           1
[4361,]        1        1        2         1         1         2           1
[4362,]        1        1        2         1         1         2           1
[4363,]        1        2       NA         1         2        NA           1
[4364,]        1        2        2         1         2         2           1
[4365,]        1        1       NA         1         1        NA           1
[4366,]        1        1       NA         1         1        NA           1
[4367,]        1        2        3         1         1         3           1
[4368,]        1       NA       NA         1        NA        NA           1
[4369,]        1       NA       NA         1        NA        NA           1
[4370,]        1        3       NA         1         3        NA           1
[4371,]        1       NA       NA         1        NA        NA           1
[4372,]        2       NA       NA         2        NA        NA           2
[4373,]        1       NA       NA         1        NA        NA           1
[4374,]        1       NA       NA         1        NA        NA           1
[4375,]        2       NA       NA         2        NA        NA           2
[4376,]        1        2       NA         1         2        NA           1
[4377,]        1        2        3         1         1         3           1
[4378,]        1        2        3         1         2         3           1
[4379,]        1        1        2         1         1         2           1
[4380,]        3        3       NA         3         3        NA           3
[4381,]        1        2        3         1         2         3           1
[4382,]        1        2        3         1         2         3           1
[4383,]        1        1        3         1         1         3           1
[4384,]        1        2       NA         1         2        NA           1
[4385,]        1       NA       NA         1        NA        NA           1
[4386,]        1        2        2         1         2         2           1
[4387,]        2       NA       NA         2        NA        NA           2
[4388,]        1       NA       NA         1        NA        NA           1
[4389,]        1       NA       NA         1        NA        NA           1
[4390,]        2       NA       NA         2        NA        NA           2
[4391,]        3        2       NA         3         2        NA           3
[4392,]        1        1       NA         1         1        NA           1
[4393,]        1        3       NA         1         3        NA           1
[4394,]        1       NA       NA         1        NA        NA           1
[4395,]        1        3        3         1         3         3           1
[4396,]        1        1        2         1         1         2           1
[4397,]        1        2        2         1         2         2           1
[4398,]        1        2        2         1         2         2           1
[4399,]        1        3        3         1         3         3           1
[4400,]        1        3        3         1         3         3           1
[4401,]        1        2        3         1         2         3           1
[4402,]        1        2        3         1         2         3           1
[4403,]        1        2        3         1         2         3           1
[4404,]        1        1       NA         1         1        NA           1
[4405,]        1        2       NA         1         2        NA           1
[4406,]        1       NA       NA         1        NA        NA           1
[4407,]        1       NA       NA         1        NA        NA           1
[4408,]        1        2        2         1         2         2           1
[4409,]        1       NA       NA         1        NA        NA           1
[4410,]        1        2       NA         1         2        NA           1
[4411,]        1        1        3         1         1         3           1
[4412,]        1        2        3         1         2         3           1
[4413,]        1        2       NA         1         2        NA           1
[4414,]        1        3       NA         1         3        NA           1
[4415,]        1       NA       NA         1        NA        NA           1
[4416,]        1       NA       NA         1        NA        NA           1
[4417,]        1        1        3         1         1         3           1
[4418,]        1        1       NA         1         1        NA           1
[4419,]        1       NA       NA         1        NA        NA           1
[4420,]        1        1       NA         1         1        NA           1
[4421,]        1       NA       NA         1        NA        NA           1
[4422,]        1        2       NA         1         2        NA           1
[4423,]        1        3        3         1         3         3           1
[4424,]        1        3       NA         1         3        NA           1
[4425,]        1        2        2         1         2         2           1
[4426,]        1        2       NA         1         2        NA           1
[4427,]        1        2       NA         1         2        NA           1
[4428,]        1        2        3         1         2         3           1
[4429,]        1       NA       NA         1        NA        NA           1
[4430,]        1       NA       NA         1        NA        NA           1
[4431,]        1        3       NA         1         3        NA           1
[4432,]        1       NA       NA         1        NA        NA           1
[4433,]        3       NA       NA         3        NA        NA           3
[4434,]        1       NA       NA         1        NA        NA           1
[4435,]        1       NA       NA         1        NA        NA           1
[4436,]        3       NA       NA         3        NA        NA           3
[4437,]        1        3       NA         1         3        NA           1
[4438,]        1        2        3         1         2         3           1
[4439,]        1        3        3         1         3         3           1
[4440,]        1        2        3         1         2         3           1
[4441,]        1        2        3         1         2         3           1
[4442,]        3        3       NA         3         3        NA           3
[4443,]        1        3        3         1         3         3           1
[4444,]        1        3        3         1         3         3           1
[4445,]        1        2        3         1         2         3           1
[4446,]        1        3       NA         1         3        NA           1
[4447,]        1        2        2         1         2         2           1
[4448,]        2       NA       NA         2        NA        NA           2
[4449,]        1       NA       NA         1        NA        NA           1
[4450,]        2       NA       NA         2        NA        NA           1
[4451,]        3       NA       NA         3        NA        NA           3
[4452,]        3        2       NA         3         2        NA           3
[4453,]        1        3       NA         1         3        NA           1
[4454,]        1       NA       NA         1        NA        NA           1
[4455,]        1        3        3         1         3         3           1
[4456,]        1        2        3         1         2         3           1
[4457,]        1        2        3         1         2         3           1
[4458,]        1        2        3         1         2         3           1
[4459,]        1        2        3         1         2         3           1
[4460,]        1        3        3         1         3         3           1
[4461,]        1        3        3         1         3         3           1
[4462,]        3        3       NA         3         3        NA           3
[4463,]        1       NA       NA         1        NA        NA           1
[4464,]        1        2        3         1         2         3           1
[4465,]        1        2        2         1         2         2           1
[4466,]        1        2        3         1         2         2           1
[4467,]        1        3        3         1         3         3           1
[4468,]        1        3        3         1         3         3           1
[4469,]        1        2        3         1         2         3           1
[4470,]        1        2        3         1         2         3           1
[4471,]        1        2        3         1         2         3           1
[4472,]        1        2       NA         1         2        NA           1
[4473,]        1        2       NA         1         2        NA           1
[4474,]        1       NA       NA         1        NA        NA           1
[4475,]        1       NA       NA         1        NA        NA           1
[4476,]        1        1       NA         1         1        NA           1
[4477,]        1       NA       NA         1        NA        NA           1
[4478,]        1       NA       NA         1        NA        NA           1
[4479,]        1       NA       NA         1        NA        NA           1
[4480,]        1       NA       NA         1        NA        NA           1
[4481,]        1        2        2         1         2         2           1
[4482,]        1        1        2         1         1         3           1
[4483,]        1        1        2         1         1         3           1
[4484,]        1        1       NA         1         1        NA           1
[4485,]        1       NA       NA         1        NA        NA           1
[4486,]        2        2       NA         2         2        NA           2
[4487,]        1       NA       NA         1        NA        NA           1
[4488,]        1       NA       NA         1        NA        NA           1
[4489,]        1        2       NA         1         2        NA           1
[4490,]        1       NA       NA         1        NA        NA           1
[4491,]        1       NA       NA         1        NA        NA           1
[4492,]        1       NA       NA         1        NA        NA           1
[4493,]        1       NA       NA         1        NA        NA           1
[4494,]        1       NA       NA         1        NA        NA           1
[4495,]        2        2        2         2         2         2           2
[4496,]        1        2       NA         1         2        NA           1
[4497,]        1        2        2         1         2         2           1
[4498,]        1        2        2         1         3         2           1
[4499,]        1        2        3         1         2         3           1
[4500,]        1       NA       NA         1        NA        NA           1
[4501,]        1        2        2         1         2         2           1
[4502,]        1       NA       NA         1        NA        NA           1
[4503,]        1       NA       NA         1        NA        NA           1
[4504,]        1        2       NA         1         2        NA           1
[4505,]        1        2       NA         1         2        NA           1
[4506,]        1       NA       NA         1        NA        NA           1
[4507,]        1       NA       NA         1        NA        NA           1
[4508,]        1        2       NA         1         2        NA           1
[4509,]        2       NA       NA         2        NA        NA           2
[4510,]        1        2        2         1         2         3           1
[4511,]        1        2        2         1         2         2           1
[4512,]        1        2       NA         1         2        NA           1
[4513,]        1       NA       NA         1        NA        NA           1
[4514,]        3       NA       NA         3        NA        NA           3
[4515,]        1       NA       NA         1        NA        NA           1
[4516,]        1       NA       NA         1        NA        NA           1
[4517,]        1       NA       NA         1        NA        NA           1
[4518,]        2        3       NA         2         3        NA           2
[4519,]        1        3       NA         1         3        NA           1
[4520,]        1        2        2         1         2         3           1
[4521,]        1        3        3         1         3         3           1
[4522,]        1        3        3         1         3         3           1
[4523,]        1        3       NA         1         3        NA           1
[4524,]        2        3       NA         2         3        NA           2
[4525,]        3        3        3         3         3         3           3
[4526,]        1       NA       NA         1        NA        NA           1
[4527,]        1        2       NA         1         2        NA           1
[4528,]        1       NA       NA         1        NA        NA           1
[4529,]        1       NA       NA         1        NA        NA           1
[4530,]        1        2       NA         1         2        NA           1
[4531,]        1        2       NA         1         2        NA           1
[4532,]        1        1        3         1         1         3           1
[4533,]        3        3        3         3         3         3           3
[4534,]        2        3       NA         2         3        NA           2
[4535,]        1       NA       NA         1        NA        NA           1
[4536,]        1       NA       NA         1        NA        NA           1
[4537,]        1       NA       NA         1        NA        NA           1
[4538,]        1       NA       NA         1        NA        NA           1
[4539,]        1       NA       NA         1        NA        NA           1
[4540,]        1       NA       NA         1        NA        NA           1
[4541,]        1        1        2         1         1         2           1
[4542,]        2        2        2         2         2         2           2
[4543,]        1        1        2         1         1         2           1
[4544,]        1        2        2         1         3         2           1
[4545,]        1        2        2         1         2         2           1
[4546,]        1       NA       NA         1        NA        NA           1
[4547,]        1        1       NA         1         1        NA           1
[4548,]        1       NA       NA         1        NA        NA           1
[4549,]        1       NA       NA         1        NA        NA           1
[4550,]        1       NA       NA         1        NA        NA           1
[4551,]        1       NA       NA         1        NA        NA           1
[4552,]        1       NA       NA         1        NA        NA           1
[4553,]        1       NA       NA         1        NA        NA           1
[4554,]        1       NA       NA         1        NA        NA           1
[4555,]        1       NA       NA         1        NA        NA           1
[4556,]        1       NA       NA         1        NA        NA           1
[4557,]        1       NA       NA         1        NA        NA           1
[4558,]        1        2       NA         1         2        NA           1
[4559,]        1        2       NA         1         2        NA           1
[4560,]        1        3       NA         1         3        NA           1
[4561,]        1        2       NA         1         2        NA           1
[4562,]        1        3        2         1         3         2           1
[4563,]        1        3        2         1         3         2           1
[4564,]        1        3        2         1         3         2           1
[4565,]        1        3        2         1         3         2           1
[4566,]        1        3        2         1         3         2           1
[4567,]        1        3        2         1         3         2           1
[4568,]        1        3        2         1         3         2           1
[4569,]        1        3        2         1         3         2           1
[4570,]        1        2        2         1         3         2           1
[4571,]        3        2        2         3         2         3           3
[4572,]        3        2        2         3         2         3           3
[4573,]        3        2        2         3         2         3           3
[4574,]        3        2        2         3         2         3           3
[4575,]        3        2        2         3         2         3           3
[4576,]        3        2        2         3         2         3           3
[4577,]        3        2        2         3         2         3           3
[4578,]        3        2        2         3         2         3           3
[4579,]        1        3       NA         1         3        NA           1
[4580,]        1        3       NA         1         3        NA           1
[4581,]        3        2        2         3         2         3           3
[4582,]        1        2        3         1         3         3           1
[4583,]        1        2        3         1         3         3           1
[4584,]        1        2        2         1         3         2           1
[4585,]        1        2       NA         1         2        NA           1
[4586,]        1        2        3         1         3         3           1
[4587,]        1        3       NA         1         3        NA           1
[4588,]        1        3       NA         1         3        NA           1
[4589,]        1        3       NA         1         3        NA           1
[4590,]        3        2        2         3         2         3           3
[4591,]        1        3       NA         1         3        NA           1
[4592,]        1        3       NA         1         3        NA           1
[4593,]        1        3       NA         1         3        NA           1
[4594,]        1        2       NA         1         2        NA           1
[4595,]        1        2       NA         1         2        NA           1
[4596,]        1        2        3         1         3         3           1
[4597,]        1        2       NA         1         2        NA           1
[4598,]        1        3        2         1         3         2           1
[4599,]        1       NA       NA         1        NA        NA           1
[4600,]        1        1       NA         1         2        NA           1
[4601,]        1        3        2         1         3         2           1
[4602,]        1       NA       NA         1        NA        NA           1
[4603,]        1        2        2         1         3         2           1
[4604,]        1        2        2         1         3         2           1
[4605,]        1        3        2         1         3         2           1
[4606,]        1        3        2         1         3         2           1
[4607,]        1        1        2         1         1         2           1
[4608,]        1        1       NA         1         1        NA           1
[4609,]        1        1        2         1         1         2           1
[4610,]        1        1        2         1         1         2           1
[4611,]        1        1        2         1         1         2           1
[4612,]        1        1        2         1         1         2           1
[4613,]        1        1        2         1         1         2           1
[4614,]        1        1        2         1         1         2           1
[4615,]        1        1        2         1         1         2           1
[4616,]        1        1        2         1         1         2           1
[4617,]        1        1        2         1         1         2           1
[4618,]        1        1        2         1         1         2           1
[4619,]        1        1        2         1         1         2           1
[4620,]        1        1        2         1         1         2           1
[4621,]        1        1        2         1         1         2           1
[4622,]        1        3        2         1         3         2           1
[4623,]        1        3        2         1         3         3           1
[4624,]        1        3        2         1         3         3           1
[4625,]        1        3        2         1         3         3           1
[4626,]        1        3        2         1         3         3           1
[4627,]        1        3        2         1         3         3           1
[4628,]        1        3        2         1         3         2           1
[4629,]        1        3        2         1         3         3           1
[4630,]        1        3        2         1         3         3           1
[4631,]        1        3        2         1         3         3           1
[4632,]        1        3        2         1         3         3           1
[4633,]        1        3        2         1         3         3           1
[4634,]        1        1        2         1         1         2           1
[4635,]        1        1        2         1         1         2           1
[4636,]        1        3        2         1         3         3           1
[4637,]        1        3        2         1         3         3           1
[4638,]        1        1        2         1         1         3           1
[4639,]        1        1        2         1         1         3           1
[4640,]        1        1        2         1         1         3           1
[4641,]        1        1        2         1         1         3           1
[4642,]        1        1        2         1         1         3           1
[4643,]        1        1        2         1         1         3           1
[4644,]        1        1        2         1         1         3           1
[4645,]        1        1        2         1         1         3           1
[4646,]        1        1        2         1         1         3           1
[4647,]        1        1        2         1         1         3           1
[4648,]        1        1        2         1         1         3           1
[4649,]        1        1        2         1         1         2           1
[4650,]        1        1        2         1         1         2           1
[4651,]        1        3        2         1         3         3           1
[4652,]        1        1        2         1         1         2           1
[4653,]        1        1        2         1         1         3           1
[4654,]        1        1        2         1         1         3           1
[4655,]        1        1        2         1         1         2           1
[4656,]        1        1        2         1         1         2           1
[4657,]        1        1        2         1         1         2           1
[4658,]        1        1        2         1         1         2           1
[4659,]        1        1        2         1         1         2           1
[4660,]        1        1        2         1         1         2           1
[4661,]        1        3        2         1         3         2           1
[4662,]        1        3        2         1         3         3           1
[4663,]        1        2       NA         1         2        NA           1
[4664,]        1        2        2         1         3         2           1
[4665,]        1        2        2         1         3         2           1
[4666,]        1        1       NA         1         1        NA           1
[4667,]        1        2        2         1         3         2           1
[4668,]        1        2        2         1         3         2           1
[4669,]        1       NA       NA         1        NA        NA           1
[4670,]        1        2        2         1         3         2           1
[4671,]        1        2        2         1         3         2           1
[4672,]        1        2        2         1         3         2           1
[4673,]        1        1        2         1         1         2           1
[4674,]        2        2        3         2         2         3           2
[4675,]        1        1        3         1         1         3           1
[4676,]        1        2        2         1         3         2           1
[4677,]        1        1       NA         1         1        NA           1
[4678,]        1        1       NA         1         1        NA           1
[4679,]        2        2        2         1         1         2           2
[4680,]        2        2        2         1         1         2           2
[4681,]        2        2        2         1         1         2           2
[4682,]        2        2        2         1         1         2           2
[4683,]        1        1       NA         1         1        NA           1
[4684,]        1        1       NA         1         1        NA           1
[4685,]        1        1       NA         1         1        NA           1
[4686,]        1        2        2         1         2         2           1
[4687,]        2       NA       NA         2        NA        NA           2
[4688,]        2       NA       NA         2        NA        NA           2
[4689,]        2       NA       NA         2        NA        NA           2
[4690,]        2       NA       NA         2        NA        NA           2
[4691,]        1       NA       NA         1        NA        NA           1
[4692,]        2       NA       NA         2        NA        NA           2
[4693,]        1        2       NA         1         2        NA           1
[4694,]        1        2       NA         1         2        NA           1
[4695,]        1        2       NA         1         2        NA           1
[4696,]        1        2       NA         1         2        NA           1
[4697,]        1        2       NA         1         2        NA           1
[4698,]        2       NA       NA         2        NA        NA           2
[4699,]        1        3       NA         1         3        NA           1
[4700,]        1       NA       NA         1        NA        NA           1
[4701,]        1        2       NA         1         2        NA           1
[4702,]        2       NA       NA         2        NA        NA           1
[4703,]        2       NA       NA         2        NA        NA           2
[4704,]        1       NA       NA         1        NA        NA           1
[4705,]        3        3        3         3         3         3           3
[4706,]        1        3        3         1         3         3           1
[4707,]        1        3        3         1         3         3           1
[4708,]        1        2       NA         1         2        NA           1
[4709,]        1        2       NA         1         2        NA           1
[4710,]        1        3        3         1         3         3           1
[4711,]        1       NA       NA         1        NA        NA           1
[4712,]        1        2       NA         1         2        NA           1
[4713,]        3        3        3         3         3         3           3
[4714,]        1        3        3         1         3         3           1
[4715,]        1        3        3         1         3         3           1
[4716,]        3        3        3         3         3         3           3
[4717,]        3        3        3         3         3         3           3
[4718,]        1        3       NA         1         3        NA           1
[4719,]        1        3        3         1         3         3           1
[4720,]        1        3        3         1         3         3           1
[4721,]        1       NA       NA         1        NA        NA           1
[4722,]        1       NA       NA         1        NA        NA           1
[4723,]        3        3        3         3         3         3           3
[4724,]        1        2       NA         1         2        NA           1
[4725,]        1       NA       NA         1        NA        NA           1
[4726,]        1       NA       NA         1        NA        NA           1
[4727,]        1        2       NA         1         2        NA           1
[4728,]        1        3       NA         1         3        NA           1
[4729,]        1        3        3         1         3         3           1
[4730,]        1        3        3         1         3         3           1
[4731,]        1        2       NA         1         2        NA           1
[4732,]        1        3        3         1         3         3           1
[4733,]        3        3        3         3         3         3           3
[4734,]        1        3        3         1         3         3           1
[4735,]        1        3       NA         1         3        NA           1
[4736,]        1        3       NA         1         3        NA           1
[4737,]        1       NA       NA         1        NA        NA           1
[4738,]        1        2       NA         1         2        NA           1
[4739,]        1        3        2         1         3         2           1
[4740,]        1        2       NA         1         2        NA           1
[4741,]        1       NA       NA         1        NA        NA           1
[4742,]        1        2       NA         1         2        NA           1
[4743,]        1        3        2         1         3         2           1
[4744,]        1        2       NA         1         2        NA           1
[4745,]        1        3        2         1         3         2           1
[4746,]        1        2       NA         1         2        NA           1
[4747,]        1        2        3         1         1         3           1
[4748,]        1        3        3         1         3         3           1
[4749,]        1        3        3         1         3         3           1
[4750,]        1        2        3         1         2         3           1
[4751,]        1        3        3         1         3         3           1
[4752,]        1        3        3         1         3         3           1
[4753,]        1        3        3         1         3         3           1
[4754,]        1        3        3         1         3         3           1
[4755,]        1        3        3         1         3         3           1
[4756,]        1        2        3         1         1         3           1
[4757,]        1        2        3         1         2         3           1
[4758,]        1        2        3         1         2         3           1
[4759,]        1        2        3         1         1         3           1
[4760,]        1        2        3         1         1         3           1
[4761,]        1        2        3         1         1         3           1
[4762,]        1        2        3         1         1         3           1
[4763,]        1        2        3         1         1         3           1
[4764,]        1        2        3         1         1         3           1
[4765,]        1        3        3         1         3         3           1
[4766,]        1        3        3         1         3         3           1
[4767,]        1        3        3         1         3         3           1
[4768,]        2       NA       NA         2        NA        NA           2
[4769,]        2       NA       NA         2        NA        NA           2
[4770,]        1        3       NA         1         3        NA           1
[4771,]        1        3        3         1         3         3           1
[4772,]        1        3        3         1         3         3           1
[4773,]        1        3        3         1         3         3           1
[4774,]        1        3        3         1         3         3           1
[4775,]        1        3        3         1         3         3           1
[4776,]        1        3        3         1         3         3           1
[4777,]        1        3        3         1         3         3           1
[4778,]        3        3        3         3         3         3           3
[4779,]        3        3        3         3         3         3           3
[4780,]        3        3        3         3         3         3           3
[4781,]        3        3        3         3         3         3           3
[4782,]        3        3        3         3         3         3           3
[4783,]        1        3       NA         1         3        NA           1
[4784,]        3        3        3         3         3         3           3
[4785,]        1        3        3         1         3         3           1
[4786,]        1        3        3         1         3         3           1
[4787,]        1        3        3         1         3         3           1
[4788,]        1        3       NA         1         3        NA           1
[4789,]        1        3       NA         1         3        NA           1
[4790,]        1        3       NA         1         3        NA           1
[4791,]        1        3       NA         1         3        NA           1
[4792,]        1        3       NA         1         3        NA           1
[4793,]        1        3       NA         1         3        NA           1
[4794,]        1        3       NA         1         3        NA           1
[4795,]        1        3       NA         1         3        NA           1
[4796,]        1        3        3         1         3         3           1
[4797,]        1        3       NA         1         3        NA           1
[4798,]        1        3        2         1         3         2           1
[4799,]        1       NA       NA         1        NA        NA           1
[4800,]        1        3        2         1         3         2           1
[4801,]        1        3        3         1         3         3           1
[4802,]        1        3        3         1         3         3           1
[4803,]        1        1       NA         1         1        NA           1
[4804,]        2        2        2         1         1         2           2
[4805,]        2        2        2         1         1         2           2
[4806,]        2        2        2         1         1         2           2
[4807,]        1        2        2         1         1         2           1
[4808,]        2        2        2         1         1         2           2
[4809,]        1        2        3         1         1         3           1
[4810,]        1        2        3         1         1         3           1
[4811,]        1        2        3         1         1         3           1
[4812,]        1        2        3         1         1         3           1
[4813,]        1        2        3         1         1         3           1
[4814,]        1        3        3         1         3         3           1
[4815,]        1        3        3         1         3         3           1
[4816,]        1        3        3         1         3         3           1
[4817,]        1        3        3         1         3         3           1
[4818,]        1        3        3         1         3         3           1
[4819,]        1        3        3         1         3         3           1
[4820,]        1        3        3         1         3         3           1
[4821,]        1        3        3         1         3         3           1
[4822,]        1        3        3         1         3         3           1
[4823,]        1        3        3         1         3         3           1
[4824,]        1        2        3         1         2         3           1
[4825,]        1        2        3         1         2         3           1
[4826,]        1        2        3         1         2         3           1
[4827,]        1        2        3         1         2         3           1
[4828,]        1        2        3         1         2         3           1
[4829,]        1        2        3         1         2         3           1
[4830,]        1        2        3         1         1         3           1
[4831,]        1        3        3         1         3         3           1
[4832,]        1        2        3         1         2         3           1
[4833,]        1        2        3         1         2         3           1
[4834,]        1        2        3         1         1         3           1
[4835,]        1        2        3         1         1         3           1
[4836,]        1        2        3         1         1         3           1
[4837,]        1        2        3         1         1         3           1
[4838,]        1        3        3         1         3         3           1
[4839,]        1        3       NA         1         3        NA           1
[4840,]        1        3        3         1         3         3           1
[4841,]        1        3        3         1         3         3           1
[4842,]        1        2       NA         1         2        NA           1
[4843,]        1        3        3         1         3         3           1
[4844,]        1        3        3         1         3         3           1
[4845,]        1        3        3         1         3         3           1
[4846,]        1        3        3         1         3         3           1
[4847,]        1       NA       NA         1        NA        NA           1
[4848,]        1       NA       NA         1        NA        NA           1
[4849,]        1       NA       NA         1        NA        NA           1
[4850,]        1       NA       NA         1        NA        NA           1
[4851,]        1       NA       NA         1        NA        NA           1
[4852,]        1       NA       NA         1        NA        NA           1
[4853,]        1       NA       NA         1        NA        NA           1
[4854,]        1       NA       NA         1        NA        NA           1
[4855,]        1       NA       NA         1        NA        NA           1
[4856,]        1       NA       NA         1        NA        NA           1
[4857,]        1       NA       NA         1        NA        NA           1
[4858,]        1       NA       NA         1        NA        NA           1
[4859,]        1       NA       NA         1        NA        NA           1
[4860,]        1       NA       NA         1        NA        NA           1
[4861,]        1       NA       NA         1        NA        NA           1
[4862,]        1       NA       NA         1        NA        NA           1
[4863,]        1       NA       NA         1        NA        NA           1
[4864,]        1       NA       NA         1        NA        NA           1
[4865,]        1       NA       NA         1        NA        NA           1
[4866,]        1       NA       NA         1        NA        NA           1
[4867,]        1       NA       NA         1        NA        NA           1
[4868,]        1       NA       NA         1        NA        NA           1
[4869,]        1        1       NA         1         1        NA           1
[4870,]        1        1       NA         1         1        NA           1
[4871,]        1        1       NA         1         1        NA           1
[4872,]        1       NA       NA         1        NA        NA           1
[4873,]        1       NA       NA         1        NA        NA           1
[4874,]        1        1       NA         1         1        NA           1
[4875,]        1       NA       NA         1        NA        NA           1
[4876,]        1       NA       NA         1        NA        NA           1
[4877,]        1       NA       NA         1        NA        NA           1
[4878,]        1       NA       NA         1        NA        NA           1
[4879,]        1       NA       NA         1        NA        NA           1
[4880,]        1       NA       NA         1        NA        NA           1
[4881,]        1       NA       NA         1        NA        NA           1
[4882,]        1       NA       NA         1        NA        NA           1
[4883,]        1       NA       NA         1        NA        NA           1
[4884,]        1       NA       NA         1        NA        NA           1
[4885,]        1       NA       NA         1        NA        NA           1
[4886,]        1       NA       NA         1        NA        NA           1
[4887,]        1       NA       NA         1        NA        NA           1
[4888,]        1       NA       NA         1        NA        NA           1
[4889,]        1       NA       NA         1        NA        NA           1
[4890,]        1       NA       NA         1        NA        NA           1
[4891,]        1        2        1         1         2         2           1
[4892,]        1        2        2         1         2         2           1
[4893,]        1       NA       NA         1        NA        NA           1
[4894,]        1       NA       NA         1        NA        NA           1
[4895,]        1       NA       NA         1        NA        NA           1
[4896,]        1       NA       NA         1        NA        NA           1
[4897,]        1       NA       NA         1        NA        NA           1
[4898,]        1       NA       NA         1        NA        NA           1
[4899,]        1       NA       NA         1        NA        NA           1
[4900,]        1       NA       NA         1        NA        NA           1
[4901,]        1       NA       NA         1        NA        NA           1
[4902,]        1       NA       NA         1        NA        NA           1
[4903,]        1       NA       NA         1        NA        NA           1
[4904,]        1       NA       NA         1        NA        NA           1
[4905,]        1       NA       NA         1        NA        NA           1
[4906,]        1       NA       NA         1        NA        NA           1
[4907,]        1       NA       NA         1        NA        NA           1
[4908,]        1       NA       NA         1        NA        NA           1
[4909,]        1       NA       NA         1        NA        NA           1
[4910,]        1       NA       NA         1        NA        NA           1
[4911,]        1       NA       NA         1        NA        NA           1
[4912,]        1       NA       NA         1        NA        NA           1
[4913,]        1       NA       NA         1        NA        NA           1
[4914,]        1        1       NA         1         1        NA           1
[4915,]        1        1       NA         1         1        NA           1
[4916,]        1        1       NA         1         1        NA           1
[4917,]        1       NA       NA         1        NA        NA           1
[4918,]        1       NA       NA         1        NA        NA           1
[4919,]        1       NA       NA         1        NA        NA           1
[4920,]        1       NA       NA         1        NA        NA           1
[4921,]        1       NA       NA         1        NA        NA           1
[4922,]        1       NA       NA         1        NA        NA           1
[4923,]        1       NA       NA         1        NA        NA           1
[4924,]        1       NA       NA         1        NA        NA           1
[4925,]        1       NA       NA         1        NA        NA           1
[4926,]        1       NA       NA         1        NA        NA           1
[4927,]        1       NA       NA         1        NA        NA           1
[4928,]        1       NA       NA         1        NA        NA           1
[4929,]        1       NA       NA         1        NA        NA           1
[4930,]        1       NA       NA         1        NA        NA           1
[4931,]        1       NA       NA         1        NA        NA           1
[4932,]        1       NA       NA         1        NA        NA           1
[4933,]        1       NA       NA         1        NA        NA           1
[4934,]        1       NA       NA         1        NA        NA           1
[4935,]        1       NA       NA         1        NA        NA           1
[4936,]        1       NA       NA         1        NA        NA           1
[4937,]        1       NA       NA         1        NA        NA           1
[4938,]        1       NA       NA         1        NA        NA           1
[4939,]        1       NA       NA         1        NA        NA           1
[4940,]        1       NA       NA         1        NA        NA           1
[4941,]        1       NA       NA         1        NA        NA           1
[4942,]        1       NA       NA         1        NA        NA           1
[4943,]        1       NA       NA         1        NA        NA           1
[4944,]        1       NA       NA         1        NA        NA           1
[4945,]        1       NA       NA         1        NA        NA           1
[4946,]        1       NA       NA         1        NA        NA           1
[4947,]        1       NA       NA         1        NA        NA           1
[4948,]        1       NA       NA         1        NA        NA           1
[4949,]        1       NA       NA         1        NA        NA           1
[4950,]        1       NA       NA         1        NA        NA           1
[4951,]        1       NA       NA         1        NA        NA           1
[4952,]        1       NA       NA         1        NA        NA           1
[4953,]        1        1       NA         1         1        NA           1
[4954,]        1        1        2         1         1         2           1
[4955,]        1        1        2         1         1         2           1
[4956,]        1        1        2         1         1         2           1
[4957,]        1        1        2         1         1         2           1
[4958,]        1        1        2         1         1         2           1
[4959,]        1        1        1         1         1         1           1
[4960,]        1        1        1         1         1         1           1
[4961,]        1        1        1         1         1         1           1
[4962,]        1        1        2         1         1         2           1
[4963,]        1        1        1         1         1         1           1
[4964,]        1        1        2         1         1         2           1
[4965,]        1        1        2         1         1         2           1
[4966,]        1        1        2         1         1         2           1
[4967,]        1        1        2         1         1         2           1
[4968,]        1        1        2         1         1         2           1
[4969,]        1        1        2         1         1         2           1
[4970,]        1        1        2         1         1         2           1
[4971,]        1        1        2         1         1         2           1
[4972,]        1        1        2         1         1         2           1
[4973,]        1        1        2         1         1         2           1
[4974,]        1        1        2         1         1         2           1
[4975,]        1        1       NA         1         1        NA           1
[4976,]        1        1       NA         1         1        NA           1
[4977,]        1        1       NA         1         1        NA           1
[4978,]        1       NA       NA         1        NA        NA           1
[4979,]        1       NA       NA         1        NA        NA           1
[4980,]        1       NA       NA         1        NA        NA           1
[4981,]        1       NA       NA         1        NA        NA           1
[4982,]        1       NA       NA         1        NA        NA           1
[4983,]        1       NA       NA         1        NA        NA           1
[4984,]        1       NA       NA         1        NA        NA           1
[4985,]        1        1       NA         1         1        NA           1
[4986,]        1       NA       NA         1        NA        NA           1
[4987,]        1       NA       NA         1        NA        NA           1
[4988,]        1       NA       NA         1        NA        NA           1
[4989,]        1       NA       NA         1        NA        NA           1
[4990,]        1       NA       NA         1        NA        NA           1
[4991,]        1       NA       NA         1        NA        NA           1
[4992,]        1       NA       NA         1        NA        NA           1
[4993,]        1       NA       NA         1        NA        NA           1
[4994,]        1        1        1         1         1         1           1
[4995,]        1        1        1         1         1         1           1
[4996,]        1        1        1         1         1         1           1
[4997,]        1        1        1         1         1         1           1
[4998,]        1        1        1         1         1         1           1
[4999,]        1        1        1         1         1         1           1
[5000,]        1        1        1         1         1         1           1
[5001,]        1        1        1         1         1         1           1
[5002,]        1        1        1         1         1         1           1
[5003,]        1        1        1         1         1         1           1
[5004,]        1        1        1         1         1         1           1
[5005,]        1        1        1         1         1         1           1
[5006,]        1        1       NA         1         1        NA           1
[5007,]        1        1       NA         1         1        NA           1
[5008,]        1        1       NA         1         1        NA           1
[5009,]        1        1        1         1         1         1           1
[5010,]        1        1        1         1         1         1           1
[5011,]        1        1        1         1         1         1           1
[5012,]        1        1        1         1         1         1           1
[5013,]        1        1        1         1         1         1           1
[5014,]        1        1        1         1         1         1           1
[5015,]        1        1        1         1         1         1           1
[5016,]        1        1        1         1         1         1           1
[5017,]        1        1        1         1         1         1           1
[5018,]        1        1       NA         1         1        NA           1
[5019,]        1        1        1         1         1         1           1
[5020,]        1        1       NA         1         1        NA           1
[5021,]        1        1        1         1         1         1           1
[5022,]        1        1       NA         1         1        NA           1
[5023,]        1        1        1         1         1         1           1
[5024,]        1        1        1         1         1         1           1
[5025,]        1        1        1         1         1         1           1
[5026,]        1        1        1         1         1         1           1
[5027,]        1        1        1         1         1         1           1
[5028,]        1       NA       NA         1        NA        NA           1
[5029,]        1        1        1         1         1         1           1
[5030,]        1        1       NA         1         1        NA           1
[5031,]        1       NA       NA         1        NA        NA           1
[5032,]        1        1        2         1         1         2           1
[5033,]        1        2        2         1         2         2           1
[5034,]        1        1        2         1         1         2           1
[5035,]        1        1        2         1         1         2           1
[5036,]        1        1       NA         1         1        NA           1
[5037,]        1        1       NA         1         1        NA           1
[5038,]        1        1       NA         1         1        NA           1
[5039,]        1        1       NA         1         1        NA           1
[5040,]        1       NA       NA         1        NA        NA           1
[5041,]        1       NA       NA         1        NA        NA           1
[5042,]        1       NA       NA         1        NA        NA           1
[5043,]        1       NA       NA         1        NA        NA           1
[5044,]        1        2        1         1         2         2           1
[5045,]        1        2        2         1         2         2           1
[5046,]        1        1        2         1         1         2           1
[5047,]        1        1        2         1         1         2           1
[5048,]        1        1        2         1         1         2           1
[5049,]        1        1        2         1         1         2           1
[5050,]        1        1       NA         1         1        NA           1
[5051,]        1        1       NA         1         1        NA           1
[5052,]        1        1       NA         1         1        NA           1
[5053,]        1        1        2         1         1         2           1
[5054,]        1        1       NA         1         1        NA           1
[5055,]        1       NA       NA         1        NA        NA           1
[5056,]        1       NA       NA         1        NA        NA           1
[5057,]        1       NA       NA         1        NA        NA           1
[5058,]        1        1       NA         1         1        NA           1
[5059,]        1        1        2         1         1         2           1
[5060,]        1        1       NA         1         1        NA           1
[5061,]        1        1       NA         1         1        NA           1
[5062,]        1        1       NA         1         1        NA           1
[5063,]        1        1        2         1         1         2           1
[5064,]        1        1        2         1         1         2           1
[5065,]        1        1        2         1         1         2           1
[5066,]        1       NA       NA         1        NA        NA           1
[5067,]        1       NA       NA         1        NA        NA           1
[5068,]        1        2        1         1         2         2           1
[5069,]        2        2        2         2         3         2           2
[5070,]        1        2        2         1         3         3           1
[5071,]        1        2        2         1         3         3           1
[5072,]        1        2        2         1         3         3           1
[5073,]        1        2        2         1         3         3           1
[5074,]        1        2        2         1         3         3           1
[5075,]        1        2        2         1         3         3           1
[5076,]        1        2        2         1         3         3           1
[5077,]        1        1       NA         1         2        NA           1
[5078,]        1        1       NA         1         2        NA           1
[5079,]        1       NA       NA         1        NA        NA           1
[5080,]        1       NA       NA         1        NA        NA           1
[5081,]        1       NA       NA         1        NA        NA           1
[5082,]        1       NA       NA         1        NA        NA           1
[5083,]        1       NA       NA         1        NA        NA           1
[5084,]        2        2        2         2         3         2           2
[5085,]        1       NA       NA         1        NA        NA           1
[5086,]        1       NA       NA         1        NA        NA           1
[5087,]        1        2        2         1         3         3           1
[5088,]        1       NA       NA         1        NA        NA           1
[5089,]        1       NA       NA         1        NA        NA           1
[5090,]        1       NA       NA         1        NA        NA           1
[5091,]        1       NA       NA         1        NA        NA           1
[5092,]        1        2        1         1         2         2           1
[5093,]        1        2        1         1         2         2           1
[5094,]        1       NA       NA         1        NA        NA           1
[5095,]        1        2        1         1         2         2           1
[5096,]        1        2        1         1         2         2           1
[5097,]        2        2        2         2         3         2           2
[5098,]        1        1       NA         1         2        NA           1
[5099,]        1        1       NA         1         1        NA           1
[5100,]        1        1       NA         1         1        NA           1
[5101,]        1        1       NA         1         1        NA           1
[5102,]        1        1       NA         1         1        NA           1
[5103,]        1        1       NA         1         1        NA           1
[5104,]        1        1       NA         1         1        NA           1
[5105,]        1       NA       NA         1        NA        NA           1
[5106,]        1       NA       NA         1        NA        NA           1
[5107,]        1       NA       NA         1        NA        NA           1
[5108,]        1       NA       NA         1        NA        NA           1
[5109,]        1       NA       NA         1        NA        NA           1
[5110,]        1       NA       NA         1        NA        NA           1
[5111,]        1       NA       NA         1        NA        NA           1
[5112,]        1       NA       NA         1        NA        NA           1
[5113,]        1        1       NA         1         1        NA           1
[5114,]        1        1       NA         1         1        NA           1
[5115,]        1        1       NA         1         1        NA           1
[5116,]        1        1       NA         1         1        NA           1
[5117,]        1        1       NA         1         1        NA           1
[5118,]        1        1       NA         1         1        NA           1
[5119,]        1       NA       NA         1        NA        NA           1
[5120,]        1       NA       NA         1        NA        NA           1
[5121,]        1       NA       NA         1        NA        NA           1
[5122,]        1       NA       NA         1        NA        NA           1
[5123,]        1       NA       NA         1        NA        NA           1
[5124,]        1       NA       NA         1        NA        NA           1
[5125,]        1       NA       NA         1        NA        NA           1
[5126,]        1       NA       NA         1        NA        NA           1
[5127,]        1       NA       NA         1        NA        NA           1
[5128,]        1       NA       NA         1        NA        NA           1
[5129,]        1       NA       NA         1        NA        NA           1
[5130,]        1       NA       NA         1        NA        NA           1
[5131,]        1       NA       NA         1        NA        NA           1
[5132,]        1       NA       NA         1        NA        NA           1
[5133,]        1       NA       NA         1        NA        NA           1
[5134,]        1       NA       NA         1        NA        NA           1
[5135,]        1       NA       NA         1        NA        NA           1
[5136,]        1       NA       NA         1        NA        NA           1
[5137,]        1       NA       NA         1        NA        NA           1
[5138,]        1       NA       NA         1        NA        NA           1
[5139,]        1       NA       NA         1        NA        NA           1
[5140,]        1       NA       NA         1        NA        NA           1
[5141,]        1       NA       NA         1        NA        NA           1
[5142,]        1       NA       NA         1        NA        NA           1
[5143,]        1       NA       NA         1        NA        NA           1
[5144,]        1       NA       NA         1        NA        NA           1
[5145,]        1       NA       NA         1        NA        NA           1
[5146,]        1       NA       NA         1        NA        NA           1
[5147,]        1       NA       NA         1        NA        NA           1
[5148,]        1       NA       NA         1        NA        NA           1
[5149,]        1       NA       NA         1        NA        NA           1
[5150,]        1       NA       NA         1        NA        NA           1
[5151,]        1       NA       NA         1        NA        NA           1
[5152,]        1       NA       NA         1        NA        NA           1
[5153,]        1       NA       NA         1        NA        NA           1
[5154,]        1        1       NA         1         1        NA           1
[5155,]        1        1       NA         1         1        NA           1
[5156,]        1        1       NA         1         1        NA           1
[5157,]        1        1       NA         1         1        NA           1
[5158,]        1        1       NA         1         1        NA           1
[5159,]        1        1       NA         1         1        NA           1
[5160,]        1        1       NA         1         1        NA           1
[5161,]        1        1       NA         1         1        NA           1
[5162,]        1        1       NA         1         1        NA           1
[5163,]        1        1       NA         1         1        NA           1
[5164,]        1        1       NA         1         1        NA           1
[5165,]        1        1       NA         1         1        NA           1
[5166,]        1        1       NA         1         1        NA           1
[5167,]        1        1       NA         1         1        NA           1
[5168,]        1        1       NA         1         1        NA           1
[5169,]        1        1       NA         1         1        NA           1
[5170,]        1        1       NA         1         1        NA           1
[5171,]        1        1       NA         1         1        NA           1
[5172,]        1        1       NA         1         1        NA           1
[5173,]        1        1       NA         1         1        NA           1
[5174,]        1        1       NA         1         1        NA           1
[5175,]        1        1       NA         1         1        NA           1
[5176,]        1        1       NA         1         1        NA           1
[5177,]        1        1       NA         1         1        NA           1
[5178,]        1        1       NA         1         1        NA           1
[5179,]        1        1       NA         1         1        NA           1
[5180,]        1        1       NA         1         1        NA           1
[5181,]        1        1       NA         1         1        NA           1
[5182,]        1        1       NA         1         1        NA           1
[5183,]        1        1       NA         1         1        NA           1
[5184,]        1        1       NA         1         1        NA           1
[5185,]        1        1       NA         1         1        NA           1
[5186,]        1        1       NA         1         1        NA           1
[5187,]        1        1       NA         1         1        NA           1
[5188,]        1        1       NA         1         1        NA           1
[5189,]        1        1       NA         1         1        NA           1
[5190,]        1        1       NA         1         1        NA           1
[5191,]        1        1       NA         1         1        NA           1
[5192,]        1        1       NA         1         1        NA           1
[5193,]        1        1       NA         1         1        NA           1
[5194,]        1        1       NA         1         1        NA           1
[5195,]        1        1       NA         1         1        NA           1
[5196,]        1        1       NA         1         1        NA           1
[5197,]        1        1       NA         1         1        NA           1
[5198,]        1        1       NA         1         1        NA           1
[5199,]        1        1       NA         1         1        NA           1
[5200,]        1        1       NA         1         1        NA           1
[5201,]        1        1       NA         1         1        NA           1
[5202,]        1        1       NA         1         1        NA           1
[5203,]        1        1       NA         1         1        NA           1
[5204,]        1        3        2         1         3         3           1
[5205,]        1        2       NA         1         2        NA           1
[5206,]        1       NA       NA         1        NA        NA           1
[5207,]        2       NA       NA         2        NA        NA           1
[5208,]        1       NA       NA         1        NA        NA           1
[5209,]        1       NA       NA         1        NA        NA           1
[5210,]        1       NA       NA         1        NA        NA           1
[5211,]        1       NA       NA         1        NA        NA           1
[5212,]        1       NA       NA         1        NA        NA           1
[5213,]        2       NA       NA         2        NA        NA           2
[5214,]        1       NA       NA         1        NA        NA           1
[5215,]        1       NA       NA         1        NA        NA           1
[5216,]        1        1       NA         1         1        NA           1
[5217,]        1       NA       NA         1        NA        NA           1
[5218,]        1        1       NA         1         1        NA           1
[5219,]        1       NA       NA         1        NA        NA           1
[5220,]        1       NA       NA         1        NA        NA           1
[5221,]        1       NA       NA         1        NA        NA           1
[5222,]        1       NA       NA         1        NA        NA           1
[5223,]        1       NA       NA         1        NA        NA           1
[5224,]        1       NA       NA         1        NA        NA           1
[5225,]        1       NA       NA         1        NA        NA           1
[5226,]        1       NA       NA         1        NA        NA           1
[5227,]        1        2       NA         1         2        NA           1
[5228,]        3        3       NA         3         3        NA           3
[5229,]        1        2        2         1         3         3           1
[5230,]        1        3       NA         1         3        NA           1
[5231,]        3        3        2         3         3         2           3
[5232,]        1        1        3         1         1         3           1
[5233,]        1        3        2         1         3         3           1
[5234,]        1        2       NA         1         3        NA           1
[5235,]        1        2        1         1         2         2           1
[5236,]        1       NA       NA         1        NA        NA           1
[5237,]        1       NA       NA         1        NA        NA           1
[5238,]        1       NA       NA         1        NA        NA           1
[5239,]        1        2       NA         1         3        NA           1
[5240,]        1        2       NA         1         2        NA           1
[5241,]        1        2        2         1         2         2           1
[5242,]        1        2        2         1         2         3           1
[5243,]        1        2        2         1         3         3           1
[5244,]        1        3        2         1         3         3           1
[5245,]        1        2       NA         1         2        NA           1
[5246,]        1        3        2         1         3         3           1
[5247,]        1        2       NA         1         2        NA           1
[5248,]        1       NA       NA         1        NA        NA           1
[5249,]        1        3       NA         1         3        NA           1
[5250,]        1        1       NA         1         2        NA           1
[5251,]        1        2        2         1         3         3           1
[5252,]        1        2       NA         1         1        NA           1
[5253,]        1        3       NA         1         2        NA           1
[5254,]        1        3       NA         1         2        NA           1
[5255,]        1        3       NA         1         2        NA           1
[5256,]        1        3       NA         1         2        NA           1
[5257,]        1        3       NA         1         3        NA           1
[5258,]        1        3       NA         1         3        NA           1
[5259,]        1       NA       NA         1        NA        NA           1
[5260,]        1       NA       NA         1        NA        NA           1
[5261,]        1       NA       NA         1        NA        NA           1
[5262,]        1       NA       NA         1        NA        NA           1
[5263,]        1       NA       NA         1        NA        NA           1
[5264,]        1       NA       NA         1        NA        NA           1
[5265,]        1       NA       NA         1        NA        NA           1
[5266,]        1       NA       NA         1        NA        NA           1
[5267,]        1       NA       NA         1        NA        NA           1
[5268,]        1       NA       NA         1        NA        NA           1
[5269,]        1       NA       NA         1        NA        NA           1
[5270,]        1       NA       NA         1        NA        NA           1
[5271,]        1       NA       NA         1        NA        NA           1
[5272,]        1       NA       NA         1        NA        NA           1
[5273,]        1       NA       NA         1        NA        NA           1
[5274,]        1       NA       NA         1        NA        NA           1
[5275,]        1        2       NA         1         1        NA           1
[5276,]        1        2       NA         1         1        NA           1
[5277,]        1       NA       NA         1        NA        NA           1
[5278,]        1       NA       NA         1        NA        NA           1
[5279,]        1       NA       NA         1        NA        NA           1
[5280,]        1       NA       NA         1        NA        NA           1
[5281,]        1       NA       NA         1        NA        NA           1
[5282,]        1       NA       NA         1        NA        NA           1
[5283,]        1       NA       NA         1        NA        NA           1
[5284,]        1       NA       NA         1        NA        NA           1
[5285,]        1       NA       NA         1        NA        NA           1
[5286,]        1        2       NA         1         1        NA           1
[5287,]        1        2       NA         1         1        NA           1
[5288,]        1       NA       NA         1        NA        NA           1
[5289,]        1       NA       NA         1        NA        NA           1
[5290,]        1       NA       NA         1        NA        NA           1
[5291,]        1       NA       NA         1        NA        NA           1
[5292,]        1        3        3         1         2         3           1
[5293,]        1       NA       NA         1        NA        NA           1
[5294,]        2        3        3         1         3         3           1
[5295,]        1       NA       NA         1        NA        NA           1
[5296,]        1       NA       NA         1        NA        NA           1
[5297,]        1       NA       NA         1        NA        NA           1
[5298,]        1       NA       NA         1        NA        NA           1
[5299,]        1       NA       NA         1        NA        NA           1
[5300,]        1       NA       NA         1        NA        NA           1
[5301,]        1       NA       NA         1        NA        NA           1
[5302,]        1       NA       NA         1        NA        NA           1
[5303,]        1       NA       NA         1        NA        NA           1
[5304,]        1       NA       NA         1        NA        NA           1
[5305,]        1       NA       NA         1        NA        NA           1
[5306,]        1        3       NA         1         3        NA           1
[5307,]        1        3       NA         1         3        NA           1
[5308,]        1        3       NA         1         3        NA           1
[5309,]        1       NA       NA         1        NA        NA           1
[5310,]        1       NA       NA         1        NA        NA           1
[5311,]        1        3       NA         1         3        NA           1
[5312,]        1       NA       NA         1        NA        NA           1
[5313,]        1       NA       NA         1        NA        NA           1
[5314,]        1       NA       NA         1        NA        NA           1
[5315,]        1       NA       NA         1        NA        NA           1
[5316,]        1        2        3         1         2         3           1
[5317,]        1        3        3         1         3         3           1
[5318,]        1        3        3         1         3         3           1
[5319,]        1        3        3         1         3         3           1
[5320,]        2        3        3         2         3         3           2
[5321,]        2        3        3         2         3         3           2
[5322,]        1       NA       NA         1        NA        NA           1
[5323,]        1        2        3         1         2         3           1
[5324,]        2        3        3         2         3         3           2
[5325,]        2        3        3         2         3         3           2
[5326,]        1        2        3         1         2         3           1
[5327,]        1        2        3         1         2         3           1
[5328,]        1        3        3         1         3         3           1
[5329,]        1        3        3         1         3         3           1
[5330,]        1        2        3         1         2         3           1
[5331,]        1        3        3         1         3         3           1
[5332,]        2        3        3         2         3         3           2
[5333,]        1        2        3         1         2         3           1
[5334,]        2        3        3         2         3         3           2
[5335,]        2        3        3         2         3         3           2
[5336,]        1        2        3         1         2         3           1
[5337,]        2        3        3         2         3         3           2
[5338,]        2        3        3         2         3         3           2
[5339,]        2        3        3         2         3         3           2
[5340,]        1        2        3         1         2         3           1
[5341,]        1        2       NA         1         2        NA           1
[5342,]        2        3        3         2         3         3           2
[5343,]        2        3        3         2         3         3           2
[5344,]        2        3        3         2         3         3           2
[5345,]        2        3        3         2         3         3           2
[5346,]        2        3        3         2         3         3           2
[5347,]        2        3        3         2         3         3           2
[5348,]        1       NA       NA         1        NA        NA           1
[5349,]        1        2       NA         1         2        NA           1
[5350,]        2        3        3         2         3         3           2
[5351,]        1        3        3         1         3         3           1
[5352,]        2        3        3         2         3         3           2
[5353,]        2        3        3         2         3         3           2
[5354,]        1        3        3         1         3         3           1
[5355,]        2        3        3         2         3         3           2
[5356,]        2        3        3         2         3         3           2
[5357,]        2        3        3         2         3         3           2
[5358,]        2        3        3         2         3         3           2
[5359,]        2        3        3         2         3         3           2
[5360,]        2        3        3         2         3         3           2
[5361,]        2        3        3         2         3         3           2
[5362,]        2        3        3         2         3         3           2
[5363,]        2        3        3         2         3         3           2
[5364,]        2        3        3         2         3         3           2
[5365,]        2        3        3         2         3         3           2
[5366,]        2        3        3         2         3         3           2
[5367,]        2        3        3         2         3         3           2
[5368,]        2        3        3         2         3         3           2
[5369,]        1        3        3         1         3         3           1
[5370,]        2        3        3         2         3         3           2
[5371,]        1        2       NA         1         2         3           1
[5372,]        1        2        3         1         2         3           1
[5373,]        1        2        3         1         2         3           1
[5374,]        1        3        3         1         3         3           1
[5375,]        1        3        3         1         3         3           1
[5376,]        1        3        3         1         3         3           1
[5377,]        1        3        3         1         3         3           1
[5378,]        1        3        3         1         3         3           1
[5379,]        1        3        3         1         3         3           1
[5380,]        1        3        3         1         3         3           1
[5381,]        1        3        3         1         3         3           1
[5382,]        1       NA       NA         1        NA        NA           1
[5383,]        1       NA       NA         1        NA        NA           1
[5384,]        1       NA       NA         1        NA        NA           1
[5385,]        1       NA       NA         1        NA        NA           1
[5386,]        1       NA       NA         1        NA        NA           1
[5387,]        1       NA       NA         1        NA        NA           1
[5388,]        1        2       NA         1         1        NA           1
[5389,]        1       NA       NA         1        NA        NA           1
[5390,]        1        2       NA         1         1        NA           1
[5391,]        1       NA       NA         1        NA        NA           1
[5392,]        1        2        3         1         2         3           1
[5393,]        1       NA       NA         1        NA        NA           1
[5394,]        1       NA       NA         1        NA        NA           1
[5395,]        1        2       NA         1         1        NA           1
[5396,]        1        2        3         1         2         3           1
[5397,]        1        2        3         1         2         3           1
[5398,]        1        2       NA         1         1        NA           1
[5399,]        1        2        3         1         2         3           1
[5400,]        1        3        3         1         3         3           1
[5401,]        1       NA       NA         1        NA        NA           1
[5402,]        1       NA       NA         1        NA        NA           1
[5403,]        1       NA       NA         1        NA        NA           1
[5404,]        1       NA       NA         1        NA        NA           1
[5405,]        1        2        3         1         2         3           1
[5406,]        1        2        3         1         2         3           1
[5407,]        1        2        3         1         2         3           1
[5408,]        1        2        3         1         2         3           1
[5409,]        1        2        3         1         2         3           1
[5410,]        1        2        3         1         2         3           1
[5411,]        1        2        3         1         2         3           1
[5412,]        1        2        3         1         2         3           1
[5413,]        1        2        3         1         1         3           1
[5414,]        1        2        3         1         1         3           1
[5415,]        1        2        3         1         2         3           1
[5416,]        1        2        3         1         2         3           1
[5417,]        1        2        3         1         2         3           1
[5418,]        1        2        3         1         2         3           1
[5419,]        1        2        3         1         2         3           1
[5420,]        1        2        3         1         2         3           1
[5421,]        1        2        3         1         2         3           1
[5422,]        1        2       NA         1         1        NA           1
[5423,]        1        2       NA         1         1        NA           1
[5424,]        1        2       NA         1         1        NA           1
[5425,]        1       NA       NA         1        NA        NA           1
[5426,]        1       NA       NA         1        NA        NA           1
[5427,]        1       NA       NA         1        NA        NA           1
[5428,]        1       NA       NA         1        NA        NA           1
[5429,]        1       NA       NA         1        NA        NA           1
[5430,]        1       NA       NA         1        NA        NA           1
[5431,]        1       NA       NA         1        NA        NA           1
[5432,]        1       NA       NA         1        NA        NA           1
[5433,]        1       NA       NA         1        NA        NA           1
[5434,]        1       NA       NA         1        NA        NA           1
[5435,]        1       NA       NA         1        NA        NA           1
[5436,]        1        2        3         1         2         3           1
[5437,]        1        2        3         1         2         3           1
[5438,]        1        2        3         1         2         3           1
[5439,]        1        2        3         1         2         3           1
[5440,]        1        2        3         1         2         3           1
[5441,]        1        2        3         1         2         3           1
[5442,]        1        2        3         1         2         3           1
[5443,]        1        3       NA         1         3        NA           1
[5444,]        1        2        3         1         2         3           1
[5445,]        1        2        3         1         2         3           1
[5446,]        1        2        3         1         2         3           1
[5447,]        1        2        3         1         2         3           1
[5448,]        1        2        3         1         2         3           1
[5449,]        1        2        3         1         2         3           1
[5450,]        1        3       NA         1         2        NA           1
[5451,]        1        2        3         1         2         3           1
[5452,]        1        3       NA         1         3        NA           1
[5453,]        1        2        3         1         2         3           1
[5454,]        1        2        3         1         2         3           1
[5455,]        1       NA       NA         1        NA        NA           1
[5456,]        1        2        3         1         2         3           1
[5457,]        1        3        3         1         3         3           1
[5458,]        1        2        3         1         2         3           1
[5459,]        1        2       NA         1         1        NA           1
[5460,]        1        2       NA         1         1        NA           1
[5461,]        1        2       NA         1         1        NA           1
[5462,]        1        2       NA         1         1        NA           1
[5463,]        1       NA       NA         1        NA        NA           1
[5464,]        1       NA       NA         1        NA        NA           1
[5465,]        1       NA       NA         1        NA        NA           1
[5466,]        1        3        3         1         3         3           1
[5467,]        1        2        3         1         2         3           1
[5468,]        1        2        3         1         2         3           1
[5469,]        1        2        3         1         2         3           1
[5470,]        1        2       NA         1         1        NA           1
[5471,]        1        2       NA         1         1        NA           1
[5472,]        1        2       NA         1         1        NA           1
[5473,]        1        2        3         1         2         3           1
[5474,]        1        2       NA         1         1        NA           1
[5475,]        1        2        3         1         2         3           1
[5476,]        1        2        3         1         2         3           1
[5477,]        1        2        3         1         2         3           1
[5478,]        1       NA       NA         1        NA        NA           1
[5479,]        1       NA       NA         1        NA        NA           1
[5480,]        1        3        3         1         3         3           1
[5481,]        2        3        3         2         3         3           2
[5482,]        1        3        3         1         3         3           1
[5483,]        1        3        3         1         3         3           1
[5484,]        1        3        3         1         3         3           1
[5485,]        1        3        3         1         3         3           1
[5486,]        1        3        3         1         3         3           1
[5487,]        1        3        3         1         3         3           1
[5488,]        1        3        3         1         3         3           1
[5489,]        1        2       NA         1         2        NA           1
[5490,]        1        2       NA         1         2        NA           1
[5491,]        1       NA       NA         1        NA        NA           1
[5492,]        1       NA       NA         1        NA        NA           1
[5493,]        1       NA       NA         1        NA        NA           1
[5494,]        1       NA       NA         1        NA        NA           1
[5495,]        1       NA       NA         1        NA        NA           1
[5496,]        1       NA       NA         1        NA        NA           1
[5497,]        1        3        3         1         3         3           1
[5498,]        1       NA       NA         1        NA        NA           1
[5499,]        1       NA       NA         1        NA        NA           1
[5500,]        1       NA       NA         1        NA        NA           1
[5501,]        1       NA       NA         1        NA        NA           1
[5502,]        1        3        3         1         3         3           1
[5503,]        1       NA       NA         1        NA        NA           1
[5504,]        1        3        3         1         3         3           1
[5505,]        1        3        3         1         3         3           1
[5506,]        2        3        3         2         3         3           2
[5507,]        1        2       NA         1         2        NA           1
[5508,]        1        2       NA         1         1        NA           1
[5509,]        1        2       NA         1         1        NA           1
[5510,]        1        2       NA         1         1        NA           1
[5511,]        1        2       NA         1         1        NA           1
[5512,]        1        2       NA         1         1        NA           1
[5513,]        1       NA       NA         1        NA        NA           1
[5514,]        1       NA       NA         1        NA        NA           1
[5515,]        1       NA       NA         1        NA        NA           1
[5516,]        1       NA       NA         1        NA        NA           1
[5517,]        1       NA       NA         1        NA        NA           1
[5518,]        1       NA       NA         1        NA        NA           1
[5519,]        1        2       NA         1         1        NA           1
[5520,]        1        2       NA         1         1        NA           1
[5521,]        1        2       NA         1         1        NA           1
[5522,]        1        2       NA         1         1        NA           1
[5523,]        1        2       NA         1         1        NA           1
[5524,]        1       NA       NA         1        NA        NA           1
[5525,]        1       NA       NA         1        NA        NA           1
[5526,]        1       NA       NA         1        NA        NA           1
[5527,]        1       NA       NA         1        NA        NA           1
[5528,]        1       NA       NA         1        NA        NA           1
[5529,]        1       NA       NA         1        NA        NA           1
[5530,]        1       NA       NA         1        NA        NA           1
[5531,]        1       NA       NA         1        NA        NA           1
[5532,]        1       NA       NA         1        NA        NA           1
[5533,]        1       NA       NA         1        NA        NA           1
[5534,]        1       NA       NA         1        NA        NA           1
[5535,]        1       NA       NA         1        NA        NA           1
[5536,]        1       NA       NA         1        NA        NA           1
[5537,]        1       NA       NA         1        NA        NA           1
[5538,]        1       NA       NA         1        NA        NA           1
[5539,]        1       NA       NA         1        NA        NA           1
[5540,]        1       NA       NA         1        NA        NA           1
[5541,]        1       NA       NA         1        NA        NA           1
[5542,]        1       NA       NA         1        NA        NA           1
[5543,]        1       NA       NA         1        NA        NA           1
[5544,]        1        3       NA         1         3        NA           1
[5545,]        1        3       NA         1         3        NA           1
[5546,]        1        3       NA         1         3        NA           1
[5547,]        1        3       NA         1         3        NA           1
[5548,]        1        3       NA         1         2        NA           1
[5549,]        1        2       NA         1         1        NA           1
[5550,]        1        3       NA         1         3        NA           1
[5551,]        1        3       NA         1         2        NA           1
[5552,]        1        3       NA         1         2        NA           1
[5553,]        1        3       NA         1         2        NA           1
[5554,]        1        3       NA         1         2        NA           1
[5555,]        1        2       NA         1         1        NA           1
[5556,]        1        2       NA         1         1        NA           1
[5557,]        1        3       NA         1         2        NA           1
[5558,]        1        3       NA         1         2        NA           1
[5559,]        1        3       NA         1         2        NA           1
[5560,]        1        3       NA         1         2        NA           1
[5561,]        1        3       NA         1         2        NA           1
[5562,]        1        3       NA         1         3        NA           1
[5563,]        1        3       NA         1         2        NA           1
[5564,]        1        3       NA         1         3        NA           1
[5565,]        1        3       NA         1         2        NA           1
[5566,]        1        3       NA         1         2        NA           1
[5567,]        1        3       NA         1         2        NA           1
[5568,]        1        3       NA         1         2        NA           1
[5569,]        1        3       NA         1         3        NA           1
[5570,]        1        3       NA         1         2        NA           1
[5571,]        1        3       NA         1         3        NA           1
[5572,]        1        3       NA         1         2        NA           1
[5573,]        1        2       NA         1         2        NA           1
[5574,]        2        2       NA         2         2        NA           2
[5575,]        1        2        2         1         2         2           1
[5576,]        1        2        2         1         2         2           1
[5577,]        2        2       NA         2         2        NA           2
[5578,]        1       NA       NA         1        NA        NA           1
[5579,]        2        3       NA         2         3        NA           2
[5580,]        2        2       NA         2         2        NA           2
[5581,]        1        2       NA         1         2        NA           1
[5582,]        2        3       NA         2         3        NA           2
[5583,]        2        2       NA         2         2        NA           2
[5584,]        3        2        2         3         2         2           3
[5585,]        1        2        2         1         2         2           1
[5586,]        1        2        2         1         2         2           1
[5587,]        1        2        2         1         2         2           1
[5588,]        1        2        2         1         2         3           1
[5589,]        1       NA       NA         1        NA        NA           1
[5590,]        1        2       NA         1         2        NA           1
[5591,]        1        2        3         1         2         3           1
[5592,]        1        2        3         1         2         3           1
[5593,]        1        2        3         1         2         3           1
[5594,]        1        2        3         1         2         3           1
[5595,]        1        2        3         1         2         3           1
[5596,]        1       NA       NA         1        NA        NA           1
[5597,]        1       NA       NA         1        NA        NA           1
[5598,]        1        2       NA         1         2        NA           1
[5599,]        1        2        2         1         2         2           1
[5600,]        2        2       NA         2         2        NA           2
[5601,]        1        2       NA         1         2        NA           1
[5602,]        1       NA       NA         1        NA        NA           1
[5603,]        1       NA       NA         1        NA        NA           1
[5604,]        2        3       NA         2         3        NA           2
[5605,]        1       NA       NA         1        NA        NA           1
[5606,]        1       NA       NA         1        NA        NA           1
[5607,]        1       NA       NA         1        NA        NA           1
[5608,]        1       NA       NA         1        NA        NA           1
[5609,]        1       NA       NA         1        NA        NA           1
[5610,]        1       NA       NA         1        NA        NA           1
[5611,]        1       NA       NA         1        NA        NA           1
[5612,]        1       NA       NA         1        NA        NA           1
[5613,]        2        2       NA         2         2        NA           2
[5614,]        1       NA       NA         1        NA        NA           1
[5615,]        1       NA       NA         1        NA        NA           1
[5616,]        1       NA       NA         1        NA        NA           1
[5617,]        1       NA       NA         1        NA        NA           1
[5618,]        1       NA       NA         1        NA        NA           1
[5619,]        1       NA       NA         1        NA        NA           1
[5620,]        1       NA       NA         1        NA        NA           1
[5621,]        1        2       NA         1         2        NA           1
[5622,]        2       NA       NA         2        NA        NA           2
[5623,]        1        2        3         1         2         3           1
[5624,]        1       NA       NA         1        NA        NA           1
[5625,]        2        2        2         2         2         2           2
[5626,]        2        2        3         2         2         3           2
[5627,]        1        3       NA         1         3        NA           1
[5628,]        1        3        3         1         3         3           1
[5629,]        1        3        3         1         3         3           1
[5630,]        1       NA       NA         1        NA        NA           1
[5631,]        2        3       NA         2         3        NA           2
[5632,]        2        2       NA         2         2        NA           2
[5633,]        2        3       NA         2         3        NA           2
[5634,]        3        3        3         3         3         3           3
[5635,]        1        2        3         1         2         3           1
[5636,]        1        2        3         1         2         3           1
[5637,]        1        2        3         1         2         3           1
[5638,]        1        2        3         1         2         3           1
[5639,]        2        3       NA         2         3        NA           2
[5640,]        1        2        3         1         2         3           1
[5641,]        1        2        3         1         2         3           1
[5642,]        1        2        3         1         2         3           1
[5643,]        1        2        3         1         2         3           1
[5644,]        1        2        3         1         2         3           1
[5645,]        1       NA       NA         1        NA        NA           1
[5646,]        1        3       NA         1         3        NA           1
[5647,]        1        2        3         1         2         3           1
[5648,]        1        3       NA         1         3        NA           1
[5649,]        1       NA       NA         1        NA        NA           1
[5650,]        1       NA       NA         1        NA        NA           1
[5651,]        1       NA       NA         1        NA        NA           1
[5652,]        1       NA       NA         1        NA        NA           1
[5653,]        1       NA       NA         1        NA        NA           1
[5654,]        2       NA       NA         1        NA        NA           1
[5655,]        1       NA       NA         1        NA        NA           1
[5656,]        1       NA       NA         1        NA        NA           1
[5657,]        2       NA       NA         2        NA        NA           2
[5658,]        2       NA       NA         1        NA        NA           1
[5659,]        2       NA       NA         1        NA        NA           2
[5660,]        2       NA       NA         1        NA        NA           1
[5661,]        2       NA       NA         1        NA        NA           2
[5662,]        2       NA       NA         1        NA        NA           2
[5663,]        1        3       NA         1         3        NA           1
[5664,]        1        2        3         1         2         3           1
[5665,]        1        2       NA         1         2        NA           1
[5666,]        1       NA       NA         1        NA        NA           1
[5667,]        1        2        3         1         2         3           1
[5668,]        1        2        3         1         2         3           1
[5669,]        1       NA       NA         1        NA        NA           1
[5670,]        1       NA       NA         1        NA        NA           1
[5671,]        1        2       NA         1         2        NA           1
[5672,]        1       NA       NA         1        NA        NA           1
[5673,]        1        3       NA         1         3        NA           1
[5674,]        2        2       NA         2         2        NA           2
[5675,]        1        3       NA         1         3        NA           1
[5676,]        3        3        3         3         3         3           2
[5677,]        2        2       NA         2         2        NA           1
[5678,]        2        2       NA         2         2        NA           2
[5679,]        1        2        3         1         2         2           1
[5680,]        1        3       NA         1         3        NA           1
[5681,]        1       NA       NA         1        NA        NA           1
[5682,]        1        3       NA         1         3        NA           1
[5683,]        1       NA       NA         1        NA        NA           1
[5684,]        2        3       NA         2         3        NA           2
[5685,]        1       NA       NA         1        NA        NA           1
[5686,]        2       NA       NA         2        NA        NA           2
[5687,]        1        2        3         1         2         3           1
[5688,]        1       NA       NA         1        NA        NA           1
[5689,]        1       NA       NA         1        NA        NA           1
[5690,]        1        3       NA         1         3        NA           1
[5691,]        2       NA       NA         2        NA        NA           2
[5692,]        1        2        3         1         2         3           1
[5693,]        2       NA       NA         2        NA        NA           2
[5694,]        1        2        3         1         2         3           1
[5695,]        2        3        3         2         2         3           2
[5696,]        1        2        3         1         2         3           1
[5697,]        1        2        3         1         2         3           1
[5698,]        1       NA       NA         1        NA        NA           1
[5699,]        1        3       NA         1         3        NA           1
[5700,]        1        3        3         1         3         3           1
[5701,]        1        2        3         1         2         3           1
[5702,]        1        2        3         1         2         3           1
[5703,]        1        2        3         1         2         3           1
[5704,]        1        2        2         1         2         2           1
[5705,]        1        2       NA         1         2        NA           1
[5706,]        1        2       NA         1         2        NA           1
[5707,]        1       NA       NA         1        NA        NA           1
[5708,]        2       NA       NA         2        NA        NA           2
[5709,]        1        2        2         1         2         2           1
[5710,]        1       NA       NA         1        NA        NA           1
[5711,]        1        3       NA         1         2        NA           1
[5712,]        1       NA       NA         1        NA        NA           1
[5713,]        1       NA       NA         1        NA        NA           1
[5714,]        1        2       NA         1         2        NA           1
[5715,]        1       NA       NA         1        NA        NA           1
[5716,]        1       NA       NA         1        NA        NA           1
[5717,]        1       NA       NA         1        NA        NA           1
[5718,]        1        2       NA         1         2        NA           1
[5719,]        2        2       NA         2         2        NA           2
[5720,]        1        2       NA         1         2        NA           1
[5721,]        1        2       NA         1         2        NA           1
[5722,]        1        2        3         1         2         2           1
[5723,]        1       NA       NA         1        NA        NA           1
[5724,]        1        3       NA         1         3        NA           1
[5725,]        1       NA       NA         1        NA        NA           1
[5726,]        1       NA       NA         1        NA        NA           1
[5727,]        1        2       NA         1         2        NA           1
[5728,]        1       NA       NA         1        NA        NA           1
[5729,]        1       NA       NA         1        NA        NA           1
[5730,]        1        3        2         1         3         2           1
[5731,]        1        2        3         1         2         3           1
[5732,]        1       NA       NA         1        NA        NA           1
[5733,]        1        2        2         1         2         2           1
[5734,]        1       NA       NA         1        NA        NA           1
[5735,]        1       NA       NA         1        NA        NA           1
[5736,]        1       NA       NA         1        NA        NA           1
[5737,]        1        3       NA         1         3        NA           1
[5738,]        1        2       NA         1         2        NA           1
[5739,]        1        2       NA         1         2        NA           1
[5740,]        2        3       NA         2         3        NA           1
[5741,]        1        2        3         1         2         3           1
[5742,]        1        2        3         1         2         3           1
[5743,]        2        3       NA         2         3        NA           2
[5744,]        1        3        3         1         3         3           1
[5745,]        1        2       NA         1         2        NA           1
[5746,]        1        2        2         1         2         2           1
[5747,]        1       NA       NA         1        NA        NA           1
[5748,]        1        3       NA         1         3        NA           1
[5749,]        2        3        3         2         3         3           2
[5750,]        1        2        3         1         2         3           1
[5751,]        1        3        3         1         3         3           1
[5752,]        1        3        3         1         3         3           1
[5753,]        1       NA       NA         1        NA        NA           1
[5754,]        1       NA       NA         1        NA        NA           1
[5755,]        1       NA       NA         1        NA        NA           1
[5756,]        1        2       NA         1         2        NA           1
[5757,]        1        2       NA         1         2        NA           1
[5758,]        1        3       NA         1         3        NA           1
[5759,]        2        3       NA         2         3        NA           2
[5760,]        1        3       NA         1         3        NA           1
[5761,]        3        3       NA         3         3        NA           3
[5762,]        1        3       NA         1         3        NA           1
[5763,]        1        3       NA         1         3        NA           1
[5764,]        2        3       NA         2         3        NA           2
[5765,]        1       NA       NA         1        NA        NA           1
[5766,]        1       NA       NA         1        NA        NA           1
[5767,]        1        1       NA         1         1        NA           1
[5768,]        1       NA       NA         1        NA        NA           1
[5769,]        1        2        3         1         2         3           1
[5770,]        2        2        3         2         2         3           1
[5771,]        1        2        3         1         2         3           1
[5772,]        1        3        3         1         3         3           1
[5773,]        2        3        3         2         3         3           1
[5774,]        1        2       NA         1         2        NA           1
[5775,]        1       NA       NA         1        NA        NA           1
[5776,]        1        2        3         1         2         3           1
[5777,]        1        3        3         1         3         3           1
[5778,]        1        3        3         1         3         3           1
[5779,]        1        2        3         1         2         3           1
[5780,]        1        3        3         1         3         3           1
[5781,]        1       NA       NA         1        NA        NA           1
[5782,]        1        3       NA         1         3        NA           1
[5783,]        1        3        3         1         3         3           1
[5784,]        1        3        3         1         3         3           1
[5785,]        1       NA       NA         1        NA        NA           1
[5786,]        2        3       NA         2         3        NA           2
[5787,]        2        2       NA         2         2        NA           2
[5788,]        3        3        3         3         3         3           3
[5789,]        1        2        3         1         2         3           1
[5790,]        1        2        3         1         2         3           1
[5791,]        1        2        3         1         2         3           1
[5792,]        1        3        3         1         3         3           1
[5793,]        1        2        3         1         2         3           1
[5794,]        1        2        3         1         2         3           1
[5795,]        1        2        3         1         2         3           1
[5796,]        1        2        3         1         2         3           1
[5797,]        1       NA       NA         1        NA        NA           1
[5798,]        1        3       NA         1         3        NA           1
[5799,]        1        2        3         1         2         3           1
[5800,]        1        3       NA         1         3        NA           1
[5801,]        1       NA       NA         1        NA        NA           1
[5802,]        1       NA       NA         1        NA        NA           1
[5803,]        3        3       NA         3         3        NA           3
[5804,]        1       NA       NA         1        NA        NA           1
[5805,]        1       NA       NA         1        NA        NA           1
[5806,]        1       NA       NA         1        NA        NA           1
[5807,]        2       NA       NA         1        NA        NA           1
[5808,]        1       NA       NA         1        NA        NA           1
[5809,]        1       NA       NA         1        NA        NA           1
[5810,]        1       NA       NA         1        NA        NA           1
[5811,]        1       NA       NA         1        NA        NA           1
[5812,]        1       NA       NA         1        NA        NA           1
[5813,]        2       NA       NA         1        NA        NA           1
[5814,]        2       NA       NA         1        NA        NA           2
[5815,]        2       NA       NA         1        NA        NA           2
[5816,]        2        3       NA         2         3        NA           2
[5817,]        1        3        3         1         3         3           1
[5818,]        1        2       NA         1         2        NA           1
[5819,]        1       NA       NA         1        NA        NA           1
[5820,]        1        3        3         1         3         3           1
[5821,]        1        2        3         1         2         3           1
[5822,]        1        2       NA         1         2        NA           1
[5823,]        1       NA       NA         1        NA        NA           1
[5824,]        1        3       NA         1         3        NA           1
[5825,]        1        3       NA         1         3        NA           1
[5826,]        3        3        3         3         3         3           3
[5827,]        2        2       NA         2         2        NA           2
[5828,]        2        2       NA         2         2        NA           2
[5829,]        1        2        3         1         2         3           1
[5830,]        1        3       NA         1         3        NA           1
[5831,]        1       NA       NA         1        NA        NA           1
[5832,]        1       NA       NA         1        NA        NA           1
[5833,]        2        3       NA         2         3        NA           2
[5834,]        1       NA       NA         1        NA        NA           1
[5835,]        2       NA       NA         2        NA        NA           2
[5836,]        1        2        3         1         2         3           1
[5837,]        1       NA       NA         1        NA        NA           1
[5838,]        1       NA       NA         1        NA        NA           1
[5839,]        1        3       NA         1         3        NA           1
[5840,]        1        3        3         1         3         3           1
[5841,]        2       NA       NA         2        NA        NA           2
[5842,]        1        2        3         1         2         3           1
[5843,]        1       NA       NA         1        NA        NA           1
[5844,]        1        2        3         1         2         3           1
[5845,]        1       NA       NA         1        NA        NA           1
[5846,]        1        2        3         1         2         3           1
[5847,]        1       NA       NA         1        NA        NA           1
[5848,]        1        3       NA         1         3        NA           1
[5849,]        1       NA       NA         1        NA        NA           1
[5850,]        1       NA       NA         1        NA        NA           1
[5851,]        2       NA       NA         1        NA        NA           2
[5852,]        3        3       NA         3         3        NA           3
[5853,]        2        2       NA         2         2        NA           2
[5854,]        1        3       NA         1         3        NA           1
[5855,]        1        2        3         1         2         3           1
[5856,]        1        3        3         1         3         3           1
[5857,]        1       NA       NA         1        NA        NA           1
[5858,]        1        3       NA         1         3        NA           1
[5859,]        1        3        3         1         3         3           1
[5860,]        2        3        3         2         3         3           2
[5861,]        1        3       NA         1         3        NA           1
[5862,]        1       NA       NA         1        NA        NA           1
[5863,]        1        2        2         1         2         2           1
[5864,]        2        2       NA         2         2        NA           2
[5865,]        1       NA       NA         1        NA        NA           1
[5866,]        1        2       NA         1         2        NA           1
[5867,]        1       NA       NA         1        NA        NA           1
[5868,]        1       NA       NA         1        NA        NA           1
[5869,]        1       NA       NA         1        NA        NA           1
[5870,]        1       NA       NA         1        NA        NA           1
[5871,]        1       NA       NA         1        NA        NA           1
[5872,]        1        2       NA         1         2        NA           1
[5873,]        1       NA       NA         1        NA        NA           1
[5874,]        1       NA       NA         1        NA        NA           1
[5875,]        2        2       NA         2         2        NA           2
[5876,]        1       NA       NA         1        NA        NA           1
[5877,]        1        2       NA         1         2        NA           1
[5878,]        1        2       NA         1         2        NA           1
[5879,]        1        2        2         1         2         2           1
[5880,]        1       NA       NA         1        NA        NA           1
[5881,]        1        2        2         1         2         2           1
[5882,]        1       NA       NA         1        NA        NA           1
[5883,]        1        2       NA         1         2        NA           1
[5884,]        3        2        2         3         2         2           3
[5885,]        1        3        3         1         3         3           1
[5886,]        1        3        3         1         3         3           1
[5887,]        1        2        2         1         2         2           1
[5888,]        1        2       NA         1         2        NA           1
[5889,]        1        2       NA         1         2        NA           1
[5890,]        1       NA       NA         1        NA        NA           1
[5891,]        1       NA       NA         1        NA        NA           1
[5892,]        1        2       NA         1         2        NA           1
[5893,]        1        2       NA         1         2        NA           1
[5894,]        1        2        2         1         2         2           1
[5895,]        1        2        2         1         2         2           1
[5896,]        1        2        2         1         2         2           1
[5897,]        1       NA       NA         1        NA        NA           1
[5898,]        1       NA       NA         1        NA        NA           1
[5899,]        1       NA       NA         1        NA        NA           1
[5900,]        1       NA       NA         1        NA        NA           1
[5901,]        1        2       NA         1         2        NA           1
[5902,]        1        3       NA         1         3        NA           1
[5903,]        1        1       NA         1         1        NA           1
[5904,]        1        3        3         1         3         3           1
[5905,]        1        3        3         1         3         3           1
[5906,]        1        3        3         1         3         3           1
[5907,]        1        3        3         1         3         3           1
[5908,]        2        2        3         2         2         3           2
[5909,]        2        2        3         2         2         3           2
[5910,]        2        2        3         2         2         3           2
[5911,]        1        2        3         1         2         3           1
[5912,]        1       NA       NA         1        NA        NA           1
[5913,]        1        1       NA         1         1        NA           1
[5914,]        1        1       NA         1         1        NA           1
[5915,]        1        1       NA         1         1        NA           1
[5916,]        1       NA       NA         1        NA        NA           1
[5917,]        1       NA       NA         1        NA        NA           1
[5918,]        1       NA       NA         1        NA        NA           1
[5919,]        1       NA       NA         1        NA        NA           1
[5920,]        1        2       NA         1         2        NA           1
[5921,]        1        3        3         2         3         3           1
[5922,]        1       NA       NA         1        NA        NA           1
[5923,]        1       NA       NA         1        NA        NA           1
[5924,]        3       NA       NA         3        NA        NA           3
[5925,]        1        2        3         1         2         3           1
[5926,]        2       NA       NA         2        NA        NA           2
[5927,]        3        2       NA         3         2        NA           3
[5928,]        2        3        3         2         3         3           2
[5929,]        1       NA       NA         1        NA        NA           1
[5930,]        1        2       NA         1         2        NA           1
[5931,]        1        3        3         1         3         3           1
[5932,]        1        3        3         1         3         3           1
[5933,]        1        2        3         1         2         3           1
[5934,]        1        2        3         1         2         3           1
[5935,]        1        2        3         1         2         3           1
[5936,]        1        2        3         1         2         3           1
[5937,]        1        3        3         1         3         3           1
[5938,]        1       NA       NA         1        NA        NA           1
[5939,]        2       NA       NA         2        NA        NA           2
[5940,]        3       NA       NA         3        NA        NA           3
[5941,]        1        3       NA         1         3        NA           1
[5942,]        1        3       NA         1         3        NA           1
[5943,]        2        3        3         2         3         3           2
[5944,]        2        2       NA         2         2        NA           2
[5945,]        1       NA       NA         1        NA        NA           1
[5946,]        1        3        3         1         3         3           1
[5947,]        2        3        3         2         3         3           2
[5948,]        1        3        3         1         3         3           1
[5949,]        1        3        3         1         3         3           1
[5950,]        1        3        2         1         3         2           1
[5951,]        1       NA       NA         1        NA        NA           1
[5952,]        2        2        3         2         2         3           2
[5953,]        1       NA       NA         1        NA        NA           1
[5954,]        2        2        3         2         2         3           2
[5955,]        2        2        3         2         2         3           2
[5956,]        1       NA       NA         1        NA        NA           1
[5957,]        1        2        3         1         2         3           1
[5958,]        1        2        2         1         2         2           1
[5959,]        1        2       NA         1         2        NA           1
[5960,]        1       NA       NA         1        NA        NA           1
[5961,]        1        2       NA         1         2        NA           1
[5962,]        1        2       NA         1         2        NA           1
[5963,]        1       NA       NA         1        NA        NA           1
[5964,]        1       NA       NA         1        NA        NA           1
[5965,]        1       NA       NA         1        NA        NA           1
[5966,]        1        1       NA         1         1        NA           1
[5967,]        1        1       NA         1         1        NA           1
[5968,]        1        2       NA         1         2        NA           1
[5969,]        1        2       NA         1         2        NA           1
[5970,]        1        2       NA         1         2        NA           1
[5971,]        1       NA       NA         1        NA        NA           1
[5972,]        1       NA       NA         1        NA        NA           1
[5973,]        1        3        3         1         3         3           1
[5974,]        1        2        3         1         2         3           1
[5975,]        1       NA       NA         1        NA        NA           1
[5976,]        1        3        3         1         3         3           1
[5977,]        1        2       NA         1         2        NA           1
[5978,]        1       NA       NA         1        NA        NA           1
[5979,]        1       NA       NA         1        NA        NA           1
[5980,]        1       NA       NA         1        NA        NA           1
[5981,]        1       NA       NA         1        NA        NA           1
[5982,]        2       NA       NA         2        NA        NA           2
[5983,]        1       NA       NA         1        NA        NA           1
[5984,]        1       NA       NA         1        NA        NA           1
[5985,]        2        2       NA         2         2        NA           2
[5986,]        1       NA       NA         1        NA        NA           1
[5987,]        1        2        3         1         2         3           1
[5988,]        2       NA       NA         2        NA        NA           2
[5989,]        1        2        3         1         2         3           1
[5990,]        2       NA       NA         2        NA        NA           2
[5991,]        1        2       NA         1         2        NA           1
[5992,]        3        2        3         3         2         3           3
[5993,]        1        3        3         1         3         3           1
[5994,]        1        2        3         1         2         3           1
[5995,]        1        3       NA         1         3        NA           1
[5996,]        1        3       NA         1         3        NA           1
[5997,]        2       NA       NA         2        NA        NA           2
[5998,]        2       NA       NA         2        NA        NA           2
[5999,]        1        3       NA         1         3        NA           1
[6000,]        1        3        3         1         3         3           1
[6001,]        1        3        3         1         3         3           1
[6002,]        1       NA       NA         2        NA        NA           1
[6003,]        2       NA       NA         2        NA        NA           2
[6004,]        1        1       NA         1         1        NA           1
[6005,]        1        2        3         1         2         3           1
[6006,]        1       NA       NA         1        NA        NA           1
[6007,]        1        2        3         1         2         3           1
[6008,]        1        3        3         1         3         3           1
[6009,]        1       NA       NA         1        NA        NA           1
[6010,]        3       NA       NA         3        NA        NA           3
[6011,]        1       NA       NA         1        NA        NA           1
[6012,]        1        2        3         1         2         3           1
[6013,]        2        2       NA         2         2        NA           2
[6014,]        1        2        3         1         2         3           1
[6015,]        3       NA       NA         3        NA        NA           3
[6016,]        1        2        3         1         2         3           1
[6017,]        1        2        3         1         2         3           1
[6018,]        3        2        3         3         2         3           3
[6019,]        1        3        3         1         3         3           1
[6020,]        1       NA       NA         1        NA        NA           1
[6021,]        2        2        3         2         2         3           2
[6022,]        1       NA       NA         1        NA        NA           1
[6023,]        1        3        3         1         3         3           1
[6024,]        3       NA       NA         3        NA        NA           3
[6025,]        1       NA       NA         1        NA        NA           1
[6026,]        2        2       NA         2         2        NA           2
[6027,]        2        2       NA         2         2        NA           2
[6028,]        1       NA       NA         1        NA        NA           1
[6029,]        1       NA       NA         1        NA        NA           1
[6030,]        1       NA       NA         1        NA        NA           1
[6031,]        1       NA       NA         1        NA        NA           1
[6032,]        1        2        2         1         2         2           1
[6033,]        1        3       NA         1         2        NA           1
[6034,]        1       NA       NA         1        NA        NA           1
[6035,]        1       NA       NA         1        NA        NA           1
[6036,]        1       NA       NA         1        NA        NA           1
[6037,]        1       NA       NA         1        NA        NA           1
[6038,]        1       NA       NA         1        NA        NA           1
[6039,]        1       NA       NA         1        NA        NA           1
[6040,]        2        2       NA         2         2        NA           2
[6041,]        1       NA       NA         1        NA        NA           1
[6042,]        1        1        3         1         1         3           1
[6043,]        2       NA       NA         2        NA        NA           2
[6044,]        1        1        3         1         1         3           1
[6045,]        2       NA       NA         2        NA        NA           2
[6046,]        1        2       NA         1         2        NA           1
[6047,]        1        2        2         1         2         2           1
[6048,]        1        2        3         1         2         3           1
[6049,]        1        2        3         1         2         3           1
[6050,]        1        3       NA         1         2        NA           1
[6051,]        1        3       NA         1         2        NA           1
[6052,]        1       NA       NA         1        NA        NA           1
[6053,]        1       NA       NA         1        NA        NA           1
[6054,]        1        3       NA         1         2        NA           1
[6055,]        1        2        2         1         2         2           1
[6056,]        1        2        2         1         2         2           1
[6057,]        1       NA       NA         1        NA        NA           1
[6058,]        1       NA       NA         1        NA        NA           1
[6059,]        1       NA       NA         1        NA        NA           1
[6060,]        1        2        3         1         2         2           1
[6061,]        1       NA       NA         1        NA        NA           1
[6062,]        1        3        3         1         2         3           1
[6063,]        1       NA       NA         1        NA        NA           1
[6064,]        3       NA       NA         3        NA        NA           3
[6065,]        2       NA       NA         2        NA        NA           2
[6066,]        1        2       NA         1         2        NA           1
[6067,]        1        1        3         1         1         2           1
[6068,]        1       NA       NA         1        NA        NA           1
[6069,]        1        3       NA         1         3        NA           1
[6070,]        2        2       NA         2         2        NA           2
[6071,]        1        3        3         1         2         3           1
[6072,]        1       NA       NA         1        NA        NA           1
[6073,]        1        2       NA         1         2        NA           1
[6074,]        1        3       NA         1         3        NA           1
[6075,]        2       NA       NA         2        NA        NA           2
[6076,]        1        2       NA         1         2        NA           1
[6077,]        1        1        3         1         1         2           1
[6078,]        1        1        3         1         1         3           1
[6079,]        2        2       NA         2         2        NA           2
[6080,]        1        3        3         1         2         3           1
[6081,]        1        1        3         1         1         2           1
[6082,]        1        2       NA         1         2        NA           1
[6083,]        1       NA       NA         1        NA        NA           1
[6084,]        1       NA       NA         1        NA        NA           1
[6085,]        3       NA       NA         3        NA        NA           3
[6086,]        2        2       NA         2         2        NA           2
[6087,]        1        3        3         1         2         3           1
[6088,]        1        3        3         1         2         3           1
[6089,]        1        2        3         1         2         3           1
[6090,]        1        3        3         1         2         3           1
[6091,]        3       NA       NA         3        NA        NA           3
[6092,]        3       NA       NA         3        NA        NA           3
[6093,]        1       NA       NA         1        NA        NA           1
[6094,]        1        1        3         1         1         2           1
[6095,]        1       NA       NA         1        NA        NA           1
[6096,]        1       NA       NA         1        NA        NA           1
[6097,]        1       NA       NA         1        NA        NA           1
[6098,]        1       NA       NA         1        NA        NA           1
[6099,]        1       NA       NA         1        NA        NA           1
[6100,]        1        1        3         1         1         2           1
[6101,]        1       NA       NA         1        NA        NA           1
[6102,]        3       NA       NA         3        NA        NA           3
[6103,]        2       NA       NA         2        NA        NA           2
[6104,]        1       NA       NA         1        NA        NA           1
[6105,]        3       NA       NA         3        NA        NA           3
[6106,]        1        1        3         1         1         2           1
[6107,]        1        2       NA         1         2        NA           1
[6108,]        1        3        3         1         2         3           1
[6109,]        1       NA       NA         1        NA        NA           1
[6110,]        1        1       NA         1         1        NA           1
[6111,]        1        1        2         1         1         2           1
[6112,]        1       NA       NA         1        NA        NA           1
[6113,]        1        2        3         1         2         3           1
[6114,]        1        2        3         1         2         3           1
[6115,]        1       NA       NA         1        NA        NA           1
[6116,]        3       NA       NA         3        NA        NA           3
[6117,]        1       NA       NA         1        NA        NA           1
[6118,]        1        2        3         1         2         3           1
[6119,]        2        2       NA         2         2        NA           2
[6120,]        1        1        3         1         1         2           1
[6121,]        3       NA       NA         3        NA        NA           3
[6122,]        1        1        3         1         1         2           1
[6123,]        1        2        2         1         2         2           1
[6124,]        1       NA       NA         1        NA        NA           1
[6125,]        1        2        2         1         2         2           1
[6126,]        1       NA       NA         1        NA        NA           1
[6127,]        1        3        3         1         2         3           1
[6128,]        3       NA       NA         3        NA        NA           3
[6129,]        1       NA       NA         1        NA        NA           1
[6130,]        2        2       NA         2         2        NA           2
[6131,]        2        2       NA         2         2        NA           2
[6132,]        1       NA       NA         1        NA        NA           1
[6133,]        1       NA       NA         1        NA        NA           1
[6134,]        1       NA       NA         1        NA        NA           1
[6135,]        1       NA       NA         1        NA        NA           1
[6136,]        1        1       NA         1         1        NA           1
[6137,]        1       NA       NA         1        NA        NA           1
[6138,]        1       NA       NA         1        NA        NA           1
[6139,]        1        2       NA         1         2        NA           1
[6140,]        2        3       NA         2         3        NA           2
[6141,]        1       NA       NA         1        NA        NA           1
[6142,]        1       NA       NA         1        NA        NA           1
[6143,]        1       NA       NA         1        NA        NA           1
[6144,]        2        3       NA         2         3        NA           2
[6145,]        1       NA       NA         1        NA        NA           1
[6146,]        2        3       NA         2         3        NA           2
[6147,]        1       NA       NA         1        NA        NA           1
[6148,]        1       NA       NA         1        NA        NA           1
[6149,]        3        2       NA         3         2        NA           3
[6150,]        1       NA       NA         1        NA        NA           1
[6151,]        1       NA       NA         1        NA        NA           1
[6152,]        1       NA       NA         1        NA        NA           1
[6153,]        1       NA       NA         1        NA        NA           1
[6154,]        1       NA       NA         1        NA        NA           1
[6155,]        2        3       NA         2         3        NA           2
[6156,]        1       NA       NA         1        NA        NA           1
[6157,]        1       NA       NA         1        NA        NA           1
[6158,]        1       NA       NA         1        NA        NA           1
[6159,]        1        2       NA         1         2        NA           1
[6160,]        1       NA       NA         1        NA        NA           1
[6161,]        1       NA       NA         1        NA        NA           1
[6162,]        1        3       NA         1         3        NA           1
[6163,]        1        3       NA         1         3        NA           1
[6164,]        1        3       NA         1         3        NA           1
[6165,]        1        3       NA         1         3        NA           1
[6166,]        1        3        3         1         3         3           1
[6167,]        1        3        2         1         3         2           1
[6168,]        1        2        2         1         2         2           1
[6169,]        1        2       NA         1         2        NA           1
[6170,]        1        2       NA         1         2        NA           1
[6171,]        1       NA       NA         1        NA        NA           1
[6172,]        1       NA       NA         1        NA        NA           1
[6173,]        1        3        2         1         3         2           1
[6174,]        1        3        3         1         3         3           1
[6175,]        1        1       NA         1         1        NA           1
[6176,]        1        1       NA         1         1        NA           1
[6177,]        1        1        2         1         1         2           1
[6178,]        1        3        2         1         3         2           1
[6179,]       NA       NA       NA        NA        NA        NA          NA
[6180,]        2        2        3         2         2         3           2
[6181,]        1        2       NA         1         2        NA           1
[6182,]        1        3        2         1         3         2           1
[6183,]        1        1       NA         1         1        NA           1
[6184,]        1        1       NA         1         1        NA           1
[6185,]        1        2        3         2         2         3           1
[6186,]        1        2        3         1         2         3           1
[6187,]        1        2        3         1         2         3           1
[6188,]        1        2        3         1         2         3           1
[6189,]        1        2       NA         1         2        NA           1
[6190,]        1        3       NA         1         3        NA           1
[6191,]        1        2        3         2         2         3           1
[6192,]        1        2       NA         1         2        NA           1
[6193,]        1        1       NA         1         1        NA           1
[6194,]        1        1       NA         1         1        NA           1
[6195,]        1        3        2         1         3         2           1
[6196,]        1        3       NA         1         3        NA           1
[6197,]        1        1        2         1         1         2           1
[6198,]        1        2        3         2         2         3           1
[6199,]        1        3        2         1         3         2           1
[6200,]        1        2       NA         1         2        NA           1
[6201,]        3        2       NA         3         2        NA           3
[6202,]        1        2       NA         1         2        NA           1
[6203,]        1        1       NA         1         1        NA           1
[6204,]        1        3       NA         1         3        NA           1
[6205,]        2        2        3         2         2         3           2
[6206,]        3        2       NA         3         2        NA           3
[6207,]        1        3        3         1         3         3           1
[6208,]        1        2       NA         1         2        NA           1
[6209,]        1        3        3         1         3         3           1
[6210,]        1       NA       NA         1        NA        NA           1
[6211,]        1       NA       NA         1        NA        NA           1
[6212,]        1       NA       NA         1        NA        NA           1
[6213,]        1        1       NA         1         1        NA           1
[6214,]        1       NA       NA         1        NA        NA           1
[6215,]        1       NA       NA         1        NA        NA           1
[6216,]        1        2       NA         1         2        NA           1
[6217,]        1       NA       NA         1        NA        NA           1
[6218,]        1       NA       NA         1        NA        NA           1
[6219,]        1       NA       NA         1        NA        NA           1
[6220,]        2        3       NA         2         3        NA           2
[6221,]        1       NA       NA         1        NA        NA           1
[6222,]        2        3       NA         2         3        NA           2
[6223,]        1       NA       NA         1        NA        NA           1
[6224,]        1       NA       NA         1        NA        NA           1
[6225,]        3        3       NA         3         3        NA           3
[6226,]        1       NA       NA         1        NA        NA           1
[6227,]        1       NA       NA         1        NA        NA           1
[6228,]        1       NA       NA         1        NA        NA           1
[6229,]        1       NA       NA         1        NA        NA           1
[6230,]        2        3       NA         2         3        NA           2
[6231,]        1       NA       NA         1        NA        NA           1
[6232,]        1        2        3         1         2         3           1
[6233,]        1       NA       NA         1        NA        NA           1
[6234,]        1       NA       NA         1        NA        NA           1
[6235,]        1        2       NA         1         2        NA           1
[6236,]        1       NA       NA         1        NA        NA           1
[6237,]        1        3       NA         1         3        NA           1
[6238,]        1        3       NA         1         3        NA           1
[6239,]        1        3       NA         1         3        NA           1
[6240,]        1        3       NA         1         3        NA           1
[6241,]        1        3        3         1         3         3           1
[6242,]        1        2        2         1         2         2           1
[6243,]        1        2        2         1         2         2           1
[6244,]        1        2       NA         1         2        NA           1
[6245,]        1        2       NA         1         2        NA           1
[6246,]        1       NA       NA         1        NA        NA           1
[6247,]        1       NA       NA         1        NA        NA           1
[6248,]        1        2        2         1         2         2           1
[6249,]        1        3        3         1         3         3           1
[6250,]        1        1       NA         1         1        NA           1
[6251,]        1        1       NA         1         1        NA           1
[6252,]        1        1        2         1         1         2           1
[6253,]        1        3        3         1         3         2           1
[6254,]        2        2        3         2         2         3           2
[6255,]        1        3        3         1         3         2           1
[6256,]        1        1       NA         1         1        NA           1
[6257,]        1        1       NA         1         1        NA           1
[6258,]        2        2        3         2         2         3           2
[6259,]        1        2        3         1         2         3           1
[6260,]        1        2        3         1         2         3           1
[6261,]        1        2        3         1         2         3           1
[6262,]        1        2       NA         1         2        NA           1
[6263,]        1        3       NA         1         3        NA           1
[6264,]        2        2        3         2         2         3           2
[6265,]        1        2       NA         1         2        NA           1
[6266,]        1        1       NA         1         1        NA           1
[6267,]        1        3        2         1         3         2           1
[6268,]        1        3       NA         1         3        NA           1
[6269,]        1        1        2         1         1         2           1
[6270,]        2        2        3         2         2         3           2
[6271,]        1        3        2         1         3         2           1
[6272,]        1        2       NA         1         2        NA           1
[6273,]       NA        3       NA         3         3        NA          NA
[6274,]        1        1       NA         1         1        NA           1
[6275,]        1        3       NA         1         3        NA           1
[6276,]        2        2        3         2         2         3           2
[6277,]        3        3       NA         3         3        NA           3
[6278,]        1        3        3         1         3         3           1
[6279,]        1        2       NA         1         2        NA           1
[6280,]        1        3        3         1         3         3           1
[6281,]        1       NA       NA         1        NA        NA           1
[6282,]        1        1        3         1         1         3           1
[6283,]        1       NA       NA         1        NA        NA           1
[6284,]        1       NA       NA         1        NA        NA           1
[6285,]        1        3        3         1         3         2           1
[6286,]        1        3        2         1         3         2           1
[6287,]        1        1        3         1         1         3           1
[6288,]        1        3        3         1         3         3           1
[6289,]        2        3       NA         2         3        NA           2
[6290,]        1        3        3         1         3         2           1
[6291,]        1        2       NA         1         2        NA           1
[6292,]        1        1        2         1         1         2           1
[6293,]        2        3       NA         2         3        NA           2
[6294,]        1       NA       NA         1        NA        NA           1
[6295,]        1       NA       NA         1        NA        NA           1
[6296,]        1        3        3         1         3         3           1
[6297,]        1       NA       NA         1        NA        NA           1
[6298,]        1       NA       NA         1        NA        NA           1
[6299,]        1       NA       NA         1        NA        NA           1
[6300,]        2        2        3         2         2         3           2
[6301,]        1        2        2         1         2         2           1
[6302,]        1       NA       NA         1        NA        NA           1
[6303,]        2       NA       NA         2        NA        NA           2
[6304,]        1        2       NA         1         2        NA           1
[6305,]        1        3        3         1         3         3           1
[6306,]        1       NA       NA         1        NA        NA           1
[6307,]        1       NA       NA         1        NA        NA           1
[6308,]        1       NA       NA         1        NA        NA           1
[6309,]        2        2       NA         2         2        NA           2
[6310,]        1        3       NA         1         3        NA           1
[6311,]        1        2       NA         1         2        NA           1
[6312,]        1       NA       NA         1        NA        NA           1
[6313,]        1        1       NA         1         1        NA           1
[6314,]        1        2       NA         1         2        NA           1
[6315,]        1       NA       NA         1        NA        NA           1
[6316,]        1       NA       NA         1        NA        NA           1
[6317,]        1        2        3         1         2         3           1
[6318,]        1       NA       NA         1        NA        NA           1
[6319,]        1       NA       NA         1        NA        NA           1
[6320,]        1       NA       NA         1        NA        NA           1
[6321,]        1       NA       NA         1        NA        NA           1
[6322,]        1       NA       NA         1        NA        NA           1
[6323,]        1        2       NA         1         2        NA           1
[6324,]        2        2        3         2         2         3           2
[6325,]        3        2        3         3         2         3           3
[6326,]        1        2        2         1         2         3           1
[6327,]        1        2        3         1         2         3           1
[6328,]        1       NA       NA         1        NA        NA           1
[6329,]        1        2        2         1         2         3           1
[6330,]        1        2       NA         1         2        NA           1
[6331,]        1        2       NA         1         2        NA           1
[6332,]        1        2       NA         1         2        NA           1
[6333,]        1       NA       NA         1        NA        NA           1
[6334,]        1       NA       NA         1        NA        NA           1
[6335,]        1       NA       NA         1        NA        NA           1
[6336,]        1       NA       NA         1        NA        NA           1
[6337,]        1        2        2         1         2         2           1
[6338,]        1        2       NA         1         2        NA           1
[6339,]        2        1       NA         2         1        NA           2
[6340,]        1        2       NA         1         2        NA           1
[6341,]        1        2       NA         1         2        NA           1
[6342,]        1        2       NA         1         2        NA           1
[6343,]        1        2        2         1         2         2           1
[6344,]        1        2        3         1         2         3           1
[6345,]        1       NA       NA         1        NA        NA           1
[6346,]        1       NA       NA         1        NA        NA           1
[6347,]        1       NA       NA         1        NA        NA           1
[6348,]        1        2        2         1         2         2           1
[6349,]        1        2        2         1         2         2           1
[6350,]        1       NA       NA         1        NA        NA           1
[6351,]        2       NA       NA         2        NA        NA           2
[6352,]        1       NA       NA         1        NA        NA           1
[6353,]        2       NA       NA         2        NA        NA           2
[6354,]        1       NA       NA         1        NA        NA           1
[6355,]        1       NA       NA         1        NA        NA           1
[6356,]        1       NA       NA         1        NA        NA           1
[6357,]        1        1       NA         1         1        NA           1
[6358,]        1        1        3         1         1         3           1
[6359,]        1        2        2         1         2         2           1
[6360,]        1        2       NA         1         2        NA           1
[6361,]        1        1        3         1         1         3           1
[6362,]        1        2       NA         1         2        NA           1
[6363,]        1        2       NA         1         2        NA           1
[6364,]        1        2       NA         1         2        NA           1
[6365,]        1        2        2         1         2         2           1
[6366,]        1        2        2         1         2         2           1
[6367,]        1        2        2         1         2         2           1
[6368,]        2       NA       NA         2        NA        NA           2
[6369,]        1        2       NA         1         2        NA           1
[6370,]        1        2       NA         1         2        NA           1
[6371,]        1        2       NA         1         2        NA           1
[6372,]        1        2        2         1         2         2           1
[6373,]        1        1       NA         1         1        NA           1
[6374,]        1        2        2         1         2         2           1
[6375,]        1        1       NA         1         1        NA           1
[6376,]        1        2        2         1         2         2           1
[6377,]        1       NA       NA         1        NA        NA           1
[6378,]        1       NA       NA         1        NA        NA           1
[6379,]        1       NA       NA         1        NA        NA           1
[6380,]        1        1        3         1         1         3           1
[6381,]        1        2        2         1         2         2           1
[6382,]        1       NA       NA         1        NA        NA           1
[6383,]        1        1       NA         1         1        NA           1
[6384,]        1       NA       NA         1        NA        NA           1
[6385,]        1        2        2         1         2         2           1
[6386,]        1       NA       NA         1        NA        NA           1
[6387,]        2        2        3         2         2         3           2
[6388,]        2        3        3         1         3         3           2
[6389,]        1        3        3         1         3         2           1
[6390,]        2        2        3         2         2         3           2
[6391,]        1       NA       NA         1        NA        NA           1
[6392,]        1       NA       NA         1        NA        NA           1
[6393,]        3        2       NA         3         2        NA           3
[6394,]        3        2       NA         3         2        NA           3
[6395,]        3        2       NA         3         2        NA           3
[6396,]        2        3       NA         2         3        NA           2
[6397,]        1        3       NA         1         3        NA           1
[6398,]        1        2       NA         1         2        NA           1
[6399,]        1        2       NA         1         2        NA           1
[6400,]        1        2       NA         1         2        NA           1
[6401,]        1       NA       NA         1        NA        NA           1
[6402,]        2       NA       NA         2        NA        NA           2
[6403,]        2       NA       NA         2        NA        NA           2
[6404,]        2       NA       NA         2        NA        NA           2
[6405,]        2       NA       NA         2        NA        NA           2
[6406,]        1       NA       NA         1        NA        NA           1
[6407,]        1       NA       NA         1        NA        NA           1
[6408,]        1       NA       NA         1        NA        NA           1
[6409,]        1       NA       NA         1        NA        NA           1
[6410,]        1        1       NA         1         1        NA           1
[6411,]        1       NA       NA         1        NA        NA           1
[6412,]        1        2       NA         1         2        NA           1
[6413,]        2        3       NA         2         3        NA           2
[6414,]        1       NA       NA         1        NA        NA           1
[6415,]        1       NA       NA         1        NA        NA           1
[6416,]        2        3       NA         2         3        NA           2
[6417,]        1       NA       NA         1        NA        NA           1
[6418,]        2        3       NA         2         3        NA           2
[6419,]        3        2       NA         3         2        NA           3
[6420,]        1       NA       NA         1        NA        NA           1
[6421,]        2        3       NA         2         3        NA           2
[6422,]        1       NA       NA         1        NA        NA           1
[6423,]        1        2        3         1         2         3           1
[6424,]        1       NA       NA         1        NA        NA           1
[6425,]        1        2       NA         1         2        NA           1
[6426,]        1        3       NA         1         2        NA           1
[6427,]        1        3       NA         1         2        NA           1
[6428,]        1        3       NA         1         2        NA           1
[6429,]        1        3       NA         1         2        NA           1
[6430,]        1        3        3         1         3         3           1
[6431,]        1        3        3         1         3         3           1
[6432,]        1        2       NA         1         2        NA           1
[6433,]        1       NA       NA         1        NA        NA           1
[6434,]        1       NA       NA         1        NA        NA           1
[6435,]        1        3        3         1         3         3           1
[6436,]        1        3        3         1         3         3           1
[6437,]        1        2        3         1         2         3           1
[6438,]        2        2        3         2         2         3           2
[6439,]        2        3        3         1         3         3           2
[6440,]        1        1       NA         1         1        NA           1
[6441,]        1        3       NA         1         3        NA           1
[6442,]        2        2        3         2         2         3           2
[6443,]        1        2       NA         1         2        NA           1
[6444,]        1        3        3         1         3         3           1
[6445,]        1        3       NA         1         3        NA           1
[6446,]        1        2        3         1         2         3           1
[6447,]        2        2        3         2         2         3           2
[6448,]        1        3        3         1         3         3           1
[6449,]        1        2       NA         1         2        NA           1
[6450,]        3        2       NA         3         2        NA           3
[6451,]        1        2       NA         1         2        NA           1
[6452,]        1        1       NA         1         1        NA           1
[6453,]        1        3       NA         1         3        NA           1
[6454,]        2        2        3         2         2         3           2
[6455,]        3        2       NA         3         2        NA           3
[6456,]        1        2       NA         1         2        NA           1
[6457,]        1        3        3         1         3         3           1
[6458,]        1       NA       NA         1        NA        NA           1
[6459,]        1        2        3         1         1         3           1
[6460,]        1        3        3         1         3         2           1
[6461,]        1        2        3         1         2         3           1
[6462,]        1        3        3         1         3         2           1
[6463,]        2        3       NA         2         3        NA           2
[6464,]        2        3        3         1         3         3           2
[6465,]        1        2       NA         1         2        NA           1
[6466,]        1        2        3         1         2         3           1
[6467,]        2        3       NA         2         3        NA           2
[6468,]        1       NA       NA         1        NA        NA           1
[6469,]        1       NA       NA         1        NA        NA           1
[6470,]        1       NA       NA         1        NA        NA           1
[6471,]        1       NA       NA         1        NA        NA           1
[6472,]        1        2       NA         1         2        NA           1
[6473,]        1        3        3         1         3         3           1
[6474,]        1       NA       NA         1        NA        NA           1
[6475,]        1       NA       NA         1        NA        NA           1
[6476,]        1       NA       NA         1        NA        NA           1
        nsumbabig.2 nsumbabig.3 dbhgrowth bagrowth
   [1,]          NA          NA        NA       NA
   [2,]           1           2        NA       NA
   [3,]           1          NA        NA       NA
   [4,]          NA          NA        NA       NA
   [5,]           1           1        NA       NA
   [6,]          NA          NA        NA       NA
   [7,]           1           2        NA       NA
   [8,]           2           3        NA       NA
   [9,]           2          NA        NA       NA
  [10,]           2           2        NA       NA
  [11,]           1           1        NA       NA
  [12,]           1          NA        NA       NA
  [13,]           1           2        NA       NA
  [14,]          NA          NA        NA       NA
  [15,]          NA          NA        NA       NA
  [16,]          NA          NA        NA       NA
  [17,]          NA          NA        NA       NA
  [18,]          NA          NA        NA       NA
  [19,]          NA          NA         1        1
  [20,]           1           2         2        2
  [21,]           1          NA         2        1
  [22,]          NA          NA         2        2
  [23,]          NA          NA        NA       NA
  [24,]          NA          NA        NA       NA
  [25,]          NA          NA        NA       NA
  [26,]          NA          NA        NA       NA
  [27,]          NA          NA        NA       NA
  [28,]          NA          NA        NA       NA
  [29,]          NA          NA        NA       NA
  [30,]           2          NA        NA       NA
  [31,]          NA          NA        NA       NA
  [32,]          NA          NA        NA       NA
  [33,]          NA          NA        NA       NA
  [34,]           1           3        NA       NA
  [35,]           1           1        NA       NA
  [36,]           3          NA        NA       NA
  [37,]           1          NA        NA       NA
  [38,]           1           3        NA       NA
  [39,]           1          NA        NA       NA
  [40,]          NA          NA        NA       NA
  [41,]           1           3        NA       NA
  [42,]           1           2        NA       NA
  [43,]           1           1        NA       NA
  [44,]          NA          NA        NA       NA
  [45,]          NA          NA        NA       NA
  [46,]           2          NA        NA       NA
  [47,]           2          NA        NA       NA
  [48,]           2          NA        NA       NA
  [49,]          NA          NA        NA       NA
  [50,]           2          NA        NA       NA
  [51,]           2           3        NA       NA
  [52,]           2           3        NA       NA
  [53,]          NA          NA         2        2
  [54,]           1           2         2        2
  [55,]           2           3         1        1
  [56,]           2          NA        NA       NA
  [57,]          NA          NA        NA       NA
  [58,]          NA          NA        NA       NA
  [59,]           2          NA        NA       NA
  [60,]          NA          NA        NA       NA
  [61,]           2          NA        NA       NA
  [62,]           2          NA        NA       NA
  [63,]          NA          NA        NA       NA
  [64,]          NA          NA        NA       NA
  [65,]          NA          NA        NA       NA
  [66,]           2          NA         2        1
  [67,]           1           1         3        3
  [68,]           1          NA         1        1
  [69,]          NA          NA        NA       NA
  [70,]          NA          NA        NA       NA
  [71,]           2           3        NA       NA
  [72,]          NA          NA        NA       NA
  [73,]          NA          NA        NA       NA
  [74,]          NA          NA        NA       NA
  [75,]           1           2         2        1
  [76,]          NA          NA         2        2
  [77,]          NA          NA        NA       NA
  [78,]          NA          NA        NA       NA
  [79,]           1          NA        NA       NA
  [80,]          NA          NA        NA       NA
  [81,]           1          NA        NA       NA
  [82,]           1          NA        NA       NA
  [83,]          NA          NA        NA       NA
  [84,]          NA          NA        NA       NA
  [85,]           1          NA        NA       NA
  [86,]           1           3        NA       NA
  [87,]          NA          NA         1        1
  [88,]          NA          NA         2        1
  [89,]          NA          NA         1        1
  [90,]           1           2         2        3
  [91,]           1          NA         2        1
  [92,]          NA          NA         2        3
  [93,]           1           3        NA       NA
  [94,]           1           2        NA       NA
  [95,]          NA          NA        NA       NA
  [96,]          NA          NA        NA       NA
  [97,]           1          NA        NA       NA
  [98,]           1           2        NA       NA
  [99,]          NA          NA         1        1
 [100,]          NA          NA         1        1
 [101,]          NA          NA         1        1
 [102,]          NA          NA         1        1
 [103,]           1           1         2        1
 [104,]           3          NA         1        1
 [105,]           1           3         1        1
 [106,]           1          NA        NA       NA
 [107,]          NA          NA        NA       NA
 [108,]           1           3         1        1
 [109,]           1           2         1        1
 [110,]           1           1         2        1
 [111,]           2          NA         1        1
 [112,]           2          NA         1        1
 [113,]           2          NA         1        1
 [114,]           1           3         1        1
 [115,]          NA          NA         2        2
 [116,]          NA          NA        NA       NA
 [117,]           2          NA        NA       NA
 [118,]           2          NA         1        1
 [119,]          NA          NA         1        1
 [120,]           2          NA         1        1
 [121,]           2          NA         1        1
 [122,]          NA          NA         1        1
 [123,]          NA          NA         1        1
 [124,]          NA          NA         1        1
 [125,]           2          NA         2        1
 [126,]           1           1         2        3
 [127,]           2          NA        NA       NA
 [128,]          NA          NA        NA       NA
 [129,]          NA          NA         1        1
 [130,]           2           3         1        1
 [131,]          NA          NA         3        2
 [132,]          NA          NA         2        1
 [133,]           1           3         2        2
 [134,]          NA          NA         2        2
 [135,]           1           2        NA       NA
 [136,]           1          NA         2        1
 [137,]          NA          NA         1        1
 [138,]          NA          NA         2        1
 [139,]           2          NA        NA       NA
 [140,]           2          NA        NA       NA
 [141,]           1          NA        NA       NA
 [142,]           2          NA        NA       NA
 [143,]           2           2        NA       NA
 [144,]           2          NA        NA       NA
 [145,]           2           1        NA       NA
 [146,]           1           2        NA       NA
 [147,]           1           2        NA       NA
 [148,]          NA          NA        NA       NA
 [149,]           1           2        NA       NA
 [150,]           2           2        NA       NA
 [151,]           1          NA        NA       NA
 [152,]          NA          NA        NA       NA
 [153,]          NA          NA        NA       NA
 [154,]           1           2        NA       NA
 [155,]           1           1        NA       NA
 [156,]           1           2        NA       NA
 [157,]           1           1        NA       NA
 [158,]           1           1        NA       NA
 [159,]          NA          NA        NA       NA
 [160,]           2           2        NA       NA
 [161,]          NA          NA        NA       NA
 [162,]           1          NA        NA       NA
 [163,]           1           2        NA       NA
 [164,]           2           2        NA       NA
 [165,]           2          NA        NA       NA
 [166,]           1           2        NA       NA
 [167,]           2          NA         2        2
 [168,]           2          NA         2        2
 [169,]           2          NA         1        1
 [170,]           2           2         2        2
 [171,]           2          NA         2        1
 [172,]           2           2        NA       NA
 [173,]           3           3        NA       NA
 [174,]           2           3        NA       NA
 [175,]           2           3        NA       NA
 [176,]           2           3        NA       NA
 [177,]           2          NA        NA       NA
 [178,]           2          NA        NA       NA
 [179,]           2          NA        NA       NA
 [180,]           2          NA        NA       NA
 [181,]           2          NA        NA       NA
 [182,]           2          NA        NA       NA
 [183,]           2           3        NA       NA
 [184,]           1          NA        NA       NA
 [185,]           2           3        NA       NA
 [186,]           1          NA        NA       NA
 [187,]           1          NA        NA       NA
 [188,]           2          NA        NA       NA
 [189,]           2           3        NA       NA
 [190,]           3           3        NA       NA
 [191,]           2          NA        NA       NA
 [192,]           1          NA        NA       NA
 [193,]           3           2        NA       NA
 [194,]           3           3        NA       NA
 [195,]          NA          NA        NA       NA
 [196,]          NA          NA        NA       NA
 [197,]           2           3        NA       NA
 [198,]          NA          NA        NA       NA
 [199,]          NA          NA        NA       NA
 [200,]          NA          NA        NA       NA
 [201,]           3           3        NA       NA
 [202,]           2           3        NA       NA
 [203,]           2          NA        NA       NA
 [204,]          NA          NA        NA       NA
 [205,]          NA          NA        NA       NA
 [206,]          NA          NA        NA       NA
 [207,]           2          NA        NA       NA
 [208,]          NA          NA        NA       NA
 [209,]          NA          NA        NA       NA
 [210,]           2          NA        NA       NA
 [211,]           1           2         3        2
 [212,]           1           2         3        2
 [213,]          NA          NA         3        2
 [214,]           2           2         3        2
 [215,]          NA          NA        NA       NA
 [216,]           3          NA        NA       NA
 [217,]          NA          NA        NA       NA
 [218,]          NA          NA        NA       NA
 [219,]           2          NA        NA       NA
 [220,]          NA          NA        NA       NA
 [221,]          NA          NA        NA       NA
 [222,]          NA          NA        NA       NA
 [223,]          NA          NA        NA       NA
 [224,]           1           2        NA       NA
 [225,]          NA          NA        NA       NA
 [226,]          NA          NA        NA       NA
 [227,]           2          NA        NA       NA
 [228,]          NA          NA        NA       NA
 [229,]           1          NA        NA       NA
 [230,]           2          NA        NA       NA
 [231,]           1           2        NA       NA
 [232,]           1          NA        NA       NA
 [233,]           2          NA        NA       NA
 [234,]          NA          NA        NA       NA
 [235,]          NA          NA        NA       NA
 [236,]          NA          NA        NA       NA
 [237,]           2          NA        NA       NA
 [238,]          NA          NA        NA       NA
 [239,]           3          NA        NA       NA
 [240,]          NA          NA        NA       NA
 [241,]           2           3        NA       NA
 [242,]           2           2         2        1
 [243,]           2          NA         2        1
 [244,]          NA          NA         2        1
 [245,]          NA          NA         2        1
 [246,]           2           2         2        1
 [247,]           1           2         2        3
 [248,]           1           1         1        2
 [249,]           1           2         1        1
 [250,]          NA          NA         3        2
 [251,]           1          NA        NA       NA
 [252,]           2          NA        NA       NA
 [253,]           2          NA        NA       NA
 [254,]           1          NA        NA       NA
 [255,]           2          NA        NA       NA
 [256,]           2          NA        NA       NA
 [257,]           2           2        NA       NA
 [258,]           2          NA        NA       NA
 [259,]           2           3        NA       NA
 [260,]           2          NA        NA       NA
 [261,]           2           2        NA       NA
 [262,]           2           2        NA       NA
 [263,]           2          NA        NA       NA
 [264,]          NA          NA        NA       NA
 [265,]          NA          NA        NA       NA
 [266,]          NA          NA        NA       NA
 [267,]          NA          NA        NA       NA
 [268,]          NA          NA        NA       NA
 [269,]          NA          NA        NA       NA
 [270,]           2           3        NA       NA
 [271,]          NA          NA        NA       NA
 [272,]          NA          NA        NA       NA
 [273,]          NA          NA        NA       NA
 [274,]          NA          NA        NA       NA
 [275,]           1          NA        NA       NA
 [276,]          NA          NA        NA       NA
 [277,]           2          NA        NA       NA
 [278,]          NA          NA        NA       NA
 [279,]           2           2        NA       NA
 [280,]           2           2        NA       NA
 [281,]           2           2         3        3
 [282,]          NA          NA         2        2
 [283,]           2          NA         2        1
 [284,]           2           3         2        2
 [285,]           2           2         2        1
 [286,]           2          NA         1        1
 [287,]           1           2         2        1
 [288,]          NA          NA        NA       NA
 [289,]          NA          NA        NA       NA
 [290,]          NA          NA        NA       NA
 [291,]          NA          NA        NA       NA
 [292,]          NA          NA        NA       NA
 [293,]           2           3        NA       NA
 [294,]           1          NA        NA       NA
 [295,]          NA          NA        NA       NA
 [296,]           2          NA        NA       NA
 [297,]          NA          NA        NA       NA
 [298,]          NA          NA        NA       NA
 [299,]           2           3        NA       NA
 [300,]          NA          NA        NA       NA
 [301,]           3           2        NA       NA
 [302,]           2           3        NA       NA
 [303,]           2          NA        NA       NA
 [304,]           2           2        NA       NA
 [305,]          NA          NA        NA       NA
 [306,]           2           2        NA       NA
 [307,]           1          NA        NA       NA
 [308,]          NA          NA        NA       NA
 [309,]          NA          NA        NA       NA
 [310,]          NA          NA        NA       NA
 [311,]          NA          NA        NA       NA
 [312,]           3           3        NA       NA
 [313,]           2           2        NA       NA
 [314,]           2          NA        NA       NA
 [315,]          NA          NA        NA       NA
 [316,]           2          NA        NA       NA
 [317,]           1          NA        NA       NA
 [318,]          NA          NA        NA       NA
 [319,]           2          NA         2        2
 [320,]           2          NA         2        2
 [321,]           2          NA         1        1
 [322,]           2           2         1        2
 [323,]           2          NA         1        1
 [324,]           2           3         1        1
 [325,]           2           3         2        1
 [326,]           3          NA         2        1
 [327,]           3          NA         1        1
 [328,]           2           3         1        1
 [329,]           1          NA         2        1
 [330,]           2          NA         1        1
 [331,]           3           3         2        1
 [332,]           1          NA         1        1
 [333,]           3           2         1        1
 [334,]           2          NA        NA       NA
 [335,]           2           3        NA       NA
 [336,]          NA          NA        NA       NA
 [337,]          NA          NA         3        1
 [338,]           2           3         2        1
 [339,]          NA          NA         1        1
 [340,]          NA          NA         1        1
 [341,]           3          NA         1        1
 [342,]          NA          NA         1        1
 [343,]           3          NA         1        1
 [344,]           1           2         2        2
 [345,]           1           2         3        3
 [346,]          NA          NA         3        3
 [347,]           2           2         2        2
 [348,]          NA          NA         3        2
 [349,]          NA          NA         2        1
 [350,]          NA          NA         2        1
 [351,]           2          NA         2        1
 [352,]          NA          NA         1        1
 [353,]          NA          NA         1        1
 [354,]           1           2         2        2
 [355,]          NA          NA         1        1
 [356,]           1          NA         3        2
 [357,]           1           2         1        1
 [358,]           1          NA         3        2
 [359,]           2          NA         2        1
 [360,]          NA          NA         2        1
 [361,]           2          NA         1        1
 [362,]           2           2         1        1
 [363,]           1           2         1        1
 [364,]           2          NA         2        2
 [365,]          NA          NA         2        2
 [366,]          NA          NA         2        1
 [367,]           1           2         3        2
 [368,]           1           1         1        2
 [369,]           1           1         2        2
 [370,]          NA          NA         3        3
 [371,]           1          NA         1        1
 [372,]           2          NA         1        1
 [373,]           2          NA         1        1
 [374,]           1           2         1        1
 [375,]          NA          NA         1        1
 [376,]           1          NA         2        1
 [377,]          NA          NA        NA       NA
 [378,]           2          NA         1        1
 [379,]          NA          NA         1        1
 [380,]           1           2         1        1
 [381,]           2           2         1        1
 [382,]           2           2         2        3
 [383,]          NA          NA         3        3
 [384,]           2          NA         1        1
 [385,]           2           3         1        1
 [386,]           2           3         1        1
 [387,]           2          NA         1        1
 [388,]           2           2         1        1
 [389,]           3          NA        NA       NA
 [390,]          NA          NA         1        1
 [391,]          NA          NA         1        1
 [392,]          NA          NA         1        1
 [393,]           2          NA         2        1
 [394,]          NA          NA         1        1
 [395,]          NA          NA         2        1
 [396,]           3           2         2        1
 [397,]           2           3         1        1
 [398,]          NA          NA         2        1
 [399,]           3           3         1        1
 [400,]           2           3         1        1
 [401,]           2          NA        NA       NA
 [402,]           2          NA         2        2
 [403,]           1          NA         2        1
 [404,]          NA          NA         1        1
 [405,]          NA          NA        NA       NA
 [406,]          NA          NA        NA       NA
 [407,]           3           3        NA       NA
 [408,]           1           2        NA       NA
 [409,]           2           3        NA       NA
 [410,]           1          NA        NA       NA
 [411,]          NA          NA        NA       NA
 [412,]          NA          NA        NA       NA
 [413,]           1          NA        NA       NA
 [414,]           1          NA        NA       NA
 [415,]           1          NA        NA       NA
 [416,]           2           3        NA       NA
 [417,]          NA          NA        NA       NA
 [418,]          NA          NA        NA       NA
 [419,]          NA          NA        NA       NA
 [420,]          NA          NA        NA       NA
 [421,]          NA          NA        NA       NA
 [422,]          NA          NA        NA       NA
 [423,]           3           2         2        2
 [424,]           1           2         3        3
 [425,]           1           3         2        1
 [426,]          NA          NA        NA       NA
 [427,]           2           3        NA       NA
 [428,]           2           3        NA       NA
 [429,]           2          NA        NA       NA
 [430,]          NA          NA        NA       NA
 [431,]           2           3        NA       NA
 [432,]           2           3        NA       NA
 [433,]           2          NA        NA       NA
 [434,]          NA          NA        NA       NA
 [435,]          NA          NA         2        1
 [436,]           1          NA         3        2
 [437,]           3           2        NA       NA
 [438,]           3           2        NA       NA
 [439,]           3           2        NA       NA
 [440,]           1           2        NA       NA
 [441,]           2          NA        NA       NA
 [442,]           2          NA        NA       NA
 [443,]           1           1        NA       NA
 [444,]           1           2        NA       NA
 [445,]           1          NA         3        2
 [446,]           1          NA         2        1
 [447,]           1          NA        NA       NA
 [448,]          NA          NA        NA       NA
 [449,]           1           1        NA       NA
 [450,]           1          NA        NA       NA
 [451,]          NA          NA        NA       NA
 [452,]           1          NA        NA       NA
 [453,]          NA          NA        NA       NA
 [454,]          NA          NA        NA       NA
 [455,]          NA          NA        NA       NA
 [456,]          NA          NA        NA       NA
 [457,]          NA          NA        NA       NA
 [458,]           1           3        NA       NA
 [459,]           1           2        NA       NA
 [460,]           1           1        NA       NA
 [461,]           1           3        NA       NA
 [462,]          NA          NA        NA       NA
 [463,]          NA          NA        NA       NA
 [464,]           1           1        NA       NA
 [465,]          NA          NA        NA       NA
 [466,]           1           2        NA       NA
 [467,]           3           3         1        1
 [468,]          NA          NA         1        1
 [469,]          NA          NA        NA       NA
 [470,]          NA          NA         1        1
 [471,]          NA          NA        NA       NA
 [472,]           3           3         2        2
 [473,]           1           2         3        3
 [474,]           2           3         2        1
 [475,]          NA          NA         1        1
 [476,]          NA          NA         2        1
 [477,]           1          NA         2        2
 [478,]           1          NA         3        2
 [479,]           2          NA         1        1
 [480,]           2          NA         1        1
 [481,]           2          NA         1        1
 [482,]          NA          NA         1        1
 [483,]          NA          NA         2        1
 [484,]          NA          NA         1        1
 [485,]           1           3         1        1
 [486,]           1           3         1        1
 [487,]           2          NA        NA       NA
 [488,]          NA          NA         1        1
 [489,]          NA          NA         1        1
 [490,]           1           3         3        1
 [491,]           2           3        NA       NA
 [492,]           1           3        NA       NA
 [493,]           1           2        NA       NA
 [494,]           2           2        NA       NA
 [495,]           1           2        NA       NA
 [496,]           2          NA        NA       NA
 [497,]           2          NA        NA       NA
 [498,]           3          NA        NA       NA
 [499,]           2           2        NA       NA
 [500,]          NA          NA        NA       NA
 [501,]           2          NA        NA       NA
 [502,]          NA          NA        NA       NA
 [503,]           2          NA        NA       NA
 [504,]          NA          NA        NA       NA
 [505,]          NA          NA        NA       NA
 [506,]           2           2        NA       NA
 [507,]          NA          NA        NA       NA
 [508,]           2           2        NA       NA
 [509,]           2          NA        NA       NA
 [510,]          NA          NA        NA       NA
 [511,]           2          NA        NA       NA
 [512,]          NA          NA        NA       NA
 [513,]          NA          NA        NA       NA
 [514,]           2           2        NA       NA
 [515,]           1           1        NA       NA
 [516,]           2           2        NA       NA
 [517,]           1           2        NA       NA
 [518,]          NA          NA        NA       NA
 [519,]          NA          NA        NA       NA
 [520,]           1          NA        NA       NA
 [521,]           1           1        NA       NA
 [522,]          NA          NA        NA       NA
 [523,]          NA          NA        NA       NA
 [524,]           2          NA        NA       NA
 [525,]           1          NA        NA       NA
 [526,]          NA          NA        NA       NA
 [527,]           1           2        NA       NA
 [528,]          NA          NA        NA       NA
 [529,]           1          NA        NA       NA
 [530,]          NA          NA        NA       NA
 [531,]           1          NA        NA       NA
 [532,]          NA          NA        NA       NA
 [533,]          NA          NA        NA       NA
 [534,]          NA          NA        NA       NA
 [535,]           2           2        NA       NA
 [536,]           1          NA        NA       NA
 [537,]          NA          NA        NA       NA
 [538,]          NA          NA        NA       NA
 [539,]           1           1        NA       NA
 [540,]          NA          NA        NA       NA
 [541,]          NA          NA        NA       NA
 [542,]           2           3         1        1
 [543,]           2           3         1        1
 [544,]           1           2         2        2
 [545,]           3           3         2        1
 [546,]           1           1         2        1
 [547,]           2          NA         2        1
 [548,]           1          NA         3        2
 [549,]           3           2         1        1
 [550,]           2           3        NA       NA
 [551,]           3           3        NA       NA
 [552,]           3           3        NA       NA
 [553,]           2           3        NA       NA
 [554,]           2           3        NA       NA
 [555,]           3           3        NA       NA
 [556,]           2           3        NA       NA
 [557,]           2           3        NA       NA
 [558,]           2           3        NA       NA
 [559,]           2           3        NA       NA
 [560,]           3           3        NA       NA
 [561,]           3           3        NA       NA
 [562,]           2          NA        NA       NA
 [563,]           2          NA        NA       NA
 [564,]           3          NA        NA       NA
 [565,]           3          NA        NA       NA
 [566,]           3          NA        NA       NA
 [567,]           3           3        NA       NA
 [568,]           3          NA        NA       NA
 [569,]           3          NA        NA       NA
 [570,]          NA          NA        NA       NA
 [571,]          NA          NA        NA       NA
 [572,]           3          NA        NA       NA
 [573,]          NA          NA        NA       NA
 [574,]          NA          NA        NA       NA
 [575,]           3          NA        NA       NA
 [576,]          NA          NA        NA       NA
 [577,]          NA          NA        NA       NA
 [578,]          NA          NA        NA       NA
 [579,]          NA          NA        NA       NA
 [580,]          NA          NA        NA       NA
 [581,]          NA          NA        NA       NA
 [582,]           3          NA        NA       NA
 [583,]          NA          NA        NA       NA
 [584,]          NA          NA        NA       NA
 [585,]          NA          NA        NA       NA
 [586,]          NA          NA        NA       NA
 [587,]          NA          NA        NA       NA
 [588,]          NA          NA         1        1
 [589,]          NA          NA         1        1
 [590,]          NA          NA         2        1
 [591,]          NA          NA        NA       NA
 [592,]          NA          NA        NA       NA
 [593,]           2           3        NA       NA
 [594,]          NA          NA        NA       NA
 [595,]           2          NA        NA       NA
 [596,]          NA          NA        NA       NA
 [597,]          NA          NA        NA       NA
 [598,]          NA          NA        NA       NA
 [599,]          NA          NA        NA       NA
 [600,]           2           3        NA       NA
 [601,]           2           3        NA       NA
 [602,]          NA          NA        NA       NA
 [603,]          NA          NA        NA       NA
 [604,]           2          NA        NA       NA
 [605,]           2           2         1        1
 [606,]           2          NA        NA       NA
 [607,]          NA          NA         1        1
 [608,]           2           2         1        1
 [609,]           2          NA         1        1
 [610,]          NA          NA         1        1
 [611,]           2          NA         1        1
 [612,]          NA          NA         1        1
 [613,]          NA          NA         2        1
 [614,]           2           3         1        1
 [615,]           1           1         2        2
 [616,]           2           2         2        1
 [617,]           1           3         2        1
 [618,]           2           2        NA       NA
 [619,]           3          NA        NA       NA
 [620,]           2           2        NA       NA
 [621,]           3          NA        NA       NA
 [622,]          NA          NA        NA       NA
 [623,]          NA          NA        NA       NA
 [624,]           2          NA        NA       NA
 [625,]           2           2        NA       NA
 [626,]           2           2        NA       NA
 [627,]           2           2        NA       NA
 [628,]           2          NA        NA       NA
 [629,]           2           2        NA       NA
 [630,]           2           2        NA       NA
 [631,]           2          NA        NA       NA
 [632,]           2           3        NA       NA
 [633,]           2          NA        NA       NA
 [634,]           2          NA        NA       NA
 [635,]           2          NA        NA       NA
 [636,]          NA          NA        NA       NA
 [637,]           2           2        NA       NA
 [638,]           2           2        NA       NA
 [639,]           2           2        NA       NA
 [640,]           2           2        NA       NA
 [641,]           2          NA        NA       NA
 [642,]           2          NA        NA       NA
 [643,]           2           2        NA       NA
 [644,]           3          NA        NA       NA
 [645,]          NA          NA         2        1
 [646,]           1          NA         1        1
 [647,]          NA          NA         3        3
 [648,]           1          NA         2        2
 [649,]           2           2        NA       NA
 [650,]           1           2        NA       NA
 [651,]          NA          NA        NA       NA
 [652,]          NA          NA        NA       NA
 [653,]          NA          NA        NA       NA
 [654,]           2           2        NA       NA
 [655,]           2           2        NA       NA
 [656,]           2           2        NA       NA
 [657,]           1           2        NA       NA
 [658,]          NA          NA        NA       NA
 [659,]          NA          NA        NA       NA
 [660,]           2          NA        NA       NA
 [661,]           2          NA        NA       NA
 [662,]           2          NA        NA       NA
 [663,]           2           2        NA       NA
 [664,]           2           2        NA       NA
 [665,]           2           2        NA       NA
 [666,]           2           2        NA       NA
 [667,]           2           2        NA       NA
 [668,]           1           2        NA       NA
 [669,]           2           2        NA       NA
 [670,]           2           2        NA       NA
 [671,]           1           2        NA       NA
 [672,]           2           2        NA       NA
 [673,]           1           2        NA       NA
 [674,]           2           2        NA       NA
 [675,]           2           2        NA       NA
 [676,]           2           2        NA       NA
 [677,]           1           2        NA       NA
 [678,]           1           2        NA       NA
 [679,]           1           2        NA       NA
 [680,]           1           2        NA       NA
 [681,]           1           2        NA       NA
 [682,]           1           2        NA       NA
 [683,]           1           2        NA       NA
 [684,]          NA          NA         3        3
 [685,]           1          NA         2        1
 [686,]          NA          NA         1        1
 [687,]           2          NA         1        1
 [688,]          NA          NA         1        1
 [689,]          NA          NA         1        1
 [690,]          NA          NA         2        2
 [691,]           2           1         2        1
 [692,]           1          NA         2        1
 [693,]          NA          NA         2        2
 [694,]          NA          NA         1        1
 [695,]          NA          NA         2        1
 [696,]           2           2        NA       NA
 [697,]           2           2        NA       NA
 [698,]           2           2        NA       NA
 [699,]           2           2        NA       NA
 [700,]           2           2        NA       NA
 [701,]           2           2        NA       NA
 [702,]           2          NA        NA       NA
 [703,]           2          NA        NA       NA
 [704,]           1           2        NA       NA
 [705,]           1           2        NA       NA
 [706,]           1           2        NA       NA
 [707,]           1           2        NA       NA
 [708,]           1           2        NA       NA
 [709,]           1           2        NA       NA
 [710,]           1           2        NA       NA
 [711,]           1           2        NA       NA
 [712,]           1           2        NA       NA
 [713,]           1           2        NA       NA
 [714,]           1           2        NA       NA
 [715,]           2           2        NA       NA
 [716,]           1           2        NA       NA
 [717,]           2          NA        NA       NA
 [718,]           1           2        NA       NA
 [719,]           1           2        NA       NA
 [720,]           2          NA        NA       NA
 [721,]           2           2        NA       NA
 [722,]           2           2        NA       NA
 [723,]           2           2        NA       NA
 [724,]           2           3         2        1
 [725,]           2           3         2        1
 [726,]           1           1         2        2
 [727,]           3           3         2        1
 [728,]           1           2         2        2
 [729,]           2          NA         2        2
 [730,]           1          NA         2        2
 [731,]           3           3         1        1
 [732,]           3           3         1        1
 [733,]           3           3         1        1
 [734,]           3           3         1        1
 [735,]           3           3         1        1
 [736,]           3           3         1        1
 [737,]           3           3        NA       NA
 [738,]           2           3         1        1
 [739,]           2           3         1        1
 [740,]           3           3         1        1
 [741,]           3           3         1        1
 [742,]           2          NA         1        1
 [743,]           2          NA        NA       NA
 [744,]           2          NA         1        1
 [745,]           2          NA         1        1
 [746,]           2          NA         1        1
 [747,]           3           3         1        1
 [748,]           2          NA         1        1
 [749,]           2          NA         1        1
 [750,]          NA          NA         1        1
 [751,]           3          NA         1        1
 [752,]           3          NA         2        1
 [753,]          NA          NA         2        1
 [754,]          NA          NA         1        1
 [755,]          NA          NA         1        1
 [756,]          NA          NA         1        1
 [757,]          NA          NA         2        1
 [758,]           2          NA         1        1
 [759,]          NA          NA         2        1
 [760,]          NA          NA         1        1
 [761,]          NA          NA         1        1
 [762,]          NA          NA         2        1
 [763,]           2          NA        NA        2
 [764,]          NA          NA         2        2
 [765,]          NA          NA         3        2
 [766,]          NA          NA        NA       NA
 [767,]          NA          NA        NA       NA
 [768,]          NA          NA        NA       NA
 [769,]          NA          NA        NA       NA
 [770,]          NA          NA        NA       NA
 [771,]           3          NA        NA       NA
 [772,]          NA          NA        NA       NA
 [773,]          NA          NA        NA       NA
 [774,]           3           3        NA       NA
 [775,]           2          NA        NA       NA
 [776,]          NA          NA        NA       NA
 [777,]           2          NA         1        1
 [778,]          NA          NA         2        1
 [779,]          NA          NA         1        1
 [780,]          NA          NA         2        1
 [781,]          NA          NA         1        1
 [782,]           2           3         1        1
 [783,]           3           3         1        1
 [784,]          NA          NA         3        1
 [785,]          NA          NA         1        1
 [786,]           1           1        NA       NA
 [787,]           2           2        NA       NA
 [788,]          NA          NA        NA       NA
 [789,]          NA          NA        NA       NA
 [790,]           2           2        NA       NA
 [791,]           2           2        NA       NA
 [792,]           2           2        NA       NA
 [793,]           2          NA        NA       NA
 [794,]           2          NA        NA       NA
 [795,]           1          NA        NA       NA
 [796,]          NA          NA        NA       NA
 [797,]          NA          NA        NA       NA
 [798,]          NA          NA        NA       NA
 [799,]          NA          NA        NA       NA
 [800,]          NA          NA        NA       NA
 [801,]          NA          NA        NA       NA
 [802,]          NA          NA        NA       NA
 [803,]          NA          NA        NA       NA
 [804,]          NA          NA        NA       NA
 [805,]           2          NA        NA       NA
 [806,]           1           1        NA       NA
 [807,]           2           2        NA       NA
 [808,]           2           2        NA       NA
 [809,]          NA          NA        NA       NA
 [810,]          NA          NA        NA       NA
 [811,]           2          NA        NA       NA
 [812,]          NA          NA        NA       NA
 [813,]           1          NA        NA       NA
 [814,]          NA          NA        NA       NA
 [815,]           2           2        NA       NA
 [816,]           2          NA        NA       NA
 [817,]          NA          NA        NA       NA
 [818,]           2           2        NA       NA
 [819,]          NA          NA        NA       NA
 [820,]           1           1        NA       NA
 [821,]          NA          NA        NA       NA
 [822,]          NA          NA        NA       NA
 [823,]          NA          NA        NA       NA
 [824,]           2           3        NA       NA
 [825,]           1          NA        NA       NA
 [826,]           2          NA        NA       NA
 [827,]           3          NA        NA       NA
 [828,]          NA          NA        NA       NA
 [829,]           3          NA        NA       NA
 [830,]           2          NA        NA       NA
 [831,]           2          NA        NA       NA
 [832,]          NA          NA        NA       NA
 [833,]           2          NA        NA       NA
 [834,]          NA          NA        NA       NA
 [835,]          NA          NA        NA       NA
 [836,]           2           1        NA       NA
 [837,]           2          NA        NA       NA
 [838,]          NA          NA        NA       NA
 [839,]          NA          NA        NA       NA
 [840,]           1          NA        NA       NA
 [841,]          NA          NA        NA       NA
 [842,]          NA          NA        NA       NA
 [843,]          NA          NA        NA       NA
 [844,]           1          NA        NA       NA
 [845,]          NA          NA        NA       NA
 [846,]          NA          NA        NA       NA
 [847,]          NA          NA        NA       NA
 [848,]          NA          NA        NA       NA
 [849,]           1           2        NA       NA
 [850,]           1           2        NA       NA
 [851,]           1           1        NA       NA
 [852,]           1           2        NA       NA
 [853,]          NA          NA        NA       NA
 [854,]           2          NA        NA       NA
 [855,]           2          NA        NA       NA
 [856,]          NA          NA        NA       NA
 [857,]           1           1         3        3
 [858,]           2           2         2        1
 [859,]          NA          NA         2        1
 [860,]           2           2         1        1
 [861,]           2           2         1        1
 [862,]           2           2         1        1
 [863,]           2          NA         2        1
 [864,]           2          NA         1        1
 [865,]           1          NA         2        1
 [866,]          NA          NA         1        1
 [867,]          NA          NA         1        1
 [868,]          NA          NA         1        1
 [869,]          NA          NA         3        1
 [870,]          NA          NA         1        1
 [871,]          NA          NA         3        1
 [872,]          NA          NA         2        1
 [873,]          NA          NA         1        1
 [874,]          NA          NA        NA       NA
 [875,]          NA          NA        NA       NA
 [876,]           1          NA        NA       NA
 [877,]          NA          NA        NA       NA
 [878,]          NA          NA        NA       NA
 [879,]           2          NA        NA       NA
 [880,]           2          NA        NA       NA
 [881,]           2          NA        NA       NA
 [882,]          NA          NA        NA       NA
 [883,]          NA          NA        NA       NA
 [884,]          NA          NA        NA       NA
 [885,]           2           3        NA       NA
 [886,]           3           3        NA       NA
 [887,]           3          NA        NA       NA
 [888,]          NA          NA         1        1
 [889,]           2          NA         1        1
 [890,]           1           1         3        2
 [891,]           2           1         3        2
 [892,]           2           2         1        1
 [893,]          NA          NA         2        1
 [894,]           2          NA         3        2
 [895,]          NA          NA         1        1
 [896,]           1          NA         2        1
 [897,]          NA          NA         2        1
 [898,]           2           2         2        1
 [899,]          NA          NA         2        1
 [900,]           2           2         2        1
 [901,]           1           1         3        2
 [902,]          NA          NA         2        1
 [903,]           2           3        NA       NA
 [904,]           3           3        NA       NA
 [905,]          NA          NA         3        2
 [906,]          NA          NA         1        1
 [907,]           2           3        NA       NA
 [908,]           3           3        NA       NA
 [909,]           3           2        NA       NA
 [910,]           3           2        NA       NA
 [911,]           3           2        NA       NA
 [912,]           3           3        NA       NA
 [913,]           3           3        NA       NA
 [914,]           3           3        NA       NA
 [915,]           2          NA        NA       NA
 [916,]           3           2        NA       NA
 [917,]           2           3         1        1
 [918,]           2           3        NA       NA
 [919,]           2          NA        NA       NA
 [920,]           2           3        NA       NA
 [921,]           2           3        NA       NA
 [922,]           2           3        NA       NA
 [923,]          NA          NA        NA       NA
 [924,]           2           3        NA       NA
 [925,]           1          NA         3        2
 [926,]           2          NA         3        1
 [927,]           3          NA         1        1
 [928,]          NA          NA         2        1
 [929,]           2          NA        NA       NA
 [930,]           2          NA         2        1
 [931,]           2          NA         2        1
 [932,]           3          NA         1        1
 [933,]          NA          NA         1        1
 [934,]           2          NA         1        1
 [935,]          NA          NA         3        2
 [936,]          NA          NA         2        1
 [937,]           3           2        NA       NA
 [938,]           3           2        NA       NA
 [939,]           3           2        NA       NA
 [940,]           3           2        NA       NA
 [941,]           3           2        NA       NA
 [942,]           3           2        NA       NA
 [943,]           2           3        NA       NA
 [944,]           2           3        NA       NA
 [945,]           2           3        NA       NA
 [946,]           3           2        NA       NA
 [947,]           3           3        NA       NA
 [948,]           2           3        NA       NA
 [949,]           3           3        NA       NA
 [950,]           2           3        NA       NA
 [951,]           2           3        NA       NA
 [952,]           2           3        NA       NA
 [953,]           3           3        NA       NA
 [954,]           3           3        NA       NA
 [955,]           3           3        NA       NA
 [956,]           3           2        NA       NA
 [957,]           2           2         1        1
 [958,]           2          NA        NA       NA
 [959,]          NA          NA        NA       NA
 [960,]          NA          NA        NA       NA
 [961,]          NA          NA        NA       NA
 [962,]           3           2        NA       NA
 [963,]          NA          NA        NA       NA
 [964,]           2           3        NA       NA
 [965,]          NA          NA        NA       NA
 [966,]          NA          NA        NA       NA
 [967,]           2          NA        NA       NA
 [968,]           2          NA         1        1
 [969,]          NA          NA         1        1
 [970,]          NA          NA         1        1
 [971,]           1          NA         3        2
 [972,]          NA          NA         2        1
 [973,]          NA          NA         1        1
 [974,]          NA          NA         2        1
 [975,]           1          NA         1        1
 [976,]          NA          NA         1        1
 [977,]          NA          NA         1        1
 [978,]          NA          NA         3        2
 [979,]          NA          NA         1        1
 [980,]           1           2         2        1
 [981,]           2           2         2        1
 [982,]           1           1         3        2
 [983,]           1           2         2        1
 [984,]          NA          NA         3        2
 [985,]           2          NA         2        1
 [986,]           2          NA         3        2
 [987,]          NA          NA         1        1
 [988,]          NA          NA        NA       NA
 [989,]          NA          NA        NA       NA
 [990,]          NA          NA        NA       NA
 [991,]          NA          NA        NA       NA
 [992,]          NA          NA        NA       NA
 [993,]           2          NA        NA       NA
 [994,]           1           2        NA       NA
 [995,]          NA          NA        NA       NA
 [996,]           1           3        NA       NA
 [997,]           2           2        NA       NA
 [998,]           2           2        NA       NA
 [999,]           2           2        NA       NA
[1000,]           2           2        NA       NA
[1001,]          NA          NA        NA       NA
[1002,]           2           2        NA       NA
[1003,]           2           2        NA       NA
[1004,]           1           3        NA       NA
[1005,]           1           3        NA       NA
[1006,]          NA          NA         3        2
[1007,]           2           3         1        1
[1008,]           3           3         1        1
[1009,]           3           3         1        1
[1010,]           3           3         1        1
[1011,]           2           3         1        1
[1012,]           2          NA         1        1
[1013,]           2           3         1        1
[1014,]          NA          NA         1        1
[1015,]           2           3         1        1
[1016,]           1          NA         3        2
[1017,]          NA          NA         2        1
[1018,]           2          NA         3        2
[1019,]           2          NA         2        1
[1020,]          NA          NA         2        1
[1021,]           2          NA         2        1
[1022,]          NA          NA         2        1
[1023,]           2           2         1        1
[1024,]           1           3         1        1
[1025,]           1           3         1        1
[1026,]           1           3         1        1
[1027,]           2           3         1        1
[1028,]           2           2         1        1
[1029,]          NA          NA         1        1
[1030,]          NA          NA         1        1
[1031,]           3          NA         1        1
[1032,]           2           2        NA       NA
[1033,]           1          NA        NA       NA
[1034,]           3          NA        NA       NA
[1035,]           2           2        NA       NA
[1036,]          NA          NA        NA       NA
[1037,]           2           2        NA       NA
[1038,]           1          NA        NA       NA
[1039,]           2           2        NA       NA
[1040,]           2           3        NA       NA
[1041,]           2           2        NA       NA
[1042,]           2           2        NA       NA
[1043,]          NA          NA        NA       NA
[1044,]           1           2        NA       NA
[1045,]          NA          NA        NA       NA
[1046,]          NA          NA        NA       NA
[1047,]           2           2        NA       NA
[1048,]           2           2        NA       NA
[1049,]           2           2        NA       NA
[1050,]           2           2        NA       NA
[1051,]           2          NA        NA       NA
[1052,]          NA          NA        NA       NA
[1053,]           2          NA        NA       NA
[1054,]          NA          NA        NA       NA
[1055,]           2           2        NA       NA
[1056,]           2           2        NA       NA
[1057,]          NA          NA        NA       NA
[1058,]          NA          NA        NA       NA
[1059,]           2           1        NA       NA
[1060,]           2           2        NA       NA
[1061,]           2           1        NA       NA
[1062,]           1          NA        NA       NA
[1063,]           1          NA        NA       NA
[1064,]           2           2        NA       NA
[1065,]           2           2        NA       NA
[1066,]          NA          NA        NA       NA
[1067,]           2           2        NA       NA
[1068,]           1           1        NA       NA
[1069,]           2           2        NA       NA
[1070,]           2          NA        NA       NA
[1071,]           2          NA        NA       NA
[1072,]           2           2        NA       NA
[1073,]          NA          NA        NA       NA
[1074,]          NA          NA        NA       NA
[1075,]           2           2        NA       NA
[1076,]          NA          NA        NA       NA
[1077,]           2           2        NA       NA
[1078,]           1          NA        NA       NA
[1079,]           2          NA        NA       NA
[1080,]           2           2        NA       NA
[1081,]           1           1        NA       NA
[1082,]          NA          NA        NA       NA
[1083,]           2           2        NA       NA
[1084,]           1           2        NA       NA
[1085,]          NA          NA        NA       NA
[1086,]           1           1        NA       NA
[1087,]           1           2        NA       NA
[1088,]           1          NA        NA       NA
[1089,]           1          NA        NA       NA
[1090,]           1           2        NA       NA
[1091,]           1          NA        NA       NA
[1092,]           2          NA        NA       NA
[1093,]           2           2        NA       NA
[1094,]          NA          NA        NA       NA
[1095,]           1           1        NA       NA
[1096,]          NA          NA        NA       NA
[1097,]           1          NA        NA       NA
[1098,]          NA          NA        NA       NA
[1099,]          NA          NA        NA       NA
[1100,]          NA          NA        NA       NA
[1101,]           2          NA        NA       NA
[1102,]          NA          NA        NA       NA
[1103,]          NA          NA        NA       NA
[1104,]           1           1        NA       NA
[1105,]           1           1        NA       NA
[1106,]           1          NA        NA       NA
[1107,]          NA          NA        NA       NA
[1108,]           1           1        NA       NA
[1109,]           1          NA        NA       NA
[1110,]           3           3         1        1
[1111,]           1          NA         1        1
[1112,]           3          NA         1        1
[1113,]           3           3         1        1
[1114,]          NA          NA         2        3
[1115,]           3           3         1        1
[1116,]           1          NA         1        1
[1117,]           3           3         1        1
[1118,]           2           3         1        1
[1119,]           2           3         1        1
[1120,]          NA          NA         1        1
[1121,]           1           2         2        3
[1122,]          NA          NA         1        1
[1123,]          NA          NA         1        3
[1124,]           2           2         1        1
[1125,]           3           3         1        1
[1126,]           2           2         2        2
[1127,]           3           3         1        1
[1128,]           2          NA         1        1
[1129,]          NA          NA         1        1
[1130,]           2          NA         1        2
[1131,]          NA          NA         1        1
[1132,]           3           2         1        1
[1133,]           2           3         1        1
[1134,]          NA          NA         1        1
[1135,]           3           3        NA       NA
[1136,]          NA          NA         1        1
[1137,]           2           1         1        1
[1138,]           3           2        NA       NA
[1139,]           3           3        NA       NA
[1140,]           3           2        NA       NA
[1141,]           3           2        NA       NA
[1142,]           3           2        NA       NA
[1143,]           1          NA        NA       NA
[1144,]           3           2        NA       NA
[1145,]           3           2        NA       NA
[1146,]           2           2        NA       NA
[1147,]           2           2        NA       NA
[1148,]           2           2        NA       NA
[1149,]           1          NA        NA       NA
[1150,]           2           2        NA       NA
[1151,]           2           2        NA       NA
[1152,]           1           2        NA       NA
[1153,]           2          NA        NA       NA
[1154,]           2           2        NA       NA
[1155,]           2           2        NA       NA
[1156,]           1          NA        NA       NA
[1157,]           2          NA        NA       NA
[1158,]           2           2        NA       NA
[1159,]           2          NA        NA       NA
[1160,]           1          NA        NA       NA
[1161,]           2           2         1        1
[1162,]           2           1         1        1
[1163,]           1          NA        NA       NA
[1164,]           2          NA        NA       NA
[1165,]          NA          NA        NA       NA
[1166,]          NA          NA        NA       NA
[1167,]           1          NA        NA       NA
[1168,]           1          NA        NA       NA
[1169,]           2          NA        NA       NA
[1170,]           2          NA        NA       NA
[1171,]           2          NA        NA       NA
[1172,]           2          NA        NA       NA
[1173,]           2          NA        NA       NA
[1174,]          NA          NA        NA       NA
[1175,]          NA          NA        NA       NA
[1176,]          NA          NA        NA       NA
[1177,]          NA          NA        NA       NA
[1178,]          NA          NA        NA       NA
[1179,]          NA          NA        NA       NA
[1180,]          NA          NA        NA       NA
[1181,]          NA          NA        NA       NA
[1182,]           1          NA        NA       NA
[1183,]           1          NA         1        2
[1184,]           2          NA        NA       NA
[1185,]           2          NA        NA       NA
[1186,]          NA          NA        NA       NA
[1187,]           2          NA        NA       NA
[1188,]          NA          NA        NA       NA
[1189,]           3           2         2        1
[1190,]           1           2         3        2
[1191,]          NA          NA        NA       NA
[1192,]           1          NA        NA       NA
[1193,]           1          NA        NA       NA
[1194,]           1          NA        NA       NA
[1195,]          NA          NA        NA       NA
[1196,]          NA          NA        NA       NA
[1197,]          NA          NA        NA       NA
[1198,]          NA          NA        NA       NA
[1199,]          NA          NA        NA       NA
[1200,]          NA          NA        NA       NA
[1201,]          NA          NA        NA       NA
[1202,]          NA          NA        NA       NA
[1203,]          NA          NA        NA       NA
[1204,]           2          NA        NA       NA
[1205,]          NA          NA        NA       NA
[1206,]           1          NA        NA       NA
[1207,]           2           2        NA       NA
[1208,]           2           2        NA       NA
[1209,]           1          NA        NA       NA
[1210,]           3           2        NA       NA
[1211,]          NA          NA        NA       NA
[1212,]           1          NA        NA       NA
[1213,]           1           2        NA       NA
[1214,]          NA          NA        NA       NA
[1215,]           3           3        NA       NA
[1216,]           2           2        NA       NA
[1217,]          NA          NA        NA       NA
[1218,]           2          NA        NA       NA
[1219,]           1          NA        NA       NA
[1220,]          NA          NA        NA       NA
[1221,]          NA          NA        NA       NA
[1222,]          NA          NA         2        1
[1223,]           2           2         2        1
[1224,]           2           2         2        1
[1225,]           1          NA         2        2
[1226,]           2          NA         2        1
[1227,]           2           3         2        1
[1228,]          NA          NA         1        2
[1229,]          NA          NA         2        2
[1230,]           2           3        NA       NA
[1231,]           2           2        NA       NA
[1232,]           2           2        NA       NA
[1233,]          NA          NA        NA       NA
[1234,]           1          NA        NA       NA
[1235,]           2          NA        NA       NA
[1236,]           2          NA        NA       NA
[1237,]           1          NA        NA       NA
[1238,]           1          NA        NA       NA
[1239,]          NA          NA        NA       NA
[1240,]           2          NA        NA       NA
[1241,]           2           3        NA       NA
[1242,]           2           2        NA       NA
[1243,]          NA          NA        NA       NA
[1244,]           3           2        NA       NA
[1245,]          NA          NA        NA       NA
[1246,]           1          NA         2        2
[1247,]           2           2         2        3
[1248,]           1           1         1        2
[1249,]          NA          NA         1        2
[1250,]           3           3         1        1
[1251,]           2           3         1        1
[1252,]          NA          NA         1        1
[1253,]           2           2         1        1
[1254,]           1           2         2        2
[1255,]           2          NA         1        1
[1256,]           1          NA         2        2
[1257,]           1           1         2        2
[1258,]           2          NA         1        1
[1259,]          NA          NA         2        2
[1260,]           2           2         1        1
[1261,]          NA          NA         1        1
[1262,]           1          NA         2        1
[1263,]          NA          NA         1        1
[1264,]          NA          NA         1        1
[1265,]          NA          NA         1        1
[1266,]           2          NA         1        1
[1267,]          NA          NA         1        1
[1268,]           1           2         1        1
[1269,]           1           2         1        1
[1270,]          NA          NA         1        1
[1271,]           1           1         2        2
[1272,]           2          NA         1        1
[1273,]          NA          NA        NA       NA
[1274,]           2           2         1        1
[1275,]           2           2         1        1
[1276,]           1          NA         2        1
[1277,]           2           2         2        1
[1278,]           2           2         1        1
[1279,]           2           2         1        1
[1280,]           2           1         2        2
[1281,]           3           2         1        1
[1282,]           1           1         3        3
[1283,]           3           2         1        1
[1284,]           2           2         1        1
[1285,]           1           1         1        2
[1286,]           1          NA         2        1
[1287,]          NA          NA         2        2
[1288,]           2          NA         2        1
[1289,]           2          NA         1        1
[1290,]           2          NA         1        1
[1291,]           2          NA         1        1
[1292,]           2          NA         1        1
[1293,]           3          NA         1        1
[1294,]          NA          NA         1        1
[1295,]          NA          NA         1        1
[1296,]          NA          NA         1        1
[1297,]          NA          NA         1        1
[1298,]           1          NA         2        2
[1299,]           2          NA         1        1
[1300,]           2          NA         1        1
[1301,]          NA          NA         1        1
[1302,]          NA          NA         1        1
[1303,]           2           2         1        1
[1304,]           1           1        NA       NA
[1305,]           1           1         2        2
[1306,]           1          NA        NA       NA
[1307,]           1          NA         1        1
[1308,]          NA          NA         2        1
[1309,]          NA          NA         1        1
[1310,]          NA          NA         1        1
[1311,]          NA          NA         1        1
[1312,]           1          NA         2        2
[1313,]           1           1         2        2
[1314,]           2           2         1        1
[1315,]          NA          NA         1        1
[1316,]          NA          NA         1        1
[1317,]           1           2        NA       NA
[1318,]           2           1        NA       NA
[1319,]           1          NA        NA       NA
[1320,]           1           1        NA       NA
[1321,]          NA          NA        NA       NA
[1322,]          NA          NA        NA       NA
[1323,]          NA          NA        NA       NA
[1324,]          NA          NA        NA       NA
[1325,]           1          NA        NA       NA
[1326,]           1          NA        NA       NA
[1327,]           1          NA        NA       NA
[1328,]           2          NA        NA       NA
[1329,]           3           2        NA       NA
[1330,]           1          NA        NA       NA
[1331,]           3           2        NA       NA
[1332,]          NA          NA        NA       NA
[1333,]           2           2        NA       NA
[1334,]           2           2        NA       NA
[1335,]          NA          NA        NA       NA
[1336,]           3           2        NA       NA
[1337,]          NA          NA        NA       NA
[1338,]           2           2        NA       NA
[1339,]           1          NA        NA       NA
[1340,]          NA          NA        NA       NA
[1341,]          NA          NA        NA       NA
[1342,]          NA          NA        NA       NA
[1343,]          NA          NA        NA       NA
[1344,]           2          NA        NA       NA
[1345,]           2          NA        NA       NA
[1346,]           3          NA        NA       NA
[1347,]           3          NA        NA       NA
[1348,]           1           3        NA       NA
[1349,]          NA          NA        NA       NA
[1350,]           1          NA        NA       NA
[1351,]           2           3        NA       NA
[1352,]           3          NA        NA       NA
[1353,]           2           3        NA       NA
[1354,]          NA          NA        NA       NA
[1355,]           2           2        NA       NA
[1356,]          NA          NA        NA       NA
[1357,]          NA          NA        NA       NA
[1358,]          NA          NA        NA       NA
[1359,]          NA          NA        NA       NA
[1360,]           3           3        NA       NA
[1361,]           3          NA        NA       NA
[1362,]          NA          NA        NA       NA
[1363,]           2           3        NA       NA
[1364,]           3           3        NA       NA
[1365,]           2          NA        NA       NA
[1366,]           2           2        NA       NA
[1367,]           3           3        NA       NA
[1368,]          NA          NA        NA       NA
[1369,]          NA          NA        NA       NA
[1370,]           2           2        NA       NA
[1371,]          NA          NA        NA       NA
[1372,]           1           2        NA       NA
[1373,]           1           2        NA       NA
[1374,]          NA          NA        NA       NA
[1375,]           2          NA        NA       NA
[1376,]           1          NA        NA       NA
[1377,]          NA          NA        NA       NA
[1378,]           1           1         3        3
[1379,]           1          NA         2        2
[1380,]          NA          NA         3        1
[1381,]          NA          NA         1        1
[1382,]          NA          NA         3        2
[1383,]           1          NA         3        2
[1384,]           2          NA         2        1
[1385,]           1           2        NA       NA
[1386,]           1          NA        NA       NA
[1387,]           2           2        NA       NA
[1388,]           2           2        NA       NA
[1389,]           2           2        NA       NA
[1390,]           2          NA        NA       NA
[1391,]           2          NA        NA       NA
[1392,]           1           2        NA       NA
[1393,]           1           2        NA       NA
[1394,]           2           2        NA       NA
[1395,]           2           2        NA       NA
[1396,]           2           2        NA       NA
[1397,]           1           1        NA       NA
[1398,]           1           1        NA       NA
[1399,]          NA          NA        NA       NA
[1400,]          NA          NA        NA       NA
[1401,]          NA          NA        NA       NA
[1402,]           1          NA        NA       NA
[1403,]          NA          NA        NA       NA
[1404,]          NA          NA        NA       NA
[1405,]          NA          NA        NA       NA
[1406,]          NA          NA        NA       NA
[1407,]           2           2         2        1
[1408,]           1          NA         2        1
[1409,]           2           2         1        1
[1410,]          NA          NA         1        1
[1411,]           1           2         1        1
[1412,]           1           2         2        2
[1413,]          NA          NA         3        2
[1414,]           2           1         1        2
[1415,]          NA          NA         2        1
[1416,]           1           1         1        2
[1417,]           1          NA         3        1
[1418,]          NA          NA         2        1
[1419,]          NA          NA         2        1
[1420,]          NA          NA        NA       NA
[1421,]           2           3        NA       NA
[1422,]           2           2        NA       NA
[1423,]          NA          NA        NA       NA
[1424,]           3           3        NA       NA
[1425,]           2           2        NA       NA
[1426,]          NA          NA         1        1
[1427,]           3          NA         1        1
[1428,]           2          NA         2        2
[1429,]           2          NA         2        2
[1430,]           1           3         3        2
[1431,]          NA          NA         2        2
[1432,]           1          NA         1        2
[1433,]           3           3         2        1
[1434,]           3          NA         2        1
[1435,]           2           3         1        1
[1436,]          NA          NA         1        1
[1437,]           2           2         2        2
[1438,]          NA          NA         2        1
[1439,]          NA          NA         2        1
[1440,]          NA          NA         1        1
[1441,]           3           3        NA       NA
[1442,]           2           3        NA       NA
[1443,]           2           3        NA       NA
[1444,]           2           3        NA       NA
[1445,]           2           3        NA       NA
[1446,]           3           3        NA       NA
[1447,]           3          NA        NA       NA
[1448,]           3           3        NA       NA
[1449,]          NA          NA        NA       NA
[1450,]          NA          NA        NA       NA
[1451,]          NA          NA        NA       NA
[1452,]          NA          NA        NA       NA
[1453,]          NA          NA        NA       NA
[1454,]          NA          NA        NA       NA
[1455,]          NA          NA        NA       NA
[1456,]           2           3        NA       NA
[1457,]           3           3        NA       NA
[1458,]           2           3        NA       NA
[1459,]           2           3        NA       NA
[1460,]           3          NA        NA       NA
[1461,]          NA          NA        NA       NA
[1462,]           2          NA        NA       NA
[1463,]           2           3        NA       NA
[1464,]           2           3        NA       NA
[1465,]           3           3        NA       NA
[1466,]           3           3        NA       NA
[1467,]           3          NA         2        2
[1468,]          NA          NA         3        2
[1469,]           2           3         1        1
[1470,]           3           3         1        1
[1471,]           2          NA         2        2
[1472,]           2           2         2        2
[1473,]           3           3         1        1
[1474,]          NA          NA         2        2
[1475,]          NA          NA         2        1
[1476,]          NA          NA         1        1
[1477,]           1           1         3        3
[1478,]           1           1         3        3
[1479,]           2          NA         1        1
[1480,]           1          NA         2        3
[1481,]          NA          NA        NA       NA
[1482,]          NA          NA         1        1
[1483,]           3          NA         2        1
[1484,]           2          NA         2        2
[1485,]           3          NA         1        1
[1486,]           1           2         3        3
[1487,]          NA          NA         3        3
[1488,]           1          NA         2        3
[1489,]           3           3         1        1
[1490,]           3           3         2        1
[1491,]          NA          NA         1        1
[1492,]           2           2         3        2
[1493,]          NA          NA         2        2
[1494,]          NA          NA         2        2
[1495,]          NA          NA         2        1
[1496,]           1           3         1        1
[1497,]           3           3         1        1
[1498,]           3           3         1        1
[1499,]          NA          NA         1        1
[1500,]          NA          NA         1        1
[1501,]           3           3         1        1
[1502,]           3           3         1        1
[1503,]           3          NA         1        1
[1504,]           3          NA         1        1
[1505,]          NA          NA        NA       NA
[1506,]          NA          NA        NA       NA
[1507,]           3          NA        NA       NA
[1508,]          NA          NA         1        1
[1509,]           2          NA         1        1
[1510,]           3           3         1        1
[1511,]           1           1        NA       NA
[1512,]           2           2        NA       NA
[1513,]           1          NA        NA       NA
[1514,]           1           1        NA       NA
[1515,]           1          NA        NA       NA
[1516,]          NA          NA        NA       NA
[1517,]           3           2        NA       NA
[1518,]           1           1        NA       NA
[1519,]           2          NA        NA       NA
[1520,]           2          NA        NA       NA
[1521,]           1          NA        NA       NA
[1522,]           2          NA        NA       NA
[1523,]          NA          NA        NA       NA
[1524,]          NA          NA        NA       NA
[1525,]           2          NA        NA       NA
[1526,]           2          NA        NA       NA
[1527,]           1           2        NA       NA
[1528,]          NA          NA        NA       NA
[1529,]           1           2        NA       NA
[1530,]           1           2        NA       NA
[1531,]           2           2        NA       NA
[1532,]          NA          NA        NA       NA
[1533,]           1           1        NA       NA
[1534,]          NA          NA        NA       NA
[1535,]           1          NA        NA       NA
[1536,]          NA          NA        NA       NA
[1537,]           2           2        NA       NA
[1538,]           1          NA        NA       NA
[1539,]          NA          NA        NA       NA
[1540,]          NA          NA        NA       NA
[1541,]           1           1        NA       NA
[1542,]          NA          NA        NA       NA
[1543,]           2          NA        NA       NA
[1544,]           1           1        NA       NA
[1545,]           2          NA        NA       NA
[1546,]          NA          NA        NA       NA
[1547,]           2          NA        NA       NA
[1548,]          NA          NA        NA       NA
[1549,]           2           2        NA       NA
[1550,]          NA          NA        NA       NA
[1551,]          NA          NA        NA       NA
[1552,]           2          NA        NA       NA
[1553,]          NA          NA        NA       NA
[1554,]           2           3        NA       NA
[1555,]          NA          NA        NA       NA
[1556,]           2          NA        NA       NA
[1557,]           3          NA        NA       NA
[1558,]           2           2        NA       NA
[1559,]          NA          NA        NA       NA
[1560,]           2          NA        NA       NA
[1561,]           2           3        NA       NA
[1562,]          NA          NA        NA       NA
[1563,]           3          NA        NA       NA
[1564,]          NA          NA        NA       NA
[1565,]          NA          NA        NA       NA
[1566,]           2           2        NA       NA
[1567,]           2          NA        NA       NA
[1568,]           2           2        NA       NA
[1569,]           2          NA        NA       NA
[1570,]           1           1         1        2
[1571,]           1           3        NA       NA
[1572,]           3           2        NA       NA
[1573,]           3           2        NA       NA
[1574,]           2           2        NA       NA
[1575,]           2           2        NA       NA
[1576,]           2           2        NA       NA
[1577,]           3           2        NA       NA
[1578,]           2           2        NA       NA
[1579,]           2           2        NA       NA
[1580,]           2          NA        NA       NA
[1581,]           2          NA        NA       NA
[1582,]           3           2        NA       NA
[1583,]           2           1        NA       NA
[1584,]           2          NA        NA       NA
[1585,]           2          NA        NA       NA
[1586,]           2          NA        NA       NA
[1587,]          NA          NA        NA       NA
[1588,]           2          NA        NA       NA
[1589,]           3           2        NA       NA
[1590,]           2          NA        NA       NA
[1591,]           2          NA        NA       NA
[1592,]           1          NA        NA       NA
[1593,]           2          NA        NA       NA
[1594,]           2           2        NA       NA
[1595,]           2           2        NA       NA
[1596,]           1           2        NA       NA
[1597,]           2           3        NA       NA
[1598,]           2           3        NA       NA
[1599,]           1           2        NA       NA
[1600,]           1           2        NA       NA
[1601,]           1           3        NA       NA
[1602,]           1           2        NA       NA
[1603,]           2           2        NA       NA
[1604,]           1           2        NA       NA
[1605,]           2           1         1        1
[1606,]           2          NA        NA       NA
[1607,]           2          NA        NA       NA
[1608,]          NA          NA        NA       NA
[1609,]           2          NA        NA       NA
[1610,]           2          NA        NA       NA
[1611,]          NA          NA        NA       NA
[1612,]          NA          NA        NA       NA
[1613,]          NA          NA        NA       NA
[1614,]          NA          NA        NA       NA
[1615,]          NA          NA        NA       NA
[1616,]          NA          NA        NA       NA
[1617,]           2          NA        NA       NA
[1618,]          NA          NA        NA       NA
[1619,]          NA          NA        NA       NA
[1620,]          NA          NA        NA       NA
[1621,]          NA          NA        NA       NA
[1622,]           2          NA        NA       NA
[1623,]          NA          NA        NA       NA
[1624,]           2          NA        NA       NA
[1625,]           2          NA        NA       NA
[1626,]          NA          NA        NA       NA
[1627,]          NA          NA        NA       NA
[1628,]           2          NA        NA       NA
[1629,]          NA          NA        NA       NA
[1630,]          NA          NA        NA       NA
[1631,]          NA          NA        NA       NA
[1632,]          NA          NA        NA       NA
[1633,]          NA          NA        NA       NA
[1634,]          NA          NA        NA       NA
[1635,]          NA          NA        NA       NA
[1636,]          NA          NA        NA       NA
[1637,]          NA          NA        NA       NA
[1638,]           2          NA        NA       NA
[1639,]          NA          NA        NA       NA
[1640,]           1          NA        NA       NA
[1641,]           1          NA        NA       NA
[1642,]           2          NA        NA       NA
[1643,]           1          NA        NA       NA
[1644,]           1          NA        NA       NA
[1645,]          NA          NA        NA       NA
[1646,]          NA          NA        NA       NA
[1647,]          NA          NA        NA       NA
[1648,]          NA          NA        NA       NA
[1649,]          NA          NA        NA       NA
[1650,]          NA          NA        NA       NA
[1651,]          NA          NA        NA       NA
[1652,]          NA          NA        NA       NA
[1653,]          NA          NA        NA       NA
[1654,]          NA          NA        NA       NA
[1655,]          NA          NA        NA       NA
[1656,]          NA          NA        NA       NA
[1657,]          NA          NA        NA       NA
[1658,]          NA          NA        NA       NA
[1659,]           2          NA        NA       NA
[1660,]           1           3        NA       NA
[1661,]           1           3        NA       NA
[1662,]           2           2        NA       NA
[1663,]           1           2        NA       NA
[1664,]           1           2        NA       NA
[1665,]           2           2        NA       NA
[1666,]           2           2        NA       NA
[1667,]           1          NA        NA       NA
[1668,]           2           2        NA       NA
[1669,]           1           2        NA       NA
[1670,]           2           3        NA       NA
[1671,]           1           3        NA       NA
[1672,]           1           3        NA       NA
[1673,]           2           2        NA       NA
[1674,]           1          NA        NA       NA
[1675,]           1          NA        NA       NA
[1676,]           2           2        NA       NA
[1677,]           2           2        NA       NA
[1678,]           2           2        NA       NA
[1679,]           2           2        NA       NA
[1680,]           1           3        NA       NA
[1681,]           2           2        NA       NA
[1682,]           2           2        NA       NA
[1683,]           2           2        NA       NA
[1684,]           2          NA        NA       NA
[1685,]           2           3        NA       NA
[1686,]           2           2        NA       NA
[1687,]           2           2        NA       NA
[1688,]           2           2        NA       NA
[1689,]           2           2        NA       NA
[1690,]           2           2        NA       NA
[1691,]           2           2        NA       NA
[1692,]           2           2        NA       NA
[1693,]           2           2        NA       NA
[1694,]           2           2        NA       NA
[1695,]           1           2        NA       NA
[1696,]           2           3        NA       NA
[1697,]           2           2        NA       NA
[1698,]           2           2        NA       NA
[1699,]           1           3        NA       NA
[1700,]           2           2        NA       NA
[1701,]           1           3        NA       NA
[1702,]           2           2        NA       NA
[1703,]           3           2        NA       NA
[1704,]           2           2        NA       NA
[1705,]           2           2        NA       NA
[1706,]           2           2        NA       NA
[1707,]           2           2        NA       NA
[1708,]           2           2        NA       NA
[1709,]          NA          NA        NA       NA
[1710,]          NA          NA        NA       NA
[1711,]          NA          NA        NA       NA
[1712,]          NA          NA        NA       NA
[1713,]          NA          NA        NA       NA
[1714,]          NA          NA        NA       NA
[1715,]          NA          NA        NA       NA
[1716,]           1          NA        NA       NA
[1717,]           1          NA        NA       NA
[1718,]           1           3        NA       NA
[1719,]          NA          NA        NA       NA
[1720,]          NA          NA        NA       NA
[1721,]          NA          NA        NA       NA
[1722,]          NA          NA        NA       NA
[1723,]          NA          NA        NA       NA
[1724,]           1          NA        NA       NA
[1725,]           1          NA        NA       NA
[1726,]          NA          NA        NA       NA
[1727,]          NA          NA        NA       NA
[1728,]           1          NA        NA       NA
[1729,]           1          NA        NA       NA
[1730,]          NA          NA        NA       NA
[1731,]          NA          NA        NA       NA
[1732,]          NA          NA        NA       NA
[1733,]          NA          NA        NA       NA
[1734,]          NA          NA        NA       NA
[1735,]          NA          NA        NA       NA
[1736,]          NA          NA        NA       NA
[1737,]          NA          NA        NA       NA
[1738,]          NA          NA        NA       NA
[1739,]          NA          NA        NA       NA
[1740,]          NA          NA        NA       NA
[1741,]          NA          NA        NA       NA
[1742,]          NA          NA        NA       NA
[1743,]          NA          NA        NA       NA
[1744,]          NA          NA        NA       NA
[1745,]          NA          NA        NA       NA
[1746,]          NA          NA        NA       NA
[1747,]          NA          NA        NA       NA
[1748,]          NA          NA        NA       NA
[1749,]           2          NA        NA       NA
[1750,]           2          NA        NA       NA
[1751,]           2          NA        NA       NA
[1752,]           2          NA        NA       NA
[1753,]           2          NA        NA       NA
[1754,]          NA          NA        NA       NA
[1755,]          NA          NA        NA       NA
[1756,]          NA          NA        NA       NA
[1757,]          NA          NA        NA       NA
[1758,]          NA          NA        NA       NA
[1759,]           2          NA        NA       NA
[1760,]           1          NA        NA       NA
[1761,]           1          NA        NA       NA
[1762,]           1          NA        NA       NA
[1763,]          NA          NA        NA       NA
[1764,]           1          NA        NA       NA
[1765,]          NA          NA        NA       NA
[1766,]           1           3        NA       NA
[1767,]          NA          NA        NA       NA
[1768,]          NA          NA        NA       NA
[1769,]          NA          NA        NA       NA
[1770,]          NA          NA        NA       NA
[1771,]          NA          NA        NA       NA
[1772,]           2           2        NA       NA
[1773,]           2           2        NA       NA
[1774,]           2           2        NA       NA
[1775,]           2          NA        NA       NA
[1776,]           2          NA        NA       NA
[1777,]           2           2        NA       NA
[1778,]           2          NA        NA       NA
[1779,]           2          NA        NA       NA
[1780,]           2           2        NA       NA
[1781,]           2           2        NA       NA
[1782,]           2          NA        NA       NA
[1783,]           1          NA        NA       NA
[1784,]           2           2        NA       NA
[1785,]           1           3        NA       NA
[1786,]           2           3        NA       NA
[1787,]           2           3        NA       NA
[1788,]           1           3        NA       NA
[1789,]           2           3        NA       NA
[1790,]           2           2        NA       NA
[1791,]           1           2        NA       NA
[1792,]           1           2        NA       NA
[1793,]          NA          NA        NA       NA
[1794,]           1           2        NA       NA
[1795,]           1           2        NA       NA
[1796,]           2           2        NA       NA
[1797,]           2           2        NA       NA
[1798,]           1           2        NA       NA
[1799,]           3           2        NA       NA
[1800,]           3           2        NA       NA
[1801,]           3           2        NA       NA
[1802,]           3           2        NA       NA
[1803,]          NA          NA        NA       NA
[1804,]          NA          NA        NA       NA
[1805,]           2          NA        NA       NA
[1806,]           1           3        NA       NA
[1807,]           2           3        NA       NA
[1808,]           2          NA        NA       NA
[1809,]          NA          NA        NA       NA
[1810,]           2          NA        NA       NA
[1811,]           2          NA        NA       NA
[1812,]           2          NA        NA       NA
[1813,]           2          NA        NA       NA
[1814,]           2          NA        NA       NA
[1815,]           2          NA        NA       NA
[1816,]           2          NA        NA       NA
[1817,]           2           2        NA       NA
[1818,]           2           2        NA       NA
[1819,]           2           2        NA       NA
[1820,]           1           3        NA       NA
[1821,]           2           2        NA       NA
[1822,]           2           2        NA       NA
[1823,]           2           2        NA       NA
[1824,]           2           2        NA       NA
[1825,]           3           2        NA       NA
[1826,]           3           2        NA       NA
[1827,]           3           2        NA       NA
[1828,]           2          NA        NA       NA
[1829,]           1          NA        NA       NA
[1830,]           2          NA        NA       NA
[1831,]          NA          NA        NA       NA
[1832,]          NA          NA        NA       NA
[1833,]          NA          NA        NA       NA
[1834,]          NA          NA        NA       NA
[1835,]           2          NA        NA       NA
[1836,]          NA          NA        NA       NA
[1837,]          NA          NA        NA       NA
[1838,]          NA          NA        NA       NA
[1839,]           2          NA        NA       NA
[1840,]           2          NA        NA       NA
[1841,]          NA          NA        NA       NA
[1842,]           1          NA        NA       NA
[1843,]           3           2        NA       NA
[1844,]           3           2        NA       NA
[1845,]           3           2        NA       NA
[1846,]           2           2        NA       NA
[1847,]           1          NA        NA       NA
[1848,]          NA          NA        NA       NA
[1849,]           3           2        NA       NA
[1850,]           2          NA        NA       NA
[1851,]          NA          NA        NA       NA
[1852,]          NA          NA        NA       NA
[1853,]           3           2        NA       NA
[1854,]           2           2        NA       NA
[1855,]           1          NA        NA       NA
[1856,]          NA          NA        NA       NA
[1857,]           2           2        NA       NA
[1858,]           2           2        NA       NA
[1859,]           2          NA        NA       NA
[1860,]          NA          NA        NA       NA
[1861,]           2           3        NA       NA
[1862,]           2          NA        NA       NA
[1863,]           1           3        NA       NA
[1864,]           1           3        NA       NA
[1865,]           1           3        NA       NA
[1866,]           1           3        NA       NA
[1867,]           2           3        NA       NA
[1868,]           2           3        NA       NA
[1869,]           2          NA        NA       NA
[1870,]          NA          NA        NA       NA
[1871,]          NA          NA        NA       NA
[1872,]           2          NA        NA       NA
[1873,]          NA          NA        NA       NA
[1874,]           2           2        NA       NA
[1875,]          NA          NA        NA       NA
[1876,]          NA          NA        NA       NA
[1877,]          NA          NA        NA       NA
[1878,]          NA          NA        NA       NA
[1879,]          NA          NA        NA       NA
[1880,]          NA          NA        NA       NA
[1881,]           3           2        NA       NA
[1882,]          NA          NA        NA       NA
[1883,]          NA          NA        NA       NA
[1884,]           2          NA         2        1
[1885,]          NA          NA         1        1
[1886,]          NA          NA         1        1
[1887,]           2          NA         1        1
[1888,]           1           1         1        2
[1889,]          NA          NA         1        1
[1890,]           1           2         1        1
[1891,]           1           2         3        3
[1892,]          NA          NA         1        1
[1893,]           2           2        NA       NA
[1894,]           2           2        NA       NA
[1895,]          NA          NA        NA       NA
[1896,]           2           2        NA       NA
[1897,]           2           2        NA       NA
[1898,]           2           2        NA       NA
[1899,]           2           2        NA       NA
[1900,]           2           2        NA       NA
[1901,]           2           2        NA       NA
[1902,]           2           2        NA       NA
[1903,]           2           2        NA       NA
[1904,]           2           2        NA       NA
[1905,]           1           1         2        3
[1906,]          NA          NA        NA       NA
[1907,]          NA          NA         1        1
[1908,]           2           2        NA       NA
[1909,]          NA          NA        NA       NA
[1910,]           1           2        NA       NA
[1911,]           1          NA         1        1
[1912,]           2           3         2        1
[1913,]          NA          NA         1        1
[1914,]          NA          NA         2        1
[1915,]           1           1         1        1
[1916,]          NA          NA         1        1
[1917,]           2          NA         1        1
[1918,]           1           1         2       NA
[1919,]           2          NA         2        1
[1920,]          NA          NA         1        1
[1921,]           2          NA         1        1
[1922,]          NA          NA         2        2
[1923,]           3           3        NA       NA
[1924,]           2           2        NA       NA
[1925,]           2           3        NA       NA
[1926,]          NA          NA        NA       NA
[1927,]           2           2         2        1
[1928,]          NA          NA         2        3
[1929,]          NA          NA         1        1
[1930,]           2          NA         2        2
[1931,]          NA          NA         2        3
[1932,]           3           3         1        1
[1933,]          NA          NA         2        2
[1934,]           2          NA         2        2
[1935,]           3          NA         1        1
[1936,]           2           3         1        1
[1937,]          NA          NA         2        1
[1938,]           2          NA         1        1
[1939,]           2           3         2        1
[1940,]          NA          NA         2        2
[1941,]           3          NA         1        1
[1942,]          NA          NA        NA       NA
[1943,]          NA          NA        NA       NA
[1944,]          NA          NA        NA       NA
[1945,]          NA          NA        NA       NA
[1946,]          NA          NA        NA       NA
[1947,]          NA          NA         2        1
[1948,]           2           2         2        2
[1949,]           2          NA        NA       NA
[1950,]           2           2        NA       NA
[1951,]           3          NA        NA       NA
[1952,]           1           3        NA       NA
[1953,]           2           2        NA       NA
[1954,]           2          NA         1        1
[1955,]           3           3         1        1
[1956,]           3           3         1        1
[1957,]           3           2         2        1
[1958,]           2           1         3        2
[1959,]           2          NA         2        1
[1960,]           2          NA         1        1
[1961,]           2          NA         1        1
[1962,]           2          NA         1        1
[1963,]           2           3         1        1
[1964,]           2           3         2        1
[1965,]           2           3         1        1
[1966,]           2           3         2        1
[1967,]           2           3         2        1
[1968,]           1          NA         3        2
[1969,]           1          NA         2        1
[1970,]          NA          NA         1        1
[1971,]          NA          NA         1        1
[1972,]          NA          NA         1        1
[1973,]          NA          NA         2        1
[1974,]           2          NA         2        1
[1975,]          NA          NA         2        1
[1976,]           2          NA         2        1
[1977,]          NA          NA         1        1
[1978,]          NA          NA         1        1
[1979,]          NA          NA         1        1
[1980,]          NA          NA         2        1
[1981,]           1          NA         2        1
[1982,]           1          NA         3        3
[1983,]          NA          NA        NA       NA
[1984,]          NA          NA         1        1
[1985,]          NA          NA         1        1
[1986,]          NA          NA         2        1
[1987,]          NA          NA         2        1
[1988,]           3           3         1        1
[1989,]           1           2         3        2
[1990,]           1           2         2        1
[1991,]           2           3         2        1
[1992,]           1           2         3        2
[1993,]           2           3         1        1
[1994,]           2           3         1        1
[1995,]           2           3         2        1
[1996,]           2          NA         2        1
[1997,]           2           3         1        1
[1998,]           3           3         1        1
[1999,]           3           3         1        1
[2000,]           2           3         3        2
[2001,]           2           2         2        1
[2002,]           2           2         2        1
[2003,]           3           3        NA       NA
[2004,]           3           3         1        1
[2005,]           2           2         3        1
[2006,]           2           3         2        1
[2007,]           2           3         1        1
[2008,]           3           3         1        1
[2009,]           3           3         1        1
[2010,]           2           2         2        1
[2011,]           3           3         1        1
[2012,]           2           2         2        1
[2013,]           3           3         1        1
[2014,]          NA          NA         2        1
[2015,]          NA          NA         2        1
[2016,]           2           3         1        1
[2017,]          NA          NA         3        2
[2018,]          NA          NA         2        1
[2019,]          NA          NA         2        1
[2020,]           1          NA         2        1
[2021,]          NA          NA         2        1
[2022,]          NA          NA         2        1
[2023,]          NA          NA         2        1
[2024,]          NA          NA        NA       NA
[2025,]          NA          NA         2        1
[2026,]          NA          NA         2        2
[2027,]          NA          NA         1        1
[2028,]          NA          NA         3        2
[2029,]           3          NA         2        1
[2030,]           3          NA         2        1
[2031,]          NA          NA         2        1
[2032,]          NA          NA         1        1
[2033,]           2          NA         2        1
[2034,]          NA          NA         3        2
[2035,]          NA          NA         1        1
[2036,]          NA          NA        NA       NA
[2037,]          NA          NA         1        1
[2038,]          NA          NA         2        1
[2039,]           3           3         1        1
[2040,]           3           3         2        1
[2041,]           2          NA         1        1
[2042,]           2           3         1        1
[2043,]           2          NA         1        1
[2044,]           2          NA         1        1
[2045,]           2           3         2        1
[2046,]           1           2         3        2
[2047,]           1           2         3        2
[2048,]           2           3         2        1
[2049,]          NA          NA         2        1
[2050,]          NA          NA        NA       NA
[2051,]          NA          NA        NA       NA
[2052,]           2           3         1        1
[2053,]           2          NA         1        1
[2054,]          NA          NA         1        1
[2055,]          NA          NA         1        1
[2056,]          NA          NA        NA       NA
[2057,]          NA          NA        NA       NA
[2058,]           2           3        NA       NA
[2059,]           2           3        NA       NA
[2060,]          NA          NA        NA       NA
[2061,]          NA          NA        NA       NA
[2062,]           2          NA        NA       NA
[2063,]           3           3        NA       NA
[2064,]          NA          NA        NA       NA
[2065,]          NA          NA        NA       NA
[2066,]          NA          NA        NA       NA
[2067,]           1           1        NA       NA
[2068,]           2           2        NA       NA
[2069,]           1           1        NA       NA
[2070,]           1           2        NA       NA
[2071,]          NA          NA        NA       NA
[2072,]           2           2        NA       NA
[2073,]           1          NA        NA       NA
[2074,]          NA          NA        NA       NA
[2075,]          NA          NA        NA       NA
[2076,]           2           1        NA       NA
[2077,]           1           2        NA       NA
[2078,]           3           3        NA       NA
[2079,]          NA          NA        NA       NA
[2080,]           3          NA        NA       NA
[2081,]          NA          NA        NA       NA
[2082,]          NA          NA        NA       NA
[2083,]           2          NA        NA       NA
[2084,]           1           1        NA       NA
[2085,]           2           3        NA       NA
[2086,]           2          NA        NA       NA
[2087,]           2           3        NA       NA
[2088,]           1          NA        NA       NA
[2089,]           3          NA        NA       NA
[2090,]           3           3        NA       NA
[2091,]           2           1        NA       NA
[2092,]          NA          NA        NA       NA
[2093,]           2          NA        NA       NA
[2094,]          NA          NA        NA       NA
[2095,]           2          NA        NA       NA
[2096,]          NA          NA        NA       NA
[2097,]           2          NA        NA       NA
[2098,]           2          NA        NA       NA
[2099,]           2          NA        NA       NA
[2100,]          NA          NA        NA       NA
[2101,]           2           2        NA       NA
[2102,]           2           2        NA       NA
[2103,]          NA          NA        NA       NA
[2104,]          NA          NA        NA       NA
[2105,]          NA          NA        NA       NA
[2106,]          NA          NA        NA       NA
[2107,]          NA          NA        NA       NA
[2108,]           3           3        NA       NA
[2109,]          NA          NA        NA       NA
[2110,]           2           2        NA       NA
[2111,]           1           1        NA       NA
[2112,]           3          NA        NA       NA
[2113,]          NA          NA        NA       NA
[2114,]           2           3        NA       NA
[2115,]           2           2        NA       NA
[2116,]           1           1        NA       NA
[2117,]           1           1        NA       NA
[2118,]           1          NA        NA       NA
[2119,]           1           1        NA       NA
[2120,]           1           2        NA       NA
[2121,]           2          NA        NA       NA
[2122,]          NA          NA        NA       NA
[2123,]           1          NA        NA       NA
[2124,]           1           1         2        2
[2125,]           2           2         1        1
[2126,]           2           2         1        1
[2127,]          NA          NA         2        1
[2128,]           1          NA         2        2
[2129,]          NA          NA        NA       NA
[2130,]          NA          NA         2        1
[2131,]          NA          NA         2        2
[2132,]           1           1         2        2
[2133,]           1           2         2        1
[2134,]           2           3         1        1
[2135,]          NA          NA         1        1
[2136,]           3          NA         2        1
[2137,]          NA          NA         2        2
[2138,]           2          NA         2        1
[2139,]           2           3         1        1
[2140,]           2           3         1        1
[2141,]           1          NA         2        2
[2142,]          NA          NA         1        1
[2143,]           2          NA         1        1
[2144,]          NA          NA         1        1
[2145,]           2          NA         1        1
[2146,]          NA          NA        NA       NA
[2147,]           3          NA        NA       NA
[2148,]           3          NA        NA       NA
[2149,]           3           3        NA       NA
[2150,]           3           3        NA       NA
[2151,]           3          NA        NA       NA
[2152,]           3          NA        NA       NA
[2153,]           3          NA        NA       NA
[2154,]          NA          NA         2        2
[2155,]           3          NA        NA       NA
[2156,]           3          NA        NA       NA
[2157,]           3           3        NA       NA
[2158,]           3           3        NA       NA
[2159,]           3          NA         2        1
[2160,]           2          NA         2        2
[2161,]           2          NA         1        1
[2162,]          NA          NA         3        3
[2163,]           2           2         2        2
[2164,]           2           2         1        1
[2165,]          NA          NA         2        1
[2166,]          NA          NA         1        1
[2167,]          NA          NA         2        1
[2168,]           2           3         1        1
[2169,]          NA          NA         1        1
[2170,]           2           2         2        2
[2171,]           3          NA        NA       NA
[2172,]          NA          NA         2        1
[2173,]           3           3        NA       NA
[2174,]          NA          NA        NA       NA
[2175,]           3           3        NA       NA
[2176,]           3           3        NA       NA
[2177,]           2           3        NA       NA
[2178,]           2           3         1        1
[2179,]           2           2         3        2
[2180,]           1           1         3        2
[2181,]           1           1         3        3
[2182,]           1          NA         3        3
[2183,]           1           2         1        1
[2184,]           1           2         3        1
[2185,]           1          NA         2        1
[2186,]           2          NA        NA       NA
[2187,]          NA          NA        NA       NA
[2188,]          NA          NA        NA       NA
[2189,]          NA          NA        NA       NA
[2190,]          NA          NA        NA       NA
[2191,]           3          NA         1        1
[2192,]           3           3         1        1
[2193,]           3           3         1        1
[2194,]           3          NA        NA       NA
[2195,]           3          NA         1        1
[2196,]          NA          NA         3        3
[2197,]          NA          NA        NA       NA
[2198,]           3          NA         1        1
[2199,]           2           3         1        1
[2200,]           3          NA         2        2
[2201,]           2          NA         3        2
[2202,]          NA          NA         3        3
[2203,]           2           2         2        2
[2204,]           2           2         2        1
[2205,]          NA          NA         2        1
[2206,]          NA          NA         1        1
[2207,]          NA          NA         3        2
[2208,]           2           3         1        1
[2209,]           2           2         3        2
[2210,]           3           3        NA       NA
[2211,]           2           3        NA       NA
[2212,]           2           3        NA       NA
[2213,]           2           3        NA       NA
[2214,]           2           3        NA       NA
[2215,]           3          NA        NA       NA
[2216,]           2           3        NA       NA
[2217,]           3           3        NA       NA
[2218,]           2           3        NA       NA
[2219,]           3           3        NA       NA
[2220,]           2           3        NA       NA
[2221,]           2           3        NA       NA
[2222,]           3          NA         3        2
[2223,]          NA          NA         2        2
[2224,]           2           3         1        1
[2225,]           3           3         1        1
[2226,]          NA          NA        NA       NA
[2227,]           2           2        NA       NA
[2228,]          NA          NA        NA       NA
[2229,]           1          NA        NA       NA
[2230,]           1          NA        NA       NA
[2231,]           1           1        NA       NA
[2232,]           1          NA        NA       NA
[2233,]          NA          NA        NA       NA
[2234,]          NA          NA        NA       NA
[2235,]          NA          NA        NA       NA
[2236,]           1           1        NA       NA
[2237,]          NA          NA        NA       NA
[2238,]           2           2        NA       NA
[2239,]          NA          NA        NA       NA
[2240,]          NA          NA        NA       NA
[2241,]          NA          NA        NA       NA
[2242,]          NA          NA        NA       NA
[2243,]           2           2        NA       NA
[2244,]           2           2        NA       NA
[2245,]           2          NA        NA       NA
[2246,]           2           2        NA       NA
[2247,]          NA          NA        NA       NA
[2248,]           2          NA        NA       NA
[2249,]           1          NA        NA       NA
[2250,]           2          NA        NA       NA
[2251,]           1          NA        NA       NA
[2252,]           2           2        NA       NA
[2253,]           1           1        NA       NA
[2254,]          NA          NA        NA       NA
[2255,]           1          NA        NA       NA
[2256,]          NA          NA        NA       NA
[2257,]           1          NA        NA       NA
[2258,]           1          NA        NA       NA
[2259,]           1           2        NA       NA
[2260,]           1          NA        NA       NA
[2261,]          NA          NA        NA       NA
[2262,]           1           1        NA       NA
[2263,]          NA          NA        NA       NA
[2264,]          NA          NA        NA       NA
[2265,]           2          NA        NA       NA
[2266,]           1          NA        NA       NA
[2267,]          NA          NA        NA       NA
[2268,]           2          NA        NA       NA
[2269,]           1          NA        NA       NA
[2270,]           1          NA        NA       NA
[2271,]           1           1        NA       NA
[2272,]          NA          NA        NA       NA
[2273,]          NA          NA        NA       NA
[2274,]          NA          NA        NA       NA
[2275,]          NA          NA        NA       NA
[2276,]           1          NA        NA       NA
[2277,]           1          NA        NA       NA
[2278,]           1          NA        NA       NA
[2279,]          NA          NA        NA       NA
[2280,]          NA          NA        NA       NA
[2281,]           2           2        NA       NA
[2282,]           1           1        NA       NA
[2283,]           2           2        NA       NA
[2284,]           2          NA        NA       NA
[2285,]          NA          NA        NA       NA
[2286,]           2          NA        NA       NA
[2287,]          NA          NA        NA       NA
[2288,]          NA          NA        NA       NA
[2289,]           2          NA        NA       NA
[2290,]          NA          NA        NA       NA
[2291,]           2           2        NA       NA
[2292,]          NA          NA        NA       NA
[2293,]           1           1        NA       NA
[2294,]           1           2        NA       NA
[2295,]           2           2        NA       NA
[2296,]          NA          NA        NA       NA
[2297,]           1           1        NA       NA
[2298,]           2           2        NA       NA
[2299,]           2           2        NA       NA
[2300,]           2           2        NA       NA
[2301,]           1           1        NA       NA
[2302,]           2          NA        NA       NA
[2303,]           2           2        NA       NA
[2304,]          NA          NA        NA       NA
[2305,]          NA          NA        NA       NA
[2306,]          NA          NA        NA       NA
[2307,]          NA          NA        NA       NA
[2308,]          NA          NA        NA       NA
[2309,]          NA          NA        NA       NA
[2310,]          NA          NA        NA       NA
[2311,]          NA          NA        NA       NA
[2312,]           2           2        NA       NA
[2313,]           2          NA        NA       NA
[2314,]          NA          NA        NA       NA
[2315,]           1          NA        NA       NA
[2316,]          NA          NA        NA       NA
[2317,]           2          NA        NA       NA
[2318,]           2          NA        NA       NA
[2319,]          NA          NA        NA       NA
[2320,]          NA          NA        NA       NA
[2321,]          NA          NA        NA       NA
[2322,]           2          NA        NA       NA
[2323,]          NA          NA        NA       NA
[2324,]           2          NA        NA       NA
[2325,]           1           1        NA       NA
[2326,]          NA          NA        NA       NA
[2327,]          NA          NA        NA       NA
[2328,]           1          NA        NA       NA
[2329,]           1          NA        NA       NA
[2330,]           2          NA        NA       NA
[2331,]           1           1        NA       NA
[2332,]           1           1        NA       NA
[2333,]           2          NA        NA       NA
[2334,]           2          NA        NA       NA
[2335,]          NA          NA        NA       NA
[2336,]          NA          NA        NA       NA
[2337,]           1           2        NA       NA
[2338,]           2          NA        NA       NA
[2339,]           1           2        NA       NA
[2340,]          NA          NA        NA       NA
[2341,]           1          NA        NA       NA
[2342,]          NA          NA        NA       NA
[2343,]           2           2        NA       NA
[2344,]           2           2        NA       NA
[2345,]           1          NA        NA       NA
[2346,]           1           2        NA       NA
[2347,]           2          NA        NA       NA
[2348,]          NA          NA        NA       NA
[2349,]           2           2        NA       NA
[2350,]          NA          NA        NA       NA
[2351,]           1          NA        NA       NA
[2352,]           2          NA        NA       NA
[2353,]           2          NA        NA       NA
[2354,]          NA          NA        NA       NA
[2355,]           1           1        NA       NA
[2356,]           1           1        NA       NA
[2357,]           2          NA        NA       NA
[2358,]          NA          NA        NA       NA
[2359,]           2           2        NA       NA
[2360,]          NA          NA        NA       NA
[2361,]          NA          NA        NA       NA
[2362,]           1           1        NA       NA
[2363,]           2           2        NA       NA
[2364,]           1           1        NA       NA
[2365,]           2           2        NA       NA
[2366,]           1          NA        NA       NA
[2367,]           2           2        NA       NA
[2368,]           2          NA        NA       NA
[2369,]           2          NA        NA       NA
[2370,]           1           1        NA       NA
[2371,]          NA          NA        NA       NA
[2372,]           2           2        NA       NA
[2373,]          NA          NA        NA       NA
[2374,]           2           2        NA       NA
[2375,]          NA          NA        NA       NA
[2376,]          NA          NA        NA       NA
[2377,]           2           2        NA       NA
[2378,]          NA          NA        NA       NA
[2379,]          NA          NA        NA       NA
[2380,]           2          NA        NA       NA
[2381,]          NA          NA        NA       NA
[2382,]          NA          NA        NA       NA
[2383,]           2          NA        NA       NA
[2384,]           1          NA        NA       NA
[2385,]          NA          NA        NA       NA
[2386,]          NA          NA        NA       NA
[2387,]          NA          NA        NA       NA
[2388,]          NA          NA         1        1
[2389,]           3           3         1        1
[2390,]          NA          NA         1        2
[2391,]           2          NA         1        1
[2392,]           1          NA         2        2
[2393,]           1          NA         1        1
[2394,]          NA          NA         1        1
[2395,]          NA          NA         2        2
[2396,]          NA          NA         1        1
[2397,]           1           1         1        2
[2398,]          NA          NA         1        1
[2399,]           2           3         1        1
[2400,]          NA          NA         1        1
[2401,]          NA          NA         1        1
[2402,]          NA          NA         1        1
[2403,]          NA          NA         2        1
[2404,]           2           2         1        1
[2405,]           2           2         1        1
[2406,]           2          NA         1        1
[2407,]           2           2         1        1
[2408,]          NA          NA         1        1
[2409,]           2          NA         1        1
[2410,]           1          NA         1        2
[2411,]           2          NA         1        1
[2412,]           2          NA         1        1
[2413,]           2           3        NA       NA
[2414,]           1           1         1        2
[2415,]          NA          NA         1        1
[2416,]           1          NA         2        3
[2417,]          NA          NA         1        1
[2418,]           2          NA         1        1
[2419,]           2          NA         1        1
[2420,]          NA          NA         1        1
[2421,]           1           1         1        1
[2422,]          NA          NA         1        1
[2423,]           2          NA         1        1
[2424,]           1          NA         1        2
[2425,]          NA          NA         1        1
[2426,]           2          NA         1        1
[2427,]           1          NA         1        1
[2428,]           1          NA         1        1
[2429,]           3           3        NA       NA
[2430,]           2          NA        NA       NA
[2431,]           2          NA        NA       NA
[2432,]           2          NA        NA       NA
[2433,]           2           3        NA       NA
[2434,]           3          NA        NA       NA
[2435,]           2           2        NA       NA
[2436,]          NA          NA        NA       NA
[2437,]           2          NA        NA       NA
[2438,]           1           1         1        1
[2439,]          NA          NA         1        1
[2440,]          NA          NA         1        1
[2441,]          NA          NA         1        1
[2442,]          NA          NA         2        2
[2443,]           2          NA         1        1
[2444,]           2          NA         1        1
[2445,]           1          NA         1        1
[2446,]          NA          NA         1        2
[2447,]          NA          NA         1        1
[2448,]           2           2         1        1
[2449,]           1           1         1        2
[2450,]           2           2         2        2
[2451,]           2          NA         1        1
[2452,]          NA          NA         1        1
[2453,]          NA          NA         2        2
[2454,]           2          NA         1        1
[2455,]          NA          NA         2        1
[2456,]           3           3        NA       NA
[2457,]           3           3        NA       NA
[2458,]           3           3        NA       NA
[2459,]           3           3        NA       NA
[2460,]           2           3        NA       NA
[2461,]           3           3        NA       NA
[2462,]           3           3        NA       NA
[2463,]           3          NA        NA       NA
[2464,]           3          NA        NA       NA
[2465,]           2           3        NA       NA
[2466,]           2           3        NA       NA
[2467,]           2           3        NA       NA
[2468,]           3           3        NA       NA
[2469,]          NA          NA        NA       NA
[2470,]          NA          NA        NA       NA
[2471,]          NA          NA        NA       NA
[2472,]           2          NA        NA       NA
[2473,]           2          NA        NA       NA
[2474,]          NA          NA        NA       NA
[2475,]          NA          NA        NA       NA
[2476,]           3          NA        NA       NA
[2477,]           3           3        NA       NA
[2478,]           2          NA        NA       NA
[2479,]           2          NA        NA       NA
[2480,]           2          NA        NA       NA
[2481,]          NA          NA        NA       NA
[2482,]           2          NA        NA       NA
[2483,]          NA          NA        NA       NA
[2484,]           2           3        NA       NA
[2485,]           3           3        NA       NA
[2486,]           2           3        NA       NA
[2487,]           3          NA        NA       NA
[2488,]           3           3        NA       NA
[2489,]           2           3        NA       NA
[2490,]           3           3        NA       NA
[2491,]           2          NA        NA       NA
[2492,]          NA          NA        NA       NA
[2493,]          NA          NA        NA       NA
[2494,]           2           3        NA       NA
[2495,]           2          NA        NA       NA
[2496,]           2          NA        NA       NA
[2497,]           2          NA        NA       NA
[2498,]           2          NA        NA       NA
[2499,]           2          NA        NA       NA
[2500,]           3           3        NA       NA
[2501,]           2           3        NA       NA
[2502,]           2           3        NA       NA
[2503,]          NA          NA         1        1
[2504,]           1           1         1        1
[2505,]           2           2         1        1
[2506,]           2           2         1        1
[2507,]          NA          NA        NA       NA
[2508,]          NA          NA        NA       NA
[2509,]          NA          NA        NA       NA
[2510,]          NA          NA        NA       NA
[2511,]          NA          NA        NA       NA
[2512,]          NA          NA        NA       NA
[2513,]          NA          NA        NA       NA
[2514,]           2          NA        NA       NA
[2515,]           2          NA        NA       NA
[2516,]          NA          NA        NA       NA
[2517,]          NA          NA        NA       NA
[2518,]           2          NA        NA       NA
[2519,]           2          NA        NA       NA
[2520,]           3           3        NA       NA
[2521,]           2          NA        NA       NA
[2522,]           2          NA        NA       NA
[2523,]           2          NA        NA       NA
[2524,]           2          NA        NA       NA
[2525,]           2          NA        NA       NA
[2526,]          NA          NA        NA       NA
[2527,]          NA          NA        NA       NA
[2528,]           3          NA        NA       NA
[2529,]           2           2        NA       NA
[2530,]          NA          NA        NA       NA
[2531,]          NA          NA        NA       NA
[2532,]          NA          NA        NA       NA
[2533,]          NA          NA        NA       NA
[2534,]           3           3        NA       NA
[2535,]           2           3        NA       NA
[2536,]           2          NA        NA       NA
[2537,]          NA          NA        NA       NA
[2538,]          NA          NA        NA       NA
[2539,]          NA          NA        NA       NA
[2540,]          NA          NA        NA       NA
[2541,]          NA          NA        NA       NA
[2542,]           2          NA        NA       NA
[2543,]          NA          NA        NA       NA
[2544,]          NA          NA        NA       NA
[2545,]          NA          NA        NA       NA
[2546,]          NA          NA        NA       NA
[2547,]          NA          NA        NA       NA
[2548,]           2           1         1        1
[2549,]           2           2         1        1
[2550,]           2           2         2        2
[2551,]           1           1         1        2
[2552,]           2          NA         1        1
[2553,]           2           2         1        1
[2554,]          NA          NA         1        1
[2555,]          NA          NA         1        1
[2556,]          NA          NA         1        1
[2557,]          NA          NA         1        1
[2558,]          NA          NA         2        1
[2559,]          NA          NA         2        2
[2560,]          NA          NA         1        1
[2561,]           2           2         1        1
[2562,]           3          NA         1        1
[2563,]          NA          NA         1        1
[2564,]           2          NA         1        1
[2565,]          NA          NA         2        1
[2566,]           1          NA         1        1
[2567,]           2          NA         1        1
[2568,]          NA          NA         1        1
[2569,]          NA          NA         1        1
[2570,]          NA          NA         1        1
[2571,]           2          NA         1        1
[2572,]          NA          NA         1        2
[2573,]           2          NA         1        1
[2574,]           2           1         1        1
[2575,]          NA          NA         1        1
[2576,]          NA          NA         1        1
[2577,]           1          NA         1        1
[2578,]           1          NA         2        3
[2579,]           2          NA         1        1
[2580,]           1           1         2        3
[2581,]           1           2         1        1
[2582,]           2          NA         1        1
[2583,]          NA          NA        NA       NA
[2584,]          NA          NA        NA       NA
[2585,]          NA          NA        NA       NA
[2586,]          NA          NA        NA       NA
[2587,]          NA          NA        NA       NA
[2588,]          NA          NA        NA       NA
[2589,]           3          NA        NA       NA
[2590,]           2           2        NA       NA
[2591,]           2          NA         1        1
[2592,]          NA          NA         1        2
[2593,]          NA          NA         1        1
[2594,]           1           2         1        1
[2595,]           2          NA         1        1
[2596,]          NA          NA         1        1
[2597,]           2          NA        NA       NA
[2598,]          NA          NA         1        1
[2599,]           2           3         1        1
[2600,]           2           3         1        1
[2601,]           1          NA         1        1
[2602,]           2           2         1        1
[2603,]           2          NA         1        1
[2604,]           2           2         2        1
[2605,]          NA          NA         1        1
[2606,]           1          NA         1        1
[2607,]           2          NA         1        1
[2608,]          NA          NA         1        1
[2609,]           1           1         1        1
[2610,]           1           1         1        1
[2611,]           2          NA         1        1
[2612,]          NA          NA         1        1
[2613,]           1           2         1        1
[2614,]          NA          NA         1        1
[2615,]          NA          NA         1        2
[2616,]           1           1         1        2
[2617,]           1           1         1        2
[2618,]           2           2         2        1
[2619,]           1          NA         1        1
[2620,]           2           2         1        1
[2621,]           2          NA         1        1
[2622,]           2          NA         1        1
[2623,]           1           1         1        1
[2624,]          NA          NA         1        1
[2625,]           2           2         1        1
[2626,]          NA          NA         1        1
[2627,]           2           3         1        1
[2628,]          NA          NA         1        1
[2629,]          NA          NA         1        1
[2630,]          NA          NA         1        1
[2631,]          NA          NA         1        1
[2632,]           2          NA         1        1
[2633,]          NA          NA         1        1
[2634,]          NA          NA         1        1
[2635,]           2          NA         1        1
[2636,]          NA          NA         1        1
[2637,]          NA          NA         1        1
[2638,]          NA          NA         2        1
[2639,]           2          NA        NA       NA
[2640,]           2          NA        NA       NA
[2641,]           2           3        NA       NA
[2642,]          NA          NA        NA       NA
[2643,]          NA          NA        NA       NA
[2644,]          NA          NA        NA       NA
[2645,]          NA          NA        NA       NA
[2646,]          NA          NA        NA       NA
[2647,]           2          NA         1        1
[2648,]           1           1         1        1
[2649,]          NA          NA         1        1
[2650,]          NA          NA         2        1
[2651,]          NA          NA         2        3
[2652,]           2          NA         1        1
[2653,]           1          NA         2        2
[2654,]          NA          NA         1        2
[2655,]          NA          NA         1        1
[2656,]           2           3         1        1
[2657,]           2           2         1        1
[2658,]           2           2         1        1
[2659,]           2          NA         1        1
[2660,]          NA          NA         1        1
[2661,]          NA          NA         2        2
[2662,]           3          NA         1        1
[2663,]          NA          NA         2        2
[2664,]           3           3         1        1
[2665,]           3           3        NA       NA
[2666,]           3           3         1        1
[2667,]           3           3         1        1
[2668,]           3           3         1        1
[2669,]           3          NA         1        1
[2670,]           3          NA         1        1
[2671,]           2           3        NA       NA
[2672,]           3           3        NA       NA
[2673,]           3           3         1        1
[2674,]           3           3         1        1
[2675,]          NA          NA         2        1
[2676,]          NA          NA         1        1
[2677,]           3          NA         1        1
[2678,]          NA          NA         1        1
[2679,]           3           3         1        1
[2680,]           3          NA         1        1
[2681,]           3          NA         1        1
[2682,]          NA          NA         1        1
[2683,]           3          NA        NA       NA
[2684,]          NA          NA         1        1
[2685,]           3           3         1        1
[2686,]           3           3         1        1
[2687,]           3          NA         1        1
[2688,]           2           3         1        1
[2689,]           2          NA         1        1
[2690,]          NA          NA         1        1
[2691,]           2          NA         1        1
[2692,]           2          NA        NA       NA
[2693,]           2          NA         1        1
[2694,]           3           3         2        1
[2695,]           3           3         2        1
[2696,]          NA          NA         1        1
[2697,]           1           1         1        1
[2698,]           2           3         1        1
[2699,]           2           2         1        1
[2700,]          NA          NA         1        1
[2701,]          NA          NA         1        1
[2702,]          NA          NA         1        1
[2703,]          NA          NA         1        1
[2704,]          NA          NA         1        1
[2705,]          NA          NA         1        1
[2706,]          NA          NA         1        1
[2707,]           3          NA        NA       NA
[2708,]           2          NA         1        1
[2709,]          NA          NA         1        1
[2710,]           3          NA         1        1
[2711,]           2          NA         2        1
[2712,]           3           3         1        1
[2713,]           2          NA         1        1
[2714,]           1          NA         3        3
[2715,]           2          NA         1        1
[2716,]           3          NA         1        1
[2717,]           2          NA         1        1
[2718,]          NA          NA         2        2
[2719,]          NA          NA        NA       NA
[2720,]           3           3         1        1
[2721,]          NA          NA         1        1
[2722,]          NA          NA         1        1
[2723,]           3           3         1        1
[2724,]           3           3         1        1
[2725,]           3          NA         2        1
[2726,]          NA          NA         1        1
[2727,]          NA          NA         1        1
[2728,]          NA          NA         1        1
[2729,]          NA          NA         1        1
[2730,]           3          NA         1        1
[2731,]          NA          NA         1        1
[2732,]          NA          NA         1        1
[2733,]          NA          NA        NA       NA
[2734,]           2           3        NA       NA
[2735,]           2           3        NA       NA
[2736,]           3          NA        NA       NA
[2737,]           2           3        NA       NA
[2738,]           3           3        NA       NA
[2739,]           1           1        NA       NA
[2740,]           2          NA        NA       NA
[2741,]           2          NA        NA       NA
[2742,]           1          NA        NA       NA
[2743,]           2          NA        NA       NA
[2744,]           1           2        NA       NA
[2745,]          NA          NA        NA       NA
[2746,]          NA          NA        NA       NA
[2747,]           2          NA        NA       NA
[2748,]           2           2        NA       NA
[2749,]           1          NA        NA       NA
[2750,]          NA          NA        NA       NA
[2751,]           1          NA        NA       NA
[2752,]          NA          NA        NA       NA
[2753,]          NA          NA        NA       NA
[2754,]           1          NA        NA       NA
[2755,]          NA          NA        NA       NA
[2756,]           3          NA        NA       NA
[2757,]          NA          NA        NA       NA
[2758,]          NA          NA        NA       NA
[2759,]           2           3        NA       NA
[2760,]          NA          NA        NA       NA
[2761,]          NA          NA        NA       NA
[2762,]          NA          NA        NA       NA
[2763,]           2           2        NA       NA
[2764,]           1          NA        NA       NA
[2765,]          NA          NA        NA       NA
[2766,]          NA          NA        NA       NA
[2767,]           3           3        NA       NA
[2768,]           2          NA        NA       NA
[2769,]           1           1        NA       NA
[2770,]           3           2        NA       NA
[2771,]           3           3        NA       NA
[2772,]          NA          NA        NA       NA
[2773,]           2          NA        NA       NA
[2774,]           2           1        NA       NA
[2775,]           2           2        NA       NA
[2776,]           1           2        NA       NA
[2777,]          NA          NA        NA       NA
[2778,]           1           1        NA       NA
[2779,]           2          NA        NA       NA
[2780,]           1           1        NA       NA
[2781,]          NA          NA         1        1
[2782,]           2           2         1        1
[2783,]           2           2         1        1
[2784,]           2           2         1        1
[2785,]           2           2         2        1
[2786,]           2           2         1        1
[2787,]           1           2         2        1
[2788,]           2           2         2        1
[2789,]           2           2         1        1
[2790,]           2          NA         2        1
[2791,]          NA          NA        NA       NA
[2792,]          NA          NA        NA       NA
[2793,]           2           2        NA       NA
[2794,]           2           2        NA       NA
[2795,]           2           2        NA       NA
[2796,]          NA          NA        NA       NA
[2797,]           2           2        NA       NA
[2798,]           2          NA        NA       NA
[2799,]          NA          NA        NA       NA
[2800,]           1          NA        NA       NA
[2801,]           2          NA        NA       NA
[2802,]          NA          NA        NA       NA
[2803,]           2           3         1        1
[2804,]          NA          NA         1        1
[2805,]          NA          NA        NA       NA
[2806,]           3           3        NA       NA
[2807,]           3           3        NA       NA
[2808,]           3           3        NA       NA
[2809,]           3           3        NA       NA
[2810,]           3           3        NA       NA
[2811,]           3           3        NA       NA
[2812,]           3          NA        NA       NA
[2813,]           3          NA        NA       NA
[2814,]           3          NA        NA       NA
[2815,]          NA          NA        NA       NA
[2816,]           3          NA        NA       NA
[2817,]          NA          NA        NA       NA
[2818,]          NA          NA        NA       NA
[2819,]           3          NA        NA       NA
[2820,]          NA          NA        NA       NA
[2821,]          NA          NA        NA       NA
[2822,]          NA          NA        NA       NA
[2823,]           1           1         2        1
[2824,]           3          NA         1        1
[2825,]           2          NA         1        1
[2826,]           1          NA         3        3
[2827,]           2          NA         3        2
[2828,]           1           1         3        2
[2829,]          NA          NA         2        1
[2830,]          NA          NA         2        1
[2831,]           3          NA         1        1
[2832,]           1          NA         2        1
[2833,]          NA          NA         1        1
[2834,]           1          NA         1        1
[2835,]          NA          NA         2        2
[2836,]          NA          NA         2        1
[2837,]           3          NA         1        1
[2838,]          NA          NA         2        1
[2839,]          NA          NA         1        1
[2840,]           3           3         1        1
[2841,]          NA          NA         2        2
[2842,]          NA          NA         1        1
[2843,]          NA          NA         1        1
[2844,]           2           2         2        1
[2845,]           2          NA         2        1
[2846,]          NA          NA         2        1
[2847,]           3           3         1        1
[2848,]           3          NA         1        1
[2849,]           1           1         2        2
[2850,]           3           3         1        1
[2851,]           3           3         1        1
[2852,]          NA          NA         2        2
[2853,]           2          NA         2        1
[2854,]           2           1         1        1
[2855,]           3           2         1        1
[2856,]           3          NA        NA       NA
[2857,]          NA          NA        NA       NA
[2858,]           3          NA        NA       NA
[2859,]           3          NA        NA       NA
[2860,]          NA          NA        NA       NA
[2861,]          NA          NA        NA       NA
[2862,]          NA          NA        NA       NA
[2863,]           3          NA        NA       NA
[2864,]           3          NA        NA       NA
[2865,]           1           2         3        1
[2866,]           3           3        NA       NA
[2867,]           3           3        NA       NA
[2868,]           3           3        NA       NA
[2869,]           3           3        NA       NA
[2870,]           3           3        NA       NA
[2871,]          NA          NA        NA       NA
[2872,]           2          NA        NA       NA
[2873,]           2          NA        NA       NA
[2874,]           2          NA        NA       NA
[2875,]           3           3        NA       NA
[2876,]           3          NA        NA       NA
[2877,]           2          NA        NA       NA
[2878,]           3           3        NA       NA
[2879,]          NA          NA         1        1
[2880,]           2           2         1        1
[2881,]          NA          NA        NA       NA
[2882,]          NA          NA        NA       NA
[2883,]           3          NA        NA       NA
[2884,]          NA          NA        NA       NA
[2885,]          NA          NA        NA       NA
[2886,]          NA          NA        NA       NA
[2887,]          NA          NA        NA       NA
[2888,]           3          NA        NA       NA
[2889,]           2          NA        NA       NA
[2890,]           3          NA        NA       NA
[2891,]           3          NA        NA       NA
[2892,]          NA          NA        NA       NA
[2893,]           3           3        NA       NA
[2894,]           3          NA        NA       NA
[2895,]           2           3         1        1
[2896,]           1          NA         2        1
[2897,]          NA          NA         1        1
[2898,]          NA          NA         1        1
[2899,]           2           2         1        1
[2900,]           3          NA         2        1
[2901,]           3          NA         2        1
[2902,]           1           2         2        1
[2903,]           2           2         1        1
[2904,]           1          NA         1        1
[2905,]           1           2         2        1
[2906,]           2           3         1        1
[2907,]           2          NA         1        1
[2908,]          NA          NA        NA       NA
[2909,]           2          NA         2        1
[2910,]          NA          NA         2        2
[2911,]           2           2         3        1
[2912,]           2           3         1        1
[2913,]           3          NA         1        1
[2914,]           1           1         1        1
[2915,]          NA          NA         1        1
[2916,]           3          NA         1        1
[2917,]          NA          NA         1        1
[2918,]          NA          NA         1        1
[2919,]           3           3         1        1
[2920,]          NA          NA         1        1
[2921,]           2          NA         1        1
[2922,]           3           2         2        1
[2923,]          NA          NA         2        1
[2924,]           2           1         2        1
[2925,]          NA          NA         1        1
[2926,]          NA          NA         2        1
[2927,]          NA          NA         1        1
[2928,]          NA          NA         2        2
[2929,]          NA          NA         1        1
[2930,]          NA          NA         2        1
[2931,]          NA          NA         1        1
[2932,]          NA          NA         1        1
[2933,]          NA          NA         2        2
[2934,]           1          NA         3        2
[2935,]          NA          NA         2        1
[2936,]          NA          NA         2        2
[2937,]           3          NA        NA       NA
[2938,]          NA          NA         1        1
[2939,]          NA          NA         3        2
[2940,]           3          NA         1        1
[2941,]           1          NA         1        1
[2942,]          NA          NA        NA       NA
[2943,]           3           3        NA       NA
[2944,]          NA          NA         1        1
[2945,]           3          NA         1        1
[2946,]           2          NA         1        1
[2947,]           1           1         2        1
[2948,]           2           3         2        1
[2949,]           2          NA         1        1
[2950,]          NA          NA         1        1
[2951,]          NA          NA         2        1
[2952,]           2           2         1        1
[2953,]           2           2         2        1
[2954,]           3          NA         2        1
[2955,]           1           1         2        1
[2956,]           3          NA         2        1
[2957,]           2           2         1        1
[2958,]          NA          NA         2        1
[2959,]          NA          NA         2        1
[2960,]          NA          NA         2        1
[2961,]           1          NA         2        1
[2962,]          NA          NA         2        1
[2963,]           2          NA        NA       NA
[2964,]           2          NA        NA       NA
[2965,]          NA          NA        NA       NA
[2966,]          NA          NA        NA       NA
[2967,]           2           3        NA       NA
[2968,]           2           3         1        1
[2969,]           3           3         2        1
[2970,]           3           3         1        1
[2971,]           3           3         1        1
[2972,]           3           3         1        1
[2973,]           3           3         1        1
[2974,]           3           3         2        1
[2975,]           2          NA         1        1
[2976,]           2          NA         2        1
[2977,]           2          NA         1        1
[2978,]          NA          NA         1        1
[2979,]           2          NA         2        1
[2980,]          NA          NA         2        1
[2981,]          NA          NA         2        1
[2982,]           3          NA         1        1
[2983,]          NA          NA         1        1
[2984,]          NA          NA         1        1
[2985,]          NA          NA         1        1
[2986,]           3          NA         1        1
[2987,]           2          NA         1        1
[2988,]           1          NA         3        3
[2989,]           3          NA         2        1
[2990,]           1           1         3        3
[2991,]          NA          NA         1        1
[2992,]          NA          NA         2        1
[2993,]           1          NA         3        2
[2994,]          NA          NA         2        1
[2995,]           1          NA         1        1
[2996,]          NA          NA         2        1
[2997,]           3          NA         1        1
[2998,]          NA          NA         1        1
[2999,]          NA          NA         1        1
[3000,]          NA          NA         2        1
[3001,]          NA          NA         2        1
[3002,]           3           2         1        1
[3003,]           2          NA         2        1
[3004,]          NA          NA         1        1
[3005,]           3          NA         1        1
[3006,]           1           1         2        1
[3007,]           3           3         1        1
[3008,]           3           3         1        1
[3009,]          NA          NA         1        1
[3010,]           2          NA         2        1
[3011,]           2           2         1        1
[3012,]           3           2         2        1
[3013,]           3          NA        NA       NA
[3014,]          NA          NA        NA       NA
[3015,]           2          NA        NA       NA
[3016,]           3           3        NA       NA
[3017,]           3           3        NA       NA
[3018,]           3           3        NA       NA
[3019,]           3           3        NA       NA
[3020,]           3           3        NA       NA
[3021,]          NA          NA         1        1
[3022,]          NA          NA         1        1
[3023,]           3           3        NA       NA
[3024,]          NA          NA         1        1
[3025,]           3          NA         1        1
[3026,]           1           2         2        1
[3027,]           3           3         1        1
[3028,]           3           3         1        1
[3029,]           3           3         1        1
[3030,]           3           3         1        1
[3031,]           3           3         1        1
[3032,]           2          NA         1        1
[3033,]           2          NA         1        1
[3034,]           3           3         2        1
[3035,]           3          NA         2        1
[3036,]           2          NA         2        1
[3037,]           3           3         1        1
[3038,]           2           2         2        1
[3039,]           2          NA         1        1
[3040,]          NA          NA         1        1
[3041,]          NA          NA         2        1
[3042,]          NA          NA         2        1
[3043,]           2          NA         1        1
[3044,]           2          NA         1        1
[3045,]           3          NA         2        1
[3046,]          NA          NA         1        1
[3047,]          NA          NA        NA       NA
[3048,]          NA          NA        NA       NA
[3049,]          NA          NA        NA       NA
[3050,]          NA          NA        NA       NA
[3051,]          NA          NA        NA       NA
[3052,]          NA          NA        NA       NA
[3053,]          NA          NA        NA       NA
[3054,]          NA          NA        NA       NA
[3055,]           3          NA        NA       NA
[3056,]          NA          NA        NA       NA
[3057,]          NA          NA        NA       NA
[3058,]          NA          NA        NA       NA
[3059,]          NA          NA        NA       NA
[3060,]          NA          NA        NA       NA
[3061,]          NA          NA        NA       NA
[3062,]           2           2        NA       NA
[3063,]          NA          NA        NA       NA
[3064,]           1           2        NA       NA
[3065,]          NA          NA        NA       NA
[3066,]           1          NA        NA       NA
[3067,]           1           2        NA       NA
[3068,]          NA          NA        NA       NA
[3069,]           2           3        NA       NA
[3070,]           2          NA        NA       NA
[3071,]           1           2        NA       NA
[3072,]           1           2        NA       NA
[3073,]           2           2        NA       NA
[3074,]           2          NA        NA       NA
[3075,]           1           2        NA       NA
[3076,]           1           2        NA       NA
[3077,]          NA          NA        NA       NA
[3078,]          NA          NA        NA       NA
[3079,]          NA          NA        NA       NA
[3080,]          NA          NA        NA       NA
[3081,]           1           2        NA       NA
[3082,]           2          NA        NA       NA
[3083,]           1          NA        NA       NA
[3084,]           1           1        NA       NA
[3085,]           2          NA        NA       NA
[3086,]           1           1        NA       NA
[3087,]           2          NA        NA       NA
[3088,]          NA          NA        NA       NA
[3089,]          NA          NA        NA       NA
[3090,]          NA          NA        NA       NA
[3091,]          NA          NA        NA       NA
[3092,]           1           2        NA       NA
[3093,]          NA          NA        NA       NA
[3094,]           1           2        NA       NA
[3095,]           1           1        NA       NA
[3096,]          NA          NA        NA       NA
[3097,]          NA          NA        NA       NA
[3098,]           2           2         2        1
[3099,]           3          NA         1        1
[3100,]           2          NA         2        1
[3101,]           2          NA         1        1
[3102,]           2          NA         1        1
[3103,]           1          NA         2        1
[3104,]           2           2         3        1
[3105,]          NA          NA         2        1
[3106,]           3          NA         1        1
[3107,]          NA          NA         2        1
[3108,]           2           2         2        1
[3109,]          NA          NA         1        1
[3110,]           2           2         1        1
[3111,]          NA          NA         2        1
[3112,]          NA          NA         1        1
[3113,]          NA          NA         2        1
[3114,]           2          NA         1        1
[3115,]           2           3         2        1
[3116,]           1           3         2        1
[3117,]          NA          NA         2        1
[3118,]           1           1         1        1
[3119,]          NA          NA         1        1
[3120,]           2          NA         1        1
[3121,]          NA          NA         2        1
[3122,]           1           2         2        1
[3123,]           2          NA         1        1
[3124,]          NA          NA        NA       NA
[3125,]          NA          NA        NA       NA
[3126,]          NA          NA        NA       NA
[3127,]          NA          NA        NA       NA
[3128,]          NA          NA        NA       NA
[3129,]           2          NA        NA       NA
[3130,]           1           3        NA       NA
[3131,]           1           3        NA       NA
[3132,]           1           3        NA       NA
[3133,]           3           3        NA       NA
[3134,]           2           3        NA       NA
[3135,]          NA          NA        NA       NA
[3136,]          NA          NA        NA       NA
[3137,]           2          NA        NA       NA
[3138,]           3          NA        NA       NA
[3139,]           1           2        NA       NA
[3140,]          NA          NA        NA       NA
[3141,]          NA          NA        NA       NA
[3142,]          NA          NA        NA       NA
[3143,]          NA          NA        NA       NA
[3144,]          NA          NA        NA       NA
[3145,]          NA          NA        NA       NA
[3146,]          NA          NA        NA       NA
[3147,]           2           3        NA       NA
[3148,]           2           3        NA       NA
[3149,]           2           2        NA       NA
[3150,]           2           3        NA       NA
[3151,]           2           3        NA       NA
[3152,]           2           3        NA       NA
[3153,]           2           3        NA       NA
[3154,]           2           3        NA       NA
[3155,]           2          NA        NA       NA
[3156,]           2          NA        NA       NA
[3157,]           2          NA        NA       NA
[3158,]          NA          NA        NA       NA
[3159,]          NA          NA        NA       NA
[3160,]          NA          NA         1        1
[3161,]           2           2         2        1
[3162,]          NA          NA         1        1
[3163,]           1           2         1        1
[3164,]          NA          NA         1        1
[3165,]           2          NA         1        1
[3166,]           2           3         1        1
[3167,]           2           3         2        1
[3168,]           2          NA         1        1
[3169,]           1           2         2        1
[3170,]           2           2         1        1
[3171,]           1           2         1        1
[3172,]           1           2         2        1
[3173,]          NA          NA         2        1
[3174,]          NA          NA         2        1
[3175,]          NA          NA         1        1
[3176,]           1           2         1        1
[3177,]           2          NA         1        1
[3178,]           2           2         2        1
[3179,]           1          NA         2        3
[3180,]           1           1         2        2
[3181,]           2          NA         1        1
[3182,]           2          NA         2        1
[3183,]           2           3        NA       NA
[3184,]          NA          NA        NA       NA
[3185,]          NA          NA        NA       NA
[3186,]          NA          NA        NA       NA
[3187,]          NA          NA        NA       NA
[3188,]           2          NA        NA       NA
[3189,]           2          NA        NA       NA
[3190,]           3          NA        NA       NA
[3191,]           2           3        NA       NA
[3192,]          NA          NA        NA       NA
[3193,]          NA          NA        NA       NA
[3194,]          NA          NA        NA       NA
[3195,]          NA          NA        NA       NA
[3196,]           3          NA        NA       NA
[3197,]           2           3        NA       NA
[3198,]           2          NA        NA       NA
[3199,]           2           3        NA       NA
[3200,]           3          NA        NA       NA
[3201,]          NA          NA        NA       NA
[3202,]          NA          NA        NA       NA
[3203,]          NA          NA        NA       NA
[3204,]          NA          NA        NA       NA
[3205,]          NA          NA        NA       NA
[3206,]          NA          NA        NA       NA
[3207,]          NA          NA        NA       NA
[3208,]           2          NA        NA       NA
[3209,]           3          NA        NA       NA
[3210,]           3          NA        NA       NA
[3211,]           2           3        NA       NA
[3212,]           2           3        NA       NA
[3213,]           2          NA        NA       NA
[3214,]           1           3        NA       NA
[3215,]           1           3        NA       NA
[3216,]           1           3        NA       NA
[3217,]           1           2        NA       NA
[3218,]           2           3        NA       NA
[3219,]           2           3        NA       NA
[3220,]           2           3        NA       NA
[3221,]           2           3        NA       NA
[3222,]           2           3        NA       NA
[3223,]          NA          NA         1        1
[3224,]          NA          NA         1        1
[3225,]          NA          NA         2        1
[3226,]           2           2         1        1
[3227,]          NA          NA         1        1
[3228,]           1           2         1        1
[3229,]          NA          NA         2        1
[3230,]          NA          NA         2        2
[3231,]           2          NA        NA       NA
[3232,]           2           3        NA       NA
[3233,]           2          NA        NA       NA
[3234,]           2           3        NA       NA
[3235,]          NA          NA        NA       NA
[3236,]           2          NA        NA       NA
[3237,]           3          NA        NA       NA
[3238,]           3          NA        NA       NA
[3239,]           2          NA         1        1
[3240,]           2           3        NA       NA
[3241,]          NA          NA         2        1
[3242,]           2           3         1        1
[3243,]           2           2         2        1
[3244,]          NA          NA         2        1
[3245,]           1           1         2        1
[3246,]          NA          NA         1        1
[3247,]          NA          NA         2        1
[3248,]           2          NA         1        1
[3249,]          NA          NA         2        1
[3250,]           3          NA         1        1
[3251,]           2           2         2        1
[3252,]           2           2         2        1
[3253,]          NA          NA         2        1
[3254,]          NA          NA         1        1
[3255,]           2          NA         1        1
[3256,]           1           2         2        1
[3257,]           2          NA         2        1
[3258,]           2          NA         1        1
[3259,]           1           1         2        1
[3260,]           1          NA         1        1
[3261,]           2           3         2        1
[3262,]          NA          NA         2        1
[3263,]          NA          NA         2        1
[3264,]           2           2         1        1
[3265,]          NA          NA         2        1
[3266,]          NA          NA        NA       NA
[3267,]          NA          NA         1        1
[3268,]           2          NA         1        1
[3269,]           2          NA         1        1
[3270,]           1           2         2        1
[3271,]           1           2         2        2
[3272,]          NA          NA         2        1
[3273,]          NA          NA        NA       NA
[3274,]          NA          NA        NA       NA
[3275,]          NA          NA        NA       NA
[3276,]          NA          NA        NA       NA
[3277,]          NA          NA        NA       NA
[3278,]           2          NA        NA       NA
[3279,]           2          NA        NA       NA
[3280,]           2          NA        NA       NA
[3281,]           2          NA        NA       NA
[3282,]           2           3        NA       NA
[3283,]           3           3         1        1
[3284,]           2          NA         2        1
[3285,]           2           2         2        1
[3286,]           3           2         2        1
[3287,]           3           3         1        1
[3288,]          NA          NA         2        2
[3289,]           1          NA         1        1
[3290,]           2           2        NA       NA
[3291,]           1          NA         2        2
[3292,]          NA          NA         2        1
[3293,]           1           1        NA        3
[3294,]          NA          NA         2        1
[3295,]           3           3         1        1
[3296,]           2           3         1        1
[3297,]          NA          NA         1        1
[3298,]           1           2         1        1
[3299,]           2          NA         1        1
[3300,]           1          NA         2        1
[3301,]           3           2         1        1
[3302,]           3           2         2        1
[3303,]           2          NA         1        1
[3304,]           1          NA         2        1
[3305,]          NA          NA         2        1
[3306,]          NA          NA         1        1
[3307,]           2           3         1        1
[3308,]           2           3         2        1
[3309,]           2           3        NA       NA
[3310,]           3           3        NA       NA
[3311,]           2           3        NA       NA
[3312,]           2          NA        NA       NA
[3313,]           2          NA        NA       NA
[3314,]           2          NA        NA       NA
[3315,]           2          NA        NA       NA
[3316,]           2          NA        NA       NA
[3317,]           2          NA        NA       NA
[3318,]          NA          NA        NA       NA
[3319,]          NA          NA        NA       NA
[3320,]          NA          NA        NA       NA
[3321,]          NA          NA        NA       NA
[3322,]          NA          NA        NA       NA
[3323,]          NA          NA        NA       NA
[3324,]          NA          NA        NA       NA
[3325,]          NA          NA        NA       NA
[3326,]           2          NA        NA       NA
[3327,]           2          NA        NA       NA
[3328,]           3           3        NA       NA
[3329,]           2          NA        NA       NA
[3330,]          NA          NA        NA       NA
[3331,]          NA          NA        NA       NA
[3332,]           2          NA        NA       NA
[3333,]           3           3        NA       NA
[3334,]           3           3        NA       NA
[3335,]           3           3        NA       NA
[3336,]           3           3        NA       NA
[3337,]           3           3        NA       NA
[3338,]           3          NA         1        1
[3339,]           3          NA         1        1
[3340,]           1           3         1        1
[3341,]          NA          NA         1        1
[3342,]          NA          NA         1        1
[3343,]          NA          NA         1        1
[3344,]           2           3         1        1
[3345,]           3          NA         1        1
[3346,]           2          NA         2        1
[3347,]          NA          NA         1        1
[3348,]          NA          NA         1        1
[3349,]          NA          NA         2        1
[3350,]           2           2         2        1
[3351,]          NA          NA         2        2
[3352,]           1           3         1        1
[3353,]          NA          NA         1        1
[3354,]           2          NA         1        1
[3355,]           2           3         2        1
[3356,]           3          NA         1        1
[3357,]           1           3         1        1
[3358,]           1           2         2        1
[3359,]          NA          NA         2        1
[3360,]          NA          NA         1        1
[3361,]          NA          NA         1        1
[3362,]           1           3         1        1
[3363,]           2          NA         1        1
[3364,]           1          NA         3        3
[3365,]           1           1         2        2
[3366,]           2          NA         1        1
[3367,]           2          NA         2        1
[3368,]          NA          NA         1        1
[3369,]          NA          NA         1        1
[3370,]          NA          NA        NA       NA
[3371,]           3          NA         1        1
[3372,]          NA          NA         1        1
[3373,]          NA          NA         2        1
[3374,]           3          NA         1        1
[3375,]           2           3         1        1
[3376,]          NA          NA         1        1
[3377,]          NA          NA         1        1
[3378,]          NA          NA         1        1
[3379,]          NA          NA         1        1
[3380,]          NA          NA         1        1
[3381,]           3          NA         1        1
[3382,]           3          NA         1        1
[3383,]           2          NA         1        1
[3384,]           1           3         1        1
[3385,]           2           3        NA       NA
[3386,]           2           3         1        1
[3387,]           2           3         1        1
[3388,]          NA          NA         1        1
[3389,]          NA          NA         1        1
[3390,]          NA          NA         1        1
[3391,]          NA          NA         1        1
[3392,]           1           2         1        1
[3393,]          NA          NA         2        2
[3394,]          NA          NA         3        2
[3395,]           3           3         1        1
[3396,]           3          NA         1        1
[3397,]           3          NA         1        1
[3398,]           3          NA         1        1
[3399,]          NA          NA        NA       NA
[3400,]          NA          NA        NA       NA
[3401,]          NA          NA        NA       NA
[3402,]          NA          NA        NA       NA
[3403,]          NA          NA        NA       NA
[3404,]          NA          NA        NA       NA
[3405,]          NA          NA        NA       NA
[3406,]          NA          NA        NA       NA
[3407,]          NA          NA        NA       NA
[3408,]          NA          NA        NA       NA
[3409,]          NA          NA        NA       NA
[3410,]           1          NA        NA       NA
[3411,]           2          NA        NA       NA
[3412,]           2          NA        NA       NA
[3413,]           2           3        NA       NA
[3414,]           1           2        NA       NA
[3415,]           1           2        NA       NA
[3416,]           1           2        NA       NA
[3417,]           2           2         3        1
[3418,]          NA          NA         2        1
[3419,]          NA          NA         1        1
[3420,]           1           1         3        3
[3421,]          NA          NA         1        1
[3422,]          NA          NA         1        1
[3423,]          NA          NA         1        1
[3424,]           1           2         3        2
[3425,]          NA          NA         3        3
[3426,]          NA          NA         1        1
[3427,]           1           3         1        1
[3428,]           2          NA         1        1
[3429,]           3           3         1        1
[3430,]          NA          NA        NA       NA
[3431,]          NA          NA         1        1
[3432,]           2          NA         1        1
[3433,]           1          NA         2        1
[3434,]           2           3         2        1
[3435,]           2           3         1        1
[3436,]          NA          NA         1        1
[3437,]           2          NA         2        1
[3438,]           1          NA         2        1
[3439,]           2          NA         1        1
[3440,]           2           3        NA       NA
[3441,]           2           3        NA       NA
[3442,]           2           2        NA       NA
[3443,]           2           2        NA       NA
[3444,]          NA          NA        NA       NA
[3445,]          NA          NA        NA       NA
[3446,]          NA          NA        NA       NA
[3447,]          NA          NA        NA       NA
[3448,]          NA          NA        NA       NA
[3449,]           2          NA        NA       NA
[3450,]           2          NA        NA       NA
[3451,]           2          NA        NA       NA
[3452,]           2          NA        NA       NA
[3453,]           2           3        NA       NA
[3454,]           2           3        NA       NA
[3455,]           2           3        NA       NA
[3456,]           3           3        NA       NA
[3457,]           3           3        NA       NA
[3458,]          NA          NA        NA       NA
[3459,]           3          NA        NA       NA
[3460,]           2           2        NA       NA
[3461,]           1           1         3        3
[3462,]           1           2         2        1
[3463,]          NA          NA         1        1
[3464,]          NA          NA        NA       NA
[3465,]          NA          NA        NA       NA
[3466,]           2           2        NA       NA
[3467,]           2           3         1        1
[3468,]          NA          NA         1        1
[3469,]          NA          NA         1        1
[3470,]           2          NA         1        1
[3471,]           2           2         1        1
[3472,]          NA          NA         1        1
[3473,]          NA          NA         1        1
[3474,]           1          NA         1        1
[3475,]           2          NA         1        1
[3476,]          NA          NA         1        1
[3477,]          NA          NA         1        1
[3478,]           1           2         2        1
[3479,]          NA          NA         1        1
[3480,]           3           2         1        1
[3481,]          NA          NA         1        1
[3482,]           2          NA         1        1
[3483,]           1          NA         2        1
[3484,]           2           2         1        1
[3485,]           2           2         1        1
[3486,]           2          NA         1        1
[3487,]           1          NA         1        1
[3488,]           2           3        NA       NA
[3489,]          NA          NA        NA       NA
[3490,]          NA          NA        NA       NA
[3491,]           3           3         1        1
[3492,]          NA          NA         2        1
[3493,]          NA          NA         1        1
[3494,]           1           3         2        1
[3495,]           2           3         1        1
[3496,]           2          NA         2        1
[3497,]           1          NA         2        2
[3498,]           2          NA         1        1
[3499,]           2           3        NA       NA
[3500,]           2           3        NA       NA
[3501,]           2           3         1        1
[3502,]           2           3         2        1
[3503,]           2           3         1        1
[3504,]           2           3        NA       NA
[3505,]          NA          NA         1        1
[3506,]          NA          NA         1        1
[3507,]          NA          NA         1        1
[3508,]          NA          NA         1        1
[3509,]          NA          NA         1        1
[3510,]           2          NA        NA       NA
[3511,]           2          NA         1        1
[3512,]           2          NA         1        1
[3513,]           2           3         1        1
[3514,]           3           3         1        1
[3515,]           3          NA         1        1
[3516,]           2           3         1        1
[3517,]           2           2        NA       NA
[3518,]          NA          NA        NA       NA
[3519,]          NA          NA        NA       NA
[3520,]          NA          NA        NA       NA
[3521,]           1           2        NA       NA
[3522,]           1          NA        NA       NA
[3523,]           2           2         3        1
[3524,]          NA          NA         1        1
[3525,]           1           2         2        2
[3526,]           1           1         1        2
[3527,]           1           1         3        3
[3528,]          NA          NA         2        1
[3529,]          NA          NA         2        1
[3530,]           1           1         3        3
[3531,]          NA          NA         1        1
[3532,]          NA          NA         1        1
[3533,]           1          NA         1        1
[3534,]           3           2        NA       NA
[3535,]           2           2        NA       NA
[3536,]           3          NA        NA       NA
[3537,]          NA          NA         1        1
[3538,]           3           2         1        1
[3539,]          NA          NA        NA       NA
[3540,]          NA          NA        NA       NA
[3541,]          NA          NA        NA       NA
[3542,]          NA          NA        NA       NA
[3543,]          NA          NA        NA       NA
[3544,]          NA          NA        NA       NA
[3545,]          NA          NA        NA       NA
[3546,]           2          NA        NA       NA
[3547,]           2          NA        NA       NA
[3548,]           2          NA        NA       NA
[3549,]           2          NA        NA       NA
[3550,]           2          NA        NA       NA
[3551,]           2          NA        NA       NA
[3552,]           2           2        NA       NA
[3553,]           1           3        NA       NA
[3554,]           3           3        NA       NA
[3555,]           3           2        NA       NA
[3556,]           2           2        NA       NA
[3557,]           2           2        NA       NA
[3558,]           2           2        NA       NA
[3559,]           2           2        NA       NA
[3560,]          NA          NA         1        1
[3561,]           1           2        NA       NA
[3562,]          NA          NA         1        1
[3563,]           1           3         1        1
[3564,]           1           1         2        2
[3565,]           1          NA         2        2
[3566,]           2           3        NA       NA
[3567,]           1           3        NA       NA
[3568,]          NA          NA        NA       NA
[3569,]           1           3        NA       NA
[3570,]           1           3        NA       NA
[3571,]           1           3        NA       NA
[3572,]           1           3        NA       NA
[3573,]          NA          NA        NA       NA
[3574,]          NA          NA        NA       NA
[3575,]           3           3        NA       NA
[3576,]           3           3        NA       NA
[3577,]           3           3        NA       NA
[3578,]           3           3        NA       NA
[3579,]          NA          NA        NA       NA
[3580,]          NA          NA        NA       NA
[3581,]          NA          NA        NA       NA
[3582,]          NA          NA        NA       NA
[3583,]          NA          NA        NA       NA
[3584,]          NA          NA        NA       NA
[3585,]          NA          NA        NA       NA
[3586,]          NA          NA        NA       NA
[3587,]          NA          NA        NA       NA
[3588,]           1           3        NA       NA
[3589,]           1           3        NA       NA
[3590,]           1           3        NA       NA
[3591,]           1           2        NA       NA
[3592,]          NA          NA        NA       NA
[3593,]          NA          NA         1        1
[3594,]           1           2         1        1
[3595,]          NA          NA         1        1
[3596,]           1           3         1        1
[3597,]           1          NA         2        3
[3598,]           1           2         2        1
[3599,]           1           3         2        1
[3600,]          NA          NA        NA       NA
[3601,]          NA          NA        NA       NA
[3602,]           3          NA        NA       NA
[3603,]           2           2        NA       NA
[3604,]          NA          NA        NA       NA
[3605,]          NA          NA        NA       NA
[3606,]           3           3        NA       NA
[3607,]           2           3        NA       NA
[3608,]          NA          NA        NA       NA
[3609,]           2           3        NA       NA
[3610,]           2           2        NA       NA
[3611,]           2           2        NA       NA
[3612,]          NA          NA        NA       NA
[3613,]          NA          NA        NA       NA
[3614,]           2          NA        NA       NA
[3615,]           2          NA        NA       NA
[3616,]           2           3        NA       NA
[3617,]           2          NA        NA       NA
[3618,]           3          NA        NA       NA
[3619,]           3           3        NA       NA
[3620,]           2          NA        NA       NA
[3621,]           3           3        NA       NA
[3622,]           2           3        NA       NA
[3623,]           2          NA         1        1
[3624,]           2           2         1        1
[3625,]           1          NA         2        2
[3626,]          NA          NA         1        1
[3627,]           2           1         2        2
[3628,]          NA          NA         1        1
[3629,]          NA          NA         1        1
[3630,]           3          NA         1        1
[3631,]          NA          NA         1        1
[3632,]          NA          NA         1        1
[3633,]           2           2         1        1
[3634,]           2           2         3        1
[3635,]          NA          NA         3        1
[3636,]           1           3         2        1
[3637,]           1           3         2        1
[3638,]          NA          NA         1        1
[3639,]           3          NA         1        1
[3640,]           2          NA         1        1
[3641,]           2           2         1        1
[3642,]           2           3        NA       NA
[3643,]           2           3        NA       NA
[3644,]           2           3        NA       NA
[3645,]           2           3        NA       NA
[3646,]           2           3        NA       NA
[3647,]           2           3        NA       NA
[3648,]           1           3        NA       NA
[3649,]           1           3        NA       NA
[3650,]           1           3        NA       NA
[3651,]           2          NA         1        1
[3652,]           1          NA         2        2
[3653,]           1           2         1        1
[3654,]          NA          NA         1        1
[3655,]          NA          NA         1        1
[3656,]           2          NA         1        1
[3657,]           1           3         1        1
[3658,]           1           2         1        1
[3659,]           3           2         1        1
[3660,]           1           1         1        1
[3661,]           2           2         1        1
[3662,]          NA          NA         2        1
[3663,]           2          NA         2        1
[3664,]           2          NA         3        1
[3665,]          NA          NA         1        1
[3666,]           3           2         1        1
[3667,]          NA          NA         1        1
[3668,]           2          NA         1        1
[3669,]           2           2         1        1
[3670,]           3           3         3        1
[3671,]           2          NA         2        1
[3672,]          NA          NA         2        1
[3673,]          NA          NA         2        2
[3674,]           1           2         1        1
[3675,]          NA          NA         1        1
[3676,]          NA          NA         2        3
[3677,]           1           2         2        2
[3678,]           1          NA         2        3
[3679,]           3          NA         2        1
[3680,]           3          NA         2        1
[3681,]           2           2         3        1
[3682,]          NA          NA         2        3
[3683,]          NA          NA         2        3
[3684,]           2          NA         1        2
[3685,]           2           3        NA       NA
[3686,]           3          NA        NA       NA
[3687,]           3          NA        NA       NA
[3688,]           3          NA        NA       NA
[3689,]           2          NA        NA       NA
[3690,]           2          NA        NA       NA
[3691,]          NA          NA        NA       NA
[3692,]           2          NA        NA       NA
[3693,]          NA          NA        NA       NA
[3694,]           2          NA         2        2
[3695,]          NA          NA         1        1
[3696,]           2           2         1        1
[3697,]          NA          NA         1        1
[3698,]          NA          NA         1        1
[3699,]           2          NA         1        1
[3700,]          NA          NA         1        1
[3701,]           2           3         1        1
[3702,]           2           2         1        1
[3703,]          NA          NA         1        1
[3704,]           2           2         2        1
[3705,]           2           2         2        1
[3706,]           2          NA         1        1
[3707,]           2          NA        NA       NA
[3708,]          NA          NA        NA       NA
[3709,]           2          NA        NA       NA
[3710,]           2          NA        NA       NA
[3711,]           2          NA        NA       NA
[3712,]           3           3        NA       NA
[3713,]           3           3        NA       NA
[3714,]           3           3        NA       NA
[3715,]           2           3        NA       NA
[3716,]           2           3        NA       NA
[3717,]           2           2        NA       NA
[3718,]           2           2        NA       NA
[3719,]           2           2        NA       NA
[3720,]           2           3         2        1
[3721,]           2           2         2        1
[3722,]           2           2         2        1
[3723,]           2           2         2        1
[3724,]           2           3         1        1
[3725,]           2           3         2        1
[3726,]           2           3         1        1
[3727,]           2           2         3        1
[3728,]           2          NA        NA       NA
[3729,]           2          NA        NA       NA
[3730,]          NA          NA        NA       NA
[3731,]          NA          NA        NA       NA
[3732,]          NA          NA        NA       NA
[3733,]          NA          NA        NA       NA
[3734,]           3          NA        NA       NA
[3735,]          NA          NA        NA       NA
[3736,]          NA          NA        NA       NA
[3737,]          NA          NA        NA       NA
[3738,]          NA          NA        NA       NA
[3739,]          NA          NA        NA       NA
[3740,]          NA          NA         3        1
[3741,]          NA          NA         1        1
[3742,]          NA          NA         2        1
[3743,]           1          NA         2        1
[3744,]           1           2         1        1
[3745,]          NA          NA         2        2
[3746,]           1           1         2        2
[3747,]           1          NA         2        1
[3748,]           1          NA         1        1
[3749,]          NA          NA         1        1
[3750,]           1          NA         1        1
[3751,]          NA          NA         2        2
[3752,]           1           2         2        1
[3753,]           1           1         1        1
[3754,]           1           1         2        2
[3755,]           3          NA         2        1
[3756,]          NA          NA         1        1
[3757,]           1           1         2        2
[3758,]           2          NA         1        1
[3759,]          NA          NA         1        1
[3760,]          NA          NA         3        3
[3761,]           2          NA         1        1
[3762,]           1           2         2        2
[3763,]           3           3         1        1
[3764,]           2           2         2        2
[3765,]          NA          NA        NA       NA
[3766,]          NA          NA         3        1
[3767,]          NA          NA         1        1
[3768,]          NA          NA         3        1
[3769,]           3           3         1        1
[3770,]           2           1         2        2
[3771,]           2           2         2        1
[3772,]           2           2         2        1
[3773,]          NA          NA         2        1
[3774,]          NA          NA         1        1
[3775,]           1          NA         2        1
[3776,]           1           1         2        2
[3777,]          NA          NA         1        1
[3778,]          NA          NA         1        1
[3779,]          NA          NA         2        1
[3780,]           1           2         2        2
[3781,]           2          NA         1        1
[3782,]          NA          NA         2        1
[3783,]           1          NA         3        3
[3784,]           3           3        NA       NA
[3785,]           3          NA         2        2
[3786,]           1          NA         2        3
[3787,]          NA          NA         2        3
[3788,]          NA          NA         1        1
[3789,]          NA          NA         1        1
[3790,]           3          NA         1        1
[3791,]           2           3        NA       NA
[3792,]           3           3        NA       NA
[3793,]           2           3        NA       NA
[3794,]           3           3        NA       NA
[3795,]           2           3        NA       NA
[3796,]           2           3        NA       NA
[3797,]           2           3        NA       NA
[3798,]           2           3        NA       NA
[3799,]           1           2        NA       NA
[3800,]           2           3        NA       NA
[3801,]           3           2        NA       NA
[3802,]           3           2        NA       NA
[3803,]           1           2        NA       NA
[3804,]           3           2        NA       NA
[3805,]           2           3        NA       NA
[3806,]           2           3        NA       NA
[3807,]          NA          NA        NA       NA
[3808,]          NA          NA        NA       NA
[3809,]           3          NA        NA       NA
[3810,]          NA          NA        NA       NA
[3811,]           3          NA        NA       NA
[3812,]           1           2        NA       NA
[3813,]           3          NA        NA       NA
[3814,]           1          NA        NA       NA
[3815,]           1           2        NA       NA
[3816,]           3          NA        NA       NA
[3817,]          NA          NA        NA       NA
[3818,]          NA          NA        NA       NA
[3819,]          NA          NA        NA       NA
[3820,]          NA          NA        NA       NA
[3821,]           1           2        NA       NA
[3822,]           1           2        NA       NA
[3823,]          NA          NA        NA       NA
[3824,]          NA          NA        NA       NA
[3825,]          NA          NA        NA       NA
[3826,]           3          NA        NA       NA
[3827,]          NA          NA        NA       NA
[3828,]           3          NA        NA       NA
[3829,]           3          NA        NA       NA
[3830,]          NA          NA         1        1
[3831,]           1           1         2        2
[3832,]           2          NA         1        1
[3833,]          NA          NA         1        1
[3834,]          NA          NA         2        2
[3835,]           1           1         2        2
[3836,]           2           3         3        2
[3837,]           1           2         2        2
[3838,]          NA          NA        NA       NA
[3839,]          NA          NA        NA       NA
[3840,]           2          NA        NA       NA
[3841,]           2          NA        NA       NA
[3842,]           2           2        NA       NA
[3843,]           2           2        NA       NA
[3844,]           2           3        NA       NA
[3845,]           2           3        NA       NA
[3846,]           2           3        NA       NA
[3847,]           1          NA        NA       NA
[3848,]           1          NA        NA       NA
[3849,]           2          NA        NA       NA
[3850,]          NA          NA        NA       NA
[3851,]          NA          NA        NA       NA
[3852,]          NA          NA        NA       NA
[3853,]          NA          NA        NA       NA
[3854,]          NA          NA        NA       NA
[3855,]          NA          NA        NA       NA
[3856,]          NA          NA        NA       NA
[3857,]          NA          NA        NA       NA
[3858,]          NA          NA        NA       NA
[3859,]          NA          NA        NA       NA
[3860,]           2          NA        NA       NA
[3861,]          NA          NA        NA       NA
[3862,]          NA          NA        NA       NA
[3863,]          NA          NA        NA       NA
[3864,]           3           2        NA       NA
[3865,]           2           3        NA       NA
[3866,]           3           3        NA       NA
[3867,]           3           3        NA       NA
[3868,]           2          NA        NA       NA
[3869,]          NA          NA        NA       NA
[3870,]           2           3        NA       NA
[3871,]           2           3        NA       NA
[3872,]           2          NA        NA       NA
[3873,]           2          NA        NA       NA
[3874,]           2          NA        NA       NA
[3875,]          NA          NA        NA       NA
[3876,]           3           2        NA       NA
[3877,]           2           3        NA       NA
[3878,]           3           3        NA       NA
[3879,]           2           3        NA       NA
[3880,]           3           3        NA       NA
[3881,]           2          NA        NA       NA
[3882,]           2           3        NA       NA
[3883,]           2          NA        NA       NA
[3884,]           3           2        NA       NA
[3885,]          NA          NA        NA       NA
[3886,]           2           3        NA       NA
[3887,]           2           3        NA       NA
[3888,]           2          NA        NA       NA
[3889,]          NA          NA        NA       NA
[3890,]           2           3        NA       NA
[3891,]           2          NA        NA       NA
[3892,]          NA          NA        NA       NA
[3893,]           2          NA        NA       NA
[3894,]          NA          NA        NA       NA
[3895,]           2          NA        NA       NA
[3896,]           2          NA        NA       NA
[3897,]           2          NA        NA       NA
[3898,]           2           3        NA       NA
[3899,]           3           3        NA       NA
[3900,]          NA          NA        NA       NA
[3901,]           2          NA        NA       NA
[3902,]           2           2        NA       NA
[3903,]           2          NA        NA       NA
[3904,]           2          NA        NA       NA
[3905,]          NA          NA         1        1
[3906,]          NA          NA         3        1
[3907,]           2           2         3        1
[3908,]           2           3         1        1
[3909,]           2           3         2        1
[3910,]           2           3         2        1
[3911,]           2          N  C-c C-cA         2        1
[3912,]           1          NA         3        1
[3913,]          NA          NA         3        1
[3914,]          NA          NA         2        1
[3915,]          NA          NA         1        1
[3916,]          NA          NA         2        1
[3917,]          NA          NA         1        1
[3918,]          NA          NA         2        1
[3919,]          NA          NA         2        1
[3920,]          NA          NA         2        1
[3921,]           2          NA         2        1
[3922,]           1           1         2        2
[3923,]           1          NA         1        1
[3924,]          NA          NA         1        3
[3925,]          NA          NA         2        1
[3926,]           1          NA         1        2
[3927,]           1           1         2        3
[3928,]           1          NA         1        2
[3929,]           3           3         1        1
[3930,]           1          NA         1        2
[3931,]           2          NA         1        2
[3932,]           1          NA         1        2
[3933,]           3          NA         1        1
[3934,]          NA          NA         1        1
[3935,]          NA          NA         3        1
[3936,]          NA          NA         1        1
[3937,]          NA          NA         3        3
[3938,]           3           2         2        1
[3939,]           3           3         2        1
[3940,]           3           3         1        1
[3941,]           2          NA         2        1
[3942,]          NA          NA         2        1
[3943,]           3           3         1        1
[3944,]           3           3         3        1
[3945,]           3          NA         1        1
[3946,]           3          NA         2        1
[3947,]           3          NA         1        1
[3948,]          NA          NA         2        1
[3949,]           3           3         2        1
[3950,]           2           3         2        1
[3951,]           3           3         2        1
[3952,]           3           3         2        1
[3953,]           2          NA         2        1
[3954,]           3           3         1        1
[3955,]           3          NA         1        1
[3956,]           3           3         2        1
[3957,]           1           1         2        3
[3958,]           2          NA         1        1
[3959,]           2           2         1        1
[3960,]          NA          NA         1        1
[3961,]           1          NA         1        2
[3962,]          NA          NA         1        1
[3963,]           2          NA         1        1
[3964,]           1          NA         1        1
[3965,]           1          NA         1        2
[3966,]          NA          NA         1        1
[3967,]           2          NA         2        2
[3968,]           1           1         1        2
[3969,]          NA          NA         1        2
[3970,]          NA          NA         2        1
[3971,]           2           3         3        1
[3972,]           2           3         2        1
[3973,]           2          NA         3        1
[3974,]          NA          NA         1        1
[3975,]          NA          NA         2        2
[3976,]          NA          NA         1        1
[3977,]          NA          NA         1        1
[3978,]          NA          NA         1        1
[3979,]          NA          NA         1        1
[3980,]          NA          NA         2        2
[3981,]          NA          NA         1        1
[3982,]          NA          NA         1        1
[3983,]          NA          NA         2        2
[3984,]          NA          NA         1        1
[3985,]          NA          NA         1        1
[3986,]          NA          NA         2        2
[3987,]          NA          NA         1        1
[3988,]           2           2         2        1
[3989,]           1          NA         2        1
[3990,]           3          NA         3        1
[3991,]          NA          NA         2        1
[3992,]           1          NA         1        1
[3993,]           1          NA         2        1
[3994,]           1          NA         1        1
[3995,]
> > tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             lm(l$ht ~ poly(l$dbh, 2))
+         })
+     }), recursive = FALSE)
+     c(assign(x, sum(getAIC(models_list)$aic2), envir=parent.frame()),
+       assign(x, sum(getMSE(models_list)$mse/72)))
+ })
Error in poly(l$dbh, 2) (from #5) : 
  'degree' must be less than number of unique points
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>=3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(assign(x, sum(getAIC(models_list)$aic2), envir=parent.frame()),
+       assign(x, sum(getMSE(models_list)$mse/72)))
+ })
Error in poly(l$dbh, 2) (from #6) : 
  'degree' must be less than number of unique points
> 
> tt
        nsumba.1 nsumba.2 nsumba.3 nsumba5.1 nsumba5.2 nsumba5.3 nsumbabig.1
   [1,]        1       NA       NA         1        NA        NA           1
   [2,]        1        1        2         1         1         2           1
   [3,]        1        1       NA         1         1        NA           1
   [4,]        1       NA       NA         1        NA        NA           1
   [5,]        1        1        2         1         1         2           1
   [6,]        1       NA       NA         1        NA        NA           1
   [7,]        1        1        2         1         1         2           1
   [8,]        1        2        3         1         2         3           1
   [9,]        1        2       NA         1         2        NA           1
  [10,]        1        2        2         1         2         2           1
  [11,]        1        1        2         1         1         2           1
  [12,]        1        2       NA         1         2        NA           1
  [13,]        1        1        2         1         1         2           1
  [14,]        1       NA       NA         1        NA        NA           1
  [15,]        1       NA       NA         1        NA        NA           1
  [16,]        1       NA       NA         1        NA        NA           1
  [17,]        1       NA       NA         1        NA        NA           1
  [18,]        1       NA       NA         1        NA        NA           1
  [19,]        1       NA       NA         1        NA        NA           1
  [20,]        1        1        3         1         1         3           1
  [21,]        1        1       NA         1         1        NA           1
  [22,]        1       NA       NA         1        NA        NA           1
  [23,]        1       NA       NA         1        NA        NA           1
  [24,]        1       NA       NA         1        NA        NA           1
  [25,]        3       NA       NA         3        NA        NA           3
  [26,]        1       NA       NA         1        NA        NA           1
  [27,]        1       NA       NA         1        NA        NA           1
  [28,]        1       NA       NA         1        NA        NA           1
  [29,]        1       NA       NA         1        NA        NA           1
  [30,]        1        2       NA         1         2        NA           1
  [31,]        3       NA       NA         3        NA        NA           3
  [32,]        1       NA       NA         1        NA        NA           1
  [33,]        3       NA       NA         3        NA        NA           3
  [34,]        1        1        3         1         1         3           1
  [35,]        1        1        1         1         1         1           1
  [36,]        1        3       NA         1         3        NA           1
  [37,]        1        1       NA         1         1        NA           1
  [38,]        1        1        3         1         1         3           1
  [39,]        1        1       NA         1         1        NA           1
  [40,]        1       NA       NA         1        NA        NA           1
  [41,]        1        1        3         1         1         3           1
  [42,]        1        1        2         1         1         2           1
  [43,]        1        1        1         1         1         1           1
  [44,]        2       NA       NA         2        NA        NA           2
  [45,]        1       NA       NA         1        NA        NA           1
  [46,]        2        2       NA         2         2        NA           2
  [47,]        2        2       NA         2         2        NA           2
  [48,]        2        2       NA         2         2        NA           2
  [49,]        1       NA       NA         1        NA        NA           1
  [50,]        2        2       NA         2         2        NA           2
  [51,]        1        2        3         1         2         3           1
  [52,]        1        2        3         1         2         3           1
  [53,]        1       NA       NA         1        NA        NA           1
  [54,]        1        2        3         1         2         2           1
  [55,]        1        2        3         1         2         3           1
  [56,]        2        2       NA         2         2        NA           2
  [57,]        1       NA       NA         1        NA        NA           1
  [58,]        2       NA       NA         2        NA        NA           2
  [59,]        1        2       NA         1         2        NA           1
  [60,]        1       NA       NA         1        NA        NA           1
  [61,]        2        2       NA         2         2        NA           2
  [62,]        2        2       NA         2         2        NA           2
  [63,]        1       NA       NA         1        NA        NA           1
  [64,]        1       NA       NA         1        NA        NA           1
  [65,]        1       NA       NA         1        NA        NA           1
  [66,]        1        2       NA         1         2        NA           1
  [67,]        1        1        2         1         1         2           1
  [68,]        1        2       NA         1         2        NA           1
  [69,]        1       NA       NA         1        NA        NA           1
  [70,]        1       NA       NA         1        NA        NA           1
  [71,]        1        2        3         1         2         3           1
  [72,]        1       NA       NA         1        NA        NA           1
  [73,]        1       NA       NA         1        NA        NA           1
  [74,]        1       NA       NA         1        NA        NA           1
  [75,]        1        1        3         1         1         3           1
  [76,]        1       NA       NA         1        NA        NA           1
  [77,]        3       NA       NA         3        NA        NA           3
  [78,]        1       NA       NA         1        NA        NA           1
  [79,]        1        1       NA         1         1        NA           1
  [80,]        1       NA       NA         1        NA        NA           1
  [81,]        1        1       NA         1         1        NA           1
  [82,]        1        1       NA         1         1        NA           1
  [83,]        1       NA       NA         1        NA        NA           1
  [84,]        1       NA       NA         1        NA        NA           1
  [85,]        1        1       NA         1         1        NA           1
  [86,]        1        1        3         1         1         3           1
  [87,]        1       NA       NA         1        NA        NA           1
  [88,]        1       NA       NA         1        NA        NA           1
  [89,]        1       NA       NA         1        NA        NA           1
  [90,]        1        1        3         1         1         3           1
  [91,]        1        1       NA         1         1        NA           1
  [92,]        1       NA       NA         1        NA        NA           1
  [93,]        1        1        3         1         1         3           1
  [94,]        1        1        2         1         1         2           1
  [95,]        1       NA       NA         1        NA        NA           1
  [96,]        1       NA       NA         1        NA        NA           1
  [97,]        1        1       NA         1         1        NA           1
  [98,]        1        1        2         1         1         2           1
  [99,]        1       NA       NA         1        NA        NA           1
 [100,]        1       NA       NA         1        NA        NA           1
 [101,]        1       NA       NA         1        NA        NA           1
 [102,]        1       NA       NA         1        NA        NA           1
 [103,]        1        1        1         1         1         1           1
 [104,]        1        3       NA         1         3        NA           1
 [105,]        2        1        3         2         1         3           2
 [106,]        1        1       NA         1         1        NA           1
 [107,]        1       NA       NA         1        NA        NA           1
 [108,]        2        1        3         2         1         3           2
 [109,]        1        1        2         1         1         2           1
 [110,]        1        1        1         1         1         1           1
 [111,]        2        2       NA         2         2        NA           2
 [112,]        2        2       NA         2         2        NA           2
 [113,]        2        2       NA         2         2        NA           2
 [114,]        1        1        3         1         1         3           1
 [115,]        1       NA       NA         1        NA        NA           1
 [116,]        1       NA       NA         1        NA        NA           1
 [117,]        2        2       NA         2         2        NA           2
 [118,]        1        2       NA         1         2        NA           1
 [119,]        1       NA       NA         1        NA        NA           1
 [120,]        2        2       NA         2         2        NA           2
 [121,]        2        2       NA         2         2        NA           2
 [122,]        1       NA       NA         1        NA        NA           1
 [123,]        1       NA       NA         1        NA        NA           1
 [124,]        1       NA       NA         1        NA        NA           1
 [125,]        1        2       NA         1         2        NA           1
 [126,]        1        1        2         1         1         2           1
 [127,]        1        2       NA         1         2        NA           1
 [128,]        1       NA       NA         1        NA        NA           1
 [129,]        1       NA       NA         1        NA        NA           1
 [130,]        1        2        3         1         2         3           1
 [131,]        1       NA       NA         1        NA        NA           1
 [132,]        1       NA       NA         1        NA        NA           1
 [133,]        1        1        3         1         1         3           1
 [134,]        1       NA       NA         1        NA        NA           1
 [135,]        1        1        2         1         1         2           1
 [136,]        1        1       NA         1         2        NA           1
 [137,]        1       NA       NA         1        NA        NA           1
 [138,]        1       NA       NA         1        NA        NA           1
 [139,]        1        2       NA         1         2        NA           1
 [140,]        1        2       NA         1         2        NA           1
 [141,]        1        2       NA         1         2        NA           1
 [142,]        1        2       NA         1         2        NA           1
 [143,]        1        2        2         1         2         2           1
 [144,]        1        2       NA         1         2        NA           1
 [145,]        1        2        2         1         2         2           1
 [146,]        1        1        2         1         1         2           1
 [147,]        1        1        2         1         1         2           1
 [148,]        1       NA       NA         1        NA        NA           1
 [149,]        1        1        2         1         1         2           1
 [150,]        1        2        2         1         2         2           1
 [151,]        1        1       NA         1         1        NA           1
 [152,]        1       NA       NA         1        NA        NA           1
 [153,]        1       NA       NA         1        NA        NA           1
 [154,]        1        1        2         1         1         2           1
 [155,]        1        1        2         1         1         2           1
 [156,]        1        1        2         1         1         2           1
 [157,]        1        1        2         1         1         2           1
 [158,]        1        1        1         1         1         2           1
 [159,]        1       NA       NA         1        NA        NA           1
 [160,]        1        2        2         1         2         2           1
 [161,]        1       NA       NA         1        NA        NA           1
 [162,]        1        1       NA         1         1        NA           1
 [163,]        1        1        3         1         1         3           1
 [164,]        2        1        2         2         1         2           2
 [165,]        1        2       NA         1         2        NA           1
 [166,]        2        1        2         2         1         2           2
 [167,]        1        2       NA         1         2        NA           1
 [168,]        1        2       NA         1         2        NA           1
 [169,]        1        2       NA         1         2        NA           1
 [170,]        1        2        2         1         2         2           1
 [171,]        1        2       NA         1         2        NA           1
 [172,]        1        2        2         1         2         2           1
 [173,]        1        3        3         1         3         3           1
 [174,]        2        2        3         2         2         3           2
 [175,]        2        2        3         2         2         3           2
 [176,]        2        2        3         2         2         3           2
 [177,]        1        2       NA         1         2        NA           1
 [178,]        1        2       NA         1         2        NA           1
 [179,]        1        2       NA         1         2        NA           1
 [180,]        1        2       NA         1         2        NA           1
 [181,]        1        2       NA         1         2        NA           1
 [182,]        1        2       NA         1         2        NA           1
 [183,]        1        2        3         1         2         3           1
 [184,]        1        1       NA         1         1        NA           1
 [185,]        1        2        3         1         2         3           1
 [186,]        1        1       NA         1         1        NA           1
 [187,]        1        1       NA         1         1        NA           1
 [188,]        1        2       NA         1         2        NA           1
 [189,]        2        2        3         2         2         3           2
 [190,]        1        3        3         1         3         2           1
 [191,]        1        2       NA         1         2        NA           1
 [192,]        1        1       NA         1         1        NA           1
 [193,]        1        3        2         1         3         2           1
 [194,]        1        3        3         1         3         2           1
 [195,]        1       NA       NA         1        NA        NA           1
 [196,]        1       NA       NA         1        NA        NA           1
 [197,]        2        2        3         2         2         3           2
 [198,]        1       NA       NA         1        NA        NA           1
 [199,]        1       NA       NA         1        NA        NA           1
 [200,]        1       NA       NA         1        NA        NA           1
 [201,]        1        3        3         1         3         3           1
 [202,]        2        2        3         2         2         3           2
 [203,]        1        2       NA         1         2        NA           1
 [204,]        1       NA       NA         1        NA        NA           1
 [205,]        1       NA       NA         1        NA        NA           1
 [206,]        1       NA       NA         1        NA        NA           1
 [207,]        1        2       NA         1         2        NA           1
 [208,]        1       NA       NA         1        NA        NA           1
 [209,]        1       NA       NA         1        NA        NA           1
 [210,]        3        2       NA         3         2        NA           3
 [211,]        1        1        2         1         1         2           1
 [212,]        1        1        2         1         1         2           1
 [213,]        1       NA       NA         1        NA        NA           1
 [214,]        1        2        2         1         2         2           1
 [215,]        1       NA       NA         1        NA        NA           1
 [216,]        2        3       NA         2         3        NA           2
 [217,]        1       NA       NA         1        NA        NA           1
 [218,]        1       NA       NA         1        NA        NA           1
 [219,]        1        2       NA         1         2        NA           1
 [220,]        1       NA       NA         1        NA        NA           1
 [221,]        1       NA       NA         1        NA        NA           1
 [222,]        1       NA       NA         1        NA        NA           1
 [223,]        1       NA       NA         1        NA        NA           1
 [224,]        1        1        3         1         1         2           1
 [225,]        1       NA       NA         1        NA        NA           1
 [226,]        1       NA       NA         1        NA        NA           1
 [227,]        1        2       NA         1         2        NA           1
 [228,]        1       NA       NA         1        NA        NA           1
 [229,]        1        1       NA         1         1        NA           1
 [230,]        1        2       NA         1         2        NA           1
 [231,]        1        1        3         1         1         2           1
 [232,]        1        1       NA         1         1        NA           1
 [233,]        1        2       NA         1         2        NA           1
 [234,]        1       NA       NA         1        NA        NA           1
 [235,]        1       NA       NA         1        NA        NA           1
 [236,]        1       NA       NA         1        NA        NA           1
 [237,]        1        2       NA         1         2        NA           1
 [238,]        1       NA       NA         1        NA        NA           1
 [239,]        2        3       NA         2         3        NA           2
 [240,]        1       NA       NA         1        NA        NA           1
 [241,]        2        2        3         2         2         3           2
 [242,]        1        2        3         1         2         3           1
 [243,]        1        2       NA         1         2        NA           1
 [244,]        1       NA       NA         1        NA        NA           1
 [245,]        1       NA       NA         1        NA        NA           1
 [246,]        1        2        3         1         2         2           1
 [247,]        1        1        2         1         1         2           1
 [248,]        1        1        2         1         1         2           1
 [249,]        1        2        2         1         2         2           1
 [250,]        1       NA       NA         1        NA        NA           1
 [251,]        1        1       NA         1         1        NA           1
 [252,]        2        2       NA         2         2        NA           2
 [253,]        2        2       NA         2         2        NA           2
 [254,]        1        1       NA         1         1        NA           1
 [255,]        2        2       NA         2         2        NA           2
 [256,]        1        2       NA         1         2        NA           1
 [257,]        1        2        2         1         2         2           1
 [258,]        2        2       NA         2         2        NA           2
 [259,]        1        2        3         1         2         3           1
 [260,]        1        2       NA         1         2        NA           1
 [261,]        1        2        2         1         2         2           1
 [262,]        1        2        2         1         2         2           1
 [263,]        1        2       NA         1         2        NA           1
 [264,]        1       NA       NA         1        NA        NA           1
 [265,]        1       NA       NA         1        NA        NA           1
 [266,]        1       NA       NA         1        NA        NA           1
 [267,]        1       NA       NA         1        NA        NA           1
 [268,]        1       NA       NA         1        NA        NA           1
 [269,]        1       NA       NA         1        NA        NA           1
 [270,]        1        2        3         1         2         3           1
 [271,]        1       NA       NA         1        NA        NA           1
 [272,]        1       NA       NA         1        NA        NA           1
 [273,]        1       NA       NA         1        NA        NA           1
 [274,]        1       NA       NA         1        NA        NA           1
 [275,]        1        1       NA         1         1        NA           1
 [276,]        1       NA       NA         1        NA        NA           1
 [277,]        2        2       NA         2         2        NA           2
 [278,]        1       NA       NA         1        NA        NA           1
 [279,]        2        2        2         2         2         2           2
 [280,]        1        2        2         1         2         2           1
 [281,]        1        2        2         1         2         2           1
 [282,]        1       NA       NA         1        NA        NA           1
 [283,]        1        2       NA         1         2        NA           1
 [284,]        1        2        3         1         2         3           1
 [285,]        2        2        3         2         2         2           2
 [286,]        1        2       NA         1         2        NA           1
 [287,]        2        2        2         2         2         2           2
 [288,]        1       NA       NA         1        NA        NA           1
 [289,]        1       NA       NA         1        NA        NA           1
 [290,]        1       NA       NA         1        NA        NA           1
 [291,]        1       NA       NA         1        NA        NA           1
 [292,]        1       NA       NA         1        NA        NA           1
 [293,]        2        2        3         2         2         2           2
 [294,]        1        1       NA         1         1        NA           1
 [295,]        1       NA       NA         1        NA        NA           1
 [296,]        1        2       NA         1         2        NA           1
 [297,]        1       NA       NA         1        NA        NA           1
 [298,]        1       NA       NA         1        NA        NA           1
 [299,]        2        2        3         2         2         2           2
 [300,]        1       NA       NA         1        NA        NA           1
 [301,]        1        3        2         1         2         2           1
 [302,]        1        2        3         1         2         3           1
 [303,]        3        2       NA         3         2        NA           3
 [304,]        1        2        2         1         2         2           1
 [305,]        1       NA       NA         1        NA        NA           1
 [306,]        1        2        3         1         2         2           1
 [307,]        1        1       NA         1         1        NA           1
 [308,]        1       NA       NA         1        NA        NA           1
 [309,]        1       NA       NA         1        NA        NA           1
 [310,]        1       NA       NA         1        NA        NA           1
 [311,]        1       NA       NA         1        NA        NA           1
 [312,]        1        3        3         1         3         3           1
 [313,]        1        2        2         1         2         2           1
 [314,]        1        2       NA         1         2        NA           1
 [315,]        1       NA       NA         1        NA        NA           1
 [316,]        1        2       NA         1         2        NA           1
 [317,]        1        1       NA         1         1        NA           1
 [318,]        1       NA       NA         1        NA        NA           1
 [319,]        1        2       NA         1         2        NA           1
 [320,]        1        2       NA         1         2        NA           1
 [321,]        1        2       NA         1         2        NA           1
 [322,]        1        2        3         1         2         3           1
 [323,]        1        2       NA         1         2        NA           1
 [324,]        1        2        3         1         2         3           1
 [325,]        2        2        3         2         2         3           2
 [326,]        1        3       NA         1         3        NA           1
 [327,]        1        3       NA         1         3        NA           1
 [328,]        1        2        3         1         2         3           1
 [329,]        1        1       NA         1         1        NA           1
 [330,]        1        1       NA         1         1        NA           1
 [331,]        1        3        3         1         3         3           1
 [332,]        1        1       NA         1         1        NA           1
 [333,]        1        3        2         1         3         2           1
 [334,]        1        2       NA         1         2        NA           1
 [335,]        2        2        3         2         2         3           2
 [336,]        1       NA       NA         1        NA        NA           1
 [337,]        1       NA       NA         1        NA        NA           1
 [338,]        2        2        3         2         2         3           2
 [339,]        1       NA       NA         1        NA        NA           1
 [340,]        1       NA       NA         1        NA        NA           1
 [341,]        1        3       NA         1         3        NA           1
 [342,]        1       NA       NA         1        NA        NA           1
 [343,]        1        3       NA         1         3        NA           1
 [344,]        1        1        2         1         1         2           1
 [345,]        1        1        2         1         1         2           1
 [346,]        1       NA       NA         1        NA        NA           1
 [347,]        1        2        2         1         2         2           1
 [348,]        1       NA       NA         1        NA        NA           1
 [349,]        1       NA       NA         1        NA        NA           1
 [350,]        2       NA       NA         2        NA        NA           2
 [351,]        1        2       NA         1         2        NA           1
 [352,]        1       NA       NA         1        NA        NA           1
 [353,]        2       NA       NA         2        NA        NA           2
 [354,]        1        1        3         1         1         3           1
 [355,]        2       NA       NA         2        NA        NA           2
 [356,]        1        1       NA         1         1        NA           1
 [357,]        1        1        3         1         1         3           1
 [358,]        1        1       NA         1         1        NA           1
 [359,]        1        3       NA         1         3        NA           1
 [360,]        1       NA       NA         1        NA        NA           1
 [361,]        2        2       NA         2         2        NA           2
 [362,]        2        2        2         2         2         2           2
 [363,]        1        1        2         1         1         2           1
 [364,]        1        2       NA         1         2        NA           1
 [365,]        1       NA       NA         1        NA        NA           1
 [366,]        1       NA       NA         1        NA        NA           1
 [367,]        1        1        2         1         1         2           1
 [368,]        1        1        2         1         1         2           1
 [369,]        1        2        2         1         2         2           1
 [370,]        1       NA       NA         1        NA        NA           1
 [371,]        1        1       NA         1         1        NA           1
 [372,]        1        2       NA         1         2        NA           1
 [373,]        1        2       NA         1         2        NA           1
 [374,]        1        1        2         1         1         2           1
 [375,]        1       NA       NA         1        NA        NA           1
 [376,]        1        1       NA         1         1        NA           1
 [377,]        1       NA       NA         1        NA        NA           1
 [378,]        2        2       NA         2         2        NA           2
 [379,]        1       NA       NA         1        NA        NA           1
 [380,]        1        1        2         1         1         2           1
 [381,]        1        2        2         1         2         2           1
 [382,]        1        3        3         1         3         3           1
 [383,]        1       NA       NA         1        NA        NA           1
 [384,]        1        2       NA         1         2        NA           1
 [385,]        1        2        3         1         2         3           1
 [386,]        2        2        3         2         2         3           2
 [387,]        1        2       NA         1         2        NA           1
 [388,]        2        2        3         2         2         3           2
 [389,]        3        3       NA         3         3        NA           3
 [390,]        1       NA       NA         1        NA        NA           1
 [391,]        1       NA       NA         1        NA        NA           1
 [392,]        1       NA       NA         1        NA        NA           1
 [393,]        1        2       NA         1         2        NA           1
 [394,]        1       NA       NA         1        NA        NA           1
 [395,]        1       NA       NA         1        NA        NA           1
 [396,]        1        3        3         1         3         3           1
 [397,]        1        2        3         1         2         3           1
 [398,]        1       NA       NA         1        NA        NA           1
 [399,]        1        3        3         1         3         3           1
 [400,]        1        2        3         1         2         3           1
 [401,]        1        2       NA         1         2        NA           1
 [402,]        1        2       NA         1         2        NA           1
 [403,]        1        1       NA         1         1        NA           1
 [404,]        1       NA       NA         1        NA        NA           1
 [405,]        1       NA       NA         1        NA        NA           1
 [406,]        1       NA       NA         1        NA        NA           1
 [407,]        1        3        3         1         3         3           1
 [408,]        1        1        2         1         1         2           1
 [409,]        1        2        3         1         2         3           1
 [410,]        1        2       NA         1         2        NA           1
 [411,]        1       NA       NA         1        NA        NA           1
 [412,]        1       NA       NA         1        NA        NA           1
 [413,]        1        1       NA         1         1        NA           1
 [414,]        1        1       NA         1         1        NA           1
 [415,]        1        1       NA         1         1        NA           1
 [416,]        1        2        3         1         2         3           1
 [417,]        1       NA       NA         1        NA        NA           1
 [418,]        1       NA       NA         1        NA        NA           1
 [419,]        1       NA       NA         1        NA        NA           1
 [420,]        1       NA       NA         1        NA        NA           1
 [421,]        1       NA       NA         1        NA        NA           1
 [422,]        1       NA       NA         1        NA        NA           1
 [423,]        1        3        3         1         3         3           1
 [424,]        1        1        2         1         1         2           1
 [425,]        1        1        3         1         2         3           1
 [426,]        1       NA       NA         1        NA        NA           1
 [427,]        3        2        3         3         2         3           3
 [428,]        1        2        3         1         2         3           1
 [429,]        1        2       NA         1         2        NA           1
 [430,]        1       NA       NA         1        NA        NA           1
 [431,]        1        2        3         1         2         3           1
 [432,]        1        2        3         1         2         3           1
 [433,]        1        2       NA         1         2        NA           1
 [434,]        1       NA       NA         1        NA        NA           1
 [435,]        2       NA       NA         2        NA        NA           2
 [436,]        1        1       NA         1         1        NA           1
 [437,]        1        3        2         1         3         2           1
 [438,]        1        3        2         1         3         2           1
 [439,]        1        3        2         1         3         2           1
 [440,]        1        1        2         1         1         2           1
 [441,]        1        2       NA         1         2        NA           1
 [442,]        1        2       NA         1         2        NA           1
 [443,]        1        1        1         1         1         1           1
 [444,]        1        1        2         1         1         2           1
 [445,]        1        1       NA         1         1        NA           1
 [446,]        2        1       NA         2         1        NA           2
 [447,]        1        1       NA         1         1        NA           1
 [448,]        1       NA       NA         1        NA        NA           1
 [449,]        1        1        1         1         1         1           1
 [450,]        1        1       NA         1         1        NA           1
 [451,]        1       NA       NA         1        NA        NA           1
 [452,]        1        1       NA         1         1        NA           1
 [453,]        1       NA       NA         1        NA        NA           1
 [454,]        1       NA       NA         1        NA        NA           1
 [455,]        1       NA       NA         1        NA        NA           1
 [456,]        1       NA       NA         1        NA        NA           1
 [457,]        1       NA       NA         1        NA        NA           1
 [458,]        1        1        3         1         1         3           1
 [459,]        1        1        3         1         1         3           1
 [460,]        1        1        1         1         1         1           1
 [461,]        1        1        3         1         1         3           1
 [462,]        1       NA       NA         1        NA        NA           1
 [463,]        1       NA       NA         1        NA        NA           1
 [464,]        1        1        1         1         1         1           1
 [465,]        1       NA       NA         1        NA        NA           1
 [466,]        1        1        3         1         1         3           1
 [467,]        1        3        3         1         3         3           1
 [468,]        1       NA       NA         1        NA        NA           1
 [469,]        1       NA       NA         1        NA        NA           1
 [470,]        1       NA       NA         1        NA        NA           1
 [471,]        1       NA       NA         1        NA        NA           1
 [472,]        1        3        3         1         3         3           1
 [473,]        1        1        3         1         1         3           1
 [474,]        1        2        3         1         2         3           1
 [475,]        1       NA       NA         1        NA        NA           1
 [476,]        2       NA       NA         2        NA        NA           2
 [477,]        1        1       NA         1         1        NA           1
 [478,]        1        1       NA         1         1        NA           1
 [479,]        2        2       NA         2         2        NA           2
 [480,]        1        2       NA         1         2        NA           1
 [481,]        1        2       NA         1         2        NA           1
 [482,]        1       NA       NA         1        NA        NA           1
 [483,]        1       NA       NA         1        NA        NA           1
 [484,]        1       NA       NA         1        NA        NA           1
 [485,]        1        1        3         1         1         3           1
 [486,]        1        1        3         1         1         3           1
 [487,]        1        2       NA         1         2        NA           1
 [488,]        1       NA       NA         1        NA        NA           1
 [489,]        1       NA       NA         1        NA        NA           1
 [490,]        1        1        3         1         1         3           1
 [491,]        1        2        3         1         2         3           1
 [492,]        1        2        3         1         2         3           1
 [493,]        1        1        2         1         1         3           1
 [494,]        1        2        2         1         2         2           1
 [495,]        1        2        2         1         2         2           1
 [496,]        1        2       NA         1         2        NA           1
 [497,]        1        3       NA         1         3        NA           1
 [498,]        2        3       NA         2         3        NA           2
 [499,]        1        2        2         1         2         2           1
 [500,]        1       NA       NA         1        NA        NA           1
 [501,]        1        3       NA         1         3        NA           1
 [502,]        1       NA       NA         1        NA        NA           1
 [503,]        1        2       NA         1         2        NA           1
 [504,]        3       NA       NA         3        NA        NA           3
 [505,]        1       NA       NA         1        NA        NA           1
 [506,]        1        2        2         1         2         2           1
 [507,]        1       NA       NA         1        NA        NA           1
 [508,]        1        2        2         1         2         3           1
 [509,]        1        2       NA         1         2        NA           1
 [510,]        1       NA       NA         1        NA        NA           1
 [511,]        1        2       NA         1         2        NA           1
 [512,]        1       NA       NA         1        NA        NA           1
 [513,]        1       NA       NA         1        NA        NA           1
 [514,]        1        2        2         1         2         2           1
 [515,]        1        1        2         1         1         2           1
 [516,]        1        2        2         1         2         2           1
 [517,]        2        2        3         2         2         3           2
 [518,]        1       NA       NA         1        NA        NA           1
 [519,]        1       NA       NA         1        NA        NA           1
 [520,]        1        1       NA         1         1        NA           1
 [521,]        1        1        2         1         1         2           1
 [522,]        2       NA       NA         2        NA        NA           2
 [523,]        1       NA       NA         1        NA        NA           1
 [524,]        1        2       NA         1         2        NA           1
 [525,]        1        1       NA         1         1        NA           1
 [526,]        1       NA       NA         1        NA        NA           1
 [527,]        1        1        2         1         1         2           1
 [528,]        1       NA       NA         1        NA        NA           1
 [529,]        1        1       NA         1         1        NA           1
 [530,]        1       NA       NA         1        NA        NA           1
 [531,]        1        1       NA         1         1        NA           1
 [532,]        1       NA       NA         1        NA        NA           1
 [533,]        1       NA       NA         1        NA        NA           1
 [534,]        1       NA       NA         1        NA        NA           1
 [535,]        1        2        2         1         2         2           1
 [536,]        1        1       NA         1         1        NA           1
 [537,]        1       NA       NA         1        NA        NA           1
 [538,]        1       NA       NA         1        NA        NA           1
 [539,]        1        1        2         1         1         2           1
 [540,]        3       NA       NA         3        NA        NA           3
 [541,]        1       NA       NA         1        NA        NA           1
 [542,]        2        2        3         2         2         3           2
 [543,]        1        2        3         1         2         3           1
 [544,]        1        2        3         1         2         3           1
 [545,]        2        3        3         2         3         3           2
 [546,]        1        2        2         1         2         2           1
 [547,]        1        2       NA         1         2        NA           1
 [548,]        1        2       NA         1         2        NA           1
 [549,]        2        3        3         2         3         3           1
 [550,]        1        2        3         1         2         3           1
 [551,]        2        3        3         2         2         3           2
 [552,]        2        3        3         2         3         3           2
 [553,]        1        2        3         1         2         3           1
 [554,]        1        2        3         1         2         3           1
 [555,]        2        3        3         2         3         3           2
 [556,]        1        2        3         1         2         3           1
 [557,]        1        2        3         1         2         3           1
 [558,]        1        2        3         1         2         3           1
 [559,]        2        2        3         2         2         3           2
 [560,]        2        3        3         2         2         3           2
 [561,]        2        3        3         2         2         3           2
 [562,]        1        2       NA         1         2        NA           1
 [563,]        1        2       NA         1         2        NA           1
 [564,]        2        3       NA         2         2        NA           2
 [565,]        2        3       NA         2         2        NA           2
 [566,]        2        3       NA         2         2        NA           2
 [567,]        2        3        3         2         2         3           2
 [568,]        2        3       NA         2         2        NA           2
 [569,]        2        3       NA         2         2        NA           2
 [570,]        1       NA       NA         1        NA        NA           1
 [571,]        1       NA       NA         1        NA        NA           1
 [572,]        2        3       NA         2         3        NA           2
 [573,]        1       NA       NA         1        NA        NA           1
 [574,]        1       NA       NA         1        NA        NA           1
 [575,]        2        3       NA         2         3        NA           2
 [576,]        3       NA       NA         3        NA        NA           3
 [577,]        3       NA       NA         3        NA        NA           3
 [578,]        3       NA       NA         3        NA        NA           3
 [579,]        3       NA       NA         3        NA        NA           3
 [580,]        2       NA       NA         2        NA        NA           2
 [581,]        2       NA       NA         2        NA        NA           2
 [582,]        2        3       NA         2         2        NA           2
 [583,]        1       NA       NA         1        NA        NA           1
 [584,]        2       NA       NA         2        NA        NA           2
 [585,]        2       NA       NA         2        NA        NA           2
 [586,]        2       NA       NA         2        NA        NA           2
 [587,]        2       NA       NA         2        NA        NA           2
 [588,]        1       NA       NA         1        NA        NA           1
 [589,]        3       NA       NA         3        NA        NA           3
 [590,]        1       NA       NA         1        NA        NA           1
 [591,]        1       NA       NA         1        NA        NA           1
 [592,]        1       NA       NA         1        NA        NA           1
 [593,]        1        2        3         1         2         3           1
 [594,]        2       NA       NA         2        NA        NA           2
 [595,]        1        2       NA         1         2        NA           1
 [596,]        1       NA       NA         1        NA        NA           1
 [597,]        1       NA       NA         1        NA        NA           1
 [598,]        1       NA       NA         1        NA        NA           1
 [599,]        2       NA       NA         2        NA        NA           2
 [600,]        2        2        3         2         2         3           2
 [601,]        1        2        3         1         2         3           1
 [602,]        1       NA       NA         1        NA        NA           1
 [603,]        3       NA       NA         3        NA        NA           3
 [604,]        1        2       NA         1         2        NA           1
 [605,]        1        2        3         1         2         3           1
 [606,]        2        2       NA         2         2        NA           2
 [607,]        1       NA       NA         1        NA        NA           1
 [608,]        1        2        3         1         2         3           1
 [609,]        1        2       NA         1         2        NA           1
 [610,]        1       NA       NA         1        NA        NA           1
 [611,]        1        2       NA         1         2        NA           1
 [612,]        1       NA       NA         1        NA        NA           1
 [613,]        1       NA       NA         1        NA        NA           1
 [614,]        1        2        3         1         2         3           1
 [615,]        1        1        2         1         1         2           1
 [616,]        1        2        2         1         2         2           1
 [617,]        2        2        3         2         2         3           2
 [618,]        3        2        2         3         2         2           3
 [619,]        1        3       NA         1         2        NA           1
 [620,]        1        2        2         1         2         2           1
 [621,]        1        3       NA         1         2        NA           1
 [622,]        3       NA       NA         3        NA        NA           3
 [623,]        1       NA       NA         1        NA        NA           1
 [624,]        1        2       NA         1         2        NA           1
 [625,]        1        2        2         1         2         2           1
 [626,]        1        2        2         1         2         2           1
 [627,]        1        2        2         1         2         2           1
 [628,]        2        2       NA         2         2        NA           2
 [629,]        3        2        2         3         2         2           3
 [630,]        1        2        2         1         2         2           1
 [631,]        1        2       NA         1         2        NA           1
 [632,]        2        2        3         2         2         3           2
 [633,]        1        2       NA         1         2        NA           1
 [634,]        2        2       NA         2         2        NA           2
 [635,]        2        2       NA         2         2        NA           2
 [636,]        3       NA       NA         3        NA        NA           3
 [637,]        1        2        2         1         2         2           1
 [638,]        3        2        2         3         2         2           3
 [639,]        2        2        2         2         2         2           2
 [640,]        1        2        2         1         2         2           1
 [641,]        1        2       NA         1         2        NA           1
 [642,]        2        2       NA         2         2        NA           2
 [643,]        1        2        2         1         2         2           1
 [644,]        1        3       NA         1         2        NA           1
 [645,]        1       NA       NA         1        NA        NA           1
 [646,]        1        1       NA         1         1        NA           1
 [647,]        1       NA       NA         1        NA        NA           1
 [648,]        1        1       NA         1         1        NA           1
 [649,]        1        2        2         1         2         2           1
 [650,]        1        1        2         1         1         2           1
 [651,]        1       NA       NA         1        NA        NA           1
 [652,]        1       NA       NA         1        NA        NA           1
 [653,]        1       NA       NA         1        NA        NA           1
 [654,]        1        2        2         1         2         2           1
 [655,]        1        2        2         1         2         2           1
 [656,]        1        2        2         1         2         2           1
 [657,]        1        1        2         1         1         2           1
 [658,]        1       NA       NA         1        NA        NA           1
 [659,]        1       NA       NA         1        NA        NA           1
 [660,]        3        2       NA         3         2        NA           3
 [661,]        3        2       NA         3         2        NA           3
 [662,]        3        2       NA         3         2        NA           3
 [663,]        1        2        2         1         2         2           1
 [664,]        1        2        2         1         2         2           1
 [665,]        1        2        2         1         2         2           1
 [666,]        1        2        2         1         2         2           1
 [667,]        1        2        2         1         2         2           1
 [668,]        2        1        2         1         1         2           2
 [669,]        1        2        2         1         2         2           1
 [670,]        1        2        2         1         2         2           1
 [671,]        1        1        2         1         1         2           1
 [672,]        1        2        2         1         2         2           1
 [673,]        1        1        2         1         1         2           1
 [674,]        1        2        2         1         2         2           1
 [675,]        1        2        2         1         2         2           1
 [676,]        1        2        2         1         2         2           1
 [677,]        2        1        2         1         1         2           2
 [678,]        1        1        2         1         1         2           1
 [679,]        2        1        2         1         1         2           2
 [680,]        2        1        2         1         1         2           2
 [681,]        2        1        2         1         1         2           2
 [682,]        2        1        2         1         1         2           1
 [683,]        2        1        2         1         1         2           2
 [684,]        1       NA       NA         1        NA        NA           1
 [685,]        1        2       NA         1         2        NA           1
 [686,]        1       NA       NA         1        NA        NA           1
 [687,]        1        2       NA         1         2        NA           1
 [688,]        1       NA       NA         1        NA        NA           1
 [689,]        1       NA       NA         1        NA        NA           1
 [690,]        1       NA       NA         1        NA        NA           1
 [691,]        1        2        2         1         2         2           1
 [692,]        1        1       NA         1         1        NA           1
 [693,]        1       NA       NA         1        NA        NA           1
 [694,]        1       NA       NA         1        NA        NA           1
 [695,]        3       NA       NA         3        NA        NA           3
 [696,]        1        2        2         1         2         2           1
 [697,]        1        2        2         1         2         2           1
 [698,]        1        2        2         1         2         2           1
 [699,]        1        2        2         1         2         2           1
 [700,]        1        2        2         1         2         2           1
 [701,]        1        2        2         1         2         2           1
 [702,]        1        2       NA         1         2        NA           1
 [703,]        1        2       NA         1         2        NA           1
 [704,]        1        1        2         1         1         2           1
 [705,]        1        1        2         1         1         2           1
 [706,]        1        1        2         1         1         2           1
 [707,]        1        1        2         1         1         2           1
 [708,]        1        1        2         1         1         2           1
 [709,]        1        1        2         1         1         2           1
 [710,]        1        1        2         1         1         2           1
 [711,]        1        1        2         1         1         2           1
 [712,]        1        1        2         1         1         3           1
 [713,]        1        1        2         1         1         2           1
 [714,]        1        1        2         1         1         2           1
 [715,]        1        2        2         1         2         2           1
 [716,]        1        1        2         1         1         2           1
 [717,]        1        2       NA         1         2        NA           1
 [718,]        1        1        2         1         1         3           1
 [719,]        1        1        2         1         1         2           1
 [720,]        1        2       NA         1         2        NA           1
 [721,]        1        2        2         1         2         2           1
 [722,]        1        2        2         1         2         2           1
 [723,]        1        2        2         1         2         2           1
 [724,]        2        2        3         2         2         3           2
 [725,]        2        2        3         2         2         3           1
 [726,]        1        2        3         1         2         3           1
 [727,]        2        3        3         2         3         3           1
 [728,]        1        2        3         1         2         3           1
 [729,]        1        2       NA         1         2        NA           1
 [730,]        1        2       NA         1         2        NA           1
 [731,]        2        3        3         2         3         3           2
 [732,]        1        3        3         1         3         3           1
 [733,]        2        3        3         2         3         3           2
 [734,]        2        3        3         2         3         3           2
 [735,]        1        3        3         1         3         3           1
 [736,]        2        3        3         2         3         3           2
 [737,]        1        2        3         1         2         3           1
 [738,]        1        2        3         1         2         3           1
 [739,]        2        2        3         2         2         3           2
 [740,]        2        3        3         2         3         3           2
 [741,]        2        3        3         2         3         3           2
 [742,]        1        2       NA         1         2        NA           1
 [743,]        1        2       NA         1         2        NA           1
 [744,]        2        2       NA         2         2        NA           2
 [745,]        2        2       NA         2         2        NA           2
 [746,]        2        2       NA         2         2        NA           2
 [747,]        2        3        3         2         3         3           2
 [748,]        2        2       NA         2         2        NA           2
 [749,]        2        2       NA         2         2        NA           2
 [750,]        1       NA       NA         1        NA        NA           1
 [751,]        2        3       NA         2         3        NA           2
 [752,]        1        3       NA         1         3        NA           1
 [753,]        3       NA       NA         3        NA        NA           3
 [754,]        3       NA       NA         3        NA        NA           3
 [755,]        3       NA       NA         3        NA        NA           3
 [756,]        2       NA       NA         2        NA        NA           2
 [757,]        2       NA       NA         2        NA        NA           2
 [758,]        2        2       NA         2         2        NA           2
 [759,]        1       NA       NA         1        NA        NA           1
 [760,]        1       NA       NA         1        NA        NA           1
 [761,]        1       NA       NA         1        NA        NA           1
 [762,]        1       NA       NA         1        NA        NA           1
 [763,]        1        2       NA         1         2        NA           1
 [764,]        2       NA       NA         2        NA        NA           2
 [765,]        1       NA       NA         1        NA        NA           1
 [766,]        1       NA       NA         1        NA        NA           1
 [767,]        3       NA       NA         3        NA        NA           3
 [768,]        1       NA       NA         1        NA        NA           1
 [769,]        3       NA       NA         3        NA        NA           3
 [770,]        3       NA       NA         3        NA        NA           3
 [771,]        1        3       NA         1         2        NA           1
 [772,]        3       NA       NA         3        NA        NA           3
 [773,]        3       NA       NA         3        NA        NA           3
 [774,]        2        3        3         2         3         3           2
 [775,]        1        2       NA         1         2        NA           1
 [776,]        2       NA       NA         2        NA        NA           2
 [777,]        1        2       NA         1         2        NA           1
 [778,]        1       NA       NA         1        NA        NA           1
 [779,]        1       NA       NA         1        NA        NA           1
 [780,]        1       NA       NA         1        NA        NA           1
 [781,]        1       NA       NA         1        NA        NA           1
 [782,]        2        2        3         2         2         3           2
 [783,]        1        3        3         1         3         3           1
 [784,]        1       NA       NA         1        NA        NA           1
 [785,]        3       NA       NA         3        NA        NA           3
 [786,]        1        1        2         1         1         2           1
 [787,]        1        2        2         1         2         2           1
 [788,]        1       NA       NA         1        NA        NA           1
 [789,]        1       NA       NA         1        NA        NA           1
 [790,]        1        2        2         1         2         2           1
 [791,]        1        2        2         1         2         2           1
 [792,]        1        2        2         1         2         2           1
 [793,]        1        2       NA         1         2        NA           1
 [794,]        1        2       NA         1         2        NA           1
 [795,]        1        2       NA         1         2        NA           1
 [796,]        1       NA       NA         1        NA        NA           1
 [797,]        1       NA       NA         1        NA        NA           1
 [798,]        1       NA       NA         1        NA        NA           1
 [799,]        1       NA       NA         1        NA        NA           1
 [800,]        1       NA       NA         1        NA        NA           1
 [801,]        1       NA       NA         1        NA        NA           1
 [802,]        1       NA       NA         1        NA        NA           1
 [803,]        1       NA       NA         1        NA        NA           1
 [804,]        1       NA       NA         1        NA        NA           1
 [805,]        2        2       NA         2         2        NA           2
 [806,]        1        2        2         1         2         2           1
 [807,]        1        2        2         1         2         2           1
 [808,]        1        2        2         1         2         2           1
 [809,]        1       NA       NA         1        NA        NA           1
 [810,]        1       NA       NA         1        NA        NA           1
 [811,]        1        2       NA         1         2        NA           1
 [812,]        1       NA       NA         1        NA        NA           1
 [813,]        1        2       NA         1         2        NA           1
 [814,]        1       NA       NA         1        NA        NA           1
 [815,]        1        2        2         1         2         2           1
 [816,]        2        2       NA         2         2        NA           2
 [817,]        1       NA       NA         1        NA        NA           1
 [818,]        1        2        2         1         2         3           1
 [819,]        1       NA       NA         1        NA        NA           1
 [820,]        1        1        2         1         1         2           1
 [821,]        1       NA       NA         1        NA        NA           1
 [822,]        1       NA       NA         1        NA        NA           1
 [823,]        1       NA       NA         1        NA        NA           1
 [824,]        1        2        3         1         2         3           1
 [825,]        1        2       NA         1         2        NA           1
 [826,]        1        3       NA         1         3        NA           1
 [827,]        1        3       NA         1         3        NA           1
 [828,]        1       NA       NA         1        NA        NA           1
 [829,]        1        3       NA         1         3        NA           1
 [830,]        1        2       NA         1         2        NA           1
 [831,]        1        2       NA         1         2        NA           1
 [832,]        1       NA       NA         1        NA        NA           1
 [833,]        1        2       NA         1         2        NA           1
 [834,]        1       NA       NA         1        NA        NA           1
 [835,]        1       NA       NA         1        NA        NA           1
 [836,]        1        2        2         1         2         2           1
 [837,]        2        2       NA         2         2        NA           2
 [838,]        1       NA       NA         1        NA        NA           1
 [839,]        2       NA       NA         2        NA        NA           1
 [840,]        1        1       NA         1         1        NA           1
 [841,]        2       NA       NA         2        NA        NA           2
 [842,]        1       NA       NA         1        NA        NA           1
 [843,]        1       NA       NA         1        NA        NA           1
 [844,]        1        1       NA         1         1        NA           1
 [845,]        1       NA       NA         1        NA        NA           1
 [846,]        1       NA       NA         1        NA        NA           1
 [847,]        1       NA       NA         1        NA        NA           1
 [848,]        1       NA       NA         1        NA        NA           1
 [849,]        1        1        2         1         1         2           1
 [850,]        1        1        2         1         1         2           1
 [851,]        1        1        2         1         1         2           1
 [852,]        1        1        2         1         1         2           1
 [853,]        1       NA       NA         1        NA        NA           1
 [854,]        2        2       NA         2         2        NA           2
 [855,]        2        2       NA         2         2        NA           1
 [856,]        1       NA       NA         1        NA        NA           1
 [857,]        1        1        2         1         1         2           1
 [858,]        1        2        2         1         2         2           1
 [859,]        1       NA       NA         1        NA        NA           1
 [860,]        1        2        2         1         2         2           1
 [861,]        1        2        2         1         2         2           1
 [862,]        1        2        2         1         2         2           1
 [863,]        1        2       NA         1         2        NA           1
 [864,]        1        2       NA         1         2        NA           1
 [865,]        1        2       NA         1         2        NA           1
 [866,]        1       NA       NA         1        NA        NA           1
 [867,]        1       NA       NA         1        NA        NA           1
 [868,]        1       NA       NA         1        NA        NA           1
 [869,]        1       NA       NA         1        NA        NA           1
 [870,]        1       NA       NA         1        NA        NA           1
 [871,]        1       NA       NA         1        NA        NA           1
 [872,]        1       NA       NA         1        NA        NA           1
 [873,]        1       NA       NA         1        NA        NA           1
 [874,]        1       NA       NA         1        NA        NA           1
 [875,]        1       NA       NA         1        NA        NA           1
 [876,]        1        1       NA         1         1        NA           1
 [877,]        1       NA       NA         1        NA        NA           1
 [878,]        1       NA       NA         1        NA        NA           1
 [879,]        1        2       NA         1         1        NA           1
 [880,]        1        2       NA         1         1        NA           1
 [881,]        1        2       NA         1         2        NA           1
 [882,]        1       NA       NA         1        NA        NA           1
 [883,]        1       NA       NA         1        NA        NA           1
 [884,]        1       NA       NA         1        NA        NA           1
 [885,]        1        2        3         1         2         3           1
 [886,]        1        3        3         1         3         3           1
 [887,]        3        3       NA         3         3        NA           3
 [888,]        1       NA       NA         1        NA        NA           1
 [889,]        2        3       NA         2         3        NA           2
 [890,]        1        2        2         1         2         2           1
 [891,]        1        2        2         1         2         2           1
 [892,]        1        2        2         1         2         2           1
 [893,]        1       NA       NA         1        NA        NA           1
 [894,]        1        2       NA         1         2        NA           1
 [895,]        1       NA       NA         1        NA        NA           1
 [896,]        1        2       NA         1         2        NA           1
 [897,]        1       NA       NA         1        NA        NA           1
 [898,]        1        2        2         1         2         2           1
 [899,]        1       NA       NA         1        NA        NA           1
 [900,]        1        2        3         1         2         3           1
 [901,]        1        1        2         1         1         2           1
 [902,]        1       NA       NA         1        NA        NA           1
 [903,]        1        2        3         1         2         3           1
 [904,]        1        3        2         1         3         2           1
 [905,]        1       NA       NA         1        NA        NA           1
 [906,]        2       NA       NA         2        NA        NA           2
 [907,]        1        2        3         1         2         3           1
 [908,]        1        3        3         1         3         3           1
 [909,]        1        3        2         1         3         2           1
 [910,]        1        3        2         1         3         2           1
 [911,]        1        3        2         1         3         2           1
 [912,]        3        3        3         3         3         3           3
 [913,]        3        3        3         3         3         3           3
 [914,]        1        3        3         1         3         3           1
 [915,]        1        2       NA         1         2        NA           1
 [916,]        1        3        2         1         3         2           1
 [917,]        1        2        3         1         2         3           1
 [918,]        1        2        3         1         2         3           1
 [919,]        1        2       NA         1         2        NA           1
 [920,]        1        2        3         1         2         3           1
 [921,]        1        2        3         1         2         3           1
 [922,]        1        2        3         1         2         3           1
 [923,]        1       NA       NA         1        NA        NA           1
 [924,]        1        2        3         1         2         3           1
 [925,]        1        2       NA         1         2        NA           1
 [926,]        1        3       NA         1         3        NA           1
 [927,]        1        3       NA         1         3        NA           1
 [928,]        1       NA       NA         1        NA        NA           1
 [929,]        1        2       NA         1         2        NA           1
 [930,]        1        3       NA         1         3        NA           1
 [931,]        1        3       NA         1         3        NA           1
 [932,]        1        3       NA         1         3        NA           1
 [933,]        1       NA       NA         1        NA        NA           1
 [934,]        1        3       NA         1         3        NA           1
 [935,]        1       NA       NA         1        NA        NA           1
 [936,]        1       NA       NA         1        NA        NA           1
 [937,]        2        3        2         2         3         2           2
 [938,]        2        3        2         2         3         2           2
 [939,]        2        3        2         2         3         2           2
 [940,]        2        3        2         2         3         2           2
 [941,]        2        3        2         2         3         2           2
 [942,]        2        3        2         2         3         2           2
 [943,]        1        2        3         1         2         3           1
 [944,]        1        2        3         1         2         3           1
 [945,]        1        2        3         1         2         3           1
 [946,]        2        3        2         2         3         2           2
 [947,]        1        3        3         1         3         3           1
 [948,]        1        2        3         1         2         3           1
 [949,]        1        3        3         1         3         3           1
 [950,]        1        2        3         1         2         3           1
 [951,]        1        2        3         1         2         3           1
 [952,]        1        2        3         1         2         3           1
 [953,]        1        3        3         1         3         3           1
 [954,]        1        3        3         1         3         3           1
 [955,]        1        3        3         1         3         3           1
 [956,]        1        3        2         1         3         2           1
 [957,]        1        3        2         1         2         2           1
 [958,]        1        2       NA         1         2        NA           1
 [959,]        1       NA       NA         1        NA        NA           1
 [960,]        1       NA       NA         1        NA        NA           1
 [961,]        2       NA       NA         2        NA        NA           2
 [962,]        1        3        2         1         3         2           1
 [963,]        1       NA       NA         1        NA        NA           1
 [964,]        1        2        3         1         2         3           1
 [965,]        1       NA       NA         1        NA        NA           1
 [966,]        1       NA       NA         1        NA        NA           1
 [967,]        1        2       NA         1         2        NA           1
 [968,]        2        2       NA         2         2        NA           2
 [969,]        1       NA       NA         1        NA        NA           1
 [970,]        2       NA       NA         2        NA        NA           1
 [971,]        1        1       NA         1         1        NA           1
 [972,]        2       NA       NA         2        NA        NA           2
 [973,]        1       NA       NA         1        NA        NA           1
 [974,]        1       NA       NA         1        NA        NA           1
 [975,]        1        2       NA         1         1        NA           1
 [976,]        1       NA       NA         1        NA        NA           1
 [977,]        1       NA       NA         1        NA        NA           1
 [978,]        1       NA       NA         1        NA        NA           1
 [979,]        1       NA       NA         1        NA        NA           1
 [980,]        1        1        2         1         1         2           1
 [981,]        1        2        2         1         2         2           1
 [982,]        1        1        2         1         1         2           1
 [983,]        1        1        3         1         1         3           1
 [984,]        1       NA       NA         1        NA        NA           1
 [985,]        2        2       NA         2         2        NA           2
 [986,]        2        2       NA         2         2        NA           1
 [987,]        1       NA       NA         1        NA        NA           1
 [988,]        1       NA       NA         1        NA        NA           1
 [989,]        1       NA       NA         1        NA        NA           1
 [990,]        2       NA       NA         2        NA        NA           2
 [991,]        2       NA       NA         2        NA        NA           2
 [992,]        2       NA       NA         2        NA        NA           2
 [993,]        1        2       NA         1         2        NA           1
 [994,]        1        1        2         1         1         2           1
 [995,]        1       NA       NA         1        NA        NA           1
 [996,]        1        1        3         1         1         3           1
 [997,]        1        2        2         1         2         2           1
 [998,]        1        2        3         1         2         2           1
 [999,]        1        2        2         1         2         2           1
[1000,]        1        2        3         1         2         2           1
[1001,]        1       NA       NA         1        NA        NA           1
[1002,]        1        2        2         1         2         2           1
[1003,]        1        2        2         1         2         2           1
[1004,]        1        1        3         1         1         3           1
[1005,]        1        1        3         1         1         3           1
[1006,]        1       NA       NA         1        NA        NA           1
[1007,]        1        2        3         1         2         3           1
[1008,]        1        3        3         1         3         3           1
[1009,]        3        3        3         3         2         3           3
[1010,]        3        3        3         3         2         3           3
[1011,]        1        2        3         1         2         3           1
[1012,]        1        2       NA         1         2        NA           1
[1013,]        1        2        3         1         2         3           1
[1014,]        1       NA       NA         1        NA        NA           1
[1015,]        1        2        3         1         2         3           1
[1016,]        1        3       NA         1         3        NA           1
[1017,]        1       NA       NA         1        NA        NA           1
[1018,]        1        3       NA         1         3        NA           1
[1019,]        1        3       NA         1         3        NA           1
[1020,]        2       NA       NA         2        NA        NA           2
[1021,]        1        3       NA         1         3        NA           1
[1022,]        1       NA       NA         1        NA        NA           1
[1023,]        1        2        2         1         2         2           1
[1024,]        1        1        3         1         1         3           1
[1025,]        1        1        3         1         1         3           1
[1026,]        1        1        3         1         1         3           1
[1027,]        1        2        3         1         2         3           1
[1028,]        1        2        2         1         2         2           1
[1029,]        1       NA       NA         1        NA        NA           1
[1030,]        1       NA       NA         1        NA        NA           1
[1031,]        2        3       NA         2         2        NA           2
[1032,]        1        3        2         1         3         2           1
[1033,]        1        1       NA         1         1        NA           1
[1034,]        1        3       NA         1         3        NA           1
[1035,]        1        3        2         1         3         2           1
[1036,]        1       NA       NA         1        NA        NA           1
[1037,]        2        2        2         2         2         2           2
[1038,]        1        1       NA         1         1        NA           1
[1039,]        1        2        2         1         2         2           1
[1040,]        1        2        3         1         2         3           1
[1041,]        2        2        2         2         2         2           2
[1042,]        1        2        2         1         2         2           1
[1043,]        1       NA       NA         1        NA        NA           1
[1044,]        1        2        2         1         2         2           1
[1045,]        1       NA       NA         1        NA        NA           1
[1046,]        1       NA       NA         1        NA        NA           1
[1047,]        1        2        2         1         2         2           1
[1048,]        2        2        2         2         2         2           2
[1049,]        1        2        2         1         2         2           1
[1050,]        1        2        2         1         2         2           1
[1051,]        1        2       NA         1         2        NA           1
[1052,]        1       NA       NA         1        NA        NA           1
[1053,]        1        2       NA         1         2        NA           1
[1054,]        1       NA       NA         1        NA        NA           1
[1055,]        1        2        2         1         2         2           1
[1056,]        1        2        2         1         2         2           1
[1057,]        3       NA       NA         3        NA        NA           3
[1058,]        1       NA       NA         1        NA        NA           1
[1059,]        1        2        2         1         2         2           1
[1060,]        1        2        2         1         2         2           1
[1061,]        1        2        2         1         2         2           1
[1062,]        1        1       NA         1         1        NA           1
[1063,]        1        1       NA         1         1        NA           1
[1064,]        1        2        2         1         2         2           1
[1065,]        1        2        2         1         2         2           1
[1066,]        1       NA       NA         1        NA        NA           1
[1067,]        3        2        2         3         2         2           3
[1068,]        1        2        1         1         2         1           1
[1069,]        1        3        2         1         3         2           1
[1070,]        1        2       NA         1         2        NA           1
[1071,]        1        2       NA         1         2        NA           1
[1072,]        1        2        2         1         3         2           1
[1073,]        1       NA       NA         1        NA        NA           1
[1074,]        1       NA       NA         1        NA        NA           1
[1075,]        1        2        2         1         2         2           1
[1076,]        1       NA       NA         1        NA        NA           1
[1077,]        1        2        2         1         2         2           1
[1078,]        1        1       NA         1         1        NA           1
[1079,]        1        2       NA         1         2        NA           1
[1080,]        1        2        3         1         2         3           1
[1081,]        1        1        2         1         1         2           1
[1082,]        1       NA       NA         1        NA        NA           1
[1083,]        1        2        2         1         3         2           1
[1084,]        1        2        2         1         2         2           1
[1085,]        1       NA       NA         1        NA        NA           1
[1086,]        1        1        2         1         1         2           1
[1087,]        1        1        2         1         1         2           1
[1088,]        1        2       NA         1         2        NA           1
[1089,]        1        1       NA         1         1        NA           1
[1090,]        1        1        2         1         1         2           1
[1091,]        1        1       NA         1         1        NA           1
[1092,]        1        2       NA         1         2        NA           1
[1093,]        1        2        2         1         2         2           1
[1094,]        1       NA       NA         1        NA        NA           1
[1095,]        1        2        2         1         2         2           1
[1096,]        1       NA       NA         1        NA        NA           1
[1097,]        1        1       NA         1         1        NA           1
[1098,]        1       NA       NA         1        NA        NA           1
[1099,]        1       NA       NA         1        NA        NA           1
[1100,]        1       NA       NA         1        NA        NA           1
[1101,]        1        2       NA         1         2        NA           1
[1102,]        1       NA       NA         1        NA        NA           1
[1103,]        1       NA       NA         1        NA        NA           1
[1104,]        1        2        2         1         2         2           1
[1105,]        1        1        2         1         1         2           1
[1106,]        1        1       NA         1         1        NA           1
[1107,]        1       NA       NA         1        NA        NA           1
[1108,]        1        2        2         1         2         2           1
[1109,]        1        1       NA         1         1        NA           1
[1110,]        1        3        3         1         3         3           1
[1111,]        1        2       NA         1         2        NA           1
[1112,]        1        3       NA         1         3        NA           1
[1113,]        1        3        3         1         3         3           1
[1114,]        1       NA       NA         1        NA        NA           1
[1115,]        2        3        3         2         3         3           2
[1116,]        1        2       NA         1         2        NA           1
[1117,]        1        3        3         1         3         3           1
[1118,]        1        3        3         1         3         3           1
[1119,]        2        2        3         2         2         3           2
[1120,]        1       NA       NA         1        NA        NA           1
[1121,]        1        2        3         1         2         3           1
[1122,]        1       NA       NA         1        NA        NA           1
[1123,]        1       NA       NA         1        NA        NA           1
[1124,]        1        2        2         1         2         2           1
[1125,]        2        3        3         2         3         3           2
[1126,]        1        3        3         1         3         3           1
[1127,]        1        3        3         1         3         3           1
[1128,]        1        3       NA         1         3        NA           1
[1129,]        2       NA       NA         2        NA        NA           2
[1130,]        1        3       NA         1         3        NA           1
[1131,]        1       NA       NA         1        NA        NA           1
[1132,]        1        3        2         1         3         2           1
[1133,]        1        2        3         1         2         2           1
[1134,]        3       NA       NA         3        NA        NA           3
[1135,]        1        3        2         1         3         2           1
[1136,]        1       NA       NA         1        NA        NA           1
[1137,]        1        3        2         1         3         2           1
[1138,]        2        3        2         2         3         2           2
[1139,]        1        3        2         1         3         2           1
[1140,]        3        3        2         3         3         2           3
[1141,]        3        3        2         3         3         2           3
[1142,]        3        3        2         3         3         2           3
[1143,]        1        2       NA         1         1        NA           1
[1144,]        2        3        2         2         3         2           2
[1145,]        1        3        2         1         3         2           1
[1146,]        1        2        2         1         2         2           1
[1147,]        1        2        2         1         2         2           1
[1148,]        1        2        2         1         2         2           1
[1149,]        1        1       NA         1         1        NA           1
[1150,]        1        2        2         1         2         2           1
[1151,]        1        2        2         1         2         2           1
[1152,]        1        2        2         1         2         2           1
[1153,]        1        2       NA         1         2        NA           1
[1154,]        1        2        2         1         2         2           1
[1155,]        1        2        2         1         2         2           1
[1156,]        1        1       NA         1         1        NA           1
[1157,]        1        2       NA         1         2        NA           1
[1158,]        1        2        2         1         2         2           1
[1159,]        1        2       NA         1         1        NA           1
[1160,]        1        2       NA         1         1        NA           1
[1161,]        1        3        2         1         3         2           1
[1162,]        1        3        2         1         3         2           1
[1163,]        1        2       NA         1         2        NA           1
[1164,]        1        2       NA         1         2        NA           1
[1165,]        1       NA       NA         1        NA        NA           1
[1166,]        1       NA       NA         1        NA        NA           1
[1167,]        1        2       NA         1         2        NA           1
[1168,]        1        1       NA         1         1        NA           1
[1169,]        1        2       NA         1         2        NA           1
[1170,]        1        2       NA         1         2        NA           1
[1171,]        1        2       NA         1         2        NA           1
[1172,]        1        2       NA         1         2        NA           1
[1173,]        1        2       NA         1         2        NA           1
[1174,]        1       NA       NA         1        NA        NA           1
[1175,]        1       NA       NA         1        NA        NA           1
[1176,]        2       NA       NA         2        NA        NA           2
[1177,]        1       NA       NA         1        NA        NA           1
[1178,]        2       NA       NA         2        NA        NA           2
[1179,]        2       NA       NA         2        NA        NA           2
[1180,]        2       NA       NA         2        NA        NA           2
[1181,]        1       NA       NA         1        NA        NA           1
[1182,]        1        2       NA         1         2        NA           1
[1183,]        1        1       NA         1         1        NA           1
[1184,]        1        2       NA         1         2        NA           1
[1185,]        1        2       NA         1         2        NA           1
[1186,]        2       NA       NA         2        NA        NA           2
[1187,]        1        2       NA         1         2        NA           1
[1188,]        1       NA       NA         1        NA        NA           1
[1189,]        1        3        2         1         3         2           1
[1190,]        1        2        2         1         2         2           1
[1191,]        1       NA       NA         1        NA        NA           1
[1192,]        2        1       NA         2         1        NA           2
[1193,]        2        1       NA         2         1        NA           2
[1194,]        2        1       NA         2         1        NA           2
[1195,]        1       NA       NA         1        NA        NA           1
[1196,]        1       NA       NA         1        NA        NA           1
[1197,]        1       NA       NA         1        NA        NA           1
[1198,]        1       NA       NA         1        NA        NA           1
[1199,]        1       NA       NA         1        NA        NA           1
[1200,]        1       NA       NA         1        NA        NA           1
[1201,]        1       NA       NA         1        NA        NA           1
[1202,]        1       NA       NA         1        NA        NA           1
[1203,]        1       NA       NA         1        NA        NA           1
[1204,]        1        2       NA         1         1        NA           1
[1205,]        1       NA       NA         1        NA        NA           1
[1206,]        1        2       NA         1         1        NA           1
[1207,]        1        2        2         1         2         2           1
[1208,]        1        2        2         1         2         2           1
[1209,]        1        1       NA         1         1        NA           1
[1210,]        3        3        2         3         3         2           3
[1211,]        1       NA       NA         1        NA        NA           1
[1212,]        2        1       NA         2         1        NA           2
[1213,]        1        2        2         1         2         2           1
[1214,]        1       NA       NA         1        NA        NA           1
[1215,]        1        3        2         1         3         2           1
[1216,]        1        2        2         1         2         2           1
[1217,]        1       NA       NA         1        NA        NA           1
[1218,]        1        2       NA         1         1        NA           1
[1219,]        2        1       NA         2         1        NA           2
[1220,]        1       NA       NA         1        NA        NA           1
[1221,]        1       NA       NA         1        NA        NA           1
[1222,]        1       NA       NA         1        NA        NA           1
[1223,]        3        2        2         3         2         2           3
[1224,]        1        3        2         1         3         2           1
[1225,]        1        2       NA         1         2        NA           1
[1226,]        1        3       NA         1         3        NA           1
[1227,]        1        3        3         1         3         3           1
[1228,]        1       NA       NA         1        NA        NA           1
[1229,]        1       NA       NA         1        NA        NA           1
[1230,]        1        2        3         1         2         3           1
[1231,]        1        2        2         1         2         2           1
[1232,]        1        2        2         1         2         2           1
[1233,]        1       NA       NA         1        NA        NA           1
[1234,]        1        2       NA         1         2        NA           1
[1235,]        1        2       NA         1         2        NA           1
[1236,]        1        2       NA         1         2        NA           1
[1237,]        1        1       NA         1         1        NA           1
[1238,]        1        1       NA         1         1        NA           1
[1239,]        1       NA       NA         1        NA        NA           1
[1240,]        1        2       NA         1         2        NA           1
[1241,]        1        2        3         1         2         3           1
[1242,]        1        3        2         1         3         2           1
[1243,]        1       NA       NA         1        NA        NA           1
[1244,]        1        3        2         1         3         2           1
[1245,]        1       NA       NA         1        NA        NA           1
[1246,]        1        1       NA         1         1        NA           1
[1247,]        1        2        3         1         2         3           1
[1248,]        1        2        2         1         2         2           1
[1249,]        1       NA       NA         1        NA        NA           1
[1250,]        1        3        3         1         3         3           1
[1251,]        1        2        3         1         2         3           1
[1252,]        1       NA       NA         1        NA        NA           1
[1253,]        1        2        2         1         2         2           1
[1254,]        1        2        2         2         2         2           1
[1255,]        1        2       NA         1         2        NA           1
[1256,]        1        1       NA         1         1        NA           1
[1257,]        1        1        2         1         1         2           1
[1258,]        1        2       NA         1         2        NA           1
[1259,]        1       NA       NA         1        NA        NA           1
[1260,]        1        2        2         1         2         2           1
[1261,]        1       NA       NA         1        NA        NA           1
[1262,]        1        1       NA         1         1        NA           1
[1263,]        1       NA       NA         1        NA        NA           1
[1264,]        1       NA       NA         1        NA        NA           1
[1265,]        1       NA       NA         1        NA        NA           1
[1266,]        1        3       NA         1         3        NA           1
[1267,]        1       NA       NA         1        NA        NA           1
[1268,]        1        2        2         1         2         2           1
[1269,]        1        1        2         1         1         2           1
[1270,]        1       NA       NA         1        NA        NA           1
[1271,]        1        2        2         1         2         2           1
[1272,]        1        2       NA         1         2        NA           1
[1273,]        1       NA       NA         1        NA        NA           1
[1274,]        1        2        2         1         2         2           1
[1275,]        1        2        2         1         2         2           1
[1276,]        1        2       NA         1         2        NA           1
[1277,]        1        2        2         1         2         2           1
[1278,]        1        2        2         1         2         2           1
[1279,]        1        2        2         1         2         2           1
[1280,]        1        2        2         1         2         2           1
[1281,]        1        3        2         1         3         2           1
[1282,]        1        2        2         1         2         2           1
[1283,]        1        3        2         1         3         2           1
[1284,]        1        2        2         1         2         2           1
[1285,]        1        2        2         1         2         2           1
[1286,]        1        2       NA         1         2        NA           1
[1287,]        1       NA       NA         1        NA        NA           1
[1288,]        1        2       NA         1         2        NA           1
[1289,]        1        2       NA         1         2        NA           1
[1290,]        1        2       NA         1         2        NA           1
[1291,]        1        2       NA         1         2        NA           1
[1292,]        1        2       NA         1         2        NA           1
[1293,]        1        3       NA         1         3        NA           1
[1294,]        1       NA       NA         1        NA        NA           1
[1295,]        1       NA       NA         1        NA        NA           1
[1296,]        1       NA       NA         1        NA        NA           1
[1297,]        1       NA       NA         1        NA        NA           1
[1298,]        1        2       NA         1         2        NA           1
[1299,]        1        3       NA         1         3        NA           1
[1300,]        1        3       NA         1         3        NA           1
[1301,]        1       NA       NA         1        NA        NA           1
[1302,]        1       NA       NA         1        NA        NA           1
[1303,]        1        2        2         1         2         2           1
[1304,]        1        2        2         1         2         2           1
[1305,]        1        3        2         1         2         2           1
[1306,]        1        2       NA         1         2        NA           1
[1307,]        1        1       NA         1         1        NA           1
[1308,]        1       NA       NA         1        NA        NA           1
[1309,]        1       NA       NA         1        NA        NA           1
[1310,]        1       NA       NA         1        NA        NA           1
[1311,]        1       NA       NA         1        NA        NA           1
[1312,]        1        1       NA         1         1        NA           1
[1313,]        1        2        2         1         2         2           1
[1314,]        1        2        2         1         2         2           1
[1315,]        1       NA       NA         1        NA        NA           1
[1316,]        1       NA       NA         1        NA        NA           1
[1317,]        1        1        2         1         1         2           1
[1318,]        1        2        2         1         2         2           1
[1319,]        1        1       NA         1         1        NA           1
[1320,]        1        1        2         1         1         2           1
[1321,]        1       NA       NA         1        NA        NA           1
[1322,]        1       NA       NA         1        NA        NA           1
[1323,]        1       NA       NA         1        NA        NA           1
[1324,]        1       NA       NA         1        NA        NA           1
[1325,]        1        1       NA         1         1        NA           1
[1326,]        1        2       NA         1         2        NA           1
[1327,]        1        1       NA         1         1        NA           1
[1328,]        1        2       NA         1         2        NA           1
[1329,]        2        3        3         3         3         3           2
[1330,]        1        1       NA         1         1        NA           1
[1331,]        1        3        3         1         3         3           1
[1332,]        1       NA       NA         1        NA        NA           1
[1333,]        1        2        2         1         2         2           1
[1334,]        1        2        2         1         2         2           1
[1335,]        1       NA       NA         1        NA        NA           1
[1336,]        1        3        3         1         3         3           1
[1337,]        1       NA       NA         1        NA        NA           1
[1338,]        1        3        3         1         3         3           1
[1339,]        1        1       NA         1         1        NA           1
[1340,]        1       NA       NA         1        NA        NA           1
[1341,]        1       NA       NA         1        NA        NA           1
[1342,]        1       NA       NA         1        NA        NA           1
[1343,]        1       NA       NA         1        NA        NA           1
[1344,]        2        3       NA         2         3        NA           2
[1345,]        2        3       NA         2         3        NA           1
[1346,]        3        3       NA         3         3        NA           3
[1347,]        2        3       NA         2         3        NA           2
[1348,]        1        1        3         1         1         3           1
[1349,]        1       NA       NA         1        NA        NA           1
[1350,]        1        2       NA         1         2        NA           1
[1351,]        1        2        3         1         2         3           1
[1352,]        3        3       NA         3         3        NA           3
[1353,]        1        2        3         1         2         3           1
[1354,]        1       NA       NA         1        NA        NA           1
[1355,]        1        3        2         1         3         2           1
[1356,]        1       NA       NA         1        NA        NA           1
[1357,]        1       NA       NA         1        NA        NA           1
[1358,]        1       NA       NA         1        NA        NA           1
[1359,]        1       NA       NA         1        NA        NA           1
[1360,]        1        3        3         1         3         3           1
[1361,]        1        3       NA         1         3        NA           1
[1362,]        1       NA       NA         1        NA        NA           1
[1363,]        2        2        3         2         2         3           2
[1364,]        1        3        3         1         3         3           1
[1365,]        1        2       NA         1         2        NA           1
[1366,]        1        3        3         1         3         3           1
[1367,]        1        3        3         1         3         3           1
[1368,]        1       NA       NA         1        NA        NA           1
[1369,]        1       NA       NA         1        NA        NA           1
[1370,]        2        2        3         2         2         3           2
[1371,]        1       NA       NA         1        NA        NA           1
[1372,]        1        1        3         1         1         3           1
[1373,]        1        1        3         1         1         3           1
[1374,]        1       NA       NA         1        NA        NA           1
[1375,]        1        2       NA         1         2        NA           1
[1376,]        1        2       NA         1         2        NA           1
[1377,]        1       NA       NA         1        NA        NA           1
[1378,]        1        1        2         1         1         2           1
[1379,]        1        1       NA         1         1        NA           1
[1380,]        1       NA       NA         1        NA        NA           1
[1381,]        1       NA       NA         1        NA        NA           1
[1382,]        1       NA       NA         1        NA        NA           1
[1383,]        1        1       NA         1         1        NA           1
[1384,]        1        2       NA         1         2        NA           1
[1385,]        1        1        2         1         1         2           1
[1386,]        1        1       NA         1         1        NA           1
[1387,]        1        2        2         1         2         2           1
[1388,]        1        2        2         1         2         2           1
[1389,]        1        2        2         1         2         2           1
[1390,]        1        2       NA         1         2        NA           1
[1391,]        1        2       NA         1         2        NA           1
[1392,]        1        1        2         1         1         2           1
[1393,]        1        1        2         1         1         2           1
[1394,]        1        2        2         1         2         2           1
[1395,]        1        2        2         1         2         2           1
[1396,]        1        2        2         1         2         2           1
[1397,]        1        1        2         1         1         2           1
[1398,]        1        1        2         1         1         2           1
[1399,]        1       NA       NA         1        NA        NA           1
[1400,]        1       NA       NA         1        NA        NA           1
[1401,]        1       NA       NA         1        NA        NA           1
[1402,]        1        1       NA         1         1        NA           1
[1403,]        1       NA       NA         1        NA        NA           1
[1404,]        1       NA       NA         1        NA        NA           1
[1405,]        1       NA       NA         1        NA        NA           1
[1406,]        1       NA       NA         1        NA        NA           1
[1407,]        1        3        3         1         3         3           1
[1408,]        1        1       NA         1         1        NA           1
[1409,]        1        2        3         1         2         3           1
[1410,]        1       NA       NA         1        NA        NA           1
[1411,]        1        2        2         1         2         2           1
[1412,]        1        2        2         1         2         2           1
[1413,]        1       NA       NA         1        NA        NA           1
[1414,]        1        3        3         1         3         3           1
[1415,]        1       NA       NA         1        NA        NA           1
[1416,]        1        2        3         1         2         3           1
[1417,]        1        1       NA         1         1        NA           1
[1418,]        1       NA       NA         1        NA        NA           1
[1419,]        1       NA       NA         1        NA        NA           1
[1420,]        1       NA       NA         1        NA        NA           1
[1421,]        1        2        3         1         2         3           1
[1422,]        1        2        2         1         2         2           1
[1423,]        1       NA       NA         1        NA        NA           1
[1424,]        1        3        3         1         3         3           1
[1425,]        1        2        2         1         2         2           1
[1426,]        1       NA       NA         1        NA        NA           1
[1427,]        2        3       NA         2         3        NA           2
[1428,]        2        3       NA         2         3        NA           2
[1429,]        3        3       NA         3         3        NA           3
[1430,]        1        1        3         1         1         3           1
[1431,]        1       NA       NA         1        NA        NA           1
[1432,]        1        2       NA         1         2        NA           1
[1433,]        1        3        3         1         3         3           1
[1434,]        3        3       NA         3         3        NA           3
[1435,]        1        3        3         1         3         3           1
[1436,]        1       NA       NA         1        NA        NA           1
[1437,]        1        3        3         1         3         3           1
[1438,]        1       NA       NA         1        NA        NA           1
[1439,]        1       NA       NA         1        NA        NA           1
[1440,]        1       NA       NA         1        NA        NA           1
[1441,]        1        3        3         1         3         3           1
[1442,]        1        2        3         1         2         3           1
[1443,]        1        2        3         1         2         3           1
[1444,]        1        2        3         1         2         3           1
[1445,]        1        2        3         1         2         3           1
[1446,]        1        3        3         1         3         3           1
[1447,]        1        2       NA         1         2        NA           1
[1448,]        1        3        3         1         3         3           1
[1449,]        1       NA       NA         1        NA        NA           1
[1450,]        1       NA       NA         1        NA        NA           1
[1451,]        1       NA       NA         1        NA        NA           1
[1452,]        1       NA       NA         1        NA        NA           1
[1453,]        1       NA       NA         1        NA        NA           1
[1454,]        1       NA       NA         1        NA        NA           1
[1455,]        1       NA       NA         1        NA        NA           1
[1456,]        1        2        3         1         2         3           1
[1457,]        1        3        3         1         3         3           1
[1458,]        1        2        3         1         2         3           1
[1459,]        1        2        3         1         2         3           1
[1460,]        3        3       NA         3         3        NA           3
[1461,]        1       NA       NA         1        NA        NA           1
[1462,]        1        2       NA         1         2        NA           1
[1463,]        1        2        3         1         2         3           1
[1464,]        1        2        3         1         2         3           1
[1465,]        1        3        3         1         3         3           1
[1466,]        1        3        3         1         3         3           1
[1467,]        1        3       NA         1         3        NA           1
[1468,]        1       NA       NA         1        NA        NA           1
[1469,]        2        2        3         2         2         3           2
[1470,]        1        3        3         1         3         3           1
[1471,]        2        2       NA         2         2        NA           1
[1472,]        1        3        3         1         3         3           1
[1473,]        1        3        3         1         3         3           1
[1474,]        1       NA       NA         1        NA        NA           1
[1475,]        1       NA       NA         1        NA        NA           1
[1476,]        1       NA       NA         1        NA        NA           1
[1477,]        1        2        3         1         2         3           1
[1478,]        1        2        3         1         2         3           1
[1479,]        1        2       NA         1         2        NA           1
[1480,]        1        2       NA         1         2        NA           1
[1481,]        1       NA       NA         1        NA        NA           1
[1482,]        1       NA       NA         1        NA        NA           1
[1483,]        2        3       NA         2         3        NA           2
[1484,]        2        3       NA         2         3        NA           1
[1485,]        3        3       NA         3         3        NA           3
[1486,]        1        2        3         1         2         3           1
[1487,]        1       NA       NA         1        NA        NA           1
[1488,]        1        2       NA         1         2        NA           1
[1489,]        1        3        3         1         3         3           1
[1490,]        1        3        3         1         3         3           1
[1491,]        1       NA       NA         1        NA        NA           1
[1492,]        1        3        3         1         3         3           1
[1493,]        1       NA       NA         1        NA        NA           1
[1494,]        1       NA       NA         1        NA        NA           1
[1495,]        1       NA       NA         1        NA        NA           1
[1496,]        1        2        3         1         1         3           1
[1497,]        1        3        3         1         3         3           1
[1498,]        1        3        3         1         3         3           1
[1499,]        1       NA       NA         1        NA        NA           1
[1500,]        1       NA       NA         1        NA        NA           1
[1501,]        1        3        3         1         3         3           1
[1502,]        1        3        3         1         3         3           1
[1503,]        3        3       NA         3         3        NA           3
[1504,]        3        3       NA         3         3        NA           3
[1505,]        1       NA       NA         1        NA        NA           1
[1506,]        1       NA       NA         1        NA        NA           1
[1507,]        1        3       NA         1         3        NA           1
[1508,]        1       NA       NA         1        NA        NA           1
[1509,]        1        2       NA         1         2        NA           1
[1510,]        1        3        3         1         3         3           1
[1511,]        1        3        2         1         3         2           1
[1512,]        1        2        3         1         2         3           1
[1513,]        1        2       NA         1         2        NA           1
[1514,]        1        1        2         1         1         2           1
[1515,]        2        2       NA         2         2        NA           1
[1516,]        1       NA       NA         1        NA        NA           1
[1517,]        1        3        2         1         3         2           1
[1518,]        2        2        2         2         2         2           1
[1519,]        3        2       NA         3         2        NA           2
[1520,]        3        2       NA         3         2        NA           3
[1521,]        1        1       NA         1         1        NA           1
[1522,]        1        2       NA         1         2        NA           1
[1523,]        1       NA       NA         1        NA        NA           1
[1524,]        1       NA       NA         1        NA        NA           1
[1525,]        1        2       NA         1         2        NA           1
[1526,]        1        2       NA         1         2        NA           1
[1527,]        1        2        2         1         2         2           1
[1528,]        1       NA       NA         1        NA        NA           1
[1529,]        1        1        2         1         1         2           1
[1530,]        1        1        2         1         1         2           1
[1531,]        1        2        2         1         2         2           1
[1532,]        1       NA       NA         1        NA        NA           1
[1533,]        1        1        2         1         1         2           1
[1534,]        2       NA       NA         2        NA        NA           2
[1535,]        1        1       NA         1         1        NA           1
[1536,]        1       NA       NA         1        NA        NA           1
[1537,]        2        2        2         2         2         3           2
[1538,]        2        1       NA         2         1        NA           1
[1539,]        1       NA       NA         1        NA        NA           1
[1540,]        1       NA       NA         1        NA        NA           1
[1541,]        1        2        2         1         2         2           1
[1542,]        2       NA       NA         2        NA        NA           2
[1543,]        2        2       NA         2         2        NA           2
[1544,]        1        1        2         1         1         2           1
[1545,]        1        2       NA         1         2        NA           1
[1546,]        1       NA       NA         1        NA        NA           1
[1547,]        1        2       NA         1         2        NA           1
[1548,]        1       NA       NA         1        NA        NA           1
[1549,]        2        3        2         2         3         2           2
[1550,]        1       NA       NA         1        NA        NA           1
[1551,]        1       NA       NA         1        NA        NA           1
[1552,]        3        2       NA         3         2        NA           3
[1553,]        1       NA       NA         1        NA        NA           1
[1554,]        1        2        3         1         2         3           1
[1555,]        1       NA       NA         1        NA        NA           1
[1556,]        1        3       NA         1         3        NA           1
[1557,]        2        3       NA         2         3        NA           2
[1558,]        1        3        3         1         3         3           1
[1559,]        1       NA       NA         1        NA        NA           1
[1560,]        2        3       NA         2         3        NA           1
[1561,]        1        3        3         1         3         3           1
[1562,]        1       NA       NA         1        NA        NA           1
[1563,]        1        3       NA         1         3        NA           1
[1564,]        1       NA       NA         1        NA        NA           1
[1565,]        1       NA       NA         1        NA        NA           1
[1566,]        1        2        2         1         2         2           1
[1567,]        2        2       NA         2         2        NA           2
[1568,]        1        2        2         1         2         2           1
[1569,]        2        2       NA         2         2        NA           2
[1570,]        1        2        2         1         2         1           1
[1571,]        1        1        2         1         1         2           1
[1572,]        2        3        2         1         2         1           2
[1573,]        2        3        2         1         2         1           2
[1574,]        1        2        2         1         2         2           1
[1575,]        1        2        2         1         2         2           1
[1576,]        1        2        2         1         2         2           1
[1577,]        3        3        2         2         2         2           3
[1578,]        3        3        2         2         2         2           3
[1579,]        2        3        2         1         2         1           1
[1580,]        2        2       NA         2         2        NA           2
[1581,]        2        2       NA         2         2        NA           2
[1582,]        2        3        2         1         2         1           2
[1583,]        1        3        2         1         2         1           1
[1584,]        2        2       NA         2         2        NA           2
[1585,]        1        2       NA         1         2        NA           1
[1586,]        2        2       NA         2         2        NA           2
[1587,]        1       NA       NA         1        NA        NA           1
[1588,]        2        2       NA         2         2        NA           2
[1589,]        2        3        2         1         2         1           2
[1590,]        1        2       NA         1         1        NA           1
[1591,]        1        2       NA         1         1        NA           1
[1592,]        1        2       NA         1         1        NA           1
[1593,]        1        2       NA         1         1        NA           1
[1594,]        1        2        2         1         1         2           1
[1595,]        1        2        2         1         1         2           1
[1596,]        1        1        2         1         1         2           1
[1597,]        1        2        3         1         1         2           1
[1598,]        1        2        3         1         1         2           1
[1599,]        1        1        2         1         1         2           1
[1600,]        1        1        2         1         1         2           1
[1601,]        1        1        2         1         1         2           1
[1602,]        1        1        2         1         1         2           1
[1603,]        1        2        2         1         2         2           1
[1604,]        1        1        2         1         1         2           1
[1605,]        1        2        2         1         2         1           1
[1606,]        1        2       NA         1         2        NA           1
[1607,]        1        2       NA         1         2        NA           1
[1608,]        1       NA       NA         1        NA        NA           1
[1609,]        1        2       NA         1         2        NA           1
[1610,]        1        2       NA         1         2        NA           1
[1611,]        1       NA       NA         1        NA        NA           1
[1612,]        1       NA       NA         1        NA        NA           1
[1613,]        1       NA       NA         1        NA        NA           1
[1614,]        1       NA       NA         1        NA        NA           1
[1615,]        1       NA       NA         1        NA        NA           1
[1616,]        1       NA       NA         1        NA        NA           1
[1617,]        1        2       NA         1         2        NA           1
[1618,]        1       NA       NA         1        NA        NA           1
[1619,]        1       NA       NA         1        NA        NA           1
[1620,]        1       NA       NA         1        NA        NA           1
[1621,]        1       NA       NA         1        NA        NA           1
[1622,]        1        2       NA         1         2        NA           1
[1623,]        1       NA       NA         1        NA        NA           1
[1624,]        2        2       NA         2         2        NA           2
[1625,]        1        2       NA         1         1        NA           1
[1626,]        1       NA       NA         1        NA        NA           1
[1627,]        1       NA       NA         1        NA        NA           1
[1628,]        2        2       NA         2         2        NA           2
[1629,]        1       NA       NA         1        NA        NA           1
[1630,]        1       NA       NA         1        NA        NA           1
[1631,]        1       NA       NA         1        NA        NA           1
[1632,]        1       NA       NA         1        NA        NA           1
[1633,]        1       NA       NA         1        NA        NA           1
[1634,]        1       NA       NA         1        NA        NA           1
[1635,]        1       NA       NA         1        NA        NA           1
[1636,]        1       NA       NA         1        NA        NA           1
[1637,]        1       NA       NA         1        NA        NA           1
[1638,]        1        2       NA         1         1        NA           1
[1639,]        1       NA       NA         1        NA        NA           1
[1640,]        1        1       NA         1         1        NA           1
[1641,]        1        1       NA         1         1        NA           1
[1642,]        1        2       NA         1         1        NA           1
[1643,]        1        1       NA         1         1        NA           1
[1644,]        1        2       NA         1         1        NA           1
[1645,]        1       NA       NA         1        NA        NA           1
[1646,]        1       NA       NA         1        NA        NA           1
[1647,]        1       NA       NA         1        NA        NA           1
[1648,]        1       NA       NA         1        NA        NA           1
[1649,]        1       NA       NA         1        NA        NA           1
[1650,]        1       NA       NA         1        NA        NA           1
[1651,]        1       NA       NA         1        NA        NA           1
[1652,]        1       NA       NA         1        NA        NA           1
[1653,]        1       NA       NA         1        NA        NA           1
[1654,]        1       NA       NA         1        NA        NA           1
[1655,]        1       NA       NA         1        NA        NA           1
[1656,]        1       NA       NA         1        NA        NA           1
[1657,]        1       NA       NA         1        NA        NA           1
[1658,]        1       NA       NA         1        NA        NA           1
[1659,]        1        2       NA         1         2        NA           1
[1660,]        1        1        2         1         1         2           1
[1661,]        1        1        2         1         1         2           1
[1662,]        1        2        2         1         2         2           1
[1663,]        1        1        2         1         1         2           1
[1664,]        1        1        2         1         1         2           1
[1665,]        1        2        2         1         1         2           1
[1666,]        1        2        2         1         1         2           1
[1667,]        1        1       NA         1         1        NA           1
[1668,]        1        2        2         1         1         2           1
[1669,]        1        1        2         1         1         2           1
[1670,]        1        2        2         1         1         2           1
[1671,]        1        1        2         1         1         2           1
[1672,]        1        1        2         1         1         2           1
[1673,]        1        2        2         1         1         2           1
[1674,]        1        1       NA         1         1        NA           1
[1675,]        1        1       NA         1         1        NA           1
[1676,]        1        2        2         1         1         2           1
[1677,]        1        2        2         1         1         2           1
[1678,]        1        2        2         1         2         2           1
[1679,]        1        2        2         1         2         2           1
[1680,]        1        1        2         1         1         2           1
[1681,]        1        2        2         1         1         2           1
[1682,]        1        2        2         1         2         2           1
[1683,]        1        2        3         1         1         2           1
[1684,]        1        2       NA         1         1        NA           1
[1685,]        1        2        2         1         1         2           1
[1686,]        2        2        2         2         2         2           2
[1687,]        2        2        2         2         2         2           2
[1688,]        2        2        2         2         2         2           2
[1689,]        2        2        2         2         2         2           2
[1690,]        2        2        2         2         2         2           2
[1691,]        2        2        2         2         2         2           2
[1692,]        2        2        2         2         2         2           2
[1693,]        3        3        2         2         2         2           3
[1694,]        1        2        2         1         2         2           1
[1695,]        1        1        2         1         1         2           1
[1696,]        1        2        2         1         1         2           1
[1697,]        1        2        2         1         2         2           1
[1698,]        1        2        2         1         2         2           1
[1699,]        1        1        2         1         1         2           1
[1700,]        2        2        2         2         2         2           2
[1701,]        1        1        2         1         1         2           1
[1702,]        3        3        2         2         2         2           3
[1703,]        3        3        2         2         2         2           3
[1704,]        3        3        2         2         2         2           3
[1705,]        3        3        2         2         2         2           2
[1706,]        3        3        2         2         2         2           2
[1707,]        3        3        2         2         2         2           2
[1708,]        3        3        2         2         2         2           3
[1709,]        1       NA       NA         1        NA        NA           1
[1710,]        1       NA       NA         1        NA        NA           1
[1711,]        1       NA       NA         1        NA        NA           1
[1712,]        1       NA       NA         1        NA        NA           1
[1713,]        1       NA       NA         1        NA        NA           1
[1714,]        1       NA       NA         1        NA        NA           1
[1715,]        1       NA       NA         1        NA        NA           1
[1716,]        1        1       NA         1         1        NA           1
[1717,]        1        1       NA         1         1        NA           1
[1718,]        1        1        2         1         1         2           1
[1719,]        1       NA       NA         1        NA        NA           1
[1720,]        1       NA       NA         1        NA        NA           1
[1721,]        1       NA       NA         1        NA        NA           1
[1722,]        1       NA       NA         1        NA        NA           1
[1723,]        1       NA       NA         1        NA        NA           1
[1724,]        1        1       NA         1         1        NA           1
[1725,]        1        1       NA         1         1        NA           1
[1726,]        1       NA       NA         1        NA        NA           1
[1727,]        1       NA       NA         1        NA        NA           1
[1728,]        1        1       NA         1         1        NA           1
[1729,]        1        1       NA         1         1        NA           1
[1730,]        1       NA       NA         1        NA        NA           1
[1731,]        1       NA       NA         1        NA        NA           1
[1732,]        1       NA       NA         1        NA        NA           1
[1733,]        1       NA       NA         1        NA        NA           1
[1734,]        1       NA       NA         1        NA        NA           1
[1735,]        1       NA       NA         1        NA        NA           1
[1736,]        1       NA       NA         1        NA        NA           1
[1737,]        2       NA       NA         2        NA        NA           2
[1738,]        2       NA       NA         2        NA        NA           2
[1739,]        2       NA       NA         2        NA        NA           2
[1740,]        2       NA       NA         2        NA        NA           2
[1741,]        2       NA       NA         2        NA        NA           2
[1742,]        1       NA       NA         1        NA        NA           1
[1743,]        1       NA       NA         1        NA        NA           1
[1744,]        1       NA       NA         1        NA        NA           1
[1745,]        1       NA       NA         1        NA        NA           1
[1746,]        1       NA       NA         1        NA        NA           1
[1747,]        1       NA       NA         1        NA        NA           1
[1748,]        1       NA       NA         1        NA        NA           1
[1749,]        1        2       NA         1         2        NA           1
[1750,]        1        2       NA         1         2        NA           1
[1751,]        1        2       NA         1         2        NA           1
[1752,]        1        2       NA         1         2        NA           1
[1753,]        1        2       NA         1         1        NA           1
[1754,]        1       NA       NA         1        NA        NA           1
[1755,]        1       NA       NA         1        NA        NA           1
[1756,]        1       NA       NA         1        NA        NA           1
[1757,]        1       NA       NA         1        NA        NA           1
[1758,]        1       NA       NA         1        NA        NA           1
[1759,]        1        2       NA         1         1        NA           1
[1760,]        1        2       NA         1         1        NA           1
[1761,]        1        2       NA         1         1        NA           1
[1762,]        1        1       NA         1         1        NA           1
[1763,]        1       NA       NA         1        NA        NA           1
[1764,]        1        2       NA         1         1        NA           1
[1765,]        1       NA       NA         1        NA        NA           1
[1766,]        1        1        2         1         1         2           1
[1767,]        1       NA       NA         1        NA        NA           1
[1768,]        1       NA       NA         1        NA        NA           1
[1769,]        1       NA       NA         1        NA        NA           1
[1770,]        1       NA       NA         1        NA        NA           1
[1771,]        1       NA       NA         1        NA        NA           1
[1772,]        2        3        2         1         2         1           1
[1773,]        2        3        2         1         2         1           1
[1774,]        2        3        2         1         2         1           2
[1775,]        2        2       NA         2         2        NA           2
[1776,]        1        2       NA         1         2        NA           1
[1777,]        2        3        2         1         2         1           1
[1778,]        2        2       NA         2         2        NA           2
[1779,]        1        2       NA         1         1        NA           1
[1780,]        1        2        2         1         1         2           1
[1781,]        1        2        2         1         1         2           1
[1782,]        1        2       NA         1         1        NA           1
[1783,]        1        1       NA         1         1        NA           1
[1784,]        1        2        2         1         1         2           1
[1785,]        1        1        2         1         1         2           1
[1786,]        1        2        3         1         1         2           1
[1787,]        1        2        3         1         1         2           1
[1788,]        1        1        2         1         1         2           1
[1789,]        1        2        3         1         1         2           1
[1790,]        1        2        2         1         2         2           1
[1791,]        1        1        2         1         1         2           1
[1792,]        1        1        2         1         1         2           1
[1793,]        1       NA       NA         1        NA        NA           1
[1794,]        1        1        2         1         1         2           1
[1795,]        1        1        2         1         1         2           1
[1796,]        2        2        2         2         2         2           2
[1797,]        1        2        2         1         2         2           1
[1798,]        1        1        2         1         1         2           1
[1799,]        3        3        2         2         2         2           3
[1800,]        3        3        2         2         2         2           3
[1801,]        3        3        2         2         2         2           3
[1802,]        3        3        2         2         2         2           3
[1803,]        1       NA       NA         1        NA        NA           1
[1804,]        2       NA       NA         2        NA        NA           2
[1805,]        1        2       NA         1         2        NA           1
[1806,]        1        1        2         1         1         2           1
[1807,]        1        2        2         1         1         2           1
[1808,]        1        2       NA         1         2        NA           1
[1809,]        1       NA       NA         1        NA        NA           1
[1810,]        1        2       NA         1         1        NA           1
[1811,]        1        2       NA         1         2        NA           1
[1812,]        1        2       NA         1         2        NA           1
[1813,]        1        2       NA         1         2        NA           1
[1814,]        1        2       NA         1         2        NA           1
[1815,]        1        2       NA         1         1        NA           1
[1816,]        1        2       NA         1         1        NA           1
[1817,]        1        2        2         1         2         2           1
[1818,]        1        2        2         1         2         2           1
[1819,]        1        2        2         1         2         2           1
[1820,]        1        1        2         1         1         2           1
[1821,]        1        2        2         1         2         2           1
[1822,]        2        2        2         2         2         2           2
[1823,]        2        2        2         2         2         2           2
[1824,]        2        2        2         2         2         2           2
[1825,]        3        3        2         2         2         2           3
[1826,]        3        3        2         2         2         2           3
[1827,]        3        3        2         2         2         2           3
[1828,]        2        2       NA         2         2        NA           2
[1829,]        1        1       NA         1         1        NA           1
[1830,]        2        2       NA         2         2        NA           2
[1831,]        1       NA       NA         1        NA        NA           1
[1832,]        1       NA       NA         1        NA        NA           1
[1833,]        1       NA       NA         1        NA        NA           1
[1834,]        1       NA       NA         1        NA        NA           1
[1835,]        1        2       NA         1         2        NA           1
[1836,]        1       NA       NA         1        NA        NA           1
[1837,]        1       NA       NA         1        NA        NA           1
[1838,]        1       NA       NA         1        NA        NA           1
[1839,]        1        2       NA         1         2        NA           1
[1840,]        1        2       NA         1         2        NA           1
[1841,]        1       NA       NA         1        NA        NA           1
[1842,]        1        1       NA         1         1        NA           1
[1843,]        2        3        2         1         2         1           2
[1844,]        2        3        2         1         2         1           2
[1845,]        2        3        2         1         2         1           2
[1846,]        1        2        2         1         1         2           1
[1847,]        1        1       NA         1         1        NA           1
[1848,]        1       NA       NA         1        NA        NA           1
[1849,]        2        3        2         1         2         1           2
[1850,]        2        2       NA         2         2        NA           2
[1851,]        1       NA       NA         1        NA        NA           1
[1852,]        1       NA       NA         1        NA        NA           1
[1853,]        3        3        2         2         2         2           3
[1854,]        1        2        2         1         1         2           1
[1855,]        1        1       NA         1         1        NA           1
[1856,]        1       NA       NA         1        NA        NA           1
[1857,]        1        2        2         1         2         2           1
[1858,]        2        2        2         2         2         2           2
[1859,]        1        2       NA         1         1        NA           1
[1860,]        1       NA       NA         1        NA        NA           1
[1861,]        1        2        3         1         1         2           1
[1862,]        2        2       NA         2         2        NA           2
[1863,]        1        1        2         1         1         2           1
[1864,]        1        1        2         1         1         2           1
[1865,]        1        1        2         1         1         2           1
[1866,]        1        1        2         1         1         2           1
[1867,]        1        2        2         1         1         2           1
[1868,]        1        2        2         1         1         2           1
[1869,]        1        2       NA         1         2        NA           1
[1870,]        1       NA       NA         1        NA        NA           1
[1871,]        1       NA       NA         1        NA        NA           1
[1872,]        2        2       NA         2         2        NA           2
[1873,]        1       NA       NA         1        NA        NA           1
[1874,]        1        2        2         1         1         2           1
[1875,]        1       NA       NA         1        NA        NA           1
[1876,]        1       NA       NA         1        NA        NA           1
[1877,]        1       NA       NA         1        NA        NA           1
[1878,]        1       NA       NA         1        NA        NA           1
[1879,]        1       NA       NA         1        NA        NA           1
[1880,]        1       NA       NA         1        NA        NA           1
[1881,]        3        3        2         2         2         2           3
[1882,]        1       NA       NA         1        NA        NA           1
[1883,]        2       NA       NA         2        NA        NA           2
[1884,]        1        2       NA         1         2        NA           1
[1885,]        1       NA       NA         1        NA        NA           1
[1886,]        1       NA       NA         1        NA        NA           1
[1887,]        1        2       NA         1         2        NA           1
[1888,]        1        2        2         1         2         2           1
[1889,]        1       NA       NA         1        NA        NA           1
[1890,]        2        1        2         2         1         2           2
[1891,]        1        1        2         1         1         2           1
[1892,]        1       NA       NA         1        NA        NA           1
[1893,]        1        2        2         1         2         2           1
[1894,]        1        2        2         1         2         2           1
[1895,]        1       NA       NA         1        NA        NA           1
[1896,]        2        2        2         2         2         2           2
[1897,]        1        2        2         1         2         2           1
[1898,]        1        2        2         1         2         2           1
[1899,]        1        2        2         1         2         2           1
[1900,]        1        2        2         1         2         2           1
[1901,]        1        2        2         1         2         2           1
[1902,]        1        2        2         1         2         2           1
[1903,]        2        2        2         2         2         2           2
[1904,]        2        2        2         2         2         2           2
[1905,]        1        1        2         1         1         2           1
[1906,]        1       NA       NA         1        NA        NA           1
[1907,]        2       NA       NA         2        NA        NA           2
[1908,]        1        2        2         1         2         2           1
[1909,]        1       NA       NA         1        NA        NA           1
[1910,]        1        2        2         1         2         2           1
[1911,]        1        2       NA         1         2        NA           1
[1912,]        2        2        3         2         2         3           2
[1913,]        1       NA       NA         1        NA        NA           1
[1914,]        1       NA       NA         1        NA        NA           1
[1915,]        1        2        2         1         2         2           1
[1916,]        2       NA       NA         2        NA        NA           2
[1917,]        2        2       NA         2         2        NA           2
[1918,]        1        2        2         1         2         2           1
[1919,]        2        2       NA         2         2        NA           1
[1920,]        2       NA       NA         2        NA        NA           2
[1921,]        1        2       NA         1         2        NA           1
[1922,]        1       NA       NA         1        NA        NA           1
[1923,]        3        3        3         3         3         3           3
[1924,]        1        2        3         1         2         3           1
[1925,]        1        2        2         1         2         3           1
[1926,]        1       NA       NA         1        NA        NA           1
[1927,]        2        3        3         2         3         3           2
[1928,]        1       NA       NA         1        NA        NA           1
[1929,]        1       NA       NA         1        NA        NA           1
[1930,]        3        3       NA         3         2        NA           3
[1931,]        1       NA       NA         1        NA        NA           1
[1932,]        1        3        3         1         3         3           1
[1933,]        1       NA       NA         1        NA        NA           1
[1934,]        1        3       NA         1         3        NA           1
[1935,]        2        3       NA         2         3        NA           2
[1936,]        1        3        3         1         3         3           1
[1937,]        1       NA       NA         1        NA        NA           1
[1938,]        2        3       NA         2         3        NA           1
[1939,]        1        3        3         1         3         3           1
[1940,]        1       NA       NA         1        NA        NA           1
[1941,]        1        3       NA         1         3        NA           1
[1942,]        2       NA       NA         1        NA        NA           2
[1943,]        1       NA       NA         1        NA        NA           1
[1944,]        2       NA       NA         1        NA        NA           2
[1945,]        2       NA       NA         1        NA        NA           2
[1946,]        1       NA       NA         1        NA        NA           1
[1947,]        1       NA       NA         1        NA        NA           1
[1948,]        2        2        3         2         2         3           2
[1949,]        1        2       NA         1         2        NA           1
[1950,]        1        2        2         1         2         2           1
[1951,]        2        3       NA         2         3        NA           2
[1952,]        1        1        3         1         1         3           1
[1953,]        1        2        2         1         2         2           1
[1954,]        1        3       NA         1         2        NA           1
[1955,]        1        3        3         1         3         3           1
[1956,]        1        3        3         1         3         3           1
[1957,]        1        3        3         1         3         3           1
[1958,]        1        3        3         1         3         3           1
[1959,]        1        2       NA         1         2        NA           1
[1960,]        2        2       NA         1         2        NA           2
[1961,]        2        2       NA         1         2        NA           2
[1962,]        2        2       NA         1         2        NA           1
[1963,]        1        2        3         1         2         3           1
[1964,]        1        2        3         1         2         3           1
[1965,]        1        2        3         1         2         3           1
[1966,]        1        2        3         1         2         3           1
[1967,]        1        2        3         1         2         3           1
[1968,]        1        2       NA         1         2        NA           1
[1969,]        1        2       NA         1         2        NA           1
[1970,]        1       NA       NA         1        NA        NA           1
[1971,]        1       NA       NA         1        NA        NA           1
[1972,]        1       NA       NA         1        NA        NA           1
[1973,]        1       NA       NA         1        NA        NA           1
[1974,]        2        2       NA         1         2        NA           1
[1975,]        1       NA       NA         1        NA        NA           1
[1976,]        1        3       NA         1         2        NA           1
[1977,]        1       NA       NA         1        NA        NA           1
[1978,]        1       NA       NA         1        NA        NA           1
[1979,]        1       NA       NA         1        NA        NA           1
[1980,]        1       NA       NA         1        NA        NA           1
[1981,]        1        2       NA         1         2        NA           1
[1982,]        1        2       NA         1         2        NA           1
[1983,]        1       NA       NA         1        NA        NA           1
[1984,]        1       NA       NA         1        NA        NA           1
[1985,]        1       NA       NA         1        NA        NA           1
[1986,]        2       NA       NA         2        NA        NA           1
[1987,]        1       NA       NA         1        NA        NA           1
[1988,]        1        3        3         1         3         3           1
[1989,]        2        2        3         2         2         3           1
[1990,]        1        2        3         1         2         3           1
[1991,]        1        2        3         1         2         3           1
[1992,]        2        2        3         2         2         3           1
[1993,]        1        2        3         1         2         3           1
[1994,]        1        2        3         1         2         3           1
[1995,]        1        2        3         1         2         3           1
[1996,]        1        2       NA         1         2        NA           1
[1997,]        1        2        3         1         2         3           1
[1998,]        1        3        3         1         3         3           1
[1999,]        1        3        3         1         3         3           1
[2000,]        1        2        3         1         2         3           1
[2001,]        2        3        3         2         3         3           2
[2002,]        2        3        3         2         3         3           2
[2003,]        3        3        3         3         3         3           3
[2004,]        3        3        3         3         3         3           3
[2005,]        1        3        3         1         3         3           1
[2006,]        1        2        3         1         2         3           1
[2007,]        1        2        3         1         2         3           1
[2008,]        3        3        3         3         3         3           3
[2009,]        3        3        3         3         3         3           3
[2010,]        3        3        3         3         3         3           3
[2011,]        3        3        3         3         3         3           3
[2012,]        3        3        3         3         3         3           3
[2013,]        3        3        3         3         3         3           3
[2014,]        2       NA       NA         2        NA        NA           2
[2015,]        2       NA       NA         2        NA        NA           2
[2016,]        1        2        3         1         2         3           1
[2017,]        2       NA       NA         2        NA        NA           1
[2018,]        1       NA       NA         1        NA        NA           1
[2019,]        1       NA       NA         1        NA        NA           1
[2020,]        1        2       NA         1         2        NA           1
[2021,]        1       NA       NA         1        NA        NA           1
[2022,]        1       NA       NA         1        NA        NA           1
[2023,]        3       NA       NA         3        NA        NA           2
[2024,]        3       NA       NA         3        NA        NA           3
[2025,]        3       NA       NA         3        NA        NA           3
[2026,]        1       NA       NA         1        NA        NA           1
[2027,]        1       NA       NA         1        NA        NA           1
[2028,]        1       NA       NA         1        NA        NA           1
[2029,]        1        3       NA         1         3        NA           1
[2030,]        1        3       NA         1         3        NA           1
[2031,]        1       NA       NA         1        NA        NA           1
[2032,]        2       NA       NA         2        NA        NA           2
[2033,]        1        2       NA         1         2        NA           1
[2034,]        1       NA       NA         1        NA        NA           1
[2035,]        1       NA       NA         1        NA        NA           1
[2036,]        1       NA       NA         1        NA        NA           1
[2037,]        1       NA       NA         1        NA        NA           1
[2038,]        1       NA       NA         1        NA        NA           1
[2039,]        1        3        3         1         3         3           1
[2040,]        1        3        3         1         3         3           1
[2041,]        2        2       NA         1         2        NA           1
[2042,]        1        2        3         1         2         3           1
[2043,]        2        2       NA         1         2        NA           1
[2044,]        1        2        3         1         2        NA           1
[2045,]        1        2        3         1         2         3           1
[2046,]        2        2        3         2         2         3           2
[2047,]        2        2        3         2         2         3           1
[2048,]        1        2        3         1         2         3           1
[2049,]        1       NA       NA         1        NA        NA           1
[2050,]        1       NA       NA         1        NA        NA           1
[2051,]        1       NA       NA         1        NA        NA           1
[2052,]        1        2        3         1         2         3           1
[2053,]        1        2       NA         1         2        NA           1
[2054,]        1       NA       NA         1        NA        NA           1
[2055,]        1       NA       NA         1        NA        NA           1
[2056,]        1       NA       NA         1        NA        NA           1
[2057,]        1       NA       NA         1        NA        NA           1
[2058,]        1        2        3         1         2         3           1
[2059,]        1        2        3         1         2         3           1
[2060,]        3       NA       NA         2        NA        NA           2
[2061,]        3       NA       NA         3        NA        NA           2
[2062,]        1        3       NA         1         2        NA           1
[2063,]        3        3        3         3         3         3           3
[2064,]        1       NA       NA         1        NA        NA           1
[2065,]        1       NA       NA         1        NA        NA           1
[2066,]        1       NA       NA         1        NA        NA           1
[2067,]        1        2        3         1         2         3           1
[2068,]        1        2        2         1         2         2           1
[2069,]        2        2        2         2         2         2           1
[2070,]        1        2        2         1         2         2           1
[2071,]        1       NA       NA         1        NA        NA           1
[2072,]        1        2        2         1         2         2           1
[2073,]        1        2       NA         1         2        NA           1
[2074,]        3       NA       NA         3        NA        NA           3
[2075,]        1       NA       NA         1        NA        NA           1
[2076,]        1        2        3         1         2         3           1
[2077,]        1        2        3         1         2         3           1
[2078,]        1        3        3         1         3         3           1
[2079,]        3       NA       NA         3        NA        NA           3
[2080,]        1        3       NA         1         3        NA           1
[2081,]        3       NA       NA         3        NA        NA           3
[2082,]        1       NA       NA         1        NA        NA           1
[2083,]        1        2       NA         1         2        NA           1
[2084,]        1        3        3         1         3         3           1
[2085,]        2        2        3         2         2         3           2
[2086,]        1        2       NA         1         2        NA           1
[2087,]        1        2        3         1         2         3           1
[2088,]        1        3       NA         1         3        NA           1
[2089,]        2        3       NA         2         3        NA           2
[2090,]        1        3        3         1         3         3           1
[2091,]        2        3        3         2         3         3           2
[2092,]        1       NA       NA         1        NA        NA           1
[2093,]        1        2       NA         1         2        NA           1
[2094,]        1       NA       NA         1        NA        NA           1
[2095,]        1        2       NA         1         2        NA           1
[2096,]        2       NA       NA         2        NA        NA           1
[2097,]        1        3       NA         1         3        NA           1
[2098,]        1        3       NA         1         3        NA           1
[2099,]        1        3       NA         1         3        NA           1
[2100,]        1       NA       NA         1        NA        NA           1
[2101,]        1        3        3         1         3         3           1
[2102,]        1        3        3         1         3         3           1
[2103,]        2       NA       NA         2        NA        NA           2
[2104,]        2       NA       NA         2        NA        NA           2
[2105,]        2       NA       NA         2        NA        NA           2
[2106,]        2       NA       NA         2        NA        NA           2
[2107,]        1       NA       NA         1        NA        NA           1
[2108,]        2        3        3         2         3         3           2
[2109,]        1       NA       NA         1        NA        NA           1
[2110,]        1        2        3         1         2         3           1
[2111,]        1        3        3         1         3         3           1
[2112,]        1        3       NA         1         3        NA           1
[2113,]        1       NA       NA         1        NA        NA           1
[2114,]        1        2        3         1         2         3           1
[2115,]        1        2        2         1         2         2           1
[2116,]        1        1        2         1         1         2           1
[2117,]        1        1        2         1         1         2           1
[2118,]        1        2       NA         1         2        NA           1
[2119,]        1        1        2         1         1         2           1
[2120,]        1        1        2         1         1         2           1
[2121,]        1        2       NA         1         2        NA           1
[2122,]        1       NA       NA         1        NA        NA           1
[2123,]        1        1       NA         1         1        NA           1
[2124,]        1        2        2         1         2         2           1
[2125,]        1        2        2         1         2         2           1
[2126,]        1        2        2         1         2         2           1
[2127,]        1       NA       NA         1        NA        NA           1
[2128,]        1        2       NA         1         2        NA           1
[2129,]        1       NA       NA         1        NA        NA           1
[2130,]        3       NA       NA         3        NA        NA           3
[2131,]        1       NA       NA         1        NA        NA           1
[2132,]        1        2        2         1         2         2           1
[2133,]        1        2        3         1         2         3           1
[2134,]        1        2        3         1         2         3           1
[2135,]        3       NA       NA         3        NA        NA           3
[2136,]        1        3       NA         1         3        NA           1
[2137,]        1       NA       NA         1        NA        NA           1
[2138,]        1        2       NA         1         2        NA           1
[2139,]        2        2        3         2         2         3           2
[2140,]        1        2        3         1         2         3           1
[2141,]        1        3       NA         1         3        NA           1
[2142,]        1       NA       NA         1        NA        NA           1
[2143,]        1        2       NA         1         2        NA           1
[2144,]        1       NA       NA         1        NA        NA           1
[2145,]        1        2       NA         1         2        NA           1
[2146,]        3       NA       NA         3        NA        NA           3
[2147,]        1        3       NA         1         3        NA           1
[2148,]        1        3       NA         1         3        NA           1
[2149,]        1        3        3         1         3         3           1
[2150,]        1        3        3         1         3         3           1
[2151,]        1        3       NA         1         3        NA           1
[2152,]        1        3       NA         1         3        NA           1
[2153,]        1        3       NA         1         3        NA           1
[2154,]        2       NA       NA         2        NA        NA           1
[2155,]        2        3       NA         2         3        NA           2
[2156,]        2        3       NA         2         3        NA           2
[2157,]        1        3        3         1         3         3           1
[2158,]        1        3        3         1         3         3           1
[2159,]        1        3       NA         1         3        NA           1
[2160,]        1        2       NA         1         2        NA           1
[2161,]        1        2       NA         1         2        NA           1
[2162,]        1       NA       NA         1        NA        NA           1
[2163,]        2        2        3         2         2         3           1
[2164,]        2        2        3         2         2         3           1
[2165,]        3       NA       NA         3        NA        NA           1
[2166,]        3       NA       NA         3        NA        NA           3
[2167,]        3       NA       NA         3        NA        NA           2
[2168,]        1        2        3         1         2         3           1
[2169,]        1       NA       NA         1        NA        NA           1
[2170,]        1        3        3         1         3         3           1
[2171,]        1        3       NA         1         3        NA           1
[2172,]        1       NA       NA         1        NA        NA           1
[2173,]        1        3        3         1         3         3           1
[2174,]        1       NA       NA         1        NA        NA           1
[2175,]        1        3        3         1         3         3           1
[2176,]        1        3        3         1         3         3           1
[2177,]        1        2        3         1         2         3           1
[2178,]        1        2        3         1         2         3           1
[2179,]        1        2        2         1         2         2           1
[2180,]        1        1        2         1         1         2           1
[2181,]        1        2        2         1         2         2           1
[2182,]        1        2       NA         1         2        NA           1
[2183,]        1        1        2         1         1         2           1
[2184,]        1        2        2         1         2         2           1
[2185,]        1        2       NA         1         2        NA           1
[2186,]        3        2       NA         3         2        NA           3
[2187,]        1       NA       NA         1        NA        NA           1
[2188,]        1       NA       NA         1        NA        NA           1
[2189,]        1       NA       NA         1        NA        NA           1
[2190,]        1       NA       NA         1        NA        NA           1
[2191,]        1        3       NA         1         3        NA           1
[2192,]        1        3        3         1         3         3           1
[2193,]        1        3        3         1         3         3           1
[2194,]        1        3       NA         1         3        NA           1
[2195,]        1        3       NA         1         3        NA           1
[2196,]        2       NA       NA         2        NA        NA           1
[2197,]        2       NA       NA         2        NA        NA           2
[2198,]        2        3       NA         2         3        NA           2
[2199,]        1        2        3         1         2         3           1
[2200,]        1        3       NA         1         3        NA           1
[2201,]        1        2       NA         1         2        NA           1
[2202,]        1       NA       NA         1        NA        NA           1
[2203,]        2        2        2         2         2         2           1
[2204,]        2        2        2         2         2         2           1
[2205,]        3       NA       NA         3        NA        NA           2
[2206,]        3       NA       NA         3        NA        NA           3
[2207,]        3       NA       NA         3        NA        NA           3
[2208,]        1        2        3         1         2         3           1
[2209,]        1        3        3         1         3         3           1
[2210,]        3        3        3         3         3         3           3
[2211,]        2        2        3         2         2         3           2
[2212,]        2        2        3         2         2         3           2
[2213,]        1        2        3         1         2         3           1
[2214,]        1        2        3         1         2         3           1
[2215,]        1        3       NA         1         3        NA           1
[2216,]        2        2        3         2         2         3           2
[2217,]        3        3        3         3         3         3           3
[2218,]        1        2        3         1         2         3           1
[2219,]        3        3        3         3         3         3           3
[2220,]        1        2        3         1         2         3           1
[2221,]        1        2        3         1         2         2           1
[2222,]        1        3       NA         1         3        NA           1
[2223,]        1       NA       NA         1        NA        NA           1
[2224,]        1        3        3         1         2         3           1
[2225,]        1        3        3         1         3         3           1
[2226,]        1       NA       NA         1        NA        NA           1
[2227,]        1        2        2         1         2         2           1
[2228,]        1       NA       NA         1        NA        NA           1
[2229,]        1        1       NA         1         2        NA           1
[2230,]        1        2       NA         1         2        NA           1
[2231,]        1        2        2         1         2         2           1
[2232,]        1        2       NA         1         2        NA           1
[2233,]        1       NA       NA         1        NA        NA           1
[2234,]        1       NA       NA         1        NA        NA           1
[2235,]        1       NA       NA         1        NA        NA           1
[2236,]        1        2        2         1         2         2           1
[2237,]        2       NA       NA         2        NA        NA           2
[2238,]        1        2        2         1         2         3           1
[2239,]        2       NA       NA         2        NA        NA           2
[2240,]        1       NA       NA         1        NA        NA           1
[2241,]        1       NA       NA         1        NA        NA           1
[2242,]        1       NA       NA         1        NA        NA           1
[2243,]        1        2        2         1         3         2           1
[2244,]        1        2        2         1         2         3           1
[2245,]        2        2       NA         2         2        NA           2
[2246,]        1        2        2         1         2         2           1
[2247,]        1       NA       NA         1        NA        NA           1
[2248,]        1        2       NA         1         2        NA           1
[2249,]        1        2       NA         1         2        NA           1
[2250,]        1        2       NA         1         2        NA           1
[2251,]        1        2       NA         1         2        NA           1
[2252,]        1        2        2         1         2         3           1
[2253,]        1        2        2         1         2         2           1
[2254,]        1       NA       NA         1        NA        NA           1
[2255,]        1        2       NA         1         2        NA           1
[2256,]        1       NA       NA         1        NA        NA           1
[2257,]        1        2       NA         1         2        NA           1
[2258,]        1        1       NA         1         2        NA           1
[2259,]        1        2        2         2         2         3           1
[2260,]        1        2       NA         1         2        NA           1
[2261,]        1       NA       NA         1        NA        NA           1
[2262,]        1        2        2         1         2         3           1
[2263,]        1       NA       NA         1        NA        NA           1
[2264,]        1       NA       NA         1        NA        NA           1
[2265,]        1        2       NA         1         2        NA           1
[2266,]        1        2       NA         1         2        NA           1
[2267,]        1       NA       NA         1        NA        NA           1
[2268,]        1        2       NA         1         2        NA           1
[2269,]        1        2       NA         1         2        NA           1
[2270,]        1        2       NA         1         2        NA           1
[2271,]        1        2        2         2         2         3           1
[2272,]        1       NA       NA         1        NA        NA           1
[2273,]        1       NA       NA         1        NA        NA           1
[2274,]        2       NA       NA         2        NA        NA           2
[2275,]        1       NA       NA         1        NA        NA           1
[2276,]        1        2       NA         1         2        NA           1
[2277,]        1        2       NA         1         2        NA           1
[2278,]        1        2       NA         1         2        NA           1
[2279,]        1       NA       NA         1        NA        NA           1
[2280,]        1       NA       NA         1        NA        NA           1
[2281,]        1        2        2         1         2         2           1
[2282,]        1        2        2         1         2         2           1
[2283,]        1        2        2         1         2         2           1
[2284,]        1        2       NA         1         2        NA           1
[2285,]        1       NA       NA         1        NA        NA           1
[2286,]        1        2       NA         1         2        NA           1
[2287,]        1       NA       NA         1        NA        NA           1
[2288,]        1       NA       NA         1        NA        NA           1
[2289,]        1        2       NA         1         2        NA           1
[2290,]        1       NA       NA         1        NA        NA           1
[2291,]        1        2        2         1         2         2           1
[2292,]        2       NA       NA         2        NA        NA           1
[2293,]        1        2        2         1         2         2           1
[2294,]        2        2        2         2         2         3           1
[2295,]        2        2        2         2         2         3           2
[2296,]        1       NA       NA         1        NA        NA           1
[2297,]        1        2        2         1         2         2           1
[2298,]        1        2        2         1         2         2           1
[2299,]        1        2        2         1         2         2           1
[2300,]        1        2        2         1         2         2           1
[2301,]        1        2        2         1         2         2           1
[2302,]        1        2       NA         1         2        NA           1
[2303,]        1        2        2         1         2         2           1
[2304,]        1       NA       NA         1        NA        NA           1
[2305,]        1       NA       NA         1        NA        NA           1
[2306,]        1       NA       NA         1        NA        NA           1
[2307,]        1       NA       NA         1        NA        NA           1
[2308,]        2       NA       NA         2        NA        NA           2
[2309,]        1       NA       NA         1        NA        NA           1
[2310,]        1       NA       NA         2        NA        NA           1
[2311,]        1       NA       NA         1        NA        NA           1
[2312,]        1        2        2         1         2         2           1
[2313,]        2        2       NA         2         2        NA           2
[2314,]        1       NA       NA         1        NA        NA           1
[2315,]        1        2       NA         1         2        NA           1
[2316,]        2       NA       NA         2        NA        NA           2
[2317,]        2        2       NA         2         2        NA           2
[2318,]        1        2       NA         1         2        NA           1
[2319,]        1       NA       NA         1        NA        NA           1
[2320,]        1       NA       NA         1        NA        NA           1
[2321,]        1       NA       NA         1        NA        NA           1
[2322,]        1        2       NA         1         2        NA           1
[2323,]        1       NA       NA         1        NA        NA           1
[2324,]        1        2       NA         1         2        NA           1
[2325,]        1        2        2         1         2         2           1
[2326,]        1       NA       NA         1        NA        NA           1
[2327,]        1       NA       NA         1        NA        NA           1
[2328,]        1        2       NA         1         2        NA           1
[2329,]        1        2       NA         1         2        NA           1
[2330,]        2        2       NA         2         2        NA           1
[2331,]        1        2        2         1         2         2           1
[2332,]        1        2        2         1         2         2           1
[2333,]        1        2       NA         1         3        NA           1
[2334,]        2        2       NA         2         2        NA           1
[2335,]        1       NA       NA         1        NA        NA           1
[2336,]        1       NA       NA         1        NA        NA           1
[2337,]        1        2        2         1         2         2           1
[2338,]        1        2       NA         1         2        NA           1
[2339,]        2        1        2         2         1         2           2
[2340,]        1       NA       NA         1        NA        NA           1
[2341,]        1        2       NA         1         2        NA           1
[2342,]        1       NA       NA         1        NA        NA           1
[2343,]        1        2        2         1         2         3           1
[2344,]        1        2        2         1         2         3           1
[2345,]        2        2       NA         2         2        NA           2
[2346,]        1        2        2         1         2         3           1
[2347,]        2        2       NA         2         2        NA           2
[2348,]        1       NA       NA         1        NA        NA           1
[2349,]        1        2        2         1         2         3           1
[2350,]        1       NA       NA         1        NA        NA           1
[2351,]        1        2       NA         1         2        NA           1
[2352,]        1        2       NA         1         2        NA           1
[2353,]        3        2       NA         3         2        NA           2
[2354,]        1       NA       NA         1        NA        NA           1
[2355,]        1        2        2         1         2         2           1
[2356,]        1        1        2         1         1         2           1
[2357,]        3        2       NA         3         2        NA           3
[2358,]        1       NA       NA         1        NA        NA           1
[2359,]        1        2        2         1         2         2           1
[2360,]        1       NA       NA         1        NA        NA           1
[2361,]        1       NA       NA         1        NA        NA           1
[2362,]        1        2        2         1         2         2           1
[2363,]        1        2        2         1         2         2           1
[2364,]        1        2        2         1         2         2           1
[2365,]        1        2        2         1         2         3           1
[2366,]        2        2       NA         2         2        NA           2
[2367,]        1        2        3         1         2         3           1
[2368,]        1        2       NA         1         2        NA           1
[2369,]        3        2       NA         3         2        NA           2
[2370,]        1        2        2         1         2         3           1
[2371,]        1       NA       NA         1        NA        NA           1
[2372,]        1        2        2         1         2         3           1
[2373,]        1       NA       NA         1        NA        NA           1
[2374,]        1        2        2         1         2         2           1
[2375,]        1       NA       NA         1        NA        NA           1
[2376,]        1       NA       NA         1        NA        NA           1
[2377,]        1        2        2         1         2         2           1
[2378,]        1       NA       NA         1        NA        NA           1
[2379,]        1       NA       NA         1        NA        NA           1
[2380,]        2        2       NA         2         2        NA           2
[2381,]        1       NA       NA         1        NA        NA           1
[2382,]        1       NA       NA         1        NA        NA           1
[2383,]        1        2       NA         1         2        NA           1
[2384,]        1        2       NA         1         2        NA           1
[2385,]        1       NA       NA         1        NA        NA           1
[2386,]        1       NA       NA         1        NA        NA           1
[2387,]        1       NA       NA         1        NA        NA           1
[2388,]        1       NA       NA         1        NA        NA           1
[2389,]        1        3        3         1         3         3           1
[2390,]        1       NA       NA         1        NA        NA           1
[2391,]        1        2       NA         1         2        NA           1
[2392,]        1        2       NA         1         2        NA           1
[2393,]        2        2       NA         2         2        NA           1
[2394,]        1       NA       NA         1        NA        NA           1
[2395,]        1       NA       NA         1        NA        NA           1
[2396,]        1       NA       NA         1        NA        NA           1
[2397,]        1        2        3         1         2         3           1
[2398,]        1       NA       NA         2        NA        NA           1
[2399,]        1        2        3         1         2         3           1
[2400,]        2       NA       NA         2        NA        NA           2
[2401,]        1       NA       NA         1        NA        NA           1
[2402,]        1       NA       NA         1        NA        NA           1
[2403,]        1       NA       NA         1        NA        NA           1
[2404,]        1        3        3         1         3         3           1
[2405,]        1        2        3         1         2         3           1
[2406,]        2        3       NA         2         3        NA           1
[2407,]        1        3        3         1         3         3           1
[2408,]        1       NA       NA         1        NA        NA           1
[2409,]        2        2       NA         2         2        NA           2
[2410,]        1        3       NA         1         3        NA           1
[2411,]        2        2       NA         2         2        NA           2
[2412,]        2        2       NA         2         2        NA           1
[2413,]        1        2        3         1         2         3           1
[2414,]        1        2        3         1         2         3           1
[2415,]        1       NA       NA         1        NA        NA           1
[2416,]        1        2       NA         1         2        NA           1
[2417,]        1       NA       NA         1        NA        NA           1
[2418,]        2        2       NA         2         2        NA           1
[2419,]        1        2       NA         1         2        NA           1
[2420,]        1       NA       NA         1        NA        NA           1
[2421,]        1        2        3         1         2         3           1
[2422,]        1       NA       NA         1        NA        NA           1
[2423,]        2        2       NA         2         2        NA           1
[2424,]        1        3       NA         1         3        NA           1
[2425,]        1       NA       NA         1        NA        NA           1
[2426,]        1        3       NA         2         3        NA           1
[2427,]        1        2       NA         1         2        NA           1
[2428,]        1        2       NA         1         2        NA           1
[2429,]        1        3        3         1         3         3           1
[2430,]        1        2       NA         1         2        NA           1
[2431,]        1        2       NA         1         2        NA           1
[2432,]        2        2       NA         2         2        NA           2
[2433,]        1        2        3         1         3         3           1
[2434,]        2        3       NA         2         3        NA           2
[2435,]        1        3        3         1         3         3           1
[2436,]        1       NA       NA         1        NA        NA           1
[2437,]        2        2       NA         2         2        NA           2
[2438,]        2        2        3         2         2         3           1
[2439,]        1       NA       NA         1        NA        NA           1
[2440,]        1       NA       NA         1        NA        NA           1
[2441,]        2       NA       NA         2        NA        NA           2
[2442,]        1       NA       NA         1        NA        NA           1
[2443,]        1        2       NA         1         2        NA           1
[2444,]        1        3       NA         1         3        NA           1
[2445,]        1        3       NA         1         2        NA           1
[2446,]        1       NA       NA         1        NA        NA           1
[2447,]        1       NA       NA         1        NA        NA           1
[2448,]        1        2        3         1         2         2           1
[2449,]        1        2        3         1         2         3           1
[2450,]        1        2        3         1         2         2           1
[2451,]        1        3       NA         1         3        NA           1
[2452,]        1       NA       NA         1        NA        NA           1
[2453,]        1       NA       NA         1        NA        NA           1
[2454,]        1        3       NA         1         3        NA           1
[2455,]        1       NA       NA         1        NA        NA           1
[2456,]        1        3        3         1         3         3           1
[2457,]        1        3        3         1         3         3           1
[2458,]        2        3        3         2         2         3           2
[2459,]        2        3        3         2         2         3           2
[2460,]        2        3        3         2         2         3           2
[2461,]        1        3        3         1         3         3           1
[2462,]        3        3        3         3         3         3           3
[2463,]        1        3       NA         1         3        NA           1
[2464,]        1        3       NA         1         3        NA           1
[2465,]        2        3        3         2         3         3           2
[2466,]        1        2        3         1         2         3           1
[2467,]        1        2        3         1         2         3           1
[2468,]        2        3        3         2         2         3           2
[2469,]        1       NA       NA         1        NA        NA           1
[2470,]        1       NA       NA         1        NA        NA           1
[2471,]        1       NA       NA         1        NA        NA           1
[2472,]        1        2       NA         1         2        NA           1
[2473,]        1        2       NA         1         2        NA           1
[2474,]        1       NA       NA         1        NA        NA           1
[2475,]        1       NA       NA         1        NA        NA           1
[2476,]        1        3       NA         1         3        NA           1
[2477,]        3        3        3         3         3         3           3
[2478,]        1        2       NA         1         2        NA           1
[2479,]        1        2       NA         1         2        NA           1
[2480,]        1        2       NA         1         2        NA           1
[2481,]        1       NA       NA         1        NA        NA           1
[2482,]        1        2       NA         1         2        NA           1
[2483,]        1       NA       NA         1        NA        NA           1
[2484,]        1        2        3         1         2         3           1
[2485,]        3        3        3         3         3         3           3
[2486,]        3        3        3         3         3         3           3
[2487,]        1        3       NA         1         3        NA           1
[2488,]        2        3        3         2         3         3           2
[2489,]        2        3        3         2         3         3           2
[2490,]        1        3        3         1         3         3           1
[2491,]        1        2       NA         1         2        NA           1
[2492,]        1       NA       NA         1        NA        NA           1
[2493,]        1       NA       NA         1        NA        NA           1
[2494,]        1        2        3         1         2         3           1
[2495,]        1        2       NA         1         2        NA           1
[2496,]        2        2       NA         2         2        NA           2
[2497,]        1        2       NA         1         2        NA           1
[2498,]        1        2       NA         1         2        NA           1
[2499,]        1        2       NA         1         2        NA           1
[2500,]        1        3        3         1         3         3           1
[2501,]        1        2        3         1         2         3           1
[2502,]        2        2        3         3         2         3           2
[2503,]        2       NA       NA         2        NA        NA           1
[2504,]        1        2        3         1         2         3           1
[2505,]        2        2        3         2         2         3           1
[2506,]        2        2        3         2         2         3           2
[2507,]        1       NA       NA         1        NA        NA           1
[2508,]        1       NA       NA         1        NA        NA           1
[2509,]        1       NA       NA         1        NA        NA           1
[2510,]        1       NA       NA         1        NA        NA           1
[2511,]        1       NA       NA         1        NA        NA           1
[2512,]        1       NA       NA         1        NA        NA           1
[2513,]        1       NA       NA         1        NA        NA           1
[2514,]        1        2       NA         1         2        NA           1
[2515,]        1        2       NA         1         2        NA           1
[2516,]        1       NA       NA         1        NA        NA           1
[2517,]        1       NA       NA         1        NA        NA           1
[2518,]        1        2       NA         1         2        NA           1
[2519,]        1        2       NA         1         2        NA           1
[2520,]        2        3        3         2         2         3           2
[2521,]        2        2       NA         2         2        NA           2
[2522,]        2        2       NA         2         2        NA           2
[2523,]        2        2       NA         2         2        NA           2
[2524,]        1        2       NA         1         2        NA           1
[2525,]        2        2       NA         2         2        NA           2
[2526,]        1       NA       NA         1        NA        NA           1
[2527,]        1       NA       NA         1        NA        NA           1
[2528,]        2        3       NA         1         3        NA           2
[2529,]        1        2        3         1         2         3           1
[2530,]        1       NA       NA         1        NA        NA           1
[2531,]        1       NA       NA         1        NA        NA           1
[2532,]        1       NA       NA         1        NA        NA           1
[2533,]        1       NA       NA         1        NA        NA           1
[2534,]        1        3        3         1         3         3           1
[2535,]        3        2        3         3         2         3           3
[2536,]        1        2       NA         1         2        NA           1
[2537,]        2       NA       NA         2        NA        NA           2
[2538,]        2       NA       NA         2        NA        NA           2
[2539,]        2       NA       NA         2        NA        NA           2
[2540,]        2       NA       NA         2        NA        NA           2
[2541,]        2       NA       NA         2        NA        NA           2
[2542,]        1        2       NA         1         2        NA           1
[2543,]        1       NA       NA         1        NA        NA           1
[2544,]        1       NA       NA         1        NA        NA           1
[2545,]        1       NA       NA         1        NA        NA           1
[2546,]        1       NA       NA         1        NA        NA           1
[2547,]        1       NA       NA         1        NA        NA           1
[2548,]        1        2        3         1         2         3           1
[2549,]        1        3        3         1         3         3           1
[2550,]        1        2        3         1         3         3           1
[2551,]        1        2        3         1         2         3           1
[2552,]        1        2       NA         1         3        NA           1
[2553,]        1        2        3         1         2         3           1
[2554,]        1       NA       NA         1        NA        NA           1
[2555,]        1       NA       NA         1        NA        NA           1
[2556,]        1       NA       NA         1        NA        NA           1
[2557,]        1       NA       NA         1        NA        NA           1
[2558,]        2       NA       NA         2        NA        NA           2
[2559,]        1       NA       NA         1        NA        NA           1
[2560,]        1       NA       NA         1        NA        NA           1
[2561,]        1        2        2         1         2         3           1
[2562,]        2        3       NA         2         3        NA           2
[2563,]        1       NA       NA         1        NA        NA           1
[2564,]        1        2       NA         1         3        NA           1
[2565,]        2       NA       NA         2        NA        NA           2
[2566,]        2        2       NA         2         2        NA           2
[2567,]        1        2       NA         1         2        NA           1
[2568,]        1       NA       NA         1        NA        NA           1
[2569,]        1       NA       NA         1        NA        NA           1
[2570,]        1       NA       NA         1        NA        NA           1
[2571,]        1        2       NA         1         2        NA           1
[2572,]        1       NA       NA         1        NA        NA           1
[2573,]     C-c C-c     1        3       NA         1         3        NA           1
[2574,]        1        3        2         1         3         3           1
[2575,]        1       NA       NA         1        NA        NA           1
[2576,]        1       NA       NA         1        NA        NA           1
[2577,]        1        3       NA         1         3        NA           1
[2578,]        1        2       NA         1         2        NA           1
[2579,]        2        3       NA         2         3        NA           2
[2580,]        1        2        3         1         3         3           1
[2581,]        1        2        3         1         2         3           1
[2582,]        1        3       NA         2         3        NA           1
[2583,]        1       NA       NA         1        NA        NA           1
[2584,]        1       NA       NA         1        NA        NA           1
[2585,]        1       NA       NA         1        NA        NA           1
[2586,]        1       NA       NA         1        NA        NA           1
[2587,]        1       NA       NA         1        NA        NA           1
[2588,]        1       NA       NA         1        NA        NA           1
[2589,]        1        3       NA         1         3        NA           1
[2590,]        1        2        3         1         2         3           1
[2591,]        2        3       NA         2         3        NA           1
[2592,]        1       NA       NA         1        NA        NA           1
[2593,]        1       NA       NA         1        NA        NA           1
[2594,]        1        2        3         1         2         3           1
[2595,]        1        3       NA         1         3        NA           1
[2596,]        1       NA       NA         1        NA        NA           1
[2597,]        1        3       NA         2         3        NA           1
[2598,]        1       NA       NA         1        NA        NA           1
[2599,]        1        2        3         1         3         3           1
[2600,]        1        2        3         1         2         3           1
[2601,]        2        3       NA         3         3        NA           2
[2602,]        1        2        3         1         3         3           1
[2603,]        2        3       NA         2         3        NA           2
[2604,]        1        2        3         1         2         3           1
[2605,]        1       NA       NA         1        NA        NA           1
[2606,]        1        2       NA         1         2        NA           1
[2607,]        3        3       NA         3         3        NA           2
[2608,]        2       NA       NA         2        NA        NA           1
[2609,]        1        2        2         1         2         2           1
[2610,]        1        1        2         1         1         3           1
[2611,]        3        3       NA         3         3        NA           3
[2612,]        1       NA       NA         1        NA        NA           1
[2613,]        1        3        3         1         3         3           1
[2614,]        2       NA       NA         2        NA        NA           1
[2615,]        1       NA       NA         1        NA        NA           1
[2616,]        1        2        2         1         2         3           1
[2617,]        1        2        3         1         2         3           1
[2618,]        1        2        3         1         2         3           1
[2619,]        2        2       NA         2         2        NA           2
[2620,]        1        3        3         1         3         3           1
[2621,]        1        2       NA         1         2        NA           1
[2622,]        3        3       NA         3         3        NA           3
[2623,]        1        2        3         1         2         3           1
[2624,]        1       NA       NA         1        NA        NA           1
[2625,]        1        2        3         1         2         3           1
[2626,]        2       NA       NA         2        NA        NA           1
[2627,]        1        3        3         1         3         3           1
[2628,]        1       NA       NA         1        NA        NA           1
[2629,]        2       NA       NA         2        NA        NA           1
[2630,]        1       NA       NA         1        NA        NA           1
[2631,]        2       NA       NA         2        NA        NA           2
[2632,]        3        2       NA         3         3        NA           3
[2633,]        2       NA       NA         2        NA        NA           2
[2634,]        2       NA       NA         2        NA        NA           1
[2635,]        1        3       NA         1         3        NA           1
[2636,]        1       NA       NA         1        NA        NA           1
[2637,]        1       NA       NA         1        NA        NA           1
[2638,]        2       NA       NA         2        NA        NA           1
[2639,]        3        2       NA         3         3        NA           3
[2640,]        1        2       NA         1         2        NA           1
[2641,]        1        2        3         1         2         3           1
[2642,]        2       NA       NA         2        NA        NA           2
[2643,]        1       NA       NA         1        NA        NA           1
[2644,]        2       NA       NA         2        NA        NA           2
[2645,]        2       NA       NA         2        NA        NA           2
[2646,]        2       NA       NA         2        NA        NA           1
[2647,]        3        2       NA         3         2        NA           3
[2648,]        2        2        3         2         2         3           1
[2649,]        1       NA       NA         1        NA        NA           1
[2650,]        1       NA       NA         1        NA        NA           1
[2651,]        1       NA       NA         1        NA        NA           1
[2652,]        1        3       NA         1         3        NA           1
[2653,]        1        3       NA         1         3        NA           1
[2654,]        1       NA       NA         1        NA        NA           1
[2655,]        2       NA       NA         2        NA        NA           2
[2656,]        1        3        3         1         3         3           1
[2657,]        2        3        3         1         3         3           1
[2658,]        1        3        3         1         3         3           1
[2659,]        1        3       NA         1         3        NA           1
[2660,]        1       NA       NA         1        NA        NA           1
[2661,]        1       NA       NA         1        NA        NA           1
[2662,]        1        3       NA         1         3        NA           1
[2663,]        1       NA       NA         1        NA        NA           1
[2664,]        1        3        3         1         3         3           1
[2665,]        1        3        3         1         3         3           1
[2666,]        1        3        3         1         3         3           1
[2667,]        2        3        3         2         3         3           2
[2668,]        1        3        3         1        NA         3           1
[2669,]        1        3       NA         1         3        NA           1
[2670,]        1        3       NA         1         3        NA           1
[2671,]        3        3        3         3         3         3           3
[2672,]        3        3        3         3         3         3           3
[2673,]        1        3        3         1         3         3           1
[2674,]        2        3        3         2         3         3           2
[2675,]        2       NA       NA         2        NA        NA           2
[2676,]        1       NA       NA         1        NA        NA           1
[2677,]        1        3       NA         1         3        NA           1
[2678,]        2       NA       NA         2        NA        NA           2
[2679,]        3        3        3         3         3         3           3
[2680,]        1        3       NA         1         3        NA           1
[2681,]        1        3       NA         1         3        NA           1
[2682,]        1       NA       NA         1        NA        NA           1
[2683,]        1        3       NA         1         3        NA           1
[2684,]        1       NA       NA         1        NA        NA           1
[2685,]        1        3        3         1         3         3           1
[2686,]        3        3        3         3         3         3           3
[2687,]        2        3       NA         2         3        NA           2
[2688,]        3        3        3         3         3         3           3
[2689,]        1        3       NA         1         2        NA           1
[2690,]        1       NA       NA         1        NA        NA           1
[2691,]        1        2       NA         1         2        NA           1
[2692,]        3        2       NA         3         2        NA           3
[2693,]        1        2       NA         1         2        NA           1
[2694,]        1        3        3         1         3         3           1
[2695,]        3        3        3         3         3         3           3
[2696,]        2       NA       NA         2        NA        NA           1
[2697,]        1        2        3         1         2         3           1
[2698,]        2        3        3         2         3         3           2
[2699,]        2        3        3         2         3         3           1
[2700,]        1       NA       NA         1        NA        NA           1
[2701,]        1       NA       NA         1        NA        NA           1
[2702,]        1       NA       NA         1        NA        NA           1
[2703,]        2       NA       NA         2        NA        NA           2
[2704,]        2       NA       NA         2        NA        NA           2
[2705,]        1       NA       NA         1        NA        NA           1
[2706,]        1       NA       NA         1        NA        NA           1
[2707,]        1        3       NA         1         2        NA           1
[2708,]        1        3       NA         1         2        NA           1
[2709,]        1       NA       NA         1        NA        NA           1
[2710,]        1        3       NA         1         3        NA           1
[2711,]        1        3       NA         1         3        NA           1
[2712,]        2        3        3         2         3         3           2
[2713,]        3        2       NA         3         2        NA           3
[2714,]        2        2       NA         2         2        NA           2
[2715,]        3        2       NA         3         2        NA           3
[2716,]        1        3       NA         1         2        NA           1
[2717,]        3        2       NA         3         2        NA           3
[2718,]        1       NA       NA         1        NA        NA           1
[2719,]        2       NA       NA         1        NA        NA           2
[2720,]        1        3        3         1         3         3           1
[2721,]        1       NA       NA         1        NA        NA           1
[2722,]        1       NA       NA         1        NA        NA           1
[2723,]        1        3        3         1         3         3           1
[2724,]        3        3        3         3         3         3           3
[2725,]        1        3       NA         1         3        NA           1
[2726,]        2       NA       NA         2        NA        NA           2
[2727,]        2       NA       NA         2        NA        NA           2
[2728,]        2       NA       NA         2        NA        NA           2
[2729,]        2       NA       NA         2        NA        NA           2
[2730,]        1        3       NA         1         3        NA           1
[2731,]        1       NA       NA         1        NA        NA           1
[2732,]        1       NA       NA         1        NA        NA           1
[2733,]        2       NA       NA         2        NA        NA           2
[2734,]        2        2        3         2         2         3           2
[2735,]        2        2        3         2         2         3           2
[2736,]        1        3       NA         1         3        NA           1
[2737,]        2        2        3         2         2         3           2
[2738,]        2        3        3         2         3         3           2
[2739,]        1        2        3         1         2         3           1
[2740,]        1        2       NA         1         2        NA           1
[2741,]        1        2       NA         1         2        NA           1
[2742,]        1        2       NA         1         2        NA           1
[2743,]        1        3       NA         1         3        NA           1
[2744,]        2        2        3         2         2         3           2
[2745,]        1       NA       NA         1        NA        NA           1
[2746,]        1       NA       NA         1        NA        NA           1
[2747,]        1        3       NA         1         3        NA           1
[2748,]        2        3        2         2         3         3           2
[2749,]        1        2       NA         1         2        NA           1
[2750,]        1       NA       NA         1        NA        NA           1
[2751,]        1        2       NA         1         2        NA           1
[2752,]        1       NA       NA         1        NA        NA           1
[2753,]        1       NA       NA         1        NA        NA           1
[2754,]        1        2       NA         1         2        NA           1
[2755,]        1       NA       NA         1        NA        NA           1
[2756,]        1        3       NA         1         3        NA           1
[2757,]        1       NA       NA         1        NA        NA           1
[2758,]        1       NA       NA         1        NA        NA           1
[2759,]        1        3        3         1         3         3           1
[2760,]        1       NA       NA         1        NA        NA           1
[2761,]        1       NA       NA         1        NA        NA           1
[2762,]        1       NA       NA         1        NA        NA           1
[2763,]        3        3        3         3         3         3           2
[2764,]        1        2       NA         1         2        NA           1
[2765,]        2       NA       NA         2        NA        NA           1
[2766,]        2       NA       NA         2        NA        NA           2
[2767,]        1        3        3         1         3         3           1
[2768,]        1        3       NA         1         3        NA           1
[2769,]        1        2        3         1         2         3           1
[2770,]        3        3        3         3         3         3           3
[2771,]        1        3        3         1         3         3           1
[2772,]        1       NA       NA         1        NA        NA           1
[2773,]        1        2       NA         1         2        NA           1
[2774,]        3        3        3         3         3         3           1
[2775,]        3        3        3         3         3         3           2
[2776,]        1        2        3         1         2         3           1
[2777,]        1       NA       NA         1        NA        NA           1
[2778,]        2        3        2         2         3         3           2
[2779,]        1        2       NA         1         2        NA           1
[2780,]        1        2        2         1         2         3           1
[2781,]        1       NA       NA         1        NA        NA           1
[2782,]        1        3        3         1         3         3           1
[2783,]        1        3        3         1         3         3           1
[2784,]        1        3        3         1         3         3           1
[2785,]        1        2        2         1         2         2           1
[2786,]        1        2        3         1         2         3           1
[2787,]        2        2        2         2         2         2           2
[2788,]        1        3        3         1         3         3           1
[2789,]        1        3        3         1         3         3           1
[2790,]        1        2       NA         1         2        NA           1
[2791,]        1       NA       NA         1        NA        NA           1
[2792,]        2       NA       NA         2        NA        NA           2
[2793,]        1        2        2         1         2         2           1
[2794,]        3        2        2         3         2         2           3
[2795,]        3        2        2         3         2         2           3
[2796,]        2       NA       NA         2        NA        NA           2
[2797,]        2        2        3         2         2         3           2
[2798,]        1        2       NA         1         2        NA           1
[2799,]        1       NA       NA         1        NA        NA           1
[2800,]        1        2       NA         1         2        NA           1
[2801,]        1        2       NA         1         2        NA           1
[2802,]        1       NA       NA         1        NA        NA           1
[2803,]        2        3        3         2         3         3           2
[2804,]        2       NA       NA         2        NA        NA           2
[2805,]        2       NA       NA         2        NA        NA           2
[2806,]        3        3        3         3         3         3           3
[2807,]        1        3        3         1         3         3           1
[2808,]        1        3        3         1         3         3           1
[2809,]        2        3        3         2         3         3           2
[2810,]        1        3        3         1         3         3           1
[2811,]        1        3        3         1         3         3           1
[2812,]        1        3       NA         1         2        NA           1
[2813,]        1        3       NA         1         2        NA           1
[2814,]        1        3       NA         1         2        NA           1
[2815,]        1       NA       NA         1        NA        NA           1
[2816,]        1        3       NA         1         2        NA           1
[2817,]        1       NA       NA         1        NA        NA           1
[2818,]        1       NA       NA         1        NA        NA           1
[2819,]        2        3       NA         2         3        NA           2
[2820,]        1       NA       NA         1        NA        NA           1
[2821,]        3       NA       NA         3        NA        NA           3
[2822,]        3       NA       NA         3        NA        NA           3
[2823,]        1        3        3         1         3         3           1
[2824,]        1        3       NA         1         3        NA           1
[2825,]        1        3       NA         1         3        NA           1
[2826,]        1        3       NA         1         3        NA           1
[2827,]        2        3       NA         1         3        NA           1
[2828,]        2        3        3         2         3         3           2
[2829,]        1       NA       NA         1        NA        NA           1
[2830,]        1       NA       NA         1        NA        NA           1
[2831,]        2        3       NA         2         3        NA           1
[2832,]        1        3       NA         1         3        NA           1
[2833,]        2       NA       NA         2        NA        NA           2
[2834,]        1        2       NA         1         2        NA           1
[2835,]        1       NA       NA         1        NA        NA           1
[2836,]        1       NA       NA         1        NA        NA           1
[2837,]        2        3       NA         2         3        NA           2
[2838,]        2       NA       NA         1        NA        NA
> 
> tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>=3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(assign(x, sum(getAIC(models_list)$aic2), envir=parent.frame()),
+       assign(x, sum(getMSE(models_list)$mse/72)))
+ })
Error in poly(l$dbh, 2) (from #6) : 
  'degree' must be less than number of unique points
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(assign(x, sum(getAIC(models_list)$aic2), envir=parent.frame()),
+       assign(x, sum(getMSE(models_list)$mse/72)))
+ })
Error in UseMethod("extractAIC") (from #4) : 
  no applicable method for 'extractAIC' applied to an object of class "NULL"
> length(models_list)
Error: object 'models_list' not found
> ?is.empty
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(assign(x, sum(getAIC(models_list[!is.na(models_list)])$aic2), envir=parent.frame()),
+       assign(x, sum(getMSE(models_list[!is.na(models_list)])$mse)))
+ })
Error in UseMethod("extractAIC") (from #4) : 
  no applicable method for 'extractAIC' applied to an object of class "NULL"
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(assign(x, sum(getAIC(models_list[models_list!=NULL])$aic2), envir=parent.frame()),
+       assign(x, sum(getMSE(models_list[models_list!=NULL])$mse)))
+ })
> tt
     elevcl aspcl soilcl slopcl nsumba.1cl nsumba.2cl nsumba.3cl nsumba5.1cl
[1,]      0     0      0      0          0          0          0           0
[2,]      0     0      0      0          0          0          0           0
     nsumba5.2cl nsumba5.3cl nsumbabig.1cl nsumbabig.2cl nsumbabig.3cl
[1,]           0           0             0             0             0
[2,]           0           0             0             0             0
     dbhgrowthcl bagrowthcl
[1,]           0          0
[2,]           0          0
> length(models_list)
Error: object 'models_list' not found
> list_models_nsumbaclq <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+     d$nsumbaclq <- as.factor(as.character(d$nsumbaclq))
+ #    print(paste("levels",length(levels(d$nsumbaclq))))
+     dlply(d, .(d$nsumbaclq), .fun = function(x) {
+         if(nrow(x) >= 3) {
+             lm(x$ht ~ poly(x$dbh, 2))
+         }
+     })
+ }), recursive = FALSE)
Error in `$<-.data.frame`(`*tmp*`, "nsumbaclq", value = integer(0)) (from #2) : 
  replacement has 0 rows, data has 138
> names(dat)
 [1] "X"              "X.1"            "pplot"          "splot"         
 [5] "tag"            "spec"           "yrmort"         "elev"          
 [9] "elevcl"         "aspcl"          "asp"            "soilcl"        
[13] "slopcl"         "bqudx"          "bqudy"          "meas87"        
[17] "p98dbh"         "p98ht"          "p10dbh"         "p10ht"         
[21] "time"           "nsumba.1"       "nsumba.2"       "nsumba.3"      
[25] "nsumba5.1"      "nsumba5.2"      "nsumba5.3"      "nsumbabig.1"   
[29] "nsumbabig.2"    "nsumbabig.3"    "dbh"            "ht"            
[33] "dbhgrowth"      "bagrowth"       "htgrowth"       "stat"          
[37] "cpos"           "ch"             "ba"             "id"            
[41] "mort"           "nsumba.1cl"     "nsumba.2cl"     "nsumba.3cl"    
[45] "nsumba5.1cl"    "nsumba5.2cl"    "nsumba5.3cl"    "nsumbabig.1cl" 
[49] "nsumbabig.2cl"  "nsumbabig.3cl"  "dbhgrowthcl"    "bagrowthcl"    
[53] "nsumba.1cl2"    "nsumba.2cl2"    "nsumba.3cl2"    "nsumba5.1cl2"  
[57] "nsumba5.2cl2"   "nsumba5.3cl2"   "nsumbabig.1cl2" "nsumbabig.2cl2"
[61] "nsumbabig.3cl2" "dbhgrowthcl2"   "bagrowthcl2"   
> # nsumbabig.1
> models_nsumbabig1 <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+     d$nsumbabig.1cl <- as.factor(as.character(d$nsumbabig.1cl))
+ #    print(paste("levels",length(levels(d$nsumbaclq))))
+     dlply(d, .(d$nsumbabig.1cl), .fun = function(x) {
+         if(nrow(x) >= 3) {
+             lm(x$ht ~ poly(x$dbh, 2))
+         }
+     })
+ }), recursive = FALSE)
Error in poly(x$dbh, 2) (from #6) : 
  'degree' must be less than number of unique points
> ################################
> # nsumbabig.1
> models_nsumbabig1 <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+     d$nsumbabig.1cl <- as.factor(as.character(d$nsumbabig.1cl))
+ #    print(paste("levels",length(levels(d$nsumbaclq))))
+     dlply(d, .(d$nsumbabig.1cl), .fun = function(x) {
+         if(nrow(x) > 3) {
+             lm(x$ht ~ poly(x$dbh, 2))
+         }
+     })
+ }), recursive = FALSE)
> models_nsumbabig1 <- unlist(dlply(dat, .(pplot), .fun = function(d) {
    d$nsumbabig.1cl <- as.factor(as.character(d$nsumbabig.1cl))
#    print(paste("levels",length(levels(d$nsumbaclq))))
    dlply(d, .(d$nsumbabig.1cl), .fun = function(x) {
        if(nrow(x) > 3) {
            lm(x$ht ~ poly(x$dbh, 2))
        }
    })
}), recursive = FALSE)
models_nsumbabig1 <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+     d$nsumbabig.1cl <- as.factor(as.character(d$nsumbabig.1cl))
+ #    print(paste("levels",length(levels(d$nsumbaclq))))
+     dlply(d, .(d$nsumbabig.1cl), .fun = function(x) {
+         if(nrow(x) > 3) {
+             lm(x$ht ~ poly(x$dbh, 2))
+         }
+     })
+ }), recursive = FALSE)
> models_nsumbabig1[1]
$`4.1`

Call:
lm(formula = x$ht ~ poly(x$dbh, 2))

Coefficients:
    (Intercept)  poly(x$dbh, 2)1  poly(x$dbh, 2)2  
          4.668           48.744           -2.668  


> is.null(models_nsumbabig1[1])
[1] FALSE
> length(models_nsumbabig1)
[1] 74
> length(models_nsumbabig1[!is.null(models_nsumbabig1])
Error: unexpected ']' in "length(models_nsumbabig1[!is.null(models_nsumbabig1]"
> length(models_nsumbabig1[!is.null(models_nsumbabig1)])
[1] 74
> ################################
> # nsumbabig.1
> models_nsumbabig <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+     d$nsumbabig.3cl <- as.factor(as.character(d$nsumbabig.3cl))
+ #    print(paste("levels",length(levels(d$nsumbaclq))))
+     dlply(d, .(d$nsumbabig.3cl), .fun = function(x) {
+         if(nrow(x) > 3) {
+             lm(x$ht ~ poly(x$dbh, 2))
+         }
+     })
+ }), recursive = FALSE)
> length(models_nsumbabig[!is.null(models_nsumbabig)])
[1] 96
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(assign(x, sum(getAIC(models_list[!is.null(models_list)])$aic2)),
+       assign(x, sum(getMSE(models_list[!is.null(models_list)])$mse)))
+ })
Error in UseMethod("extractAIC") (from #4) : 
  no applicable method for 'extractAIC' applied to an object of class "NULL"
> 
> tt
     elevcl aspcl soilcl slopcl nsumba.1cl nsumba.2cl nsumba.3cl nsumba5.1cl
[1,]      0     0      0      0          0          0          0           0
[2,]      0     0      0      0          0          0          0           0
     nsumba5.2cl nsumba5.3cl nsumbabig.1cl nsumbabig.2cl nsumbabig.3cl
[1,]           0           0             0             0             0
[2,]           0           0             0             0             0
     dbhgrowthcl bagrowthcl
[1,]           0          0
[2,]           0          0
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(sum(getAIC(models_list[!is.null(models_list)])$aic2),
+       sum(getMSE(models_list[!is.null(models_list)])$mse))
+ })
Error in UseMethod("extractAIC") (from #4) : 
  no applicable method for 'extractAIC' applied to an object of class "NULL"
> 
> tt
     elevcl aspcl soilcl slopcl nsumba.1cl nsumba.2cl nsumba.3cl nsumba5.1cl
[1,]      0     0      0      0          0          0          0           0
[2,]      0     0      0      0          0          0          0           0
     nsumba5.2cl nsumba5.3cl nsumbabig.1cl nsumbabig.2cl nsumbabig.3cl
[1,]           0           0             0             0             0
[2,]           0           0             0             0             0
     dbhgrowthcl bagrowthcl
[1,]           0          0
[2,]           0          0
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(#sum(getAIC(models_list[!is.null(models_list)])$aic2),
+       sum(getMSE(models_list[!is.null(models_list)])$mse))
+ })
> tt
       elevcl         aspcl        soilcl        slopcl    nsumba.1cl 
      28.0495       28.0495       28.0495       28.0495           NaN 
   nsumba.2cl    nsumba.3cl   nsumba5.1cl   nsumba5.2cl   nsumba5.3cl 
          NaN           NaN           NaN           NaN           NaN 
nsumbabig.1cl nsumbabig.2cl nsumbabig.3cl   dbhgrowthcl    bagrowthcl 
          NaN           NaN           NaN           NaN           NaN 
> # function to get mse
> getMSE <- function(list_of_models) {
+     ldply(list_of_models,
+           function(model) {
+               if(!is.null(model) {
Error: unexpected '{' in:
"          function(model) {
              if(!is.null(model) {"
>                   c(
+                       mse = mean(residuals(model)^2)
+                       )
Error in residuals(model) : object 'model' not found
>               }
Error: unexpected '}' in "              }"
>           })
Error: unexpected '}' in "          }"
> }
Error: unexpected '}' in "}"
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clqcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(#sum(getAIC(models_list[!is.null(models_list)])$aic2),
+       sum(getMSE(models_list[!is.null(models_list)])$mse))
+ })
> tt
       elevcl         aspcl        soilcl        slopcl    nsumba.1cl 
      28.0495       28.0495       28.0495       28.0495           NaN 
   nsumba.2cl    nsumba.3cl   nsumba5.1cl   nsumba5.2cl   nsumba5.3cl 
          NaN           NaN           NaN           NaN           NaN 
nsumbabig.1cl nsumbabig.2cl nsumbabig.3cl   dbhgrowthcl    bagrowthcl 
          NaN           NaN           NaN           NaN           NaN 
> names(dat)[clqcols]
 [1] "elevcl"        "aspcl"         "soilcl"        "slopcl"       
 [5] "nsumba.1cl"    "nsumba.2cl"    "nsumba.3cl"    "nsumba5.1cl"  
 [9] "nsumba5.2cl"   "nsumba5.3cl"   "nsumbabig.1cl" "nsumbabig.2cl"
[13] "nsumbabig.3cl" "dbhgrowthcl"   "bagrowthcl"   
> clqcols <- intersect(grep("^n",names(dat), grep("cl$", names(dat)))
+ clqcols <- intersect(grep("^n",names(dat), grep("cl$", names(dat))))
Error: unexpected symbol in:
"clqcols <- intersect(grep("^n",names(dat), grep("cl$", names(dat)))
clqcols"
> clqcols <- intersect(grep("^n",names(dat)), grep("cl$", names(dat)))
> names(dat)[clqcols]
[1] "nsumba.1cl"    "nsumba.2cl"    "nsumba.3cl"    "nsumba5.1cl"  
[5] "nsumba5.2cl"   "nsumba5.3cl"   "nsumbabig.1cl" "nsumbabig.2cl"
[9] "nsumbabig.3cl"
> s
function (..., k = -1, fx = FALSE, bs = "tp", m = NA, by = NA, 
    xt = NULL, id = NULL, sp = NULL) 
{
    vars <- as.list(substitute(list(...)))[-1]
    d <- length(vars)
    by.var <- deparse(substitute(by), backtick = TRUE, width.cutoff = 500)
    if (by.var == ".") 
        stop("by=. not allowed")
    term <- deparse(vars[[1]], backtick = TRUE, width.cutoff = 500)
    if (term[1] == ".") 
        stop("s(.) not yet supported.")
    if (d > 1) 
        for (i in 2:d) {
            term[i] <- deparse(vars[[i]], backtick = TRUE, width.cutoff = 500)
            if (term[i] == ".") 
                stop("s(.) not yet supported.")
        }
    for (i in 1:d) term[i] <- attr(terms(reformulate(term[i])), 
        "term.labels")
    k.new <- round(k)
    if (all.equal(k.new, k) != TRUE) {
        warning("argument k of s() should be integer and has been rounded")
    }
    k <- k.new
    if (length(unique(term)) != d) 
        stop("Repeated variables as arguments of a smooth are not permitted")
    full.call <- paste("s(", term[1], sep = "")
    if (d > 1) 
        for (i in 2:d) full.call <- paste(full.call, ",", term[i], 
            sep = "")
    label <- paste(full.call, ")", sep = "")
    if (!is.null(id)) {
        if (length(id) > 1) {
            id <- id[1]
            warning("only first element of `id' used")
        }
        id <- as.character(id)
    }
    ret <- list(term = term, bs.dim = k, fixed = fx, dim = d, 
        p.order = m, by = by.var, label = label, xt = xt, id = id, 
        sp = sp)
    class(ret) <- paste(bs, ".smooth.spec", sep = "")
    ret
}
<bytecode: 0x08da0320>
<environment: namespace:mgcv>
> clcols <- intersect(grep("^n",names(dat)), grep("cl$", names(dat)))
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(#sum(getAIC(models_list[!is.null(models_list)])$aic2),
+       sum(getMSE(models_list[!is.null(models_list)])$mse))
+ })
> 
> tt
   nsumba.1cl    nsumba.2cl    nsumba.3cl   nsumba5.1cl   nsumba5.2cl 
          NaN           NaN           NaN           NaN           NaN 
  nsumba5.3cl nsumbabig.1cl nsumbabig.2cl nsumbabig.3cl 
          NaN           NaN           NaN           NaN 
> # create list of models for each plot/size class, extract AIC values + MSE, return sums
> #  for each neighborhood calculation method...
> tt <- sapply(names(dat)[clcols], function (x) {
+     models_list <- unlist(dlply(dat, .(pplot), .fun = function(d) {
+         d[,x] <- as.factor(as.character(d[,x]))
+         dlply(d, x, .fun = function(l) {
+             if(nrow(l)>3) {
+                 lm(l$ht ~ poly(l$dbh, 2))
+             }
+         })
+     }), recursive = FALSE)
+     c(#sum(getAIC(models_list[!is.null(models_list)])$aic2),
+       sum(getMSE(models_list)$mse))
+ })
> 
> tt
   nsumba.1cl    nsumba.2cl    nsumba.3cl   nsumba5.1cl   nsumba5.2cl 
          NaN           NaN           NaN           NaN           NaN 
  nsumba5.3cl nsumbabig.1cl nsumbabig.2cl nsumbabig.3cl 
          NaN           NaN           NaN           NaN 
> table(dat$nsumba.1cl)

   1    2    3 
5282  850  341 
> table(dat$nsumba.2cl)

   1    2    3 
 804 2103 1221 
> table(dat$nsumba.3cl)

   1    2    3 
  55 1014 1312 
> 