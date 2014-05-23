###################################################################
#####										#######
#####				NCI Master BA				#######
#####										#######
###################################################################
data.directory<-"C:\\Users\\Noah\\Dropbox\\Shared\\Noah\\Datasets"
data.desktop<-"C:\\Documents and Settings\\Noah\\My Documents\\Dropbox\\Shared\\Noah\\Datasets"
code.directory<-"C:\\Users\\Noah\\Dropbox\\Shared\\Noah\\Growth"
output.directory<-"C:\\Users\\Noah\\Dropbox\\Shared\\Noah\\Growth\\Output"
input.filename <- "pptreemas10.csv"

#### get data and make small test dataset, make target dataset and neighbors
##  Note: if wanting to use BA or DBH - search and replace "bvs","bv","BV","BV98",and "BVGROWTH"
##  will need to be replaced with other growth column

##setwd(data.directory)
pp <- read.csv("pptreemas10.csv")
pp$BA86 <- 0.00007854*pp$DBH86*pp$DBH86 ### Basal area column, m^2
pp$BA87 <- 0.00007854*pp$DBH87*pp$DBH87
pp$BA98 <- 0.00007854*pp$DBH98*pp$DBH98 
pp$BA10 <- 0.00007854*pp$DBH10*pp$DBH10 
pp$BAGROWTH1 <- pp$BA98-pp$BA86 
pp$BAGROWTH2 <- pp$BA98-pp$BA87 
pp$BAGROWTH3 <- pp$BA10-pp$BA98 
#### change column names in pp
names(pp)[1] <- "PLOT"
names(pp)[81] <- "BQUADX"
names(pp)[82] <- "BQUADY"
names(pp)[4] <- "SPECIES"

### Make target dataset with trees from three periods, but no repeats, and annualize the BAGROWTH
plots <- c(4:27)
pp <- subset(pp, PLOT %in% plots)
pp <- pp[!is.na(pp$BQUADX),]
period1 <- subset(pp, !is.na(BAGROWTH1))
period1$BAGROWTH2 <- NA
period1$BAGROWTH3 <- NA
period1$BAGROWTH1 <- period1$BAGROWTH1/12
period1$PRIORBA <- period1$BA86

period2 <- subset(pp, !is.na(BAGROWTH2) & is.na(BAGROWTH1))
period2$PLOT <- period2$PLOT + 100
period2$BAGROWTH3 <- NA
period2$BAGROWTH2 <- period2$BAGROWTH2/11
period2$PRIORBA <- period2$BA87

period3 <- subset(pp, !is.na(BAGROWTH3) & is.na(BAGROWTH1) & is.na(BAGROWTH2))
period3$PLOT <- period3$PLOT + 200
period3$BAGROWTH3 <- period3$BAGROWTH3/12
period3$PRIORBA <- period3$BA98

ppnew <- rbind(period1,period2,period3)

ppnew$BAGROWTH <-  ifelse(!is.na(ppnew$BAGROWTH1),ppnew$BAGROWTH1, ifelse(!is.na(ppnew$BAGROWTH2),ppnew$BAGROWTH2,ppnew$BAGROWTH3))
ppnew <- ppnew[ppnew$BAGROWTH >= 0,]
ppnew <- ppnew[-c(104:106)]
targets <- subset(ppnew,BQUADX > 0 & BQUADX < 11 & BQUADY > 0 & BQUADY < 11)    ### define the target tree dataset

### create the neighbors dataset, with plots numbered as in targets, but including all neighbors
### alive at the beginning of the sampling period
neigh1 <- subset(pp, !is.na(BA86))
neigh1$PRIORBA <- neigh1$BA86
neigh2 <- subset(pp, !is.na(BA87))
neigh2$PLOT <- neigh2$PLOT + 100
neigh2$PRIORBA <- neigh2$BA87
neigh3 <- subset(pp, !is.na(BA98))
neigh3$PLOT <- neigh3$PLOT+200
neigh3$PRIORBA <- neigh3$BA98

neighbors <- rbind(neigh1,neigh2,neigh3)		### define the neighbor trees dataset (the whole dataset)

### Aspect?
asp <- "E"
targets <- subset(targets, ASPCL == asp)
neighbors <- subset(neighbors, ASPCL == asp)

### elevation?
elev <- "L"
targets <- subset(targets, ELEVCL == elev)
neighbors <- subset(neighbors, ELEVCL == elev)

### iterations for neighborhoods
iter <- 100000

### neighbor radius, in quadrats
sr <- 1 

###################################################################
#####										#######
#####			Set up neighbor matrices			#######
#####										#######
###################################################################

neighdist<-function(targetx, targety, neighborx, neighbory) {
  sqrt((targetx-neighborx)^2 + (targety-neighbory)^2)
}
##### determine max number of neighbors
  max.neighbors <- 0
  row.with.max.neighbors <- 0
  for (i in 1:nrow(targets)) 
  { max.neighbors <- max(max.neighbors,
     nrow(subset(neighbors, PLOT == targets$PLOT[i] & TAG != targets$TAG[i] & 
  		BQUADX < targets$BQUADX[i]+sr & BQUADX > targets$BQUADX[i]-sr &
		BQUADY < targets$BQUADY[i]+sr & BQUADY > targets$BQUADY[i]-sr)))
	if(nrow(subset(neighbors, PLOT == targets$PLOT[i] & TAG != targets$TAG[i] & 
  		BQUADX < targets$BQUADX[i]+sr & BQUADX > targets$BQUADX[i]-sr &
		BQUADY < targets$BQUADY[i]+sr & BQUADY > targets$BQUADY[i]-sr&
		PRIORBA > targets$PRIORBA[i])) >= max.neighbors)
		row.with.max.neighbors <- i
}

# initialize matrices for species, distance and ba to neighbors

  distances <- matrix(0, nrow=nrow(targets), ncol=max.neighbors)
  bas <- matrix(0, nrow=nrow(targets), ncol=max.neighbors)
  species <- matrix(0, nrow=nrow(targets), ncol=max.neighbors)

# Go back and populate matrix
  for (i in 1:nrow(targets)) {
    neighbors.for.tree <- subset(neighbors, PLOT == targets$PLOT[i] & TAG!=targets$TAG[i] & 
	BQUADX < targets$BQUADX[i]+sr & BQUADX > targets$BQUADX[i]-sr &
	BQUADY < targets$BQUADY[i]+sr & BQUADY > targets$BQUADY[i]-sr &
	PRIORBA >= targets$PRIORBA[i])
    if (nrow(neighbors.for.tree) > 0) {
      distances[i,1:nrow(neighbors.for.tree)] <-
      neighdist(targets$BQUADX[i]*2, targets$BQUADY[i]*2,
              neighbors.for.tree$BQUADX*2, neighbors.for.tree$BQUADY*2)
      bas[i,1:nrow(neighbors.for.tree)] <- neighbors.for.tree$PRIORBA      # use whatever variable name you have for BA and species codes
      species[i,1:nrow(neighbors.for.tree)] <- neighbors.for.tree$SPECIES
    }
  }
# Replace 0 distances with 1
  distances <- ifelse(distances == 0, 1, distances)
# drop neighbors in the square that are outside the desired neighbor radius
  radius <- 20
  distances <- ifelse(distances > 20, NA, distances)
###  calculate NCI assuming alpha =2 beta = 1
  alpha = 2
  beta = 1
  targets$NCI <- rowSums((bas^alpha)/(distances^beta), na.rm=T)

###################################################################
#####										#######
#####				Models					#######
#####										#######
###################################################################
size.linear.model <- function(a) {
	size.effect <- a*targets$PRIORBA
	size.effect
}
size.power.model <- function(a,b) {
	size.effect <- a*targets$PRIORBA^b
	size.effect
}
size.logistic.model <- function(PG, sizeX0, sizeXb)
{
  size.effect <- exp(-0.5*(log(targets$PRIORBA/sizeX0)/sizeXb)^2)
  PG * size.effect
}

size.logistic.nci.model <- function(PG, sizeX0, sizeXb, alpha, beta, C, D)
{
  size.effect <- exp(-0.5*(log(targets$PRIORBA/sizeX0)/sizeXb)^2)
  nci <- rowSums(((bas ^ alpha)/(distances ^ beta)), na.rm=T)
  competition.effect <- exp(-(C) * nci^D)

  PG * size.effect * competition.effect
}
size.logistic.nci.model1 <- function(PG, sizeX0, sizeXb, alpha, C, D)
{
  size.effect <- exp(-0.5*(log(targets$PRIORBA/sizeX0)/sizeXb)^2)
  nci <- rowSums((bas ^ alpha), na.rm=T)
  competition.effect <- exp(-(C) * nci^D)

  PG * size.effect * competition.effect
}
size.logistic.density.nci.model <- function(PG, sizeX0, sizeXb, C, D) 
{
  size.effect <- exp(-0.5*(log(targets$PRIORBA/sizeX0)/sizeXb)^2)
  nci <- rowSums(bas>0)
  competition.effect <- exp(-(C) * nci^D)
  PG * size.effect * competition.effect
}
nci.model <- function(PG, alpha, beta, C, D)
{
  nci <- rowSums(((bas ^ alpha)/(distances ^ beta)), na.rm=T)
  competition.effect <- exp(-(C) * nci^D)
  PG * competition.effect
}

density.nci.model <- function(PG, C, D) 
{
  nci <- rowSums(bas>0)
  competition.effect <- exp(-(C) * nci^D)
  PG * competition.effect
}
size.logistic.nci.lambda.model <- function(PG, sizeX0, sizeXb, alpha, beta, lambda, C, D)
{
  size.effect <- exp(-0.5*(log(targets$PRIORBA/sizeX0)/sizeXb)^2)
  #lambda.vals <- ifelse(neigh.lambda.indexes==0,0,lambda[neigh.lambda.indexes])
  lambda.vals <- lambda[neigh.lambda.indexes]
  dim(lambda.vals) <- dim(neigh.lambda.indexes)
  nci <- rowSums((lambda.vals * (bas ^ alpha)/(distances ^ beta)), na.rm=T)
  competition.effect <- exp(-(C) * nci^D)

  PG * size.effect * competition.effect
}
nci.lambda.model <- function(PG, alpha, beta, lambda, C, D)
{
  lambda.vals <- lambda[neigh.lambda.indexes]
  dim(lambda.vals) <- dim(neigh.lambda.indexes)
  nci <- rowSums((lambda.vals * (bas ^ alpha)/(distances ^ beta)), na.rm=T)
  competition.effect <- exp(-(C) * nci^D)

  PG * competition.effect
}

###################################################################
#####										#######
#####				Set-Up					#######
#####										#######
###################################################################

# Target species
spp <- "ABBA"
sppcode <- 2
# Minimum target tree BA, m^2
min.target.ba <- 0
# Minimum neighbor tree BA, m^2
##min.neighbor.ba <- 0.0019635
# Max radius to search for neighbors, m
max.neighbor.radius <- 100
# Max radius from plot center for targets, m
max.target.distance <- 100
# Min growth threshold to be included, 
min.growth.allowed <- -0.01
# Max growth threshold to be included, 
max.growth.allowed <- 2

########## LAMBDA setup -- Different Param for all 	##########  
########## 			species				##########  
top.neighbors <- sort(table(ifelse(species==0,NA,species)), decreasing=T)
top.neighbors.sppcodes <- as.numeric(names(top.neighbors))
neigh.lambda.indexes <- matrix(0, nrow=nrow(species), ncol=ncol(species))
neigh.lambda.indexes <- apply(species, c(1,2), 
   function(x) {if (x %in% top.neighbors.sppcodes) x else 18}) ### create sppcode for nonspecies
neigh.lambda.indexes <- ifelse(species == 0, 18, neigh.lambda.indexes)
# Fix non-tree lambda placeholder at 0
initial.lambdas<-rep(1,length(top.neighbors)+1)
initial.lambdas[length(top.neighbors)+1]<-0 # For non-trees
lambda.lower.bounds<-rep(0,length(top.neighbors)+1)
lambda.upper.bounds<-c(rep(100,length(top.neighbors)),0)
##lambda.lower.bounds[top.neighbors.sppcodes == sppcode] <- 1
##lambda.upper.bounds[top.neighbors.sppcodes == sppcode] <- 1
names(initial.lambdas)<-c(as.character(top.neighbors.sppcodes), "Placeholder")
names(lambda.lower.bounds)<-c(as.character(top.neighbors.sppcodes), "Placeholder")
names(lambda.upper.bounds)<-c(as.character(top.neighbors.sppcodes), "Placeholder")

########## LAMBDA setup -- Different Param for inter  	##########  
########## 		and intra specific comp				##########  
neigh.lambda.indexes1 <- apply(neigh.lambda.indexes, c(1,2),
	function(x) { 	if(x != 18 & x != sppcode) 17 else x } )
initial.lambdas1 <- rep(1,3)
initial.lambdas1[3] <- 0 ## for non-trees
lambda.lower.bounds1 <- rep(0,3)
lambda.upper.bounds1 <- c(rep(100,2),0)
names(initial.lambdas1) <- c(as.character(sppcode),as.character(17),"Placeholder")
names(lambda.lower.bounds1) <- c(as.character(sppcode),as.character(17),"Placeholder")
names(lambda.upper.bounds1) <- c(as.character(sppcode),as.character(17),"Placeholder")

########## LAMBDA setup -- Params for ABBA, PIRU, BECO,  	##########  
########## 		SOAM, BEAL, ACSP, rest in OTHERS		########## 
neighbors.to.combine <- top.neighbors[which(top.neighbors < 100)]
neighbors.to.combine.sppcodes <- as.numeric(names(neighbors.to.combine))
top.neighbors2 <- top.neighbors[which(top.neighbors >= 100)]
top.neighbors.sppcodes2 <- as.numeric(names(top.neighbors2))
neigh.lambda.indexes2 <- apply(neigh.lambda.indexes, c(1,2), 
   function(x) {if (x %in% neighbors.to.combine.sppcodes) 16 else x}) 
initial.lambdas2 <- rep(1,length(top.neighbors2)+1)
initial.lambdas2[length(top.neighbors2)+1] <- 0 ## for non-trees
lambda.lower.bounds2<-rep(0,length(top.neighbors2)+1)
lambda.upper.bounds2<-c(rep(100,length(top.neighbors2)),0)
names(initial.lambdas2)<-c(as.character(top.neighbors.sppcodes2), "Placeholder")
names(lambda.lower.bounds2)<-c(as.character(top.neighbors.sppcodes2), "Placeholder")
names(lambda.upper.bounds2)<-c(as.character(top.neighbors.sppcodes2), "Placeholder")

########### Dataset setup ###############                                  
# Dataset of target trees - alive, right species, right size, within max
# radius
using.targets <- which(	targets$SPECIES == spp &
###				targets$ELEVCL == "M" &
				targets$PRIORBA >= min.target.ba &
                       	targets$BAGROWTH >= min.growth.allowed)
targets <- targets[using.targets,]

# Eliminate neighbors for targets we're not using
bas <- bas[using.targets,]
distances <- distances[using.targets,]
species <- species[using.targets,]
neigh.lambda.indexes<-neigh.lambda.indexes[using.targets,]
neigh.lambda.indexes1<-neigh.lambda.indexes1[using.targets,]
neigh.lambda.indexes2<-neigh.lambda.indexes2[using.targets,]

# Get rid of neighbors under the minimum BA
#bas <- ifelse(bas < min.neighbor.ba, 0, bas)
# Get rid of neighbors beyond the max radius
distances <- ifelse(distances > max.neighbor.radius, 0, distances)
bas[which(distances == 0)] <- 0
distances[which(bas == 0)] <- 0
distances <- ifelse(distances == 0, NA, distances)

###################################################################
#####										#######
#####				Annealing					#######
#####										#######
###################################################################
# Put together informative info for the comment
comment.txt <- paste("BA annual growth 86-98,87-98,98-2010.",
  "Species:",spp,"Species code:",sppcode,
  "Neighbor BA >= Target BA", "Max neighbor search radius:", max.neighbor.radius,
  "Max distance from plot center for targets:", max.target.distance,
  "Minimum allowed growth rate:", min.growth.allowed, 
  "Maximum allowed growth rate:", max.growth.allowed, "Input file:",
  input.filename, sep=" ")
 
library(likelihood)
#######################################################################
#
#   size.logistic.nci model
#   

par <- list(PG = 0.001799368,
            sizeX0 = 0.2556862,
            sizeXb = 1.536174,
            alpha = 0.9307221,  
            beta = 0.4980267, 
            C = 1,
            D = 1,
            sd = 0.0001922697)

var <- list(mean = "predicted", x = "BAGROWTH", log=T)
par_hi <- list(PG=1, sizeX0 = 2, sizeXb = 50, 
               alpha = 10, beta = 10, C = 10, D = 10, sd=1)

par_lo <- list(PG=0, sizeX0 = 0, sizeXb = 0.001, alpha = 0, 
                 beta = 0, C = 0, D = 0, sd = 0.000001)

size.logistic.nci.results<-anneal(
  model = size.logistic.nci.model, par = par, var = var,
  source_data = targets, par_lo = par_lo, par_hi = par_hi,
  pdf = dnorm, dep_var = "BAGROWTH", max_iter = iter,note = comment.txt)

###setwd(output.directory)
write_results(size.logistic.nci.results,
               paste(asp,elev,sr,iter,"nci.txt"),
              data=F, print_whole_hist=F)





