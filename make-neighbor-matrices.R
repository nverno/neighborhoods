# Create a neighborhood model with crowding and dbh as independent variables
#  and growth in height or dbh as dependent variables
dat <- read.csv("long.csv")
df <- read.csv("long-df.csv") # doug fir data

# choose sr(sr must be >= 1) and other variables
sr <- 2
spec <- "ABBA"
dep.var <- "bagrowth"
ind.var <- "ba"

# define targets and neighbors, for moose data
targets <- subset(dat, bqudx < (12-sr) & bqudx > (-1 + sr) & bqudy < (12 - sr) &
                  bqudy > (-1 + sr) & stat=="ALIVE")
neighbors <- subset(dat, bqudx < 11 & bqudx > 0 & bqudy < 11 &
                    bqudy > 0 & stat=="ALIVE")

# remove trees that dont satisfy certain conditions
grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0)
targets <- targets[grew,]

# make neighbor matrices using square radius (i.e bqudx,bqudy)
make.neighbor.matrices(targets, neighbors, sr, ind.var=ind.var, bigger=TRUE)


# define targets for doug fir data, using trees in range(x,y) [-11,11]
sr <- 5
spec <- "FD"
targets <- subset(df, abs(x) < (11-sr) & abs(y) < (11-sr) & stat=="ALIVE")
neighbors <- subset(df, abs(x) <= 11 & abs(y) <= 11 & stat=="ALIVE")

# remove trees that dont satisfy certain conditions
grew <- which(!is.na(targets[,dep.var]) & targets$spec==spec & targets[,dep.var]>0)
targets <- targets[grew,]

# graph targets and neighbors
range(targets$x)
plot(df$x, df$y)
points(targets$x,targets$y, col="red")

# make neighbor matrices using exact locations (x,y)
make.neighbor.matrices(targets, neighbors, sr=5, bigger = TRUE, ind.var = "ba",
                       realdist = TRUE)



