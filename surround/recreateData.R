## Data manipulation for neighborhood surrounds
## Run this script to recreate all datasets used in this project
## Note: also uses canhts.csv created in the boles project
source("~/work/functions/functions-datatrans.R")

## read master data
source("~/work/data/data-prep/read-moose.R")

## add/remove columns, annualize growth, rename columns if they changed
## creates "~/work/data/data/moose-wide.csv"
source("~/work/data/data-prep/clean-moose.R")

## make long version of data
## creates "~/work/data/data/moose-long.csv"
source("~/work/data/data-trans/make-long-moose.R")

## add canopy heights/change time 10 to time 100 for linearity in path graphs
## creates "~/work/data/data/dynamicallometry/moose-long-canopies.csv"
source("~/work/dynamicallometry/add-canopy-height.R")
