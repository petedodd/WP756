library(odin)

## ODEs for model
## mf <- here('pete/R/TREATSmodel.R') #model file
## TREATS <- odin::odin(mf,target='c') #model generator

mf <- here('R/TREATSmodelmixed.R') #model file
TREATS <- odin::odin(mf,target='c') #model generator

## BL interpolator
source(here("R/dynamicIRR.R"))


## build community-specific parms
source(here("R/get_params.R"))

## data to run
load(here("treats_odin/Parameters/P.Rdata")) #list of popart comm'y data
load(here("treats_odin/Parameters/L.Rdata"))      #new grid of alpha HR
load(here("treats_odin/Parameters/IRRGrid.Rdata")) #Grid values of IRR
load(here("pete/data/contactmatrix.Rdata")) #mixing if used

## community & HR/alph bases
intcoms <- c(1, 2, 5, 6, 8, 9, 10, 11, 13, 14, 16, 18, 19, 20)
concoms <- c(3, 4, 7, 12, 15, 17, 21)
zmcomms <- 1:12
zacomms <- 13:21
noltbi <- c(1,  6,  9, 11, 13, 18, 20)
t <-  seq(1970, 2030, by=1/12)
alph.base <- 0.3
HR.base <- 0.2
