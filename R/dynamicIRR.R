
bl <- function(F11,F12,F21,F22,x1,x2,y1,y2,x,y){
    A <- (x2-x1)*(y2-y1)
    (F11*(x2-x)*(y2-y) + F12*(x2-x)*(y-y1) + F21*(x-x1)*(y2-y) + F22*(x-x1)*(y-y1)) / A
}

## i  = alpha, j = HR
## list of lists
BLI <- function(LL,         #lists of IRRs list of lists (to handle matrices/arrays)
                xz,yz,      #vector of alph and HR
                x,y         #new values of alph and HR
                ){
    i <- sum(x>xz) ; j <- sum(y>yz)
    i2 <- i+1 ; j2 <- j+1
    bl(LL[[i]][[j]], LL[[i]][[j2]], LL[[i2]][[j]], LL[[i2]][[j2]],
       xz[i],xz[i2],yz[j],yz[j2],
       x,y)
}


## tidying slightly
## May 4 below (dynamic_irr)


## This function takes in grid data (list of IRRs, and vectors of alpha and HR, and 
## New scalar alpha and HR values to calculate interpolated IRRs. It requires IRRgrid data
## Need to specify comid and scenario


dynamic_irr <- function(comid, interv, alph, HR) {
    
    IrrSubset <-IRRGrid[which(sapply(IRRGrid, `[[`, 'ComID') == comid & 
                              sapply(IRRGrid, `[[`, 'scenario') == interv)]
    
    tz <-seq(from=1970,to=2030,by=1)

    ## approximations
    irr_hivm <- BLI(IrrSubset[[1]]$irr_hivm, IrrSubset[[1]]$alph, IrrSubset[[1]]$HR, alph, HR) #HF
    irr_hivm <- as.data.table(exp(irr_hivm)); irr_hivm[,Year:=tz]
    irr_hivm[,ComID:=IrrSubset[[1]]$ComID];   irr_hivm[,scenario:=IrrSubset[[1]]$scenario] 

    tst <- BLI(IrrSubset[[1]]$irr_hivf, IrrSubset[[1]]$alph,IrrSubset[[1]]$HR,alph, HR) #HM
    tst <- as.data.table(exp(tst)); tst[,Year:=tz]
    tst[,ComID:=IrrSubset[[1]]$ComID]; tst[,scenario:=IrrSubset[[1]]$scenario]
    irr_hivf <- tst
    
    tst <- BLI(IrrSubset[[1]]$irr_artm, IrrSubset[[1]]$alph, IrrSubset[[1]]$HR,alph, HR) #AF
    tst <- as.data.table(exp(tst)); tst[,Year:=tz]
    tst[,ComID:=IrrSubset[[1]]$ComID]; tst[,scenario:=IrrSubset[[1]]$scenario]
    irr_artm <- tst

    tst <- BLI(IrrSubset[[1]]$irr_artf,IrrSubset[[1]]$alph,IrrSubset[[1]]$HR,alph, HR) #AM
    tst <- as.data.table(exp(tst)); tst[,Year:=tz]
    tst[,ComID:=IrrSubset[[1]]$ComID]; tst[,scenario:=IrrSubset[[1]]$scenario]
    irr_artf <- tst
    
    return(list(irr_hivm=irr_hivm, irr_hivf=irr_hivf, irr_artm=irr_artm, irr_artf=irr_artf))
} 
