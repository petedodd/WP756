 
nage  <- 17
nsex  <-  2
nhiv  <-  3
##===========DEMOGRAPHY===================


popart_year[]      <- user()
dim(popart_year)   <- user()
length_popart_year <- length(popart_year)

###====== birth==============
MB[] <-user()
FB[] <-user()
dim(MB)  <- length_popart_year
dim(FB)  <- length_popart_year
birthM   <- interpolate(popart_year,  MB,'linear')
birthF   <- interpolate(popart_year,  FB,'linear')

birth[1,1, 1] <- birthM 
birth[1,2, 1] <- birthF 

dim(birth) <- c(nage, nsex, nhiv)

##======migration==============
migration[]        <- user()               
unpd_year[]        <- user() # 5year time step
dim(unpd_year)     <- user()  
length_unpd_year   <- length(unpd_year)
dim(migration)     <- length_unpd_year 
mgr    <-  interpolate(unpd_year, migration,'linear')

###====natural death========
mortality_ratF[, ]  <- user()   #matrix(t, nage)
mortality_ratM[, ]  <- user()   #matrix(t, nage)
dim(mortality_ratF) <- c(length_popart_year, nage)
dim(mortality_ratM) <- c(length_popart_year, nage)
mortality[, , 1]    <- mortality_ratM[i, j]      # array(t, nage, nsex)
mortality[, , 2]    <- mortality_ratF[i, j]
dim(mortality)      <- c(length_popart_year, nage, nsex)
mortality_t[, ]     <- interpolate(popart_year, mortality, "linear")
dim(mortality_t)    <- c(nage, nsex)

mort[, , 1] <- mortality_t[i, j]   # array(nage, nsex, nhiv) at a given time
mort[, , 2] <- mortality_t[i, j] 
mort[, , 3] <- mortality_t[i, j] 
dim(mort)   <- c(nage, nsex, nhiv)


## contact matrix
CM[,] <- user()
dim(CM) <- c(6,6)

##======END DEMOGRAPHY=========================


##========HIV|ART MORTALITY============

##====HIV mortality==============
HIV_mortM[,]      <- user()
HIV_mortF[,]      <- user()
dim(HIV_mortM)    <- c(length_popart_year, nage)
dim(HIV_mortF)    <- c(length_popart_year, nage)
hiv_mortality[, , 1]  <- HIV_mortM[i, j]
hiv_mortality[, , 2]  <- HIV_mortF[i, j]
dim(hiv_mortality)    <- c(length_popart_year, nage, nsex)
hiv_mortality_t[, ]   <- interpolate(popart_year, hiv_mortality, "linear")
dim(hiv_mortality_t)  <- c(nage, nsex)

hiv_mort[, , 1] <- 0
hiv_mort[, , 2] <- hiv_mortality_t[i, j]
hiv_mort[, , 3] <- 0
dim(hiv_mort)   <- c(nage, nsex, nhiv)
##======ART mortality===============
ART_mortM[,]      <- user()
ART_mortF[,]      <- user()
dim(ART_mortM)    <- c(length_popart_year, nage)
dim(ART_mortF)    <- c(length_popart_year, nage)
art_mortality[, , 1]  <- ART_mortM[i, j]
art_mortality[, , 2]  <- ART_mortF[i, j]
dim(art_mortality)    <- c(length_popart_year, nage, nsex)
art_mortality_t[, ]   <- interpolate(popart_year, art_mortality, "linear")
dim(art_mortality_t)  <- c(nage, nsex)

art_mort[, , 1] <- 0
art_mort[, , 2] <- 0
art_mort[, , 3] <- art_mortality_t[i, j] 
dim(art_mort)   <- c(nage, nsex, nhiv)

##====Mortalityacombined====
mortall[, ,]  <- mort[i, j, k] + hiv_mort[i, j, k] + art_mort[i, j, k]
dim(mortall)  <- c(nage, nsex, nhiv)

##========HIV|ART MORTALITY ENDS======================


##=======HIV==================================
HincM[,]   <- user()
HincF[,]   <- user()
dim(HincM)      <- c(length_popart_year,nage)
dim(HincF)      <- c(length_popart_year,nage)
hiv_inc[, , 1]  <- HincM[i, j]
hiv_inc[, , 2]  <- HincF[i, j]
dim(hiv_inc)    <- c(length_popart_year, nage, nsex)

hivinc[, ]  <- interpolate(popart_year, hiv_inc, "linear")
dim(hivinc) <- c(nage, nsex)

hivincS[,,1] <- -hivinc[i,j]*S[i,j,1]
hivincS[,,2] <-  hivinc[i,j]*S[i,j,1]
hivincS[,,3] <-  0
hivincE[,,1] <- -hivinc[i,j]*E[i,j,1]
hivincE[,,2] <-  hivinc[i,j]*E[i,j,1]
hivincE[,,3] <-  0
hivincL[,,1] <- -hivinc[i,j]*L[i,j,1]
hivincL[,,2] <-  hivinc[i,j]*L[i,j,1]
hivincL[,,3] <-  0
hivincA[,,1] <- -hivinc[i,j]*A[i,j,1]
hivincA[,,2] <-  hivinc[i,j]*A[i,j,1]
hivincA[,,3] <-  0

hivincX[,,1] <- -hivinc[i,j]*X[i,j,1]
hivincX[,,2] <-  hivinc[i,j]*X[i,j,1]
hivincX[,,3] <-  0

hivincI[,,1] <- -hivinc[i,j]*I[i,j,1]
hivincI[,,2] <-  hivinc[i,j]*I[i,j,1]
hivincI[,,3] <-  0

hivincT[,,1] <- -hivinc[i,j]*T[i,j,1]
hivincT[,,2] <-  hivinc[i,j]*T[i,j,1]
hivincT[,,3] <-  0

hivincR[,,1] <- -hivinc[i,j]*I[i,j,1]
hivincR[,,2] <-  hivinc[i,j]*I[i,j,1]
hivincR[,,3] <-  0


##========HIV ENDS=====================

##========ART==========================
ARTincM[,] <- user()
ARTincF[,] <- user()
dim(ARTincM)     <- c(length_popart_year,nage)
dim(ARTincF)     <- c(length_popart_year,nage)
art_inc[, , 1]   <- ARTincM[i, j]
art_inc[, , 2]   <- ARTincF[i, j]
dim(art_inc)     <- c(length_popart_year, nage, nsex)
artinc[, ]  <- interpolate(popart_year, art_inc, "linear")
dim(artinc) <- c(nage, nsex)

artincS[,,1] <- 0
artincS[,,2] <- -artinc[i,j]*S[i,j,2]
artincS[,,3] <-  artinc[i,j]*S[i,j,2]
artincE[,,1] <- 0
artincE[,,2] <- -artinc[i,j]*E[i,j,2]
artincE[,,3] <-  artinc[i,j]*E[i,j,2]
artincL[,,1] <- 0
artincL[,,2] <- -artinc[i,j]*L[i,j,2]
artincL[,,3] <-  artinc[i,j]*L[i,j,2]

artincA[,,1] <- 0
artincA[,,2] <- -artinc[i,j]*A[i,j,2]
artincA[,,3] <-  artinc[i,j]*A[i,j,2]

artincX[,,1] <- 0
artincX[,,2] <- -artinc[i,j]*X[i,j,2]
artincX[,,3] <-  artinc[i,j]*X[i,j,2]

artincI[,,1] <- 0
artincI[,,2] <- -artinc[i,j]*I[i,j,2]
artincI[,,3] <-  artinc[i,j]*I[i,j,2]

artincT[,,1] <- 0
artincT[,,2] <- -artinc[i,j]*T[i,j,2]
artincT[,,3] <-  artinc[i,j]*T[i,j,2]

artincR[,,1] <- 0
artincR[,,2] <- -artinc[i,j]*R[i,j,2]
artincR[,,3] <-  artinc[i,j]*R[i,j,2]


##========ART ENDS================================

##========ART LTFU================================

ART_LTFM[,]   <- user()
ART_LTFF[,]   <- user()
dim(ART_LTFM)     <- c(length_popart_year,nage)
dim(ART_LTFF)     <- c(length_popart_year,nage)
art_default[, , 1]  <- ART_LTFM[i, j]
art_default[, , 2]  <- ART_LTFF[i, j]
dim(art_default)    <- c(length_popart_year, nage, nsex)
art_lost[,]      <- interpolate(popart_year, art_default,'linear')
dim(art_lost)    <- c(nage, nsex)

artlostS[,,1] <- 0
artlostS[,,2] <-  art_lost[i,j]*S[i,j,3]
artlostS[,,3] <- -art_lost[i,j]*S[i,j,3]
artlostE[,,1] <- 0
artlostE[,,2] <-  art_lost[i,j]*E[i,j,3]
artlostE[,,3] <- -art_lost[i,j]*E[i,j,3]
artlostL[,,1] <- 0
artlostL[,,2] <-  art_lost[i,j]*L[i,j,3]
artlostL[,,3] <- -art_lost[i,j]*L[i,j,3]

artlostA[,,1] <- 0
artlostA[,,2] <-  art_lost[i,j]*A[i,j,3]
artlostA[,,3] <- -art_lost[i,j]*A[i,j,3]
artlostX[,,1] <- 0
artlostX[,,2] <-  art_lost[i,j]*X[i,j,3]
artlostX[,,3] <- -art_lost[i,j]*X[i,j,3]

artlostI[,,1] <- 0
artlostI[,,2] <-  art_lost[i,j]*I[i,j,3]
artlostI[,,3] <- -art_lost[i,j]*I[i,j,3]

artlostT[,,1] <- 0
artlostT[,,2] <-  art_lost[i,j]*T[i,j,3]
artlostT[,,3] <- -art_lost[i,j]*T[i,j,3]

artlostR[,,1] <- 0
artlostR[,,2] <-  art_lost[i,j]*R[i,j,3]
artlostR[,,3] <- -art_lost[i,j]*R[i,j,3]

##=======ART LTFU ENDS===========================
##===========IRR=================================
IRR_hivf[,] <- user()
IRR_hivm[,] <- user()
IRR_artf[,] <- user()
IRR_artm[,] <- user()
dim(IRR_hivf) <-c(length_popart_year, nage)
dim(IRR_hivm) <-c(length_popart_year, nage)
dim(IRR_artf) <-c(length_popart_year, nage)
dim(IRR_artm) <-c(length_popart_year, nage)

irr_hiv[, , 1]  <- IRR_hivm[i, j]
irr_hiv[, , 2]  <- IRR_hivf[i, j]
irr_art[, , 1]  <- IRR_artm[i, j]
irr_art[, , 2]  <- IRR_artf[i, j]

dim(irr_hiv)    <- c(length_popart_year, nage, nsex)
dim(irr_art)    <- c(length_popart_year, nage, nsex)
irrHiv[,]       <- interpolate(popart_year, irr_hiv,'linear')
irrArt[,]       <- interpolate(popart_year, irr_art,'linear')

dim(irrHiv)     <- c(nage, nsex)
dim(irrArt)     <- c(nage, nsex)

irr[, ,1]   <-1
irr[, ,2]   <-irrHiv[i,j]
irr[, ,3]   <-irrArt[i,j]

##===========IRR ENDS===========================


## Initial values -----
popinitM[] <- user()
popinitF[] <- user()


ari0 <- user(0.01)  

ages[] <- i*5-2.5
L0[,] <- 1-exp(-ari0*ages[i]) #guess mean age
E0[,] <- L0[i,j]*ari0/(2+ari0)
d0 <- 2*ari0/beta
A0[,] <- d0/(3)
X0[,] <- d0/(3)
I0[,] <- d0/(3)
T0[,] <- I0[i,j]/(3)
R0[,] <- L0[i,j]/10
S0[,] <- 1-L0[i,j]
N0[,] <- S0[i,j]+E0[i,j]+L0[i,j]+A0[i,j]+X0[i,j]+I0[i,j]+
    T0[i,j]+R0[i,j]

dim(S0) <- c(nage,nsex)
dim(E0) <- c(nage,nsex)
dim(L0) <- c(nage,nsex)
dim(A0) <- c(nage,nsex)
dim(X0) <- c(nage,nsex)
dim(I0) <- c(nage,nsex)
dim(T0) <- c(nage,nsex)
dim(R0) <- c(nage,nsex)
dim(N0) <- c(nage,nsex)
dim(ages) <- c(nage)


initial(S[,1,1])   <-popinitM[i]*S0[i,1]/N0[i,1]
initial(S[,2,1])   <-popinitF[i]*S0[i,2]/N0[i,2]
initial(E[,1,1])   <-popinitM[i]*E0[i,1]/N0[i,1]
initial(E[,2,1])   <-popinitF[i]*E0[i,2]/N0[i,2]
initial(L[,1,1])   <-popinitM[i]*L0[i,1]/N0[i,1]
initial(L[,2,1])   <-popinitF[i]*L0[i,2]/N0[i,2]
initial(A[,1,1])   <-popinitM[i]*A0[i,1]/N0[i,1]
initial(A[,2,1])   <-popinitF[i]*A0[i,2]/N0[i,2]
initial(X[,1,1])   <-popinitM[i]*X0[i,1]/N0[i,1]
initial(X[,2,1])   <-popinitF[i]*X0[i,2]/N0[i,2]
initial(I[,1,1])   <-popinitM[i]*I0[i,1]/N0[i,1]
initial(I[,2,1])   <-popinitF[i]*I0[i,2]/N0[i,2]
initial(T[,1,1])   <-popinitM[i]*T0[i,1]/N0[i,1]
initial(T[,2,1])   <-popinitF[i]*T0[i,2]/N0[i,2]
initial(R[,1,1])   <-popinitM[i]*R0[i,1]/N0[i,1]
initial(R[,2,1])   <-popinitF[i]*R0[i,2]/N0[i,2]

##===========================
dim(popinitM) <- nage
dim(popinitF) <- nage

##====Aging==========
aging[1:(nage-1),,] <- 0.2
aging[nage,,]       <- 0


AgingS[1,,]      <--aging[1,j,k]* S[1,j,k]
AgingE[1,,]      <--aging[1,j,k]* E[1,j,k]
AgingL[1,,]      <--aging[1,j,k]* L[1,j,k]
AgingA[1,,]      <--aging[1,j,k]* A[1,j,k]
AgingX[1,,]      <--aging[1,j,k]* X[1,j,k]
AgingI[1,,]      <--aging[1,j,k]* I[1,j,k]
AgingT[1,,]      <--aging[1,j,k]* T[1,j,k]
AgingR[1,,]      <--aging[1,j,k]* R[1,j,k]
AgingS[2:nage,,] <- aging[i-1,j,k]* S[i-1,j,k]-aging[i,j,k] * S[i,j,k]
AgingE[2:nage,,] <- aging[i-1,j,k]* E[i-1,j,k]-aging[i,j,k] * E[i,j,k]
AgingL[2:nage,,] <- aging[i-1,j,k]* L[i-1,j,k]-aging[i,j,k] * L[i,j,k]
AgingA[2:nage,,] <- aging[i-1,j,k]* A[i-1,j,k]-aging[i,j,k] * A[i,j,k]
AgingX[2:nage,,] <- aging[i-1,j,k]* X[i-1,j,k]-aging[i,j,k] * X[i,j,k]
AgingI[2:nage,,] <- aging[i-1,j,k]* I[i-1,j,k]-aging[i,j,k] * I[i,j,k]
AgingT[2:nage,,] <- aging[i-1,j,k]* T[i-1,j,k]-aging[i,j,k] * T[i,j,k]
AgingR[2:nage,,] <- aging[i-1,j,k]* R[i-1,j,k]-aging[i,j,k] * R[i,j,k]


##======TB params======================
##======TB params======================
beta  <- user(10)
stab  <- user(1.97)
fast  <- user(0.1)
react <- user(0.005) 
smearProg <-user(0.1) #prop smear progresion
propSp    <-user(0.45) #prop smear pos
durA      <-user(0.5)  # duration asymptomatic
durT      <- user(0.5)    #treatment_duration

recovSpHn <- user(0.231) # romain
recovSnHn <- user(0.13)  # romain
mortiSpHn <- user(0.389) # romain
mortiSnHn <- user(0.025) # romain
relaps    <- user(0.016)  
##morttr    <- user(0.004) 
psin      <- user(0.79)

propCured  <- user(0.90) #Proportion
propDefault<- user(0.04)
propMorttr <- 1-(propCured+propDefault)  #Three competing events on Treated compartment
##===========================================
median_time_tb_death    <- user(0.16) # 2 months
median_time_tb_default  <- user(0.16) # 2 months
##===========================================
rsmearProg   <- -log(1-smearProg)
## default_rate <- -log(1-propDefault)
## morttr       <- -log(1-propMorttr)
morttr        <- propMorttr*1/median_time_tb_death
default_rate  <- propDefault*1/median_time_tb_default

irrARTp <- user(1.0) #interpolator between HIV+ (p=1) and HIV- (p=0) "hiv-like"
irrUntreatedTbMort <- user(10)    # RR of untreated TB
irrTreatedTbMort   <- user(1.5)  # RR of of treated TB
rrRelapse          <- user(1.5)
rrDefault          <- user(0.6)
##=============TB params start==============

## durations asymp - now HIV dept
durav[1] <- durA
durav[2] <- durA/irrUntreatedTbMort
durav[3] <- durA/(irrUntreatedTbMort*irrARTp + (1-irrARTp))
dim(durav) <-c(nhiv)

## TODO consider different proportions smr+/- by HIV also

recovi[, ,1]  <-recovSpHn
recovi[, ,2]  <-0
recovi[, ,3]  <-recovSpHn*(1-irrARTp)

recovx[, ,1]  <-recovSnHn
recovx[, ,2]  <-0
recovx[, ,3]  <-recovSnHn*(1-irrARTp)

##====
morti[, ,1]   <-mortiSpHn
morti[, ,2]   <-mortiSpHn*irrUntreatedTbMort
morti[, ,3]   <-mortiSpHn*(irrUntreatedTbMort*irrARTp + (1-irrARTp))

mortx[, ,1]   <-mortiSnHn
mortx[, ,2]   <-mortiSnHn*irrUntreatedTbMort
mortx[, ,3]   <-mortiSnHn*(irrUntreatedTbMort*irrARTp + (1-irrARTp))


## TB mortality among TB treated and recovered folks
mortt[, ,1]   <-morttr
mortt[, ,2]   <-morttr*irrTreatedTbMort
mortt[, ,3]   <-morttr  

mortr[, ,1]   <-morttr
mortr[, ,2]   <-morttr*irrTreatedTbMort
mortr[, ,3]   <-morttr  


relapse[, ,1]   <-relaps
relapse[, ,2]   <-relaps*rrRelapse
relapse[, ,3]   <-relaps*rrRelapse*0.5   #TODO 0.5 guess

                                        # TB treatment default
lostTB[,,1] <- default_rate
lostTB[,,2] <- default_rate
lostTB[,,3] <- default_rate*rrDefault

##=============TB params end==================



CDRi  <-user(0.65)
CDRx  <-user(0.65)

## ACFi <- user(0)
## ACFx <- user(0)
ACFa <- user(0) #comment!

psi[,,1] <- 1-psin
psi[,,2] <- 0
psi[,,3] <- (1-psin)/2

##=======introduce ACF intervention===============
rrcdri <-user(1) #HR of ACF on smear+ TB
rrcdrx <-user(1) #HR of ACF on smear- TB
eyear  <-user(2030)

rrcdri1  <- if(t > eyear ) 1.0 else rrcdri
hr_cdri  <- if(t > 2014 ) rrcdri1 else 1.0


rrcdrx1  <- if(t > eyear ) 1.0 else rrcdrx
hr_cdrx  <- if(t > 2014 ) rrcdrx1 else 1.0


## hr_cdri  <- if(t > 2014 ) rrcdri else 1.0
## hr_cdrx  <- if(t > 2014 ) rrcdrx else 1.0

cdri[,,] <- hr_cdri*CDRi *(morti[i,j,k] +recovi[i,j,k])/(1-CDRi)
cdrx[,,] <- hr_cdrx*CDRx *(mortx[i,j,k] +recovx[i,j,k]+rsmearProg)/(1-CDRx)


##transmission
relinfA <- user(0.25)
relinfX <- user(0.2)


## age groupings for contacts: u5, 5-15, 15-20,20-30,30-45,45+
## indices                      1,  2:3,  4   , 5:6 ,7:10 ,11:17
wprev[1] <- (sum(I[1,,]) + relinfA*sum(A[1,,])+relinfX*sum(X[1,,])) / sum(N[1,,])
wprev[2] <- (sum(I[2:3,,]) + relinfA*sum(A[2:3,,])+relinfX*sum(X[2:3,,])) / sum(N[2:3,,])
wprev[3] <- (sum(I[4,,]) + relinfA*sum(A[4,,])+relinfX*sum(X[4,,])) / sum(N[4,,])
wprev[4] <- (sum(I[5:6,,]) + relinfA*sum(A[5:6,,])+relinfX*sum(X[5:6,,])) / sum(N[5:6,,])
wprev[5] <- (sum(I[7:10,,]) + relinfA*sum(A[7:10,,])+relinfX*sum(X[7:10,,])) / sum(N[7:10,,])
wprev[6] <- (sum(I[11:17,,]) + relinfA*sum(A[11:17,,])+relinfX*sum(X[11:17,,])) / sum(N[11:17,,])

## 5 & 6 are numbers for IC survey
## make correspondences
ariv[1] <- beta * (CM[1,1]*wprev[1] + CM[1,2]*wprev[2] + CM[1,3]*wprev[3]+
                   CM[1,4]*wprev[4] + CM[1,5]*wprev[5] + CM[1,6]*wprev[6]) #u5
ariv[2:3]  <- beta * (CM[2,1]*wprev[1] + CM[2,2]*wprev[2] + CM[2,3]*wprev[3]+
                      CM[2,4]*wprev[4] + CM[2,5]*wprev[5] + CM[2,6]*wprev[6])  #5-15
ariv[4]  <- beta * (CM[3,1]*wprev[1] + CM[3,2]*wprev[2] + CM[3,3]*wprev[3]+
                    CM[3,4]*wprev[4] + CM[3,5]*wprev[5] + CM[3,6]*wprev[6])    #15-20
ariv[5:6]  <- beta * (CM[4,1]*wprev[1] + CM[4,2]*wprev[2] + CM[4,3]*wprev[3]+
                      CM[4,4]*wprev[4] + CM[4,5]*wprev[5] + CM[4,6]*wprev[6])  #20-30
ariv[7:10]  <- beta * (CM[5,1]*wprev[1] + CM[5,2]*wprev[2] + CM[5,3]*wprev[3]+
                       CM[5,4]*wprev[4] + CM[5,5]*wprev[5] + CM[5,6]*wprev[6]) #30-45
ariv[11:17]  <- beta * (CM[6,1]*wprev[1] + CM[6,2]*wprev[2] + CM[6,3]*wprev[3]+
                        CM[6,4]*wprev[4] + CM[6,5]*wprev[5] + CM[6,6]*wprev[6])  #45+

## for IC outputs - keep to maintain code
ari <- ariv[5]
output(ari) <- TRUE
ariRR <- ariv[5]/(1e-10+ariv[2])
output(ariRR) <- TRUE

## incidences
incfast[,,]  <- fast*E[i,j,k]  * irr[i,j,k]
incslow[,,]  <- react*L[i,j,k] * irr[i,j,k]
increlapse[,,] <- relapse[i,j,k] * R[i,j,k]



deriv(S[,,]) <- birth[i,j,k] + mgr*S[i,j,k] - mortall[i,j,k]*S[i,j,k]+ hivincS[i,j,k] + artincS[i,j,k]+ artlostS[i,j,k]+
    AgingS[i,j,k]   + mortx[i,j,k]*X[i,j,k]+ morti[i,j,k]*I[i,j,k] + mortt[i,j,k]*T[i,j,k]+ mortr[i,j,k]*R[i,j,k]+ (1-propCured)*T[i,j,k]/durT- ariv[i]*S[i,j,k] 

deriv(E[,,]) <- mgr*E[i,j,k] - mortall[i,j,k]*E[i,j,k]+ hivincE[i,j,k] + artincE[i,j,k]+ artlostE[i,j,k]+
    AgingE[i,j,k]+ ariv[i]*S[i,j,k] + ariv[i]*psi[i,j,k]*L[i,j,k]- stab *E[i,j,k]-incfast[i,j,k] 

deriv(L[,,]) <- mgr*L[i,j,k] - mortall[i,j,k]*L[i,j,k]+ hivincL[i,j,k] + artincL[i,j,k]+ artlostL[i,j,k]+
    AgingL[i,j,k]+ stab*E[i,j,k]+ recovi[i,j,k]*I[i,j,k]+ recovx[i,j,k]*X[i,j,k]-incslow[i,j,k]- ariv[i]*psi[i,j,k]*L[i,j,k] 

deriv(A[,,]) <- mgr*A[i,j,k]- mortall[i,j,k]*A[i,j,k]+  hivincA[i,j,k] + artincA[i,j,k]+ artlostA[i,j,k]+
    AgingA[i,j,k] +incfast[i,j,k]+incslow[i,j,k]+ relapse[i,j,k]*R[i,j,k] -A[i,j,k]/durav[k] + lostTB[i,j,k]*T[i,j,k]

deriv(X[,,]) <- mgr*X[i,j,k] - mortall[i,j,k]*X[i,j,k]+ hivincX[i,j,k] + artincX[i,j,k]+ artlostX[i,j,k]+
  AgingX[i,j,k]+((1-propSp)*(1-ACFa)*A[i,j,k]/durav[k]) -rsmearProg*X[i,j,k]-(mortx[i,j,k]+recovx[i,j,k]+cdrx[i,j,k])*X[i,j,k]#mu[i]*X[i]

deriv(I[,,]) <- mgr*I[i,j,k]- mortall[i,j,k]*I[i,j,k]+ hivincI[i,j,k] + artincI[i,j,k]+ artlostI[i,j,k]+
  AgingI[i,j,k]-(morti[i,j,k]+recovi[i,j,k]+cdri[i,j,k])*I[i,j,k]+rsmearProg*X[i,j,k] + (propSp*(1-ACFa)*A[i,j,k]/durav[k])#-mu[i]*I[i]

deriv(T[,,]) <-  mgr*T[i,j,k]+ - mortall[i,j,k]*T[i,j,k]+ hivincT[i,j,k] + artincT[i,j,k]+ artlostT[i,j,k]+
    AgingT[i,j,k]+cdri[i,j,k]*I[i,j,k] + cdrx[i,j,k]*X[i,j,k] + ACFa*A[i,j,k]/durav[k] -T[i,j,k]/durT-mortt[i,j,k]*T[i,j,k]-lostTB[i,j,k]*T[i,j,k] #-mu[i]*T[i]

deriv(R[,,]) <-  mgr*R[i,j,k] - mortall[i,j,k]*R[i,j,k]+ hivincR[i,j,k] + artincR[i,j,k]+artlostR[i,j,k]+
    AgingR[i,j,k]+ propCured*(T[i,j,k]/durT)- relapse[i,j,k]*R[i,j,k]-mortr[i,j,k]*R[i,j,k] 



## Epidemiology
N[,,]         <- (S[i,j,k]+E[i,j,k]+L[i,j,k]+ A[i,j,k]+X[i,j,k]+I[i,j,k] +T[i,j,k]+ R[i,j,k]) 

NN            <- sum(S)+ sum(E)+sum(L) +sum(A)+sum(X)+ sum(I)+sum(T) + sum(R)
Nad           <- sum(S[4:17,,])+ sum(E[4:17,,])+sum(L[4:17,,]) + sum(A[4:17,,])+
    sum(X[4:17,,])+ sum(I[4:17,,])+sum(T[4:17,,]) + sum(R[4:17,,])
tb_notif[,,]  <- cdri[i,j,k]*I[i,j,k] + cdrx[i,j,k]*X[i,j,k] + (ACFa/durav[k])*A[i,j,k]
incTB[,,]     <- incfast[i,j,k] + incslow[i,j,k]+ increlapse[i,j,k]
inc           <- (sum(incfast[4:17,,]) + sum(incslow[4:17,,])+ sum(increlapse[4:17,,]))/Nad  
prev          <- (sum(I[4:17,,]) +sum(X[4:17,,]) + sum(A[4:17,,]))/Nad #overall prevalence
propasymp[1] <- (sum(A[4:17,,1]))/(sum(I[4:17,,1]) +sum(X[4:17,,1]) + sum(A[4:17,,1]))
propasymp[2] <- (sum(A[4:17,,2]))/(sum(I[4:17,,2]) +sum(X[4:17,,2]) + sum(A[4:17,,2]))
propasymp[3] <- (sum(A[4:17,,3]))/(sum(I[4:17,,3]) +sum(X[4:17,,3]) + sum(A[4:17,,3]))
dim(propasymp) <- nhiv
output(propasymp) <- TRUE
notif         <- sum(tb_notif[4:17,,])/Nad
prevatt       <- (sum(T[4:17,,]))/Nad #overall prevalence of being on ATT
incatt       <- sum(tb_notif[4:17,,])/Nad #overall incidence of ATT
prevtxhx       <- (sum(R[4:17,,]))/Nad #overall prevalence of previous ATT
output(incatt) <- TRUE
## TB/HIV prevalences and incidences
prevtbh <- (sum(I[4:17,,2:3]) +sum(X[4:17,,2:3]) + sum(A[4:17,,2:3]))/Nad
inctbh <- (sum(incfast[4:17,,2:3]) + sum(incslow[4:17,,2:3])+ sum(increlapse[4:17,,2:3]))/Nad
output(prevtbh) <- TRUE
output(inctbh) <- TRUE
prevtbona <- (sum(I[4:17,,3])+sum(X[4:17,,3]) + sum(A[4:17,,3]))/Nad
inctbona <- (sum(incfast[4:17,,3]) + sum(incslow[4:17,,3])+ sum(increlapse[4:17,,3]))/Nad
output(prevtbona) <- TRUE
output(inctbona) <- TRUE
prevtbnoa <- (sum(I[4:17,,2])+sum(X[4:17,,2]) + sum(A[4:17,,2]))/Nad
inctbnoa <- (sum(incfast[4:17,,2]) + sum(incslow[4:17,,2])+ sum(increlapse[4:17,,2]))/Nad
output(prevtbnoa) <- TRUE
output(inctbnoa) <- TRUE


## HIV
hivprevalence <- (sum(S[4:17,,2:3])+sum(E[4:17,,2:3])+sum(L[4:17,,2:3])+
                  sum(A[4:17,,2:3])+sum(X[4:17,,2:3])+sum(I[4:17,,2:3])+
                  sum(T[4:17,,2:3])+sum(R[4:17,,2:3]))/Nad
artprevalence <- (sum(S[4:17,,3])+sum(E[4:17,,3])+sum(L[4:17,,3])+
                  sum(A[4:17,,3])+sum(X[4:17,,3])+sum(I[4:17,,3])+
                  sum(T[4:17,,3])+sum(R[4:17,,3]))/Nad
output(hivprevalence) <- TRUE
output(artprevalence) <- TRUE

output(IRR_HIV)<- (sum(incTB[4:17,,2])/ sum(N[4:17, ,2]))/(sum(incTB[4:17,,1])/ sum(N[4:17, ,1]))
output(IRR_ART)<- (sum(incTB[4:17,,3])/ sum(N[4:17, ,3]))/(sum(incTB[4:17,,1])/ sum(N[4:17, ,1]))

## LTBI in 15-19
output(ltbi.ym) <- (sum(E[5,1,])+sum(L[5,1,]) + sum(A[5,1,])+
                    sum(X[5,1,])+ sum(I[5,1,])+sum(T[5,1,]) + sum(R[5,1,])) /
    (sum(S[5,1,])+ sum(E[5,1,])+sum(L[5,1,]) + sum(A[5,1,])+
     sum(X[5,1,])+ sum(I[5,1,])+sum(T[5,1,]) + sum(R[5,1,])) # males
output(ltbi.yf) <- (sum(E[5,2,])+sum(L[5,2,]) + sum(A[5,2,])+
                    sum(X[5,2,])+ sum(I[5,2,])+sum(T[5,2,]) + sum(R[5,2,])) /
    (sum(S[5,2,])+ sum(E[5,2,])+sum(L[5,2,]) + sum(A[5,2,])+
     sum(X[5,2,])+ sum(I[5,2,])+sum(T[5,2,]) + sum(R[5,2,])) # females
## LTBI in 20-24
output(ltbi.om) <- (sum(E[6,1,])+sum(L[6,1,]) + sum(A[6,1,])+
    sum(X[6,1,])+ sum(I[6,1,])+sum(T[6,1,]) + sum(R[6,1,])) /
       (sum(S[6,1,])+ sum(E[6,1,])+sum(L[6,1,]) + sum(A[6,1,])+
        sum(X[6,1,])+ sum(I[6,1,])+sum(T[6,1,]) + sum(R[6,1,])) # males
output(ltbi.of) <- (sum(E[6,2,])+sum(L[6,2,]) + sum(A[6,2,])+
                    sum(X[6,2,])+ sum(I[6,2,])+sum(T[6,2,]) + sum(R[6,2,])) /
    (sum(S[6,2,])+ sum(E[6,2,])+sum(L[6,2,]) + sum(A[6,2,])+
     sum(X[6,2,])+ sum(I[6,2,])+sum(T[6,2,]) + sum(R[6,2,])) # females


##==========Health economics============
TBDeath[,,] <- mortx[i,j,k]*X[i,j,k]+ morti[i,j,k]*I[i,j,k] + mortt[i,j,k]*T[i,j,k] #add TBdeath onRx and beforeRx
output(TBDeath) <- TRUE
output(TotTBdeaths) <- sum(TBDeath[,,])/NN #NOTE actually per capita



PY_to_TB_Mort_LTFU[,,]  <- median_time_tb_death*mortt[i,j,k]*T[i,j,k] + 
    median_time_tb_default*lostTB[i,j,k]*T[i,j,k]+
    durT*propCured*T[i,j,k]

dim(PY_to_TB_Mort_LTFU) <-c(nage, nsex, nhiv)
output(PYonTBTreatment)<- sum(PY_to_TB_Mort_LTFU[4:17,,])


dim(TBDeath) <- c(nage, nsex, nhiv)
##dim(mortTreated)   <- c(nage, nsex, nhiv)
##dim(mort_recovered) <- c(nage, nsex, nhiv)


output(NumDiagnosedTB)     <- notif*Nad
output(NumTreatedTB)       <- sum(T)
output(NumTreatCompleted)  <- sum(R)

output(NN)    <- TRUE
output(prev)  <- TRUE
output(prevatt)  <- TRUE
output(prevtxhx)  <- TRUE
output(inc)   <- TRUE
output(notif) <- TRUE

##=====================================
dim(N)        <- c(nage,nsex, nhiv)
dim(tb_notif) <- c(nage,nsex, nhiv)
dim(incTB)    <- c(nage,nsex, nhiv)

##===========DIMENSIONS=========================

dim(S) <- c(nage,nsex,nhiv)
dim(E) <- c(nage,nsex,nhiv)
dim(L) <- c(nage,nsex,nhiv)
dim(A) <- c(nage,nsex,nhiv)
dim(X) <- c(nage,nsex,nhiv)
dim(I) <- c(nage,nsex,nhiv)
dim(T) <- c(nage,nsex,nhiv)
dim(R) <- c(nage,nsex,nhiv)

dim(incfast) <- c(nage,nsex,nhiv)
dim(incslow) <- c(nage,nsex,nhiv)
dim(increlapse) <- c(nage,nsex,nhiv)

dim(cdri)=  c(nage, nsex,nhiv)  
dim(cdrx)=  c(nage, nsex,nhiv)  
dim(psi) =  c(nage, nsex,nhiv)  

dim(recovi) <- c(nage, nsex, nhiv)
dim(recovx) <- c(nage, nsex, nhiv)
dim(morti)  <- c(nage, nsex, nhiv)
dim(mortx)  <- c(nage, nsex, nhiv)
dim(mortt)  <- c(nage, nsex, nhiv)
dim(mortr)  <- c(nage, nsex, nhiv)
dim(relapse)<- c(nage, nsex, nhiv)
dim(lostTB)<- c(nage, nsex, nhiv)

dim(aging)        <- c(nage,nsex,nhiv)

dim(AgingS)       <- c(nage,nsex,nhiv)
dim(AgingE)       <- c(nage,nsex,nhiv)
dim(AgingL)       <- c(nage,nsex,nhiv)
dim(AgingA)       <- c(nage,nsex,nhiv)
dim(AgingX)       <- c(nage,nsex,nhiv)
dim(AgingI)       <- c(nage,nsex,nhiv)
dim(AgingT)       <- c(nage,nsex,nhiv)
dim(AgingR)       <- c(nage,nsex,nhiv)

dim(hivincS)      <- c(nage, nsex, nhiv)
dim(hivincE)      <- c(nage, nsex, nhiv)
dim(hivincL)      <- c(nage, nsex, nhiv)
dim(hivincA)      <- c(nage, nsex, nhiv)
dim(hivincX)      <- c(nage, nsex, nhiv)
dim(hivincI)      <- c(nage, nsex, nhiv)
dim(hivincT)      <- c(nage, nsex, nhiv)
dim(hivincR)      <- c(nage, nsex, nhiv)
dim(artincS)      <- c(nage, nsex, nhiv)
dim(artincE)      <- c(nage, nsex, nhiv)
dim(artincL)      <- c(nage, nsex, nhiv)
dim(artincA)      <- c(nage, nsex, nhiv)
dim(artincX)      <- c(nage, nsex, nhiv)
dim(artincI)      <- c(nage, nsex, nhiv)
dim(artincT)      <- c(nage, nsex, nhiv)
dim(artincR)      <- c(nage, nsex, nhiv)
dim(artlostS)     <- c(nage, nsex, nhiv)
dim(artlostE)     <- c(nage, nsex, nhiv)
dim(artlostL)     <- c(nage, nsex, nhiv)
dim(artlostA)     <- c(nage, nsex, nhiv)
dim(artlostX)     <- c(nage, nsex, nhiv)
dim(artlostI)     <- c(nage, nsex, nhiv)
dim(artlostT)     <- c(nage, nsex, nhiv)
dim(artlostR)     <- c(nage, nsex, nhiv)
dim(irr)          <- c(nage, nsex, nhiv)
dim(ariv) <- c(nage)
dim(wprev) <- c(6)
