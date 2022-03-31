## taken from May4
get_params <- function(comid, interv, P, alph, HR){

  t.min =1970
  t.max= 2030

  if (comid <=12){
    migration   = P[["migration"]][country=='ZA', 5:17]/1000
    mortNatural = P[["natural_mortality"]][country=='ZA'] #NOTE differing converntions here
    contact.matrix = contactmatrix[['ZM']]
  } else {
    migration   = P[["migration"]][country=='SA', 5:17]/1000
    mortNatural = P[["natural_mortality"]][country=='SA']
    contact.matrix = contactmatrix[['ZA']]
  }

  FBirth <- as.numeric(unlist(P[["numb_birth"]][ComID==comid &
                                                scenario==interv &
                                                Sex=='F', 2]))
  MBirth <- as.numeric(unlist(P[["numb_birth"]][ComID==comid &
                                                scenario==interv &
                                                Sex=='M', 2]))
  popInit <-P[["pop_init"]][ComID==comid & scenario==interv]
  hivInc <- P[['hiv_inc']][ComID==comid & scenario==interv & Year <=t.max]
  artInc <- P[['art_inc']][ComID==comid & scenario==interv & Year <=t.max]
  hivMort<- P[['hiv_mort']][ComID==comid & scenario==interv & Year<=t.max]
  artMort<- P[['art_mort']][ComID==comid & scenario==interv & Year<=t.max]
  artLtfu<- P[['art_deflt']][ComID==comid & scenario==interv &Year<=t.max]

  unpd_year   = seq(t.min, t.max, by= 5)
  popart_year = seq(t.min, t.max, by= 1)
  nage = 17
  HincM        <-HincF    <- ARTincM   <- ARTincF  <-matrix(0, nrow = length(popart_year), ncol = nage)
  HIV_mortM    <-HIV_mortF<- ART_mortM <-ART_mortF<-matrix(0, nrow = length(popart_year), ncol = nage)
  ART_LTFM     <-ART_LTFF <-matrix(0, nrow = length(popart_year), ncol = nage)
  IRR_hivf     <-IRR_hivm <-IRR_artf <-IRR_artm<-matrix(0, nrow = length(popart_year), ncol = nage)

  HincM[, 3:nage]     <- as.matrix(hivInc[Sex=='M',5:19])
  HincF[, 3:nage]     <- as.matrix(hivInc[Sex=='F',5:19])
  ARTincM[, 3:nage]   <- as.matrix(artInc[Sex=='M',5:19])
  ARTincF[, 3:nage]   <- as.matrix(artInc[Sex=='F',5:19])
  HIV_mortM[, 3:nage] <- as.matrix(hivMort[Sex=='M',5:19])
  HIV_mortF[, 3:nage] <- as.matrix(hivMort[Sex=='F',5:19])
  ART_mortM[, 3:nage] <- as.matrix(artMort[Sex=='M',5:19])
  ART_mortF[, 3:nage] <- as.matrix(artMort[Sex=='F',5:19])
  ART_LTFM[, 3:nage]  <- as.matrix(artLtfu[Sex=='M',5:19])
  ART_LTFF[, 3:nage]  <- as.matrix(artLtfu[Sex=='F',5:19])

  migration <- as.numeric(migration)
  popinitM   = as.numeric(popInit[Sex=='M', 4:20])
  popinitF   = as.numeric(popInit[Sex=='F', 4:20])
  mortality_ratM <- as.matrix(mortNatural[sex=='M', 1:17])
  mortality_ratF <- as.matrix(mortNatural[sex=='F', 1:17])

  ## named as per Odin model inputs
  return(list(popinitM=popinitM,
              popinitF=popinitF, MB=MBirth, FB=FBirth, 
              mortality_ratM=mortality_ratM ,
              mortality_ratF=mortality_ratF, 
              popart_year=popart_year,
              unpd_year=unpd_year, HincM = HincM, HincF = HincF, 
              ARTincM = ARTincM, ARTincF = ARTincF,
              migration = migration,
              CM=contact.matrix,
              HIV_mortM= HIV_mortM, HIV_mortF= HIV_mortF,
              ART_mortM=ART_mortM, 
              ART_mortF=ART_mortF, ART_LTFM=ART_LTFM,
              ART_LTFF=ART_LTFF,
              IRR_hivf=IRR_hivf, IRR_hivm=IRR_hivm,
              IRR_artf=IRR_artf,IRR_artm=IRR_artm,
              ##TB_params
              ##ACF <- data.table(year=seq(1950, 2030, by = 1), acf_sp=0, acf_sn=0,acf_as=0)
              rrcdri= 1,rrcdrx= 1, ACFa =0,  # ACF data
              CDRi= 0.65, CDRx=0.65,     # CDR data
              smearProg=0.1,
              propSp=0.45,
              durA= 0.5,durT=0.5,    
              propCured=0.90,
              recovSpHn=0.231, # romain
              recovSnHn=0.13,  # romain
              mortiSpHn=0.389, # romain
              mortiSnHn= 0.025, # romain
              relaps   =0.016,   
              ##morttr =0.004,
              beta  =16,
              stab  =1.97,
              fast  =0.1,
              react =0.004,
              psin =0.79,
              propDefault= 0.04,
              irrUntreatedTbMort=10,
              irrTreatedTbMort=3,
              rrRelapse=1,
              rrDefault=0.6,
              ari0 =0.01,
              eyear=2030
              ))
}
