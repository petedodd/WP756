## priors for single community


priors <- list(
  psi_a = 77.9,
  psi_b= 20.7,
  recovSp_m =-1.473,
  recovSp_s = 0.1221,
  recovSn_m =-2.075,
  recovSn_s = 0.2623,
  mutbSp_m =-0.947,
  mutbSp_s = 0.07466,
  mutbSn_m =-3.705,
  mutbSn_s = 0.1822,
  smProg_m= 9.36,
  smProg_s= 84.25,
  stab_m =0.6200967,
  stab_s =0.06877732, 
  fast_m = -2.368396,
  fast_s = 0.3221463,
  react_m = -6.886404,
  react_s = 0.5751487,
  relapse_m = -3.95, 
  relapse_s=0.27,
  cdrSp_m=5.6,
  cdrSp_s=3.5,
  cdrSn_m=5.6,
  cdrSn_s=3.5,
  beta_m=1.678, 
  beta_s=0.371,
  propSp_m=44.25881,
  propSp_s=54.30141,
  irrARTpa = 5,
  irrARTpb = 5,
  irrUntreated_mutb_m=1.522152,
  irrUntreated_mutb_s=0.4178189,
  irrTreated_mutb_m=1.227867,
  irrTreated_mutb_s=0.2231408,
  ari_scale = 2e-2,
  ari_shape = 2,
  acfhr_scale=1,
  acfhr_shape=2,
  acfhr_meanlog=0,
  acfhr_sdlog=0.5,
  HR.a=2,
  HR.b=2,
  alph.a=2,
  alph.b=2
)

priorquantiles <- list(
  beta = function(x) qlnorm(x,log(2)+priors$beta_m,priors$beta_s),  
  react = function(x) qlnorm(x,priors$react_m,priors$react_s),
  fast = function(x) qlnorm(x,priors$fast_m,priors$fast_s),
  stab = function(x) qlnorm(x,priors$stab_m,priors$stab_s),
  recovSpHn = function(x) qlnorm(x,priors$recovSp_m,priors$recovSp_s),
  recovSnHn = function(x) qlnorm(x,priors$recovSn_m,priors$recovSn_s),
  mortiSpHn   = function(x) qlnorm(x,priors$mutbSp_m,priors$mutbSp_s),
  mortiSnHn   = function(x) qlnorm(x,priors$mutbSn_m,priors$mutbSn_s),
  psin = function(x) qbeta(x,priors$psi_a,priors$psi_b),
  relaps = function(x) qlnorm(x,priors$relapse_m,priors$relapse_s),
  smearProg =function(x) qbeta(x,priors$smProg_m,priors$smProg_s),
  propSp =  function(x) qbeta(x,priors$propSp_m,priors$propSp_s),
  irrARTp = function(x) qbeta(x,priors$irrARTpa,priors$irrARTpb), #interpolator
  irrUntreatedTbMort= function(x) qlnorm(x,
                                         priors$irrUntreated_mutb_m,
                                         priors$irrUntreated_mutb_s),# RR of untreated TB
  irrTreatedTbMort  = function(x) qlnorm(x,
                                         priors$irrTreated_mutb_m,
                                         priors$irrTreated_mutb_s) ,  # RR of of treated TB
  ari0 = function(x) qgamma(x,scale=priors$ari_scale,
                            shape=priors$ari_shape),
  CDRx=function(x) qbeta(x,priors$cdrSn_m, priors$cdrSn_s),
  CDRi=function(x) qbeta(x,priors$cdrSp_m, priors$cdrSp_s),
  ## Note that priors are not specified for ACF, cure rate, durA, durT
  ## NOTE this was >1
  ## rrcdri=function(x) qgamma(x,
  ##                           scale=priors$acfhr_scale,
  ##                           shape=priors$acfhr_shape), ## + 1,
  rrcdri=function(x) qlnorm(x,
                            meanlog=priors$acfhr_meanlog,
                            sdlog=priors$acfhr_sdlog), ## + 1,

  ## HIV stuff
  HR = function(x) qbeta(x,priors$HR.a,priors$HR.b)*(max(IRRGrid[[1]]$HR)-min(IRRGrid[[1]]$HR)) + min(IRRGrid[[1]]$HR),
  alph = function(x) qbeta(x,priors$alph.a,priors$alph.b)*(max(IRRGrid[[1]]$alph)-min(IRRGrid[[1]]$alph)) + min(IRRGrid[[1]]$alph)
)

