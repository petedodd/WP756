## Debebe's function; slightly modified
## for HE outputs
load(here('treats_odin/Parameters/ACF_output_count.Rdata')) # gives proportions in ACF
load(here('treats_odin/Parameters/CHiPs_visit.Rdata'))      # gives total pop and CHiPs vist by com


cost_effectiveness_output <- function (comid, interv, params) {

    acf_hr <- params$rrcdri

    mod  <- TREATS(user=params)
    out  <- data.frame(mod$run(t= seq(1970, 2030, by =1/12))) ##TODO change to reflec eyear if necessary
    

    Out <-data.table(out)
    names(Out)[1] <-'Time'
    
    colm_out <- grep("Time|S|E|L|A|I|X|T|R|Death", colnames(Out))
    outall<-  Out[, ..colm_out]
    outL <-setDT(reshape2::melt(outall, id.vars= c('Time')))
    outL[, c("State", "Age", 'Sex', 'HIV'):= tstrsplit(variable, ".", fixed=TRUE)]
    outL[, variable:=NULL]
    outL[, Year:=floor(Time)]       ##total PYlost due to mortality
    
    ##========Elligible population=========
    ## time_step= 12
    ## outL <- data.table(outL)
    ## outL$Age <-as.numeric(as.character(outL$Age))
    ## total_pop <-outL[State=='S'|State=='E'|State=="L"|State=="A"|State=="X"|State=="I"|State=="T"|State=="R",]
    ## total_pop[Time>=2014 & Time <2015 & Age==4, value:=2*value/5]   ##TODO Round 1 elligibles are >=18years- check round length
    ## elligible_population<- total_pop[Age > 3, .(CHiPsEligiblePopulationInvited= sum(value)/time_step), by=.(Year)]
    
    ##==========Death, PY,etc========
    
    outL_TB_death <- outL[State=='TBDeath',]
    setnames(outL_TB_death, 'value', 'mort')
    
    outL_TB_death[,PYDied:= ceiling(Time)-Time] ##fraction of a year lost due to mortality
    outL_TB_death[PYDied==0, PYDied:=1]         ##if dies at the beginning say 2015, YLL in that year is 1
    outL_TB_death[, PYDead:='PYLostByTBdeath'] 
    outL_TB_death[State=='mortTreated', PYDead:='PYDtreatedTB'] 
    
    
    ## multiply PY lost in a year to get total PY lost = death*PYDied
    outL_TB_death[, PYDied:=mort*PYDied]     ##total PYlost due to mortality
    outL_TB_death$Age <- as.numeric(outL_TB_death$Age)
    ##outL <-outL[, .(mort=sum(mort), PYDied=sum(PYDied)), by=.(Year, Mortality, Age, Sex, HIV, PYDead)]
    outL_TB_death[Sex==1, Sex:='M']; outL_TB_death[Sex==2, Sex:='F']; 
    outL_TB_death[HIV==1, HIV:='hivNeg']; outL_TB_death[HIV==2, HIV:='artNaive']; 
    outL_TB_death[HIV==3, HIV:='onART'];
    
    outL_PYD <- outL_TB_death[, .(PYDied=sum(PYDied)),by=.(Year, PYDead,  HIV)]
    outL_mort <-outL_TB_death[Age>3, .(mort=sum(mort)),  by=.(Year, State, Age, Sex, HIV)]
    
    outL_mort[, Age:= as.factor(Age)]
    levels(outL_mort$Age) <- list('15-19'=4,'20-24'=5,
                                  '25-29'=6, '30-34'=7,'35-39'=8, '40-44'=9,
                                  '45-49'=10, '50-54'=11, '55-59'=12, '60-64'=13,
                                  '65-69'=14,'70-74'=15,  '75-79'=16, '80+'=17)
    
    mortW <-dcast(outL_mort[], Year ~ State+Age+Sex+HIV, value.var = "mort")
    PYDw<- dcast(outL_PYD, Year ~ PYDead+HIV, value.var = "PYDied")
    outw <- merge(mortW, PYDw, by= 'Year')
    ##outw <-merge(outw,elligible_population, by='Year')
    
    outw <-setDT(outw)
    outw$CommID =comid
    
    others <-Out[, .(Time,NumDiagnosedTB,
                     NumTreatedTB, NumTreatCompleted,PYonTBTreatment)]
    
    ##others[,PYonTBTreatment2:=(0.93*NumTreatedTB*6/12)+ (0.07*NumTreatedTB*2/12)] ##PYonTB treatment
    others[, Year:=floor(Time)]
    tmp <-others[, lapply(.SD, mean), by=Year]
    outw <-merge(outw,tmp, by= 'Year')
    
    ## work on external data
    DT <- if(comid<13) ACF_output_count[Country=='ZA',] else ACF_output_count[Country=='SA',]
    
    denominator <-CHiPs_visit[Year>=2013 & Year<=2017, .(denom=sum(AnnualCHIPSvisits)), by = Country] ##add CHiP visits in all community
    denom <- if(comid<13) denominator[Country=='ZA', denom] else denominator[Country=='SA', denom]
    
    
    pop <- CHiPs_visit[ComID==comid]
    screened <- pop
    
    if (acf_hr==1 & interv=='CF') screened[,AnnualCHIPSvisits:=0]  ## CHiPs vist==0 when no UTT and ACF
    
    DTT <- if(comid<13) ACF_output_count[Country=='ZA',] else ACF_output_count[Country=='SA',]
    
    DT <- if (acf_hr==1) DTT[,(2:7):=0] else DTT     ## TB screening stuff=0 when when no ACF
    
    screened[,CHiPsNumberInvited:=(DT$number_elligibles_invited/denom)*screened$AnnualCHIPSvisits]
    screened[,CHiPsNumberSymptScreened:=(DT$number_screened_for_symptoms/denom)*screened$AnnualCHIPSvisits]
    screened[,CHiPsNumberSputumTested:=(DT$number_symptom_postive/denom)*screened$AnnualCHIPSvisits]
    screened[,CHiPsNumberDiagnosed:=(DT$number_TB_detected/denom)*screened$AnnualCHIPSvisits]
    outw <-merge(outw,screened, by= 'Year')
    
    setcolorder(outw, c("Year", "CommID"))

    outw[, Time:=NULL]

    return(outw)
}

#### ## test NOTE have changed usage
#### CEA_utt_acf<-cost_effectiveness_output(comid = 1, interv = '1', acf_hr=1.5, interv_end_yr= 2030)   ##UTT and  ACF
#### CEA_CF1    <-cost_effectiveness_output(comid = 1, interv = 'CF', acf_hr=1, interv_end_yr= 2030)    ##no UTT and no ACF
#### CEA_CF_hiv <-cost_effectiveness_output(comid = 1, interv = '1', acf_hr=1, interv_end_yr= 2030)     ##UTT and no ACF
#### CEA_CF_acf <-cost_effectiveness_output(comid = 1, interv = 'CF', acf_hr=1.5, interv_end_yr= 2030)  ##no UTT and ACF
