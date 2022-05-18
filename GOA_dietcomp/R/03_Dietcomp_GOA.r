#Calculate diet proportions

library(tidyverse)


ppfile       <- "../data/GOA_raw.csv.gz"                    # raw Pred/prey diet file (RACEBASE query result) #BF added "../data/" and BF added ".gz"
pplookfile   <- "../lookups/Alaska_PreyLookup_MASTER.csv"  # lookup file for grouping prey #BF added "../lookups/"
preylook_col <- "ECOPATH_PREY"                      # Column name in pplookfile to use to group prey
LWparamfile      <- "../results/LengthWeightRegressionParams.csv"  #table of length weight parameters calculated by "00_LengthWeightRegression.r" from BTS specimens
GOA_stratafile <-"../lookups/GOA_strata_area2021.csv"
min_sample   <- 5                               # Minimum sample size for using diet data

### Length weight regression parameters
# List of predator-specific values of Length/ Weight Regression (W =a*L^b)  
# run script that calculates species-specific parameters in regression from Racebase (groundfish bottom trawl survey) 
# script queries racebase data so is updated when new survey data are enterred

#source("00_LengthWeightRegression.r") (if need updating)
LWparam=read.csv(LWparamfile)
head(LWparam) #output is a table with NODC and RACE species codes, species name, common name, LW regression intercept (a), and LW regression slope (b) (only calculated for species with min sample size=5)


##################

POP=LWparam[which(LWparam[,1]=="8826010102"),] #Pacific ocean perch"),] #30060
#ATF=LWparam[which(LWparam$NODC.species.code=="8857040100" & LWparam$NODC.species.code=="8857040102"),] #)  #10110),] #10110 # arrowtooth flounder
PLK=LWparam[which(LWparam[,1]=="8791030701"),]  #walleye pollock (juvenile)"),] #21741
#PLK=LWparam[which(LWparam[,1]=="walleye pollock (adult)"),] #21742
COD=LWparam[which(LWparam[,1]=="8791030401"),] #"Pacific cod (adult)"),] #21722
#COD=LWparam[which(LWparam[,1]=="Pacific cod (juvenile)"),] #21721
HAL=LWparam[which(LWparam[,1]=="8857041901"),]



preds <- list(
  "W.pollock"  = list(nodc=PLK$NODC.species.code, A_L=PLK$intercept_a, B_L=PLK$slope_b, LCLASS=c(0,10,25,40,55,999)),
  "P.cod"      = list(nodc=COD$NODC.species.code, A_L=COD$intercept_a, B_L=COD$slope_b, LCLASS=c(0,10,30,60,85,999)),
  "P.halibut"  = list(nodc=HAL$NODC.species.code, A_L=HAL$intercept_a, B_L=HAL$slope_b, LCLASS=c(0,10,50,70,999)),
  "P.ocean perch"  = list(nodc=POP$NODC.species.code, A_L=POP$intercept_a, B_L=POP$slope_b, LCLASS=c(0,10,30,60,85,999)) #fix the length class - I just copied cod for now
#  "Arrowtooth" = list(nodc=c("8857040100", "8857040102"), # ATF includes Atheresthes sp. unid  A_L=0.00443866, B_L=3.19894001, LCLASS=c(0,10,30,50,999) ),
)
  

# Predators to calculate (all must be on above list)
predators <- c("W.pollock","P.cod", "P.halibut","P.ocean perch")   
  
# Years to output
#yearlist <- 1985:2019
yearlist <- 1991:2013

##Select which INPFC regions to calculate diet for (and list strata codes within each INPFC region)
## Group strata as desired
GOAstrata=read.csv(GOA_stratafile)

#from EBS code - strat in EBS are strata but GOA should be individual stratum
#stratblock_GOA <- list(
#  "Shumagin"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Shumagin")], 
#  "Chirikof"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Chirikof")], 
#  "Kodiak"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Kodiak")],
#  "Yakutat"=GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Yakutat")],
#  "Southeastern" =GOAstrata$Stratum[which(GOAstrata$INPFC_area=="Southeastern")] 
#)

#list of all strata in GOA to calculate diet
stratlist=GOAstrata$Stratum

# Read in lookup and raw data and ensure 10-digit nodc numbers are read as text keys
preylooktable <- read.csv(pplookfile)
  preylook <- preylooktable[,preylook_col]
  names(preylook)<-sprintf("%010.0f",preylooktable$NODC_CODE)
  fullprey<-unique(preylooktable[,preylook_col])
rawdat   <- read.csv(ppfile)
  rawdat$PREY_NODC <-sprintf("%010.0f",rawdat$PREY_NODC)
  rawdat$PRED_NODC <-sprintf("%010.0f",rawdat$PRED_NODC)  
  rawdat$preyguild <- preylook[rawdat$PREY_NODC]

    
# Start main loop here
outdat <- e_logvals <- e_sdvals <- e_vals <- o_vals <- cp_vals <- cper_dat <- NULL     

for (PRED in predators){    
  # Pick the predator 
  ThisPred <- preds[[PRED]] 
  preddat <- rawdat[rawdat$PRED_NODC %in% ThisPred$nodc,]  

  # Create a crosstab table SURV and list of prey allprey using the preylook_col as the crosstab
  gtab <- tapply(preddat$TWT,list(as.character(preddat$PREDJOIN),preddat$preyguild),sum)
  gtab[is.na(gtab)]<-0
  predtab <- unique(preddat[,c("PREDJOIN","PRED_NODC","PRED_NAME","PRED_LEN","HAULJOIN","REGION","STRATUM","YEAR","MONTH","DAY","GEAR_TEMP",
                                "GEAR_DEPTH","BOTTOM_DEPTH","SURFACE_TEMP","STATIONID","CRUISE_TYPE","RLAT","RLONG")])
  preycross <- gtab[as.character(predtab$PREDJOIN),]
  # Bin by pred LCLASS (in cm)
  lbin  <- cut(predtab$PRED_LEN,ThisPred$LCLASS,right=F)
  GDAT    <-cbind(predtab,lbin,preycross)
  allprey <- colnames(preycross)
  alllen  <- unique(as.character(lbin))
  
  # add any additional filters for the data here:     
  SURV <- GDAT[GDAT$CRUISE_TYPE=="Race_Groundfish" & GDAT$MONTH%in%6:8,]
  stratblock <- stratblock_GOA #  full GOA
  
 for (STRAT in stratlist){
  # for (STRAT in names(stratblock)){ 
    cat(PRED,STRAT,"\n"); flush.console()
   # stratcodes <- unlist(stratblock[STRAT])
  
    
    for (LL in alllen){
      lencodes <- LL 
      
      for (YY in yearlist){
        yearcodes <- YY
      
         #bin all predator of size (lengcodes), in year (yearcodes) within strata 
        SELPRED <- SURV[   SURV$lbin %in% lencodes    &
                           SURV$YEAR %in% yearcodes   ,]  
        
         #only calculate diet proportions if >min sample (5) stomachs per pred/length/year/strata
        if (length(SELPRED[,1])>=min_sample){
          allfood <- SELPRED[,allprey]
          goodprey <- allfood #allfood[,colSums(allfood)>0] to remove colums with 0 for dirichlet  
          sptot    <- colSums(goodprey)
          spadd <- 0.0 #spadd    <- DETECT*sptot/sum(sptot) #remove zeros for dirichlet (add trace amounts of prey that are zero) (commented out as don't include dirichlet)
          SCI <- t(t(goodprey)+spadd)/ (ThisPred$A_L * (SELPRED$PRED_LEN ^ ThisPred$B_L)) #divide prey weight  by individ pred weight
          sp_prey  <- colnames(goodprey)
          Pfull <- colSums(goodprey>0)  
          Pnum  <- colSums(goodprey>=0)
          elog  <- log((Pfull+0.5)/(Pnum-Pfull+0.5))
          e_sd  <- sqrt(1.0/(Pfull+0.5) + 1.0/(Pnum-Pfull+0.5))
          
          # Make matrix of individual stomachs
          sampmat <- matrix(as.numeric(unlist(SCI)),length(SCI[,1]),length(sp_prey))
          colnames(sampmat)<-sp_prey
          Nind <- length(sampmat[,1])    
          IND  <- 1:Nind #IND <- sample.int(Nind, Nind, replace = T)  
          tot_wt    <- sum(rowSums(sampmat[IND,]))  # sum total weight of all prey in individual stomach
          cperw     <- sum(rowSums(sampmat[IND,]))/Nind # divide sum weight of all prey in indiv stomach by # individual predators
          pathsum <- colSums(sampmat[IND,]) #+ spadd
          dietprop <- pathsum/sum(pathsum)
          cperfull  <- pathsum/Pfull
          
           # Saving the data     
          Nsamp <- Nind
          Nfull <- sum(rowSums(sampmat[IND,])>0)
          SCIperN <- cperw 
          #outdat    <- rbind(outdat,data.frame(PRED,STRAT,YY,LL,Nsamp,Nfull,SCIperN,t(dietprop)))
          #cper_dat  <- rbind(cper_dat,data.frame(PRED,STRAT,YY,LL,Nsamp,Nfull,SCIperN,t(cperfull)))
          #e_logvals <- rbind(e_logvals,data.frame(PRED,STRAT,YY,LL,Nsamp,Nfull,SCIperN,t(elog) ))
          #e_sdvals  <- rbind(e_sdvals, data.frame(PRED,STRAT,YY,LL,Nsamp,Nfull,SCIperN,t(e_sd) ))
          o_vals    <- rbind(o_vals,data.frame(PRED,STRAT, YY,LL,Nsamp,Nfull,SCIperN,sp_prey,dietprop,cperfull,elog,e_sd))
          #cp_vals   <- rbind(cp_vals,data.frame(PRED,STRAT,YY,LL,Nsamp,Nfull,SCIperN,sp_prey,cperfull))
          #e_vals    <- rbind(e_vals,data.frame(PRED,STRAT,YY,LL,Nsamp,Nfull,SCIperN,sp_prey,elog,e_sd))
          #latmat[latind,sp_prey] <- dietprop[sp_prey]
        } # end of if sample size conditional 
        
        
        #cat(stratcodes," ",lencodes,"\n") 
      } # end of yearlist
    } # end of lenblock
  } #end of stratblock      
}  # end predator loop 
  
o_vals$cperfull[is.nan(o_vals$cperfull)]<-0        

# Add transformed logit values for frequency of occurrence 
o_vals$meanper <- exp(o_vals$elog)/(1+exp(o_vals$elog))       
o_vals$lo95per <- exp(o_vals$elog-1.96*o_vals$e_sd)/(1+exp(o_vals$elog-1.96*o_vals$e_sd))
o_vals$hi95per <- exp(o_vals$elog+1.96*o_vals$e_sd)/(1+exp(o_vals$elog+1.96*o_vals$e_sd))

#diet proportion by pred of length by year by strata         
write.csv(o_vals,"../results/out_diets_allGOA2.csv",row.names=F) # WGOA removes SE and Yakutat

###NEXT STEPS
#maybe don't need to read in area calculations file for WGOA (comment out?)
#add rest of predators
