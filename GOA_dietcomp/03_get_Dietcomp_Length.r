
  
# This function calculates diet composition () by predator by length bin by strata
# run "02_diet_dataprep_Length.R" first 

### old function code
  #get_dietcomp_bylength <- function(diet_tables = list(
  #  ppfile   = ppfile  ,
  #  pplookfile = pplookfile,
  #  LWparamfile = LWparamfile,
  #  GOA_stratafile = GOA_stratafile 
  #),                             
  #predators = c("W.pollock"))
  #  {
    
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
 # stratblock <- stratblock_GOA #  full GOA
  
 for (STRAT in stratlist){        #stratlist are individ stratum
 # for (STRAT in names(stratblock)){               #stratblock are NPFC areas (super strata)
    cat(PRED,STRAT,"\n"); flush.console()
    #stratcodes <- unlist(stratblock[STRAT])
   stratcodes<-stratlist[STRAT]
    
    for (LL in alllen){
      lencodes <- LL 
      
      #for (YY in yearlist){
      #  yearcodes <- YY
      
         #bin all predator of size (lengcodes), in year (yearcodes) within strata 
        SELPRED <- SURV[   SURV$lbin %in% lencodes & SURV$STRATUM %in% stratcodes ,]  #& SURV$YEAR %in% yearcodes   ,]  
        
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
          o_vals    <- rbind(o_vals,data.frame(PRED,STRAT,LL,Nsamp,Nfull,SCIperN,sp_prey,dietprop,cperfull,elog,e_sd)) #, meanper, lo95per, hi95per)) #YY removed
          #cp_vals   <- rbind(cp_vals,data.frame(PRED,STRAT,YY,LL,Nsamp,Nfull,SCIperN,sp_prey,cperfull))
          #e_vals    <- rbind(e_vals,data.frame(PRED,STRAT,YY,LL,Nsamp,Nfull,SCIperN,sp_prey,elog,e_sd))
          #latmat[latind,sp_prey] <- dietprop[sp_prey]
        } # end of if sample size conditional 
        
        
        #cat(stratcodes," ",lencodes,"\n") 
     # } # end of yearlist
    } # end of lenblock
  } #end of stratblock      
}  # end predator loop 

o_vals$cperfull[is.nan(o_vals$cperfull)]<-0        

# Add transformed logit values for frequency of occurrence 
o_vals$meanper <- exp(o_vals$elog)/(1+exp(o_vals$elog))       
o_vals$lo95per <- exp(o_vals$elog-1.96*o_vals$e_sd)/(1+exp(o_vals$elog-1.96*o_vals$e_sd))
o_vals$hi95per <- exp(o_vals$elog+1.96*o_vals$e_sd)/(1+exp(o_vals$elog+1.96*o_vals$e_sd))


#return(o_vals)
 # }  #end function
  

#diet proportion by pred of length by year by strata         
#write.csv(o_vals,"../results/out_diets_allGOA2Lengths2.csv",row.names=F) 

##########
# Double check
###########
#x <- get_dietcomp_bylength(predators = c("W.pollock"))
#head(x)

#regular script test (not function)
o_vals%>%
  filter(PRED=="W.pollock")

o_vals%>%
  filter(PRED=="P.cod")

o_vals%>%
  filter(PRED=="P.halibut") #"P.ocean perch" "Rex Sole"      "FH. Sole"      "Sablefish"     "Arrowtooth" 


o_vals%>%
  filter(PRED=="Arrowtooth") 

