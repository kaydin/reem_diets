
## Add any final selection criteria here
SURV <- data.frame(pred_crosstab %>%
                   filter(month %in% 5:8))

## IMPORTANT:  The above crosstab will drop any prey categories that have 0's for 
## the chosen predator.  So each predator will have prey columns in a different order.
yearlist <- 1985:2021

# List out ranges for looping and adding
allstrat <- sort(unique(SURV$stratum_bin))
alllen   <- sort(as.character(unique(SURV$lbin)))
allyears <- yearlist
allprey  <- colnames(SURV)[ (which(names(SURV)=="stratum_bin")+1):ncol(SURV) ]
prey_guild <- colnames(pred_crosstab)[ (which(names(pred_crosstab)=="stratum_bin")+1):ncol(pred_crosstab) ]

min_sample   <- 5

o_vals <- NULL
for (STRAT in allstrat){
  cat(PRED,STRAT,"\n"); flush.console()
  for (LL in alllen){
    cat(LL,"\n"); flush.console()
    for (YY in allyears){
      cat(YY,"\n"); flush.console()
      
      SELPRED <- SURV[ SURV$stratum_bin %in% STRAT &
                         SURV$lbin %in%  LL    &
                         SURV$year %in%  YY   ,]  
      #SELPRED <- SURV %>%
      #           filter(stratum_bin==STRAT & lbin==LL & year==YY)
      #cat(nrow(SELPRED),"\n"); flush.console()
          
      if (nrow(SELPRED) >= min_sample){
        gear_temp <- mean(SELPRED$gear_temp,na.rm=T)
        surface_temp <- mean(SELPRED$surface_temp,na.rm=T)
        
        allfood  <- SELPRED[,allprey]
        goodprey <- as.matrix(allfood) #allfood[,colSums(allfood)>0] to remove colums with 0 for dirichlet  
        Nsamp    <- nrow(goodprey)
        prey_sp  <- colnames(goodprey)
        prey_N   <- colSums(goodprey>0) 
        prey_g   <- colSums(goodprey)
        spadd    <- 0.0 #spadd    <- DETECT*preywt_g/sum(preywt_g) for dirichlet
        SCI      <- t(t(goodprey)+spadd) / (SELPRED$bodywt)
        prey_SCI <- colSums(SCI)
        tot_g   <- sum(goodprey)
        tot_SCI <- sum(SCI)
        dietprop_g   <- colSums(goodprey)/tot_g
        dietprop_SCI <- colSums(SCI)/tot_SCI
        Nfull <- sum(rowSums(sampmat[IND,])>0)

        outvals <- data.frame(model,PRED,STRAT,YY,LL,Nsamp,Nfull,gear_temp,surface_temp,tot_g,tot_SCI,prey_guild,prey_N,prey_g,prey_SCI,dietprop_g,dietprop_SCI)#,cperfull,elog,e_sd)
        o_vals  <- rbind(o_vals,outvals)
      } # end of if sample size conditional 

    } # end of yearlist
  } # end of lenblock
} #end of stratblock      


}  # end predator loop 







