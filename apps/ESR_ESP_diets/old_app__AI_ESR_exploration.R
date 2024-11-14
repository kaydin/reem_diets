
# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")

####################################################################

source("R/REEM_fooddata_functions.R")

# Load Race data and perform some preliminary cleanups/calculations
REEM.loadclean.RACE(path = "data/local_racebase")

# Load diet data and perform some preliminary cleanups/calculations
REEM.loadclean.diets(data_path = "data/local_reem_data")

# Load lookup tables and create summary lookups - re-run this line 
# if lookups change.
REEM.loadclean.lookups(strata_lookup_file    = "apps/ESR_ESP_diets/combined_BTS_strata.csv",
                       stratum_bin_column    = "strat_groups",
                       preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv",
                       prey_guild_column     = "fish_or_invert")

#predlist <- read.clean.csv("lookups/Alaska_Predators_GOA.csv")

#race_lookup <- read.clean.csv("lookups/goa_race_lookup_apr_04_2023.csv") %>% 
#  mutate(race_guild  = .data[["final_goa"]])

################################################################################

load_predators <- function(this.model, predfile){
  predlist   <- read.clean.csv(predfile)
  preds      <- predlist %>% filter(model==this.model)
  pred_names <- unique(preds$predator)
  pred_params=list()
  for (p in pred_names){
    pdat <- as.list(preds[preds$predator==p,])
    pred_params[[p]] <- pdat
    pred_params[[p]]$LCLASS <- sort(unique(c(0,pdat$juv_cm, pdat$adu_1, pdat$adu_2, pdat$adu_3,999)))
    pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
    pred_params[[p]]$lw_b   <- pdat$b_l_mm_g
    pred_params[[p]]$lw_a   <- pdat$a_l_mm_g*(10^pdat$b_l_mm_g)  
    pred_params[[p]]$bioen  <- list(CA=pdat$ca, CB=pdat$cb, C_TM=pdat$c_tm, C_T0=pdat$c_t0, C_Q=pdat$c_q)
  }
  return(pred_params)
}  

################################################################################  
# EBS and NBS diet ESR indicators

# diet_combined <- NULL
# diet_strat_combined <- NULL
# just_diets <- NULL
# all_classes <- FALSE #TRUE
# 
# for (this.model in c("EBS","NBS","AI")){
#   pred_params <- load_predators(this.model, "apps/ESR_ESP_diets/predator_cod.csv")  
# 
#   for (p in names(pred_params)){
#     
#     if(all_classes){pred_params[[p]]$LCLASS <- c(seq(0,120,5),9999)}
#     
#     just_diets <- rbind(just_diets,predprey_tables(predator=p, model=this.model))
#     
#     juv_adu_lencons  <- get_stratum_length_cons(predator=p, model=this.model)
#     strat_dietcons <- add_diets_to_strata_length_cons(juv_adu_lencons, predator=p, model=this.model, min_sample=5) %>%
#       mutate( jcat = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu"))
#     
#     strat_dietprops <- strat_dietcons %>%
#       group_by(year, model, species_name, jcat, lbin, prey_guild) %>%
#       summarize(cons_tot_prey_vonb = sum(cons_rel_vonb), 
#                 cons_tot_prey_SCI  = sum(cons_tons_day), .groups="keep") %>%
#       
#       group_by(year, model, species_name, jcat, lbin) %>%
#       mutate(cons_tot_vonb = sum(cons_tot_prey_vonb),
#              cons_tot_SCI  = sum(cons_tot_prey_SCI)) %>%
#       ungroup() %>%
#       mutate(diet_prop_vonb = cons_tot_prey_vonb/cons_tot_vonb,
#              diet_prop_SCI  = cons_tot_prey_SCI/cons_tot_SCI)
#     
#     diet_strat_combined <- rbind(diet_strat_combined,strat_dietcons)  
#     diet_combined <- rbind(diet_combined,strat_dietprops)
#     
#   }  
#     
# }
# 
# write.csv(just_diets,"apps/ESR_ESP_diets/cod_just_diets.csv")
# 
# if(all_classes){
#   write.csv(diet_strat_combined,"apps/ESR_ESP_diets/cod_allclass_diet_strat_combined_Bering_ESR.csv",row.names=F)
#   write.csv(diet_combined,"apps/ESR_ESP_diets/cod_allclass_diet_combined_Bering_ESR.csv",row.names=F)  
# } else {
#   write.csv(diet_strat_combined,"apps/ESR_ESP_diets/cod_diet_strat_combined_Bering_ESR.csv",row.names=F)
#   write.csv(diet_combined,"apps/ESR_ESP_diets/cod_diet_combined_Bering_ESR.csv",row.names=F)
#     
# }


min_sample <- 10

library(matrixStats)

SCI_out <- NULL

for (this.model in c("AI")){  
  pred_params <- load_predators(this.model, "apps/ESR_ESP_diets/predator_cod.csv")
  for (p in names(pred_params)){
    diet_all <- predprey_tables(predator=p, model=this.model,all.data=T)  
    # Crosstab query
    all_dat <- diet_all$predprey_table %>%
      pivot_wider(names_from=prey_guild, values_from=prey_wt, values_fill=0.0)
    
    lc_list    <- sort(unique(all_dat$lbin))
    year_list  <- sort(unique(all_dat$year))
    strat_list <- sort(unique(all_dat$stratum_bin))

    for (YY in year_list){ #YY <- 1994
      for (SS in strat_list){ #SS <- "MiddleNW"
        for (LL in lc_list){ #LL <- "[40,60)"
            
          wt_dat <- all_dat %>%
            filter(lbin==LL & year==YY & stratum_bin==SS) %>%
            select(bodywt:last_col()) # %>% select(-bodywt)
          
          dat    <- data.frame(wt_dat/wt_dat$bodywt) %>% select(-bodywt) 
  
          n <- nrow(dat)
          cat(LL,YY,SS,n,"\n"); flush.console()
          if(n >= min_sample){
            samples <- 10000
            sdat <- matrix(NA,samples,ncol(dat))
            
            SCI_mean <- colMeans(data.frame(dat))
            
            for (i in 1:samples){
              pick <- sample.int(n, replace=T)
               sdat[i,]=colSums(dat[pick,])/n
            }
            
            dquant <- rownames_to_column(data.frame(SCI_mean, colQuantiles(sdat,probs=c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1.0))))
            dout <- data.frame(this.model,p,YY,SS,LL,n,dquant)
            SCI_out <- rbind(SCI_out,dout)
                        
          } else {
            # what to do if min_sample is too small
          }
            
          
        } ## LL loop
      } ## SS loop
    } ## YY Loop
    
  } #pred_params loop
} # this.model loop

SCI_cleaned <- SCI_out %>%
  rename(Model=this.model, Predator=p, Year=YY, Stratgroup=SS, Lbin=LL, Pred_n=n, Prey_guild=rowname)

write.csv(SCI_cleaned,"apps/ESR_ESP_diets/AI_cod_SCI_cleaned.csv", row.names=F)

library(ggplot2)

#par(mfrow=c(3,2))
X11()
plist <- list()
i<-1
for(LL in c("[30,60)","[60,85)")){
  for(PP in c("Atka_mackerel", "Other_fish", "Invertebrates")){

  sci_plot <- SCI_cleaned %>% filter(Lbin==LL & Prey_guild==PP)

  p<- ggplot(sci_plot, aes(x=Year, y=SCI_mean, group=Stratgroup, color=Stratgroup)) + 
    geom_line() +
    geom_point()+
    geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=.2,
                  position=position_dodge(0.05)) +
    labs(title=PP, x="", y = "")+
    theme_classic() 

  plist[[i]] <- p
  i <- i+1
  }
}

library(Rmisc)
multiplot(plotlist=plist,cols=2)

library(sparkline)
sparkline(c(1,2,7,6,5), type = "bar", barColor = "green")
################################################################################
#


################################################################################  
# GOA POLLOCK ESP indicator
# GOA %diet of Euphausiids by [10,25) pollock

diet_combined <- NULL
diet_strat_combined <- NULL


for (this.model in c("WGOA")){

REEM.loadclean.strata(strata_lookup_file    = "apps/ESR_ESP_diets/combined_BTS_strata.csv",
                      stratum_bin_column    = "strat_groups")
  pred_params <- load_predators(this.model, "apps/ESR_ESP_diets/predators_WCGOA_ESP.csv")
  # From previous years' analysis
    #A_L=0.00553096, B_L=3.044172
  pred_params[["Walleye_pollock"]]$lw_a <- 0.00553096
  pred_params[["Walleye_pollock"]]$lw_b <- 3.044172
  for (p in names(pred_params)){
    
    juv_adu_lencons  <- get_stratum_length_cons(predator=p, model=this.model)
    strat_dietcons <- add_diets_to_strata_length_cons(juv_adu_lencons, predator=p, model=this.model, min_sample=5) %>%
      mutate( jcat = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu"))

    strat_dietprops <- strat_dietcons %>%
      group_by(year, model, species_name, jcat, lbin, prey_guild) %>%
      summarize(cons_tot_prey_vonb = sum(cons_rel_vonb), 
                cons_tot_prey_SCI  = sum(cons_tons_day), .groups="keep") %>%
      
      group_by(year, model, species_name, jcat, lbin) %>%
      mutate(cons_tot_vonb = sum(cons_tot_prey_vonb),
             cons_tot_SCI  = sum(cons_tot_prey_SCI)) %>%
      ungroup() %>%
      mutate(diet_prop_vonb = cons_tot_prey_vonb/cons_tot_vonb,
             diet_prop_SCI  = cons_tot_prey_SCI/cons_tot_SCI)
    
  diet_strat_combined <- rbind(diet_strat_combined,strat_dietcons)  
  diet_combined <- rbind(diet_combined,strat_dietprops)
  
  }
}

esp_indicator <- diet_combined %>% 
  filter(species_name=="Walleye_pollock" & lbin=="[10,25)" & prey_guild=="Euphausiids")

write.csv(diet_strat_combined,"apps/ESR_ESP_diets/diet_strat_combined_ESP.csv",row.names=F)
write.csv(diet_combined,"apps/ESR_ESP_diets/diet_combined_ESP.csv",row.names=F)


##################################################################
##################################################################
