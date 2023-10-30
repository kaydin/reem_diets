
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
                       prey_guild_column     = "peec_2023")

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

diet_combined <- NULL
diet_strat_combined <- NULL
just_diets <- NULL
all_classes <- FALSE #TRUE

for (this.model in c("EBS","NBS","AI")){
  pred_params <- load_predators(this.model, "apps/ESR_ESP_diets/predator_cod.csv")  

  for (p in names(pred_params)){
    
    if(all_classes){pred_params[[p]]$LCLASS <- c(seq(0,120,5),9999)}
    
    just_diets <- rbind(just_diets,predprey_tables(predator=p, model=this.model))
    
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

write.csv(just_diets,"apps/ESR_ESP_diets/cod_just_diets.csv")

if(all_classes){
  write.csv(diet_strat_combined,"apps/ESR_ESP_diets/cod_allclass_diet_strat_combined_Bering_ESR.csv",row.names=F)
  write.csv(diet_combined,"apps/ESR_ESP_diets/cod_allclass_diet_combined_Bering_ESR.csv",row.names=F)  
} else {
  write.csv(diet_strat_combined,"apps/ESR_ESP_diets/cod_diet_strat_combined_Bering_ESR.csv",row.names=F)
  write.csv(diet_combined,"apps/ESR_ESP_diets/cod_diet_combined_Bering_ESR.csv",row.names=F)
    
}
  


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
