
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
#REEM.loadclean.diets(data_path = "data/local_reem_data")

# Load Lookup (file and column) for aggregating strata (ecosystem model and stratum numbers as primary key)
REEM.loadclean.strata(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                       stratum_bin_column    = "strat_groups")

# Lookup file and column for mapping RACE codes to biomass
race_lookup <- read.clean.csv("lookups/goa_race_lookup_apr_04_2023.csv") %>% 
  mutate(race_guild  = .data[["species_code"]])
  #race_lookup$race_guild <- as.character(race_lookup$race_guild)
  race_lookup$race_guild <- paste("race",race_lookup$race_guild,sep="_")

##############################################################################
# Biomass extraction by item

# Can loop through multiple models, saving results in a single
# table (bio_combined)
#for (this.model in c("EGOA","WGOA")){

this.model <- "EGOA"  
predlist   <- read.clean.csv("lookups/Alaska_Predators_EGOA.csv")
#predlist   <- read.clean.csv("lookups/Alaska_Predators_EGOA_small.csv")

bio_combined <-NULL
# get_cpue_all() mirrors the RACE get_cpue function (returning by haul), except it gets
# biomasses for all groups at once, binned by the race_lookup names (race_guild column)
# haul_stratum_summary() gets the total stations and area for each model domain.
# This code groups the two together at the stratum level to get stratum biomass
# in both tons and t/km^2
  stratsum <- get_cpue_all(model=this.model) %>%
    group_by(year, model, race_guild, species_code, stratum) %>%
    summarize(wgtcpue = sum(wgtcpue),
              numcpue = sum(numcpue),.groups="keep") %>%
    left_join(haul_stratum_summary(this.model),by=c("year","model","stratum")) %>%
    mutate(bio_t_km2 = wgtcpue/1000/stations,
           bio_tons  = bio_t_km2 * area)
  
# These two lines then sum stratsum to the total biomass and biomass density
# for the entire model area
  model_area <- sum(strata_lookup$area[strata_lookup$model==this.model])
  bio_totals <- stratsum %>%
    group_by(year, model, race_guild) %>%
    summarize(bio_tons = sum(bio_tons),
              bio_tkm2 = bio_tons/model_area, .groups="keep")
  

# Loop through the list of predators from the predlist table and
# load parameters for each species into a list. The pred entries
# are indexed by name and model
  preds      <- predlist %>% filter(model==this.model); #preds <- preds[c(9,11),]
  pred_names <- unique(as.character(preds$predator))
  pred_params=list()
  for (p in pred_names){
    pdat <- as.list(preds[preds$predator==p,])
    pred_params[[p]] <- pdat
    pred_params[[p]]$LCLASS <- sort(unique(c(0,pdat$juv_cm, pdat$adu_1, pdat$adu_2, pdat$adu_3,9999)))
    pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
    pred_params[[p]]$lw_b   <- pdat$b_l_mm_g
    pred_params[[p]]$lw_a   <- pdat$a_l_mm_g*(10^pdat$b_l_mm_g)  
    pred_params[[p]]$bioen  <- list(CA=pdat$ca, CB=pdat$cb, C_TM=pdat$c_tm, C_T0=pdat$c_t0, C_Q=pdat$c_q)
  }

  
# Getting biomass-at-length and converting to split pool biomass
# Loop through each predator.  the juv_adu_lencons splits out
# length samples based on the length class divisions in the
# predator lookup, and converts to % by weight using the a and b LW
# regression.  This % by weight is applied to the total biomass
# for the predator calculated above, to calculate biomass in
# each length class.  It is output into bio_combined as two biomass groupings
# (juv and adu) where adu is the sum of all the adu size classes.
  
  juv_combined <- NULL
  for (p in pred_names){
    cat(p,pred_params[[p]]$base_name,"\n"); flush.console()
    bio_pred <- bio_totals %>%
      filter(race_guild==p)
    
    juv_adu_lencons  <- get_stratum_length_cons(predator=p, model=this.model) %>%
    group_by(year, model, species_name, stratum, lbin) %>%
      summarize(strat_bio_sum = sum(tot_wlcpue_t_km2), .groups="keep") %>%
      left_join(haul_stratum_summary(this.model),by=c("year","model","stratum")) %>%
      mutate(bio_t_km2 = strat_bio_sum/stations,
             bio_tons  = bio_t_km2 * area,
             jcat      = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu")) 
    
    juv_proportions <- juv_adu_lencons %>%
      group_by(year,model,species_name,jcat) %>%
      summarize(bio_tons = sum(bio_tons), .groups="keep") %>%
      pivot_wider(names_from=jcat, values_from=bio_tons) %>%
      mutate(juv_bio_prop = juv/(juv+adu))
    
    juv_proportions<- juv_proportions %>% 
      left_join(preds %>% select(predator,base_name),by=c("species_name"="predator"))
    
    juv_combined <- rbind(juv_combined,juv_proportions)
  } # end of pred_names loop
  
# Now we've looped through all predators.  Attach to biomass records,
# removing all species without a juv/adu split (via base_name)
  bio_with_juvs <- bio_totals %>%
    left_join(juv_combined,by=c("year","model","race_guild"="species_name")) %>%
    filter(!is.na(base_name)) %>%
    select(-c(juv,adu)) %>%
    mutate(juv_bio_prop  = replace_na(juv_bio_prop,0.0),
           adu_bio_tkm2  = (1.0-juv_bio_prop) * bio_tkm2,
           juv_bio_tkm2  = juv_bio_prop       * bio_tkm2)
  
  
bio_combined <- rbind(bio_combined,bio_with_juvs)      
  

write.csv(bio_combined,"results/goa_bio_combined_juvadu.csv",row.names=F)


# Biomass code stops here
