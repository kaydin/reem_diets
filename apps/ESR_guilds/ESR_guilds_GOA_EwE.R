
# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")

####################################################################

source("R/REEM_fooddata_functions.R")

# Load all Race data and perform some preliminary cleanups/calculations
  REEM.loadclean.RACE(path = "data/local_racebase")

# Load strata table (linking models to strata)
  REEM.loadclean.strata(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                        stratum_bin_column    = "strat_groups")

# Load lookup file for mapping RACE codes to biomass. Later need to specify
# which column to use for group mapping (varies by ecosystem)
  race_lookup_base <- read.clean.csv("lookups/race_lookup_combined.csv")

#race_lookup_col <- c("EBS"="ebs_ecopath", "AI"="ai_ecopath", "WGOA"= "goa_ecopath")


  
  ##############################################################################  
#  get_cpue_all() mirrors the RACE get_cpue function (returning by haul), except it gets
# biomasses for all groups at once, binned by the race_lookup names (race_group column)
# haul_stratum_summary() gets the total stations and area for each model domain.
#
# Note: BS Survey filter (for slope stations) moved to core loading code

#race_lookup_older <- read.clean.csv("lookups/goa_race_lookup_apr_04_2023.csv") %>%
#  select(species_code,final_goa)
#
#race_lookup_combined <- race_lookup_base %>%
#  left_join(race_lookup_older,by="species_code")
#
#write.csv(race_lookup_combined,"lookups/race_lookup_combined.csv",row.names=F)


# WGOA GUILDS -----------------------------------------------
  this.model  <- "WGOA"  
  race_lookup      <- race_lookup_base %>% mutate(race_group  = .data[["final_goa"]])  
  q_table          <- read.clean.csv("apps/ESR_guilds/GroupQ_2021_GOA.csv")
  domains_included <-  c("Chirikof_shelf", "Chirikof_gully", "Chirikof_slope", "Kodiak_shelf", 
                         "Kodiak_gully", "Kodiak_slope", "Shumagin_shelf", "Shumagin_gully", 
                         "Shumagin_slope")
  
  tot_model_area <- sum(strat_areas$area[strat_areas$model==this.model & 
                                         strat_areas$stratum_bin %in% domains_included])
  cpue_dat  <- get_cpue_all(model=this.model)
  check_RACE_codes(cpue_dat)
  
  #stratsum_q <- get_stratsum_q(cpue_dat, q_table)
  #domain_sum_q <- get_domain_sum_q(cpue_dat, q_table)
# Code to do means and sds
  domain_stats <- haul_domain_summary(this.model)
  
  haul_sum <- cpue_dat %>%
    group_by(year, model, race_group, stratum_bin, hauljoin) %>%
    summarize(haul_wgtcpue = sum(wgtcpue),
              haul_numcpue = sum(numcpue),.groups="keep") 
  
  domain_sum <- haul_sum %>%
    group_by(year, model, race_group, stratum_bin) %>%
    summarize(tot_wtcpue  = sum(haul_wgtcpue),
              tot_wtcpue2 = sum(haul_wgtcpue * haul_wgtcpue),
              .groups="keep") %>%
    left_join(domain_stats, by=c("year","model","stratum_bin")) %>%
    mutate(mean_wtcpue   = tot_wtcpue/stations,
           var_wtcpue    = tot_wtcpue2/stations - mean_wtcpue*mean_wtcpue,
           bio_t_km2     = mean_wtcpue/1000,
           bio_tons      = bio_t_km2 * area,
           var_bio_t_km2 = var_wtcpue/(1000 * 1000),
           var_bio_tons  = var_wtcpue * (area/1000) * (area/1000))

  model_sum <- domain_sum %>%
    group_by(year, model, race_group) %>%
    summarize(bio_tons     = sum(bio_tons),
              var_bio_tons = sum(var_bio_tons),
              .groups="keep") %>%
    mutate(model_area       = tot_model_area,
           bio_tons_km2     = bio_tons/model_area,
           var_bio_tons_km2 = var_bio_tons/(model_area*model_area),
           sd_bio_tons      = sqrt(var_bio_tons),
           cv_bio           = sd_bio_tons/bio_tons)
  
    
# Following code does means but not sds  
#  domain_sum <- cpue_dat %>%
#    group_by(year, model, race_group, stratum_bin) %>%
#    summarize(wgtcpue = sum(wgtcpue),
#              numcpue = sum(numcpue),.groups="keep") %>%
#    left_join(haul_domain_summary(this.model), by=c("year","model","stratum_bin")) %>%
#    mutate(bio_t_km2 = wgtcpue/1000/stations,
#           bio_tons  = bio_t_km2 * area)
#  
#  model_sum <- domain_sum %>%
#    group_by(year, model, race_group) %>%
#    summarize(tot_bio_tons = sum(bio_tons),
#              tot_bio_tkm2 = sum(bio_tons)/tot_model_area,
#      .groups="keep")
    
  write.csv(model_sum, "apps/ESR_guilds/WGOA_groundfish_bio.csv",row.names=F)
  
  
#cpue_test <- cpue_dat %>%
#  filter(year==1982 & stratum==10 & race_group =="ak_plaice")    
# These two lines then sum stratsum to the total biomass and biomass density
# for the entire model area
#model_area <- sum(strata_lookup$area[strata_lookup$model==this.model])
#bio_totals <- stratsum %>%
#  group_by(year, model, race_group) %>%
#  summarize(bio_tons = sum(bio_tons),
#            bio_tkm2 = bio_tons/model_area, .groups="keep")

# IF Looping through multiple ecosystems, need to save or rbind bio_totals
# at the end of each loop here.
#}

#write.csv(bio_totals,"apps/ESR_guilds/bio_totals_2024test.csv",row.names=F)
