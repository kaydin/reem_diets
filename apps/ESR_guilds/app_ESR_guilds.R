
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

# Load lookup tables and create summary lookups - re-run this line 
# if lookups change.

# Load Lookup (file and column) for aggregating strata (ecosystem model and stratum numbers as primary key)
# Load Lookup (file and column) for diet items (with NODC code as primary key)

REEM.loadclean.strata(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                       stratum_bin_column    = "strat_groups")

# Lookup file and column for mapping RACE codes to biomass
# Primary key is RACE codes, race lookup names should match
# names in prey and predator lookups
race_lookup <- read.clean.csv("lookups/goa_race_lookup_apr_04_2023.csv") %>% 
  mutate(race_guild  = .data[["ecopath_name"]])

##############################################################################
# Biomass extraction by item
#
# NOTE:  race_guild refers to the Ecopath model (or other chosen column) 
# groupings, not the "final" guilds

missing_codes <- sort(unique(catch$species_code[which(!(catch$species_code %in% race_lookup$species_code))]))
if(length(missing_codes)>0){
  message("The following RACE species codes are not in the local RACE lookup table:")
  print(missing_codes)
  message("From raw RACE species table:")
  print(species[species$species_code %in% missing_codes,])
  message("From raw RACE species_classification table:")
  print(species_classification[species_classification$species_code %in% missing_codes,])
}

for (this.model in c("EBS")){  #c("EBS","NBS","EGOA","WGOA","AI")

# get_cpue_all() mirrors the RACE get_cpue function (returning by haul), except it gets
# biomasses for all groups at once, binned by the race_lookup names (race_guild column)
# haul_stratum_summary() gets the total stations and area for each model domain.
#
# This is filtered here by survey_id from the v_cruises table to filter out 
# EBS slope stations mainly.
  #survey_ids <- c("EBS"=98, "NBS"=143, "WGOA"=47, "EGOA"=47, "AI"=78)
  cpue_dat <- get_cpue_all(model=this.model) %>%
    filter(survey_id==survey_ids[this.model])

#old filter for slope - don't use
# The filter tries to eliminate slope species by keeping only lines that
# have a stationid that starts with a letter (EBS) or is an NA (NBS), so
# discarding stationids that start with a number.  This seems to eliminate
# the 2000-onward slope stations, but not sure about 1988 or 1991.
#  bs_cpue_dat <- rbind(get_cpue_all(model="EBS"),get_cpue_all(model="NBS")) %>%
#    filter(grepl("^[A-Za-z]", stationid) | is.na(stationid))

# Check for RACE codes missing a guild assignment on the local lookoup table
  cpue_code_test <- bs_cpue_dat %>%
    filter(is.na(race_guild) & wgtcpue>0.0)
  missing_guilds <- sort(unique(cpue_code_test$species_code))
  if(length(missing_guilds)>0){
    message("The following RACE codes have biomass in this model ecosystem, but no guild assigned:")
    print(missing_guilds)
    message("From raw RACE species table:")
    print(species[species$species_code %in% missing_guilds,])
    message("From raw RACE species_classification table:")
    print(species_classification[species_classification$species_code %in% missing_guilds,])  
  }
  
# This code groups the two together at the stratum level to get stratum biomass
# in both tons and t/km^2
  stratsum <- cpue_dat %>%
    group_by(year, model, race_guild, stratum) %>%
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
  
  # IF Looping through multiple ecosystems, need to save or rbind bio_totals
  # at the end of each loop here.
}

write.csv(bio_totals,"apps/ESR_guilds/bio_totals_test.csv",row.names=F)
write.csv(stratsum, "apps/ESR_guilds/stratsum_test.csv",row.names=F)

