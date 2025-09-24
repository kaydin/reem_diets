
# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")
library("dplyr")

####################################################################

source("R/REEM_fooddata_functions.R")

# Load all Race data and perform some preliminary cleanups/calculations
  REEM.loadclean.RACE(path = "data/local_racebase")

# Load strata table (linking models to strata)
  REEM.loadclean.strata(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                        stratum_bin_column    = "strat_groups")

# Load lookup file for mapping RACE codes to biomass. Later need to specify
# which column to use for group mapping (varies by ecosystem)
  race_lookup_base <- read.clean.csv("apps/ESR_guilds/species_test_join.csv")

#race_lookup_col <- c("EBS"="ebs_ecopath", "AI"="ai_ecopath", "WGOA"= "goa_ecopath")

##############################################################################
# Biomass extraction by item
#for (this.model in c("EBS")){  #c("EBS","NBS","EGOA","WGOA","AI")
source("R/REEM_fooddata_functions.R")
  
  
get_stratsum_q <- function(cpuedat, q_table){
  stratsum <- cpuedat %>%
    group_by(year, model, race_group, stratum) %>%
    summarize(wgtcpue = sum(wgtcpue),
              numcpue = sum(numcpue),.groups="keep") %>%
    left_join(haul_stratum_summary(this.model), by=c("year","model","stratum")) %>%
    mutate(bio_t_km2 = wgtcpue/1000/stations,
           bio_tons  = bio_t_km2 * area)
  
  stratsum_q <- stratsum %>%
    left_join(q_table,by=c("race_group"="group")) %>%
    mutate(bio_tons_q  = bio_tons  *qq,
           bio_t_km2_q = bio_t_km2 *qq)  

  return(stratsum_q)
}
  ##############################################################################
  get_domain_sum_q <- function(cpuedat, q_table){
  
    domain_sum <- cpuedat %>%
      group_by(year, model, race_group, stratum_bin) %>%
      summarize(wgtcpue = sum(wgtcpue),
                numcpue = sum(numcpue),.groups="keep") %>%
      left_join(haul_domain_summary(this.model), by=c("year","model","stratum_bin")) %>%
      mutate(bio_t_km2 = wgtcpue/1000/stations,
             bio_tons  = bio_t_km2 * area)
    
    domain_sum_q <- domain_sum %>%
      left_join(q_table,by=c("race_group"="group")) %>%
      mutate(bio_tons_q  = bio_tons  *qq,
             bio_t_km2_q = bio_t_km2 *qq)  
    
    return(domain_sum_q)
  } 
  
  ##############################################################################  
#  get_cpue_all() mirrors the RACE get_cpue function (returning by haul), except it gets
# biomasses for all groups at once, binned by the race_lookup names (race_group column)
# haul_stratum_summary() gets the total stations and area for each model domain.
#
# Note: BS Survey filter (for slope stations) moved to core loading code

# EBS GUILDS ------------------------------------------------
  this.model  <- "EBS"

  race_lookup      <- race_lookup_base %>% mutate(race_group  = .data[["ebs_ecopath"]])  
  q_table          <- read.clean.csv("apps/ESR_guilds/GroupQ_EBS_2022.csv")
  domains_included <- c("SE_inner","NW_inner","SE_middle","Pribs","NW_middle", "StMatt", "SE_outer", "NW_outer")
  
  # fix big sandlance in 1982, vessel 1, haul 24 ---------- ----------
  # here's the problem: each sandlance weighs 0.9 kg
  big_sandlance <- catch %>% filter(catch$vessel==1 & catch$cruise==198203 &
                                      catch$haul==24 & catch$hauljoin==880 & 
                                      catch$species_code==20202); big_sandlance
  # here's the fix: Original data sheet shows total sandlance wt should be 0.8 kg
  # source: email with Duane Stevenson October 4, 2024
  catch <- catch %>% mutate(weight = replace(weight,
                                             vessel==1 & cruise==198203 & 
                                               haul==24 & hauljoin==880 & 
                                               species_code==20202,
                                             0.8))
  # Did the fix work?
  catch %>% filter(catch$vessel==1 & catch$cruise==198203 & catch$haul==24 & 
                     catch$hauljoin==880 & catch$species_code==20202)
  
  cpue_dat  <- get_cpue_all(model=this.model)
  check_RACE_codes(cpue_dat)

  #stratsum_q <- get_stratsum_q(cpue_dat, q_table)
  domain_sum_q <- get_domain_sum_q(cpue_dat, q_table)
  
  guild_bio_table <-domain_sum_q %>%
    filter(stratum_bin %in% domains_included) %>%
    group_by(year,guild) %>%
    summarize(bio_tons_q_tot = sum(bio_tons_q), .groups="keep") %>%
    spread(guild,bio_tons_q_tot,fill=0)

  write.csv(domain_sum_q, "apps/ESR_guilds/EBS_domain_sum_2025.csv",row.names=F)
  # write.csv(guild_bio_table, "apps/ESR_guilds/EBS_guild_bio_2025.csv",row.names=F)

  ebs_esr_guild_names <- colnames(guild_bio_table)[c(1:3,6:7)] # EBS ESR report card guild names
  ebs_esr_guilds <- guild_bio_table[,ebs_esr_guild_names] # select report card guilds
  # convert to 1,000 t
  ebs_esr_guilds[, ebs_esr_guild_names[-1]] <- ebs_esr_guilds[, ebs_esr_guild_names[-1]]/1000
  write.csv(ebs_esr_guilds, "apps/ESR_guilds/EBS_ESR_guilds_2025.csv",row.names=F)
  
  
# AI GUILDS -----------------------------------------------
  this.model  <- "AI"  
  race_lookup      <- race_lookup_base %>% mutate(race_group  = .data[["ai_ecopath"]])  
  q_table          <- read.clean.csv("apps/ESR_guilds/GroupQ_2018_AI.csv")
  domains_included <-  c("Eastern_AI","Central_AI","Western_AI")
  
  cpue_dat  <- get_cpue_all(model=this.model)
  check_RACE_codes(cpue_dat)
  
  #stratsum_q <- get_stratsum_q(cpue_dat, q_table)
  domain_sum_q <- get_domain_sum_q(cpue_dat, q_table)
  
  guild_bio_table <-domain_sum_q %>%
    filter(stratum_bin %in% domains_included) %>%
    group_by(year,guild) %>%
    summarize(bio_tons_q_tot = sum(bio_tons_q), .groups="keep") %>%
    spread(guild,bio_tons_q_tot,fill=0)
  
  write.csv(domain_sum_q, "apps/ESR_guilds/AI_domain_sum_2024.csv",row.names=F)
  
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
