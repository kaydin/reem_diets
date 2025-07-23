#------------------------------------------------------------------------------#
# AUTHORS: This script originally written by Bia Dias for the WGOA. Modified here
# by Andy Whitehouse for the SEBS of the AI survey. NOTE this script is not yet
# set up to output stanza biomass.
# AFFILIATIONS: CICOES University of Washington/ Alaska Fisheries Science Center
# E-MAIL OF CORRESPONDENCE AUTHOR: andy.whitehouse@noaa.gov
#
# script to download race Biomass from the Bottom Trawl Survey (BTS) data
# Original code from reem_diets repo
# Location of original file: reem_diets/apps/ESR_guilds/ESR_guilds_GOA_EwE.R
#
# WARNING: this script will not run without the googledrive subfolders or locally
# stored copy of data. This could be written to directly link with oracle.
#------------------------------------------------------------------------------#

# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")

################################################################################

source("R/REEM_fooddata_functions.R")

# Load all Race data and perform some preliminary cleanups/calculations
REEM.loadclean.RACE(path = "data/local_racebase")

# Load strata table (linking models to strata)
REEM.loadclean.strata(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                      stratum_bin_column    = "strat_groups")

REEM.loadclean.lookups()

# ──────────────────────────────────────────────────────────────────────────────
# Before: race_lookup_col was already defined as:
race_lookup_col <- c(
  "EBS"  = "ebs_aclim3", #here you have to see if the lookup table matches the name. We might need an updated lookup table. 
  # "AI"   = "ai_ecopath",
  "AI"   = "ai_sebs_aclim3", # lookup specific to AI SEBS strata for use with EBS aclim3 Rpath
  "WGOA" = "final_wgoa",
  "EGOA" = "final_egoa"
)
# ──────────────────────────────────────────────────────────────────────────────

# Specification of what model to run. Change this to "EBS", "AI", "WGOA", or "EGOA".
this.model <- "AI"

# Pick the correct lookup‐column name
lookup_col_name <- race_lookup_col[[this.model]]
# e.g. if this.model <-  "WGOA", lookup_col_name = "final_wgoa"
# this.model <- "EBS", lookup_col_name = "ebs_ecopath"

# Load & clean the base RACE lookup table
race_lookup_base <- read.clean.csv("lookups/race_lookup_base_v3.csv")

# Region‐specific "race_group" column by pulling from the right field
race_lookup <- race_lookup_base %>%
  mutate(
    # .data[[lookup_col_name]] evaluates to whatever column is named "final_wgoa" 
    # or "ebs_ecopath", depends on this.model
    race_group = .data[[ lookup_col_name ]]
  )

# NOTE: should not need Q file to generate survey time series for fitting. -gaw
# Here we should have the GroupQ files per model, let's keep this naming for simplicity
# e.g "lookups/GroupQ_2021_WGOA.csv" and "lookups/GroupQ_2021_EBS.csv"
# q_table <- read.clean.csv(
#   paste0("lookups/GroupQ_2021_", this.model, ".csv")
# )

# Build stratum‐level biomass ####
stratsum <- get_cpue_all(model = this.model) %>%
  group_by(year, model, race_group, stratum, hauljoin) %>%
  summarize(
    haul_wgtcpue = sum(wgtcpue),
    haul_numcpue = sum(numcpue),
    .groups = "keep"
  ) %>%
  group_by(year, model, race_group, stratum) %>%
  summarize(
    tot_wtcpue  = sum(haul_wgtcpue),
    tot_wtcpue2 = sum(haul_wgtcpue * haul_wgtcpue),
    .groups = "keep"
  ) %>%
  left_join(
    haul_stratum_summary(this.model),
    by = c("year", "model", "stratum")
  ) %>%
  mutate(
    n_stations     = sum(stations),
    mean_wtcpue    = tot_wtcpue / n_stations,
    # Division by n-1 same as used in the GAP method
    var_wtcpue     = (tot_wtcpue2 - ((tot_wtcpue^2) / n_stations)) / (n_stations - 1),
    varest_wtcpue  = ifelse(is.na(var_wtcpue), 0, var_wtcpue / n_stations),
    # Units of wtcpue as returned by get_cpue_all() are kg/km2, convert to t/km2
    # by dividing by 1000, then scale up to tons by multiplying by stratum area.
    # Scale up to tons by stratum area
    bio_tons       = mean_wtcpue * area / 1000,
    varest_tons    = varest_wtcpue * area^2 / (1000 * 1000)
  )

# Summing these means and variances should match outputs from GAP method
# prior to applying juvenile/adult calculations
domains_included <-  c("Eastern_AI")
model_area <- sum(strata_lookup$area[strata_lookup$model == this.model &
                    strata_lookup$stratum_bin %in% domains_included])

ai_sebs_stratsum <- stratsum %>%
  filter(stratum_bin == "Eastern_AI")

bio_totals <- ai_sebs_stratsum %>%
  group_by(year, model, race_group) %>%
  summarize(
    tot_bio_tons    = sum(bio_tons),
    tot_varest_tons = sum(varest_tons),
    .groups = "keep"
  ) %>%
  mutate(
    bio_tons      = tot_bio_tons,
    bio_tkm2      = bio_tons / model_area,
    se_tkm2       = sqrt(tot_varest_tons) / model_area,
    se_tons       = sqrt(tot_varest_tons),
    var_bio_tons  = tot_varest_tons,
    var_bio_t_km2 = tot_varest_tons / (model_area^2),
    cv            = se_tons / bio_tons
  ) %>%
  select(-tot_bio_tons, -tot_varest_tons)
# write.csv(bio_totals, "C:/Users/andy.whitehouse/Work/Andy/REEM/Eco_Considerations/Contributions/Guilds/EBS/2024/bio_totals_test.csv")
write.csv(bio_totals, "apps/ESR_guilds/AI-sebs_bio_totals.csv",row.names=F)


# a few formatting details -----------------------------------------------------
# add scale column for fitting script (scale = 1/model_area)
bio_totals$scale <- rep(1/533102.49,dim(bio_totals)[1])
# select only the columns from bio_summary2 that are needed for fitting
ai_sebs_fitting_index <- bio_totals[,c(1,3,4,7,10,11)]
# Add a type column
ai_sebs_fitting_index["Type"] <- NA
# Add SE column
ai_sebs_fitting_index["SE"] <- NA
# Add Species column
ai_sebs_fitting_index["Species"] <- NA
# Add Loc column
ai_sebs_fitting_index["Loc"] <- NA
# Add n column
ai_sebs_fitting_index["n"] <- NA
# Add source column
ai_sebs_fitting_index["Source"] <- "ai_sebs_race"
# Add column names as used in fitting files
colnames(ai_sebs_fitting_index) <- c("Year","Group","Value","Stdev","CV","Scale",
                                       "Type","SE","Species","Loc","n","Source")
# Reorganize columns
ai_sebs_fitting_index <- ai_sebs_fitting_index[,c(1,2,7,4,8,3,6,5,9:12)]
# output file
write.csv(ai_sebs_fitting_index,
          "C:/Users/andy.whitehouse/Work/Andy/REEM/ACLIM/ACLIM_3/ai_sebs_fitting_index.csv",
          row.names = FALSE)


