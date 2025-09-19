#------------------------------------------------------------------------------#
# AUTHORS: This script originally written by Bia Dias for the WGOA. Modified here
# by Andy Whitehouse for the EBS shelf survey.
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
  "AI"   = "ai_ecopath",
  "WGOA" = "final_wgoa",
  "EGOA" = "final_egoa"
)
# ──────────────────────────────────────────────────────────────────────────────

# Specification of what model to run. Change this to "EBS", "AI", "WGOA", or "EGOA".
this.model <- "EBS"

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
model_area <- sum(strata_lookup$area[strata_lookup$model == this.model])

bio_totals <- stratsum %>%
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

# Read in the predator lookup, then filter by whatever model you chose:
# predlist <- read.clean.csv("lookups/Alaska_Multistanza_GOA_vonb_2025_04_30_v2.csv") #replace this with the EBS file. we can fix this later
predlist <- read.clean.csv("lookups/ebs_multistanza_vonb.csv")
preds     <- predlist %>% filter(model == this.model)
pred_names <- unique(preds$predator)

pred_params <- list()
for (p in pred_names) {
  pdat <- as.list(preds[preds$predator == p, ])
  pred_params[[p]]            <- pdat
  pred_params[[p]]$LCLASS     <- sort(unique(c(0, pdat$juv_cm, pdat$adu_1, pdat$adu_2, pdat$adu_3, 9999)))
  pred_params[[p]]$jsize      <- paste0("[", pred_params[[p]]$LCLASS[1], ",", pred_params[[p]]$LCLASS[2], ")")
  pred_params[[p]]$lw_b       <- pdat$b_l_mm_g
  pred_params[[p]]$lw_a       <- pdat$a_l_mm_g * (10^pdat$b_l_mm_g)
  pred_params[[p]]$bioen      <- list(CA= pdat$ca, CB= pdat$cb, C_TM= pdat$c_tm, 
                                      C_T0= pdat$c_t0, C_Q= pdat$c_q)
  pred_params[[p]]$vonb <- list(h= pdat$vb_k, Linf= pdat$vb_linf_mm, t0= pdat$vb_t0,
                                rec_len= pdat$von_b_rec_len_cm)
}

# Loop through predators and get all the length data, add juv/adu categories
len_dat <- NULL
for (p in pred_names) {
  len_dat <- bind_rows(
    len_dat,
    get_stratum_length_cons(predator = p, model = this.model) %>%
      mutate(jcat = ifelse(lbin == pred_params[[p]]$jsize, "juv", "adu"))
  )
}

# Sum length classes to juv/adu by-haul biomass (biomass from l/w regressions)
juv_adu_lencons <- len_dat %>%
  group_by(year, model, species_name, stratum, hauljoin, jcat) %>%
  summarize(haul_wlcpue_tkm2 = sum(tot_wlcpue_t_km2),.groups = "keep")

# Go to stratum level calculating variance of estimate per stratum
juv_adu_strat <- juv_adu_lencons %>%
  group_by(year, model, species_name, stratum, jcat) %>%
  summarize(
    tot_wtlcpue_tkm2  = sum(haul_wlcpue_tkm2),
    tot_wtlcpue2_tkm2 = sum(haul_wlcpue_tkm2^2),
    .groups = "keep"
  ) %>%
  left_join(haul_stratum_summary(this.model),
            by = c("year", "model", "stratum")) %>%
  mutate(
    n_stations         = sum(stations),
    mean_wtlcpue_tkm2  = tot_wtlcpue_tkm2 / n_stations,
    var_wtlcpue_tkm2   = (tot_wtlcpue2_tkm2 - ((tot_wtlcpue_tkm2^2) / n_stations)) / (n_stations - 1),
    varest_wtlcpue_tkm2 = ifelse(is.na(var_wtlcpue_tkm2), 0, var_wtlcpue_tkm2 / n_stations),
    bio_l_tons         = mean_wtlcpue_tkm2 * area,
    varest_l_tons      = varest_wtlcpue_tkm2 * area^2
  )

# sum to ecosystem level
juvadu_totals <- juv_adu_strat %>%
  group_by(year, model, species_name, jcat) %>%
  summarize(
    bio_tons    = sum(bio_l_tons),
    varest_tons = sum(varest_l_tons),
    se_tons     = sqrt(varest_tons),
    bio_tkm2    = bio_tons / model_area,
    se_tkm2     = se_tons / model_area,
    cv          = se_tons / bio_tons,
    .groups = "keep"
  ) %>%
  rename(race_group = species_name)

# Merge juv/adu totals with stratum totals exactly as before

## AT THIS POINT, ALL CALCULATIONS SHOULD BE DONE, Everything else is
## renaming and formatting.  
## We want to use bio_tons for Value and se_tons for StDev read into Rpath
## (or if preferred bio_tkm2 and se_tkm2, but keeping it in tons is easier to
## compare to other data sources.

stratsum_jad <- bio_totals %>%
  left_join(juvadu_totals, by = c("year", "model", "race_group"))

strata_long <- stratsum_jad %>%
  mutate(
    race_group = case_when(
      !is.na(jcat) & jcat == "juv" ~ paste0(race_group, "_juv"),
      !is.na(jcat) & jcat == "adu" ~ paste0(race_group, "_adu"),
      TRUE                         ~ race_group
    ),
    bio_tons     = if_else(!is.na(jcat), bio_tons.y, bio_tons.x),
    se_tons      = if_else(!is.na(jcat), se_tons.y, se_tons.x),
    bio_tkm2     = if_else(!is.na(jcat), bio_tkm2.y, bio_tkm2.x),
    se_tkm2      = if_else(!is.na(jcat), se_tkm2.y, se_tkm2.x),
    cv           = if_else(!is.na(jcat), cv.y, cv.x),
    var_bio_tons = if_else(!is.na(jcat), varest_tons, var_bio_tons)
  ) %>%
  select(
    -ends_with(".x"),
    -ends_with(".y"),
    -varest_tons,
    -jcat
  ) %>%
  filter(!race_group %in% c("ZERO", "MISC_NA", "MISC_SHELLS"))

# Finally, produce your output CSVs.  
# This is the final output, which will be read into Rpath
bio_summary2 <- strata_long %>%
  mutate(total_area = model_area) %>%
  select(
    year, model, total_area, race_group,
    bio_tons, var_bio_tons, se_tons, cv,
    bio_tkm2, var_bio_t_km2, se_tkm2
  )

# a few formatting details -----------------------------------------------------
# add scale column for fitting script (scale = 1/model_area)
bio_summary2$scale <- rep(1/533102.49,dim(bio_summary2)[1])
# select only the columns from bio_summary2 that are needed for fitting
ebs_fitting_shelf_index <- bio_summary2[,-c(2,3,6,9,10,11)]
# Add a type column
ebs_fitting_shelf_index["Type"] <- NA
# Add SE column
ebs_fitting_shelf_index["SE"] <- NA
# Add Species column
ebs_fitting_shelf_index["Species"] <- NA
# Add Loc column
ebs_fitting_shelf_index["Loc"] <- NA
# Add n column
ebs_fitting_shelf_index["n"] <- NA
# Add source column
ebs_fitting_shelf_index["Source"] <- "ebs_race_shelf"
# Add column names as used in fitting files
colnames(ebs_fitting_shelf_index) <- c("Year","Group","Value","Stdev","CV","Scale",
                                       "Type","SE","Species","Loc","n","Source")
# Reorganize columns
ebs_fitting_shelf_index <- ebs_fitting_shelf_index[,c(1,2,7,4,8,3,6,5,9:12)]
# output file
# write.csv(ebs_fitting_shelf_index,
#           "C:/Users/andy.whitehouse/Work/Andy/REEM/ACLIM/ACLIM_3/ebs_shelf_fitting_shelf_index.csv",
#           row.names = FALSE)


################################################################################
################################################################################
################################################################################
# This part isspecific to WGOA and EGOA only. 
# If there is any naming conventions for EBS we can write something similar
#if (this.model == "WGOA") {
#  # apply any WGOA‐specific rename, e.g.:
#  bio_summary2 <- bio_summary2 %>%
#    mutate(race_group = case_when(
#      race_group == "Pacific herring" ~ "Pacific herring adult",
#      TRUE                             ~ race_group
#    ))
#}

bio_summary2[, c("Type", "Scale1", "Scale2", "Species", "SE", "Loc", "n", "Source")] <-
  list(NA, 1, 1 / model_area, "", NA, "", "", paste0("race_", tolower(this.model)))

# Now choose where to write:
out_fn_km2  <- paste0(
  tolower(this.model),
  "_data_rpath_fitting/",
  tolower(this.model),
  "_race_biomass_ts_km2.csv"
)
out_fn_tons <- paste0(
  tolower(this.model),
  "_data_rpath_fitting/",
  tolower(this.model),
  "_race_biomass_ts_tons.csv"
)

bio_summary_v2_km2 <- bio_summary2 %>%
  ungroup() %>%
  select(
    Year = year,
    Group = race_group,
    Type, Stdev = se_tkm2, SE = se_tkm2,
    Value = bio_tkm2, Scale = Scale1,
    CV, Species, Loc, n, Source
  )

bio_summary_v2_tons <- bio_summary2 %>%
  ungroup() %>%
  select(
    Year = year,
    Group = race_group,
    Type, Stdev = se_tons, SE = se_tons,
    Value = bio_tons, Scale = Scale2,
    CV, Species, Loc, n, Source
  )

write.csv(bio_summary_v2_km2, out_fn_km2, row.names = FALSE)
write.csv(bio_summary_v2_tons, out_fn_tons, row.names = FALSE)
