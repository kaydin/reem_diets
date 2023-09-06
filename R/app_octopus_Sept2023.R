
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
REEM.loadclean.lookups(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                       stratum_bin_column    = "strat_groups",
                       preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv",
                       prey_guild_column     = "octo_2023")

##############################################################################
## OCTOPUS ASSESSMENT ANALYSIS 2023

source("R/REEM_fooddata_functions.R")

# Standard REEM predator length categories were reduced due to missing data, 
# and lack of octopus consumption at smaller size classes.
#pred_params <- list("P.cod" = list(nodc="8791030401", race="21720", LCLASS=c(0,10,30,60,85,999) ))
pred_params <- list("P.cod" = list(nodc="8791030401", race="21720", LCLASS=c(0,40,60,150) ))
this.pred  <- "P.cod"
this.model <- "EBS"

# set up len-weight regression parameters and consumption weighting
lwp <- get_lw(predator=this.pred, model=this.model, years=1987:2022, all.data=F) 
  pred_params[[this.pred]]$lw_a  = lwp$lw_a
  pred_params[[this.pred]]$lw_b  = lwp$lw_b

# Consumption scaling (temperature-dependent) currently uses Wisconsin
# model eq 2 parameters for Cmax (per day) and temperature corrections.  
# To use a fixed proportion of body weight (biomass), set CA = desired daily proportion of body weight
# (or Ecopath annual QB/365) and CB=0, C_TM=100, C_T0=-100, C_Q=1.000000001
# If you're using this for diet proportions not total consumption, can simply
# use 1.0 for CA.
  
# Best params if switching to bioenergetics method
  pred_params[[this.pred]]$bioen = list(ref="P.cod.old", CA=0.041,     CB= -0.122,  C_TM=21,  C_T0=13.7,  C_Q=2.41)
#  pred_params[[this.pred]]$bioen = list(ref="cod.qb"   , CA=1.39/365,  CB=0,  C_TM=100,  C_T0=-100,  C_Q=1.000000001)

# Generalized Von B parameters from 2016 ADMB estimation (old parameters)
# includes estimation outputs (sigma and f).  Note "generalized" Von B K
# is a different parameter/scale from the specialized (typical) Von B K.
  vonb  <- list (Winf=21448.1, K=0.57325, t0=-0.495495, dexp=0.742355,
                 sigma=0.4564,H=7.48695,f=-3824.7)

# Important note: as per original 2016 assessment, consumption estimate is for
# "assimilated" consumption only (consumption that turns into cod biomass).
# This is a very conservative estimate - generally this can be converted
# to biomass consumed by dividing by conversion efficiency (typically 0.6
# for coldwater gadids).
  
# Get consumption per lengthclass per haul, removing "generic"
# von-B calculations and adding more specific values, using von B consumption
# formula Cons(annual, grams) = h * W^d, where W is a*L^b in grams.
  
conslens <- get_cpue_length_cons(predator=this.pred, model=this.model) %>%
            select(-cons_vonb_rel_g,-cons_vonb_rel_pop) %>%
            mutate(cons_g_assim = vonb$H * body_wt^vonb$dexp,
                   cons_assim_t_km2 = NumLBin_CPUE_km2 * cons_g_assim / 1.0e6)

# Sum individual body lengths up to diet length categories
haul_sum <- conslens %>%
  group_by(year,model,stratum_bin,species_name, hauljoin, 
           stationid, stratum, lat, lon, bottom_temp, surface_temp, 
           lbin) %>%
  summarize(tot_wtcpue_t_km2       = mean(wgtcpue)/1000,
            tot_wlcpue_t_km2       = sum(WgtLBin_CPUE_kg_km2)/1000,
            f_t                    = mean(f_t),
            tot_cons_bioen_t_km2   = sum(cons_kg_km2_bioen)/1000,
            tot_cons_assim_t_km2   = sum(cons_assim_t_km2),
            .groups="keep")

# Get diet tables for Pcod
  diet <- predprey_tables(predator=this.pred, model=this.model)

# Combine diets with length-specific consumption rates and sum
len_diet <- haul_sum %>%
  left_join(diet, by=c("species_name"="predator", "model", "stratum_bin", "year", "lbin")) %>%
  replace_na(list(prey_guild="MISSING", dietprop_wt=1.0,dietprop_sci=1.0)) %>%
  mutate(preycons_sci_bioen_t_km2 = tot_cons_bioen_t_km2 * dietprop_sci,
         preycons_sci_assim_t_km2 = tot_cons_assim_t_km2 * dietprop_sci)

diet_sum <- len_diet %>%
  group_by(year,model,stratum_bin,species_name,lbin,prey_guild) %>%
  summarize(strat_bioen_preycons_t_km2 = mean(preycons_sci_bioen_t_km2), 
            strat_assim_preycons_t_km2 = mean(preycons_sci_assim_t_km2),
            .groups="keep") %>%
  left_join(strat_areas,by=c("model","stratum_bin")) %>%
  mutate(cons_bioen_tons_day  = strat_bioen_preycons_t_km2 * area,
         cons_assim_tons_year = strat_assim_preycons_t_km2 * area)


write.csv(diet_sum,"results/diet_octopus_origlc.csv",row.names=F)


######################################
######################################

