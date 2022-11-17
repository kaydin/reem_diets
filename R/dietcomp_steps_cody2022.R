
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
                       prey_guild_column     = "ecopath_prey")

pred_params <- list(
    "P.cod"        = list(nodc="8791030401", race="21720", LCLASS=c(0,10,30,60,85,999) ) #,
    )


##############################################################################
## CODY ANALYSIS 2022

source("R/REEM_fooddata_functions.R")

this.pred  <- "P.cod"
this.model <- "EBS"

# set up len-weight regression parameters and consumption weighting
lwp <- get_lw(predator=this.pred, model=this.model, years=1982:2021, all.data=F) 
  pred_params[[this.pred]]$lw_a  = lwp$lw_a
  pred_params[[this.pred]]$lw_b  = lwp$lw_b

# Consumption scaling (temperature-dependent) currently uses Wisconsin
# model eq 2 parameters for Cmax (per day) and temperature corrections.  
# To use a fixed proportion of body weight (biomass), set CA = desired daily proportion of body weight
# (or Ecopath annual QB/365) and CB=0, C_TM=100, C_T0=-100, C_Q=1.000000001
# If you're using this for diet proportions not total consumption, can simply
# use 1.0 for CA.
  pred_params[[this.pred]]$bioen = list(ref="P.cod.old", CA=0.041,     CB= -0.122,  C_TM=21,  C_T0=13.7,  C_Q=2.41)
  pred_params[[this.pred]]$bioen = list(ref="cod.qb"   , CA=1.39/365,  CB=0,  C_TM=100,  C_T0=-100,  C_Q=1.000000001)

pred_params[[this.pred]]    

conslens <- get_cpue_length_cons(predator=this.pred, model=this.model)


# Sum individual body lengths up to diet length categories
haul_sum <- conslens %>%
  group_by(year,model,stratum_bin,species_name, hauljoin, 
           stationid, stratum, lat, lon, bottom_temp, surface_temp, 
           lbin) %>%
  summarize(tot_wtcpue_t_km2  = mean(wgtcpue)/1000,
            tot_wlcpue_t_km2 = sum(WgtLBin_CPUE_kg_km2)/1000,
            f_t              = mean(f_t),
            tot_cons_t_km2   = sum(cons_kg_km2)/1000,
            .groups="keep")


diet <- predprey_tables(predator=this.pred, model=this.model, months=5:8)

len_diet <- haul_sum %>%
  left_join(diet, by=c("species_name"="predator", "model", "stratum_bin", "year", "lbin")) %>%
  replace_na(list(prey_guild="MISSING", dietprop_wt=1.0,dietprop_sci=1.0)) %>%
  mutate(preycons_sci_t_km2 = tot_cons_t_km2 * dietprop_sci)

diet_sum <- len_diet %>%
  group_by(year,model,stratum_bin,species_name,lbin,prey_guild) %>%
  summarize(strat_preycons_t_km2 = mean(preycons_sci_t_km2), .groups="keep") %>%
  left_join(strat_areas,by=c("model","stratum_bin")) %>%
  mutate(cons_tons_day = strat_preycons_t_km2 * area)

#write.csv(diet_sum,"diet2new.csv",row.names=F)

# alpha_wt_m
prey_a <- 0.000267
# beta_wt_m
prey_b <- 3.097253

preylen_freqs <- preylength_splits(pred_params[[this.pred]]$nodc, 
                          c("6187010300", "6187010301"), # Opilio and Unid Chion
                          pred_params[[this.pred]]$LCLASS, 
                          c(0,30,95,999),
                          model="EBS") %>%
        mutate(prey_wt_g  =  prey_a * prey_size_mm^prey_b,
               prey_sum_g = freq * prey_wt_g) %>%
        group_by(model,year,pred_lbin_cm,prey_lbin_mm) %>%
        summarize(nprey=sum(freq),
                  wtprey=sum(prey_sum_g),
                  .groups="keep") %>%
        group_by(model,year,pred_lbin_cm) %>%
        mutate(tot_n=sum(nprey),
               tot_w=sum(wtprey)) %>%
        ungroup() %>%
        mutate(n_prop  = nprey/tot_n,
               wt_prop = wtprey/tot_w)

diet_prey <- diet_sum %>%
  filter(prey_guild=="Opilio") %>%
  left_join(preylen_freqs, by=c("model","year","lbin"="pred_lbin_cm")) %>%
  mutate(cons_prey_tons_day = cons_tons_day * wt_prop)

write.csv(diet_prey, "diet_prey.csv",row.names=F)

#test.new <- fc_T_eq2(seq(-1,30,0.2), bioen_pars[["P.cod.new"]])
#test.old <- fc_T_eq2(seq(-1,30,0.2), bioen_pars[["P.cod.old"]])

#end cody query
######################################
######################################

