
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

##############################################################################
## OCTOPUS ASSESSMENT ANALYSIS 2023

setwd("apps/octopus")

# Load lookup tables and create summary lookups - re-run this line 
# if lookups change.

REEM.loadclean.lookups(strata_lookup_file    = "./combined_BTS_strata.csv",
                       stratum_bin_column    = "octopus_2016",
                       preynames_lookup_file = "./Alaska_PreyLookup_MASTER.csv",
                       prey_guild_column     = "octo_2023")

#source("R/REEM_fooddata_functions.R")

# Standard REEM predator length categories were reduced due to missing data, 
# and lack of octopus consumption at smaller size classes.
#pred_params <- list("P.cod" = list(nodc="8791030401", race="21720", LCLASS=c(0,10,30,60,85,999) ))
pred_params <- list("P.cod" = list(nodc="8791030401", race="21720", LCLASS=c(0,40,60,150) ))
this.pred  <- "P.cod"
this.model <- "EBS"

# set up len-weight regression parameters and consumption weighting
lwp <- get_lw(predator=this.pred, model=this.model, years=1987:2023, all.data=F) 
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

strat_sum <- haul_sum %>%
  group_by(year,model,stratum_bin,species_name,lbin) %>%
  summarize(tot=n(), .groups="keep")

#setwd("../.."); source("R/REEM_fooddata_functions.R"); setwd("apps/octopus")
all_hauls <- haul_list(this.model) 
all_lbin <- unique(haul_sum$lbin)

all_haul_len <- NULL
for (LL in all_lbin){
  all_haul_len <- rbind(all_haul_len, all_hauls %>% mutate(lbin=LL))
}

all_haul_sum <- all_haul_len %>%
  left_join(haul_sum,by=c("model","stratum_bin","stratum","year","hauljoin","lbin")) %>%
  replace_na(list(species_name=this.pred, tot_cons_assim_t_km2=0.0)) 

haul_bio_stats <- all_haul_sum %>%
  group_by(model,stratum_bin,year,lbin) %>%
  left_join(strat_areas,by=c("model","stratum_bin")) %>%
  summarize(stations=n(), 
            tot_cons_t_mean  = mean(area*tot_cons_assim_t_km2),
            tot_cons_t_sd    = sd(area*tot_cons_assim_t_km2),
            tot_cons_t_sderr = tot_cons_t_sd/sqrt(stations), 
            .groups="keep")
  


# # Get diet tables for Pcod
#   min_sample <- 10
# 
#   diet <- predprey_tables(predator=this.pred, model=this.model) %>%
#     filter(pred_n >= min_sample)
# 
# # Combine diets with length-specific consumption rates and sum
# len_diet <- haul_sum %>%
#   left_join(diet, by=c("species_name"="predator", "model", "stratum_bin", "year", "lbin")) %>%
#   replace_na(list(prey_guild="MISSING", dietprop_wt=1.0,dietprop_sci=1.0)) %>%
#   mutate(preycons_sci_bioen_t_km2 = tot_cons_bioen_t_km2 * dietprop_sci,
#          preycons_sci_assim_t_km2 = tot_cons_assim_t_km2 * dietprop_sci)
# 
# diet_sum <- len_diet %>%
#   group_by(year,model,stratum_bin,species_name,lbin,prey_guild) %>%
#   summarize(strat_bioen_preycons_t_km2 = mean(preycons_sci_bioen_t_km2), 
#             strat_assim_preycons_t_km2 = mean(preycons_sci_assim_t_km2),
#             .groups="keep") %>%
#   left_join(strat_areas,by=c("model","stratum_bin")) %>%
#   mutate(cons_bioen_tons_day  = strat_bioen_preycons_t_km2 * area,
#          cons_assim_tons_year = strat_assim_preycons_t_km2 * area)
# 
# 
# 
# write.csv(diet_sum,"diet_octopus_origlc_minsample.csv",row.names=F)


# Get diet tables for Pcod
diet_all <- predprey_tables(predator=this.pred, model=this.model,all.data=T)

# Crosstab query, hardcoded to "Octopus" and "Other"
all_dat <- diet_all$predprey_table %>%
  pivot_wider(names_from=prey_guild, values_from=prey_wt, values_fill=0.0) %>%
  mutate(oct_sci =  Octopus/bodywt, 
         tot_sci = (Octopus+Other)/bodywt)

lc_list    <- sort(unique(diet_all$predprey_table$lbin))
year_list  <- sort(unique(diet_all$predprey_table$year))
strat_list <- sort(unique(diet_all$predprey_table$stratum_bin))

min_sample <- 10

beta_table <- NULL
for (LL in lc_list){ #LL <- "[40,60)"
  for (YY in year_list){ #YY <- 1994
    for (SS in strat_list){ #SS <- "MiddleNW"

      dat <- all_dat %>%
        filter(lbin==LL & year==YY & stratum_bin==SS)

      n <- nrow(dat)

      cat(LL,YY,SS,n,"\n"); flush.console()
      
      if(n >= min_sample){
        samples <- 10000
        sdat <- rep(NA,samples)

        for (i in 1:samples){
          pick <- sample.int(n, replace=T)
          sdat[i] <- sum(dat$oct_sci[pick])/sum(dat$tot_sci[pick])
        }
   
        mu    <- mean(sdat)
        sig2  <- var(sdat)
      
        if(mu==0){
          alpha <- 1
          beta  <- Inf
        } else {
          alpha <- mu * mu * ( (1-mu)/sig2 - 1/mu ) 
          beta  <- alpha * ( 1/mu - 1)
        }
      }
      else {
        mu <- sig2 <- alpha <- beta <- NA
      }
      out <- data.frame(lbin=LL, year=YY, stratum_bin=SS, pred_n=n, 
                  mu=mu, sig2=sig2, alpha=alpha, beta=beta)
      beta_table <- rbind(beta_table,out)
        #plot(ecdf(sdat),xlim=c(0,.1))
        #lines(seq(0,.1,0.001),pbeta(seq(0,.1,0.001),alpha,beta))
    }
  }
}

cons_bio_stats <- haul_bio_stats %>%
  left_join(beta_table, by=c("year","stratum_bin","lbin")) %>%
  filter(!is.na(stratum_bin)) %>%
  replace_na(list(alpha=0,beta=Inf))

cons_summary <- NULL
for (YY in year_list){ #YY <- 2010
  cat(YY,"\n"); flush.console()
  stat_table <- cons_bio_stats %>% filter(year==YY)
  nstrat  <- nrow(stat_table)
  samples <- 1000000
  dat <- matrix(NA,samples,nstrat)
  for (i in 1:nstrat){
    cmean <- stat_table$tot_cons_t_mean[i]
    csd   <- stat_table$tot_cons_t_sderr[i]
    alpha <- stat_table$alpha[i]
    beta  <- stat_table$beta[i]
    sd_log   <- sqrt(log(1+(csd*csd)/(cmean*cmean)))
    mu_log   <- log(cmean) - 0.5 * sd_log * sd_log
    
    dat[,i] <- rlnorm(samples, mu_log, sd_log) * rbeta(samples, alpha, beta)
  }  
  cons_samples <- rowSums(dat)

  cons_summary <- rbind(cons_summary, c(year=YY,
                a_mean = mean(cons_samples),
                g_mean = exp(mean(log(cons_samples))),
                h_mean = 1/(mean(1/cons_samples)),
                quantile(cons_samples, probs= c(0, 0.025, 0.25, 0.50, 0.75, 0.975, 1.0))))
}

cons_summary <- data.frame(cons_summary)

write.csv(cons_summary,"cons_summary_1million_samples.csv",row.names=F)





################################################################################

# Testing some distribution stuff - ignore below this

test <- c(rep(0,99), rep(.001,1))

samples <- 10000
sdat    <- rep(NA,samples) 

for (i in 1:samples){
  sdat[i] = mean(test[sample.int(length(test), replace=T)])
}

mu    <- mean(sdat)
sig2  <- var(sdat)
#if(sig2 > mu*(1-mu))
alpha <- mu * mu * ( (1-mu)/sig2 - 1/mu ) 
beta  <- alpha * ( 1/mu - 1)

plot(ecdf(sdat),xlim=c(0,1))
points(seq(0,1,0.01),pbeta(seq(0,1,0.01),alpha,beta))

######################################
######################################

