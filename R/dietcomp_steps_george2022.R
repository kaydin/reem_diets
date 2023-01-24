
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
                       prey_guild_column     = "troy")

pred_params <- list(
  "P.cod"        = list(nodc="8791030401", race="21720", LCLASS=c(0,10,30,60,85,999),
                        bioen=list(ref="cod2015", CA=0.041, CB= -0.122, C_TM=21, C_T0=13.7, C_Q=2.41)),
  "W.pollock"    = list(nodc="8791030701", race="21740", LCLASS=c(0,10,25,40,55,999), 
                        bioen=list(ref="poll2015", CA=0.119, CB=-0.46, C_TM=15, C_T0=10, C_Q=2.6)),
  "Arrowtooth"   = list(nodc="8857040102", race="10110", LCLASS=c(0,10,30,50,999),
                        bioen=list(ref="atf2015", CA=0.125, CB=-0.199, C_TM=26, C_T0=20.512, C_Q=2.497)),
  "P.halibut"    = list(nodc="8857041901", race="10120", LCLASS=c(0,10,50,70,999),
                        bioen=list(ref="hal2018", CA=0.0625, CB=-0.1076, C_TM=18, C_T0=12.97, C_Q=3.084))
)

# Consumption scaling (temperature-dependent) currently uses Wisconsin
# model eq 2 parameters for Cmax (per day) and temperature corrections.  
# To use a fixed proportion of body weight (biomass), set CA = desired daily proportion of body weight
# (or Ecopath annual QB/365) and CB=0, C_TM=100, C_T0=-100, C_Q=1.000000001
# If you're using this for diet proportions not total consumption, can simply
# use 1.0 for CA.
#pred_params[["P.cod"]]$bioen = list(ref="P.cod.old", CA=0.041,     CB= -0.122,  C_TM=21,  C_T0=13.7,  C_Q=2.41)
#pred_params[["P.cod"]]$bioen = list(ref="cod.qb"   , CA=1.39/365,  CB=0,  C_TM=100,  C_T0=-100,  C_Q=1.000000001)


##############################################################################
## GEORGE ANALYSIS 2022

source("R/REEM_fooddata_functions.R")

predlist <- c("W.pollock","P.cod","Arrowtooth","P.halibut")
#this.pred  <- "W.pollock"
this.model <- "EBS"


combined_cons <- NULL
for (this.pred in predlist){
  # set up len-weight regression parameters and consumption weighting
  lwp <- get_lw(predator=this.pred, model=this.model, years=1982:2021, all.data=F) 
  pred_params[[this.pred]]$lw_a  = lwp$lw_a
  pred_params[[this.pred]]$lw_b  = lwp$lw_b
  strat_lencons  <- get_stratum_length_cons(predator=this.pred, model=this.model)
  strat_dietcons <- add_diets_to_strata_length_cons(strat_lencons, predator=this.pred, model=this.model, min_sample=5)
  
  combined_cons <- rbind(combined_cons,strat_dietcons)
}

write.csv(combined_cons,paste(this.model,"_stratcons_min5full.csv",sep=""),row.names=F)


#$W.pollock$lw_a
#[1] 0.005646066
#
#$W.pollock$lw_b
#[1] 3.037561
#prey_params <- list(
#  "W.pollock"  = list(
#                   nodc="8791030701", race="21740", LCLASS=c(0,10,25,999)
#                 ),
#  "Opilio"     = list(
#                   nodc=c("6187010300", "6187010301"), LCLASS=c(0,30,95,999),
#                   prey_a=0.000267, prey_b=3.097253
#                 )
#               )                        
                        
this.prey<-"W.pollock"

preysplit <- preylength_splits(pred_params[[this.pred]]$nodc, 
                                   pred_params[[this.pred]]$LCLASS,
                                   pred_params[[this.prey]]$nodc,
                                   c(0,80,200,999),
                                   model="EBS")

prey_b <- pred_params[[this.prey]]$lw_b
prey_a <- pred_params[[this.prey]]$lw_a/(10^prey_b)

preylen_freqs <- preysplit %>%
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

