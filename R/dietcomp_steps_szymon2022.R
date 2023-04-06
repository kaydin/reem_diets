
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
                       prey_guild_column     = "egoa")

predlist <- read.clean.csv("lookups/Alaska_Predators_GOA.csv")
  
# Consumption scaling (temperature-dependent) currently uses Wisconsin
# model eq 2 parameters for Cmax (per day) and temperature corrections.  
# To use a fixed proportion of body weight (biomass), set CA = desired daily proportion of body weight
# (or Ecopath annual QB/365) and CB=0, C_TM=100, C_T0=-100, C_Q=1.000000001
# If you're using this for diet proportions not total consumption, can simply
# use 1.0 for CA.
#pred_params[["P.cod"]]$bioen = list(ref="P.cod.old", CA=0.041,     CB= -0.122,  C_TM=21,  C_T0=13.7,  C_Q=2.41)
#pred_params[["P.cod"]]$bioen = list(ref="cod.qb"   , CA=1.39/365,  CB=0,  C_TM=100,  C_T0=-100,  C_Q=1.000000001)


##############################################################################

this.model <- "EGOA"

model_area <- sum(strat_areas$area[strat_areas$model==this.model])

preds      <- predlist %>% filter(model==this.model)
pred_names <- unique(preds$predator)
pred_params=list()
for (p in pred_names){
  pdat <- as.list(preds[preds$predator==p,])
  pred_params[[p]] <- pdat
  pred_params[[p]]$LCLASS <- sort(unique(c(0,pdat$juv_cm, pdat$adu_1, pdat$adu_2, pdat$adu_3,999)))
  pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
  pred_params[[p]]$lw_b   <- pdat$b_l_mm_g
  pred_params[[p]]$lw_a   <- pdat$a_l_mm_g*(10^pdat$b_l_mm_g)  
  pred_params[[p]]$bioen  <- list(CA=pdat$ca, CB=pdat$cb, C_TM=pdat$c_tm, C_T0=pdat$c_t0, C_Q=pdat$c_q)
}

combined_cons <- NULL
combined_bio  <- NULL
combined_sbio <- NULL
for (this.pred in pred_names){
  strat_lencons  <- get_stratum_length_cons(predator=this.pred, model=this.model)

  strat_dietcons <- add_diets_to_strata_length_cons(strat_lencons, predator=this.pred, model=this.model)
  combined_cons <- rbind(combined_cons,strat_dietcons)

  strat_bio <- strat_lencons %>%     
    group_by(year,model,stratum_bin,species_name,lbin) %>%
    summarize(strat_bio_t_km2 = mean(tot_wlcpue_t_km2), .groups="keep") %>%
    left_join(strat_areas,by=c("model","stratum_bin")) %>%
    mutate(bio_tons = strat_bio_t_km2 * area,
           jcat     = ifelse(lbin==pred_params[[this.pred]]$jsize,"juv","adu"))    
  
  combined_bio  <- rbind(combined_bio,strat_bio)    
  #test <- get_biomass_stratum(predator=this.pred,model=this.model)
  #combined_sbio <- rbind(combined_sbio, test)
}

bio_sum <- combined_bio %>%     
  group_by(year, model, species_name, jcat) %>%
  summarize(bio_tons = sum(bio_tons), .groups="keep") %>%
  mutate(model_area = model_area,
         bio_tkm2   = bio_tons/model_area)


test <- get_cpue_length_cons(predator=this.pred, model=this.model)



#}

write.csv(combined_cons,paste(this.model,"_stratcons.csv",sep=""),row.names=F)


prey_params <- list(
  "W.pollock"  = list(
                   nodc="8791030701", race="21740", LCLASS=c(0,10,25,999)
                 ),
  "Opilio"     = list(
                   nodc=c("6187010300", "6187010301"), LCLASS=c(0,30,95,999),
                   prey_a=0.000267, prey_b=3.097253
                 )
               )                        
                        
this.prey<-"W.Pollock"
this.pred<-"W.Pollock"

this.model <- "EGOA"
preylen_freqs <- preylength_splits(
  pred_params[[this.pred]]$nodc, 
  pred_params[[this.prey]]$nodc, 
  pred_params[[this.pred]]$LCLASS, 
  c(0,250,999),
  model="EBS")


preylen_freqs <- preylength_splits(
                          pred_params[[this.pred]]$nodc, 
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

