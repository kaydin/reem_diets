
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

# Load Lookup (file and column) for aggregating strata (ecosystem model and stratum numbers as primary key)
# Load Lookup (file and column) for diet items (with NODC code as primary key)
REEM.loadclean.lookups(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                       stratum_bin_column    = "strat_groups",
                       preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv",
                       prey_guild_column     = "egoa")

# Load lookup file for predators.  Name needs to match the name
# (including case sensitivity) in prey guild and race lookups
# Ecosystem Model and name as primary key
predlist <- read.clean.csv("lookups/Alaska_Predators_GOA.csv")

# Lookup file and column for mapping RACE codes to biomass
# Primary key is RACE codes, race lookup names should match
# names in prey and predator lookups
race_lookup <- read.clean.csv("lookups/goa_race_lookup_apr_04_2023.csv") %>% 
  mutate(race_guild  = .data[["final_goa"]])

##############################################################################
# Diet sample summaries

nodc_key <- read.clean.csv("lookups/nodc_predators.csv")

# this does it once for all ecosystem models
allpred_tab <- predprey %>%
  left_join(preynames_lookup, by=c("prey_nodc"="nodc_code")) %>%
  left_join(strata_lookup, by=c("region"="survey","stratum"="stratum")) %>%
  relocate(stratum_bin) %>% relocate(model) %>%
  group_by(year, model, stratum_bin, pred_nodc, predjoin) %>%
  summarize(stom_wt_g=sum(twt),.groups="keep") %>%
  group_by(year, model, stratum_bin, pred_nodc) %>%
  summarize(n_stomachs=n(), stom_wt_g=sum(stom_wt_g), .groups="keep")

bio_combined <- NULL
for (this.model in c("EGOA","WGOA")){

  strat_bio_totals <- get_cpue_all(model=this.model) %>%
    group_by(year, model, race_guild, species_code, stratum) %>%
    summarize(wgtcpue = sum(wgtcpue),
            numcpue = sum(numcpue),.groups="keep") %>%
    left_join(haul_stratum_summary(this.model),by=c("year","model","stratum")) %>%
    mutate(bio_t_km2 = wgtcpue/1000/stations,
           bio_tons  = bio_t_km2 * area) %>%
    left_join(race_lookup[,c("species_code","species_name","common_name")], by=c("species_code")) %>%
    group_by(year, model, race_guild, species_code, species_name, common_name, stratum_bin) %>%
    summarize(n_stations=sum(stations),bio_tons=sum(bio_tons),.groups="keep") %>%
    left_join(nodc_key[,c("species_code","pred_nodc","pred_name")],by=c("species_code"))

  bio_combined <- rbind(bio_combined,strat_bio_totals)  
}

bio_and_pred_sums <- bio_combined %>%
  left_join(allpred_tab, by=c("year","model","stratum_bin","pred_nodc"))

write.csv(bio_and_pred_sums,"sample_totals.csv",row.names=F)


##############################################################################
# Biomass extraction by item

bio_combined <-NULL

# Can loop through multiple models, saving results in a single
# table (bio_combined)
for (this.model in c("EGOA","WGOA")){

# get_cpue_all() mirrors the RACE get_cpue function (returning by haul), except it gets
# biomasses for all groups at once, binned by the race_lookup names (race_guild column)
# haul_stratum_summary() gets the total stations and area for each model domain.
# This code groups the two together at the stratum level to get stratum biomass
# in both tons and t/km^2
  stratsum <- get_cpue_all(model=this.model) %>%
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
  

# Loop through the list of predators from the predlist table and
# load parameters for each species into a list. The pred entries
# are indexed by name and model
  preds      <- predlist %>% filter(model==this.model)
  pred_names <- unique(preds$predator)
  pred_params=list()
  for (p in pred_names){
    pdat <- as.list(preds[preds$predator==p,])
    pred_params[[p]] <- pdat
    pred_params[[p]]$LCLASS <- sort(unique(c(0,pdat$juv_cm, pdat$adu_1, pdat$adu_2, pdat$adu_3,9999)))
    pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
    pred_params[[p]]$lw_b   <- pdat$b_l_mm_g
    pred_params[[p]]$lw_a   <- pdat$a_l_mm_g*(10^pdat$b_l_mm_g)  
    pred_params[[p]]$bioen  <- list(CA=pdat$ca, CB=pdat$cb, C_TM=pdat$c_tm, C_T0=pdat$c_t0, C_Q=pdat$c_q)
  }

  
# Getting biomass-at-length and converting to split pool biomass
# Loop through each predator.  the juv_adu_lencons splits out
# length samples based on the length class divisions in the
# predator lookup, and converts to % by weight using the a and b LW
# regression.  This % by weight is applied to the total biomass
# for the predator calculated above, to calculate biomass in
# each length class.  It is output into bio_combined as two biomass groupings
# (juv and adu) where adu is the sum of all the adu size classes.
  
  juv_combined <- NULL
  for (p in pred_names){
    #p <- pred_names[1]
    bio_pred <- bio_totals %>%
      filter(race_guild==p)
    
    juv_adu_lencons  <- get_stratum_length_cons(predator=p, model=this.model) %>%
    group_by(year, model, species_name, stratum, lbin) %>%
      summarize(strat_bio_sum = sum(tot_wlcpue_t_km2), .groups="keep") %>%
      left_join(haul_stratum_summary(this.model),by=c("year","model","stratum")) %>%
      mutate(bio_t_km2 = strat_bio_sum/stations,
             bio_tons  = bio_t_km2 * area,
             jcat      = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu")) 
    
    juv_proportions <- juv_adu_lencons %>%
      group_by(year,model,species_name,jcat) %>%
      summarize(bio_tons = sum(bio_tons), .groups="keep") %>%
      pivot_wider(names_from=jcat, values_from=bio_tons) %>%
      mutate(juv_bio_prop = juv/(juv+adu))
    
    juv_combined <- rbind(juv_combined,juv_proportions)
  } # end of pred_names loop
  
  bio_with_juvs <- bio_totals %>%
    left_join(juv_combined,by=c("year","model","race_guild"="species_name")) %>%
    select(-c(juv,adu)) %>%
    mutate(juv_bio_prop  = replace_na(juv_bio_prop,0.0),
           adu_bio_tkm2  = (1.0-juv_bio_prop) * bio_tkm2,
           juv_bio_tkm2  = juv_bio_prop       * bio_tkm2)
  
  
    bio_combined <- rbind(bio_combined,bio_with_juvs)      
  
} # end of loop by model (e.g. WGOA, EGOA)

write.csv(bio_combined,"results/goa_bio_combined_juvadu.csv",row.names=F)


# Biomass code stops here

###########################################################################
# Diet data
# to be commented later

diet_combined <- NULL

for (this.model in c("EGOA","WGOA")){

#this.model <- "EGOA"

all_preds <- bio_and_pred_sums %>%
  ungroup() %>%
  select(common_name,pred_nodc,species_code) %>%
  distinct() %>%
  filter(!is.na(pred_nodc))

pred_nodc <- all_preds$pred_nodc;    names(pred_nodc)<-all_preds$common_name
pred_race <- all_preds$species_code; names(pred_race)<-all_preds$common_name

pred_params=list()
#pred_nodc <- pred_nodc[1:2]
for (p in names(pred_nodc)){
  pred_params[[p]] <-list(base_name=p, nodc_code=as.numeric(pred_nodc[p]), race=as.numeric(pred_race[p]))
  pred_params[[p]]$LCLASS <- c(0,9999)
  #pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
  pred_params[[p]]$lw_b   <- 1.0
  pred_params[[p]]$lw_a   <- 3.0
  #pred_params[[p]]$bioen  <- list(CA=NA, CB=NA, C_TM=NA, C_T0=NA, C_Q=NA)
  
  diet_summary <- predprey_tables(predator=p, model=this.model) %>%
    filter(prey_n>0) %>%
    mutate(species_code=pred_params[[p]]$race) # %>%
    #group_by(predator,model,stratum_bin,year,lbin,pred_n,pred_full) %>%
    #summarize(dtot = sum(dietprop_sci),.groups="keep")
  #if(nrow(diet_summary)>0){
   diet_combined <- rbind(diet_combined, diet_summary)
  #}
  
}
}

bio_diets <- bio_and_pred_sums %>%
  left_join(diet_combined,by=c("model","year","stratum_bin","species_code")) %>%
  replace_na(list(prey_guild="UNSAMPLED", pred_n=0, dietprop_wt=1.0,dietprop_sci=1.0)) %>%
  mutate(sampled=ifelse(pred_n==0,FALSE,TRUE))

write.csv(bio_diets,"GOA_bio_diets_Jul18_23.csv",row.names=F)


##############################################
#diet data by length
diet_combined <- NULL

MIN_SAMPLE <- 5

for (this.model in c("EGOA","WGOA")){
  
  preds      <- predlist %>% filter(model==this.model)
  pred_names <- unique(preds$predator)
  pred_params=list()
  for (p in pred_names){
    pdat <- as.list(preds[preds$predator==p,])
    pred_params[[p]] <- pdat
    pred_params[[p]]$LCLASS <- c(0,20,9999)
    pred_params[[p]]$jsize  <- paste("[",pred_params[[p]]$LCLASS[1],",",pred_params[[p]]$LCLASS[2],")",sep='')
    pred_params[[p]]$lw_b   <- 1.0
    pred_params[[p]]$lw_a   <- 3.0
    pred_params[[p]]$bioen  <- list(CA=NA, CB=NA, C_TM=NA, C_T0=NA, C_Q=NA)
  }
  
  
  for (p in pred_names){
    
    juv_adu_lencons  <- get_stratum_length_cons(predator=p, model=this.model)
    strat_dietcons <- add_diets_to_strata_length_cons(juv_adu_lencons, predator=p, model=this.model, min_sample=MIN_SAMPLE) %>%
      mutate( jcat = ifelse(lbin==pred_params[[p]]$jsize,"juv","adu"))

    strat_dietprops <- strat_dietcons %>%
      group_by(year, model, species_name, jcat, prey_guild) %>%
      summarize(cons_tot_prey = sum(cons_rel_vonb), .groups="keep") %>%
      group_by(year, model, species_name, jcat) %>%
      mutate(cons_tot = sum(cons_tot_prey)) %>%
      ungroup() %>%
      mutate(diet_prop = cons_tot_prey/cons_tot)
    
  diet_combined <- rbind(diet_combined,strat_dietprops)
  
  }
}

write.csv(diet_combined,"diet_combined2.csv",row.names=F)

# Sample sizes

diet_counts_combined <- NULL

for (this.model in c("EGOA","WGOA")){ #this.model <- "WGOA"

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

for (p in pred_names){ #p <- pred_names[1]
juv_adu_lencons_summary  <- get_stratum_length_cons(predator=p, model=this.model) %>%
  group_by(species_name,model,stratum_bin,year,lbin) %>%
  summarize(cons_vonb_tot = sum(tot_cons_vonb_rel),.groups="keep")

diet_summary <- predprey_tables(predator=p, model=this.model) %>%
  group_by(predator,model,stratum_bin,year,lbin,pred_n,pred_full) %>%
  summarize(dtot = sum(dietprop_sci),.groups="keep")

pred_counts <- juv_adu_lencons_summary %>%
  left_join(diet_summary,by=c("species_name"="predator","model","stratum_bin","year","lbin"))

diet_counts_combined <- rbind(diet_counts_combined,pred_counts)
}
}

write.csv(diet_counts_combined, "diet_counts_combined.csv",row.names=F)

##################################################################
##################################################################

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

