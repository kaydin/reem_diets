

# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")

source("R/REEM_fooddata_functions.R")

REEM.loadclean.RACE(path = "data/local_racebase")

REEM.loadclean.diets(data_path = "data/local_reem_data")

REEM.loadclean.lookups(strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                       stratum_bin_column    = "strat_groups",
                       preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv",
                       prey_guild_column     = "hudson")

###############################################################################

pred_params <- list(
    "W.pollock"    = list(nodc="8791030701", race="21740", LCLASS=c(0,10,25,40,55,999) ),
    "P.cod"        = list(nodc="8791030401", race="21720", LCLASS=c(0,10,30,60,85,999) ) ,
    "Arrowtooth"   = list(nodc="8857040102", race="10110", LCLASS=c(0,10,30,50,999) ),
    "P.halibut"    = list(nodc="8857041901", race="10120", LCLASS=c(0,10,50,70,999) ),
    "Atka.mackerel"= list(nodc="8827010501", race="21921", LCLASS=c(0,20,999)), 
    "POP"          = list(nodc="8826010102", race="30060", LCLASS=c(0,20,999))
    )


#################################
######################################################################################
#Hudson Length-Weight example

# LW regressions
source("R/REEM_fooddata_functions.R")

lwdat <- rbind(get_lw(predator="P.cod", model="AI",all.data=T),
              get_lw(predator="W.pollock", model="AI",all.data=T),
              get_lw(predator="Atka.mackerel", model="AI",all.data=T),
              get_lw(predator="POP", model="AI",all.data=T)
)

strat.water  <- haul_summary(model="AI")

strat.bio    <- rbind( get_cpue(predator="P.cod",         model="AI"),
                       get_cpue(predator="W.pollock",     model="AI"),
                       get_cpue(predator="Atka.mackerel", model="AI"),
                       get_cpue(predator="POP",           model="AI")) %>%
                group_by(year,model,stratum_bin,species_name) %>%
                summarize(mean_wtcpue_t_km2  = mean(wgtcpue)/1000 ,
                         .groups="keep") %>%
                pivot_wider(names_from=species_name, values_from=mean_wtcpue_t_km2, names_prefix="biomass_")

lw.out <- lwdat %>%
  left_join(strat.water, by=c("model","stratum_bin","year")) %>%
  left_join(strat.bio,   by=c("model","stratum_bin","year")) %>%
  mutate(Lon_360 = Lon%%360, .after=Lon) %>% 
  mutate(CF_log_diff = log_diff) %>% relocate(CF_log_diff)

#write.csv(lw.out,"hudson_lw_01.csv",row.names=F)

source("R/REEM_fooddata_functions.R")

strat.diet <- NULL
strata_lookup$stratum_bin <- strata_lookup$stratum
for(this.pred in c("Atka.mackerel","POP","W.pollock","P.cod")){
  pred_params[[this.pred]] <- c(pred_params[[this.pred]], 
                              get_lw(predator=this.pred, model="AI", years=1982:2021, all.data=F) )
  #strat.diet <- rbind(strat.diet, predprey_tables(predator=this.pred, model="AI", months=5:8))  
  strat.diet <- rbind(strat.diet, predprey_tables(predator=this.pred, model="AI")) 
}

logit_diets <- strat.diet %>% 
      filter(!is.na(prey_guild)) %>%
      mutate(mean_sci = tot_sci/pred_n, .after=tot_sci) %>%
      mutate(logit   = log((prey_n+0.5)/(pred_n-prey_n+0.5)),
             logitsd = sqrt(1.0/(prey_n+0.5) + 1.0/(pred_n-prey_n+0.5))) %>%
      select(c(1:prey_guild,logit)) %>%
      spread(prey_guild,logit,sep="_")


lw_joined <- lw.out %>%
  left_join(logit_diets,by=c("predator","model","stratum_bin","year","lbin")) %>%
  select(c(-species_code,-region,-cruisejoin,-vessel,-haul,-stratum,-stationid,
           -Lon,-bottom_depth,-AreaSwept_km2,-specimenid,-log_diff,
           -stations,-bottom_temp_mean,-surface_temp_mean,-tot_wt,-pred_full,-tot_sci))

write.csv(lw_joined,"gam_hudson_nov29_2022.csv",row.names=F)







