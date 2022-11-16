
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
                       prey_guild_column     = "ecopath_prey")

####################################################################

pred_params <- list(
    "P.cod"        = list(nodc="8791030401", race="21720", LCLASS=c(0,10,30,60,85,999) ),
    "W.pollock"    = list(nodc="8791030701", race="21740", LCLASS=c(0,10,25,40,55,999) )#,
    )

bioen_pars<-list(
  "pcod.2015"    = list(CA=0.041,  CB= -0.122,  C_TM=21,  C_T0=13.7,  C_Q=2.41),
  "pollock.2015" = list(CA=0.119,  CB= -0.46,   C_TM=15,  C_T0=10,    C_Q=2.6)
)

##############################################################################
## CODY ANALYSIS 2022

source("R/REEM_fooddata_functions.R")

this.pred  <- "W.pollock"
this.model <- "EBS"
bpar = bioen_pars[["pollock.2015"]]

# Get biomass/cpue
#test    <- get_cpue(predator=this.pred, model=this.model)
testlen <- get_cpue_length(predator=this.pred, model=this.model)
lwp     <- get_lw(predator=this.pred, model="EGOA",all.data=F)

pred_params[[this.pred]]$lw_a = lwp$lw_a
pred_params[[this.pred]]$lw_b = lwp$lw_b

wtlens <- testlen %>% 
  mutate(
    lw_a = pred_params[[this.pred]]$lw_a, 
    lw_b = pred_params[[this.pred]]$lw_b,
    body_wt = lw_a * (length/10.0)^lw_b,
    lbin = as.character(cut(length/10.0, pred_params[[this.pred]]$LCLASS, right=F)),
    WgtLBin_CPUE_kg_km2 = NumLBin_CPUE_km2*body_wt/1000)

haulmeans <- haul_summary(this.model)

wthaul <- wtlens %>%
  left_join(haulmeans,by=c("model","stratum_bin","year")) %>%
  mutate(bottom_temp_clean = ifelse(is.na(bottom_temp),bottom_temp_mean,bottom_temp),
         surface_temp_clean = ifelse(is.na(surface_temp),surface_temp_mean,surface_temp))

conslens <- wthaul %>%
  mutate(cmax_g            = bpar$CA * body_wt^(1+bpar$CB),
         f_t               = fc_T_eq2(bottom_temp_mean, bpar),
         cons_kg_km2       = NumLBin_CPUE_km2 * cmax_g * f_t / 1000)


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
  replace_na(list(
    prey_guild="MISSING",
    dietprop_wt=1.0,
    dietprop_sci=1.0)) %>%
  mutate(preycons_sci_t_km2 = tot_cons_t_km2 * dietprop_sci)

strat_areas <- strata_lookup %>%
  select(model,strat_groups,area) %>%
  rename(stratum_bin=strat_groups) %>%
  group_by(model, stratum_bin) %>%
  summarize(area=sum(area),.groups="keep")

diet_sum <- len_diet %>%
  group_by(year,model,stratum_bin,species_name,lbin,prey_guild) %>%
  summarize(strat_preycons_t_km2 = mean(preycons_sci_t_km2),
    .groups="keep") %>%
  left_join(strat_areas,by=c("model","stratum_bin")) %>%
  mutate(cons_tons_day = strat_preycons_t_km2 * area)

write.csv(diet_sum,"pollock_diet.csv",row.names=F)

#write.csv(diet_sum,"diet2new.csv",row.names=F)

## Apply by prey weights
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




