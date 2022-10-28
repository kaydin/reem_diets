
# setwd() if needed here.  Everything should be run from the main repo
# directory (that contains the data/ R/ and lookups/ subfolders).  
library("tidyverse")
library("janitor") 
library("lubridate")

####################################################################

source("R/REEM_fooddata_functions.R")

REEM.loadclean.RACE(path = "data/local_racebase")

REEM.loadclean.diets(data_path = "data/local_reem_data",
                     strata_lookup_file    = "lookups/combined_BTS_strata.csv",
                     preynames_lookup_file = "lookups/Alaska_PreyLookup_MASTER.csv")

preylook_col <- "ecopath_prey"    
stratbin_col <- "strat_groups"

pred_params <- list(
    "W.pollock"    = list(nodc="8791030701", race="21740", LCLASS=c(0,10,25,40,55,999) ),
    "P.cod"        = list(nodc="8791030401", race="21720", LCLASS=c(0,10,30,60,85,999) ),
    "Arrowtooth"   = list(nodc="8857040102", race="10110", LCLASS=c(0,10,30,50,999) ),
    "P.halibut"    = list(nodc="8857041901", race="10120", LCLASS=c(0,10,50,70,999) ),
    "Atka.mackerel"= list(node="8827010501", race="21921", LCLASS=c(0,20,999)), 
    "POP"          = list(node="8826010102", race="30060", LCLASS=c(0,20,999))
    )


##############################################################################
## CODY ANALYSIS 2022

source("R/REEM_fooddata_functions.R")

this.pred  <- "P.cod"
this.model <- "EBS"

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

bioen_pars<-list(
  "P.cod.new"= list(CA=0.03468844, CB= -0.1222201, C_TM=25.90149, C_T0=10.9575, C_Q=3.078766),
  "P.cod.old"= list(CA=0.041,      CB= -0.122,     C_TM=21,       C_T0=13.7,    C_Q=2.41)
)

haulmeans <- haul_summary(this.model)

wthaul <- wtlens %>%
  left_join(haulmeans,by=c("model","stratum_bin","year")) %>%
  mutate(bottom_temp_clean = ifelse(is.na(bottom_temp),bottom_temp_mean,bottom_temp),
         surface_temp_clean = ifelse(is.na(surface_temp),surface_temp_mean,surface_temp))


bpar = bioen_pars[["P.cod.old"]]
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

#write.csv(diet_sum,"diet2new.csv",row.names=F)


source("R/REEM_fooddata_functions.R")



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

test.new <- fc_T_eq2(seq(-1,30,0.2), bioen_pars[["P.cod.new"]])
test.old <- fc_T_eq2(seq(-1,30,0.2), bioen_pars[["P.cod.old"]])

#end cody query
######################################
######################################






#################################
strat.props.bs <- predprey_tables(predator="P.cod", model="EBS", months=5:8)

strat.cpue.bs <- get_cpue(predator="P.cod", model="EBS")
strat.bio <- get_biomass_stratum(predator="P.cod", model="EBS")

strat.bio    <- rbind( get_biomass_stratum(predator="P.cod", model="EBS"),
                       get_biomass_stratum(predator="W.pollock", model="EBS"),
                       get_biomass_stratum(predator="Arrowtooth", model="EBS"),
                       get_biomass_stratum(predator="P.cod", model="NBS"),
                       get_biomass_stratum(predator="W.pollock", model="NBS"),
                       get_biomass_stratum(predator="Arrowtooth", model="NBS"),
                       get_biomass_stratum(predator="P.cod", model="WGOA"),
                       get_biomass_stratum(predator="W.pollock", model="WGOA"),
                       get_biomass_stratum(predator="Arrowtooth", model="WGOA"),
                       get_biomass_stratum(predator="P.cod", model="EGOA"),
                       get_biomass_stratum(predator="W.pollock", model="EGOA"),
                       get_biomass_stratum(predator="Arrowtooth", model="EGOA"),
                       get_biomass_stratum(predator="P.cod", model="AI"),
                       get_biomass_stratum(predator="W.pollock", model="AI"),
                       get_biomass_stratum(predator="Arrowtooth", model="AI")                       
                       )



source("R/REEM_fooddata_functions.R")
testall <- rbind(get_cpue_all(model="WGOA"),get_cpue_all(model="EGOA"))
test2 <- guild_out(testall)

source("R/REEM_fooddata_functions.R")
for(m in c("EBS","NBS","WGOA","EGOA","AI")){
  datout <- guild_out(get_cpue_all(model=m))
  write.csv(datout,paste(m,"all_years.csv",sep=''), row.names=F)
}


source("R/REEM_fooddata_functions.R")
test <- get_haul_means("AI")

strat.bio.wgoa <- get_biomass_stratum(predator="P.cod", model="EBS")
strat.props.goa <- predprey_tables(predator="Arrowtooth", model="WGOA", months=5:8)


######################################################################################
#Hudson Length-Weight example

# LW regressions
source("R/REEM_fooddata_functions.R")



test <- rbind(get_lw(predator="P.cod", model="AI",all.data=T),
              get_lw(predator="W.pollock", model="AI",all.data=T),
              get_lw(predator="Atka.mackerel", model="AI",all.data=T),
              get_lw(predator="POP", model="AI",all.data=T)
)

LW <- test %>%
  group_by(predator, year, stratum_bin, lbin) %>%
  dplyr::summarize(
    specimen_count = length(specimenid), # number of total abundance hauls
    mean_log_diff = mean(log_diff, na.rm = TRUE),
    sd_log_diff   = sd(log_diff,na.rm = TRUE),
    .groups="keep")
    
    

#################################################################################
source("R/REEM_fooddata_functions.R")
REEM.loadclean.RACE(path = "data/local_racebase")



source("R/REEM_fooddata_functions.R")

    
all_species <- unique(catch$species_code)


cpue_all    <- get_cpue(speciescode=21740, survey_area="BS")




source("R/REEM_fooddata_functions.R")  
biomass_stratum <- get_biomass_stratum_noLength(speciescode = pred_params[["P.cod"]]$race, survey_area = "GOA")


biomass_stratum <- get_biomass_stratum(speciescode = pred_params[["P.cod"]]$race, survey_area = "GOA")



source("R/REEM_fooddata_functions.R")

all_species <- unique(catch$species_code)
biomass_all <- get_cpue_noLength(speciescode=all_species, survey_area="BS")



biomass_all <- get_cpue_noLength_all_REEM(survey_area = "BS")


#################################################################################
htest <- haul %>% 
  filter(abundance_haul == "Y") %>% 
  mutate(year=floor(cruise/100)) %>% 
  left_join(strata_lookup, by=c("region"="survey", "stratum"="stratum")) %>%
  group_by(region,strat_groups,year) %>% 
  tally()


#########################################################
#{r load_clean_data, eval=FALSE}

# REEM.loadclean.RACE loads all files with a .csv extension that are in the 
# directory indicated by path and names them after the file.  Any csv file
# in the directory will be loaded.
REEM.loadclean.RACE(path = "data/local_racebase")

# Above RACE data is all regions.  For food habits and strata, only the
# region-specific data is loaded (re-run from here if switching regions).
# REEM.loadclean.diets [region]_predprey and _predlength data.
REEM.loadclean.diets(region=REGION, path="data/local_reem_data")

# Load the table of combined survey strata.  
raw_strata <- read.csv("lookups/combined_BTS_strata.csv") %>% janitor::clean_names()
#region_strata <- raw_strata %>% filter(survey == REGION)
#model_strata  <- raw_strata %>% filter(submodel == SUBMODEL)


# Load length/weight regression parameters (currently from pre-made file, could
# re-generate here)  
LWparam=read.csv("lookups/LengthWeightRegressionParams.csv") %>%
  filter(EwEReg == SUBMODEL)  

## Raw data should be loaded into the Workspace at this point

# Cleanup-type data manipulations
# convert to area in km given width in m
haul <- haul %>% 
  mutate(AreaSwept_km2 = distance_fished * (0.001 * net_width)) 
# Simple tidy up function for the dates in the cruise data  
cruisedat <- tidy.cruise.dates(cruise) 
# Add length/weight regressions to length table
lengthdat<-length %>%
  filter(region == REGION)%>%
  group_by (cruise, haul, species_code)%>%
  mutate(NumSamp_SpHaul=sum(frequency))%>%
  ungroup()%>%
  left_join(LWparam, by=c("species_code"="sp_code"))%>%
  mutate(PropLength_Num = frequency/NumSamp_SpHaul)%>%
  mutate (weight_Lbin=LW.intercept_a*length^LW.slope_b) #weight of fish (sp, haul) in given length bin
# KYA - not sure if the below are needed  
#lengthhaul <- lengthdat %>%
#  left_join(haul,by=c("hauljoin"="hauljoin"))
#length_sp <- lengthhaul %>%
#  filter()    

#Species-specific additions  
#LCLASS=c(0,10,25,40,55,999)
#lbin  mutate(lbin = cut(pred_len, LCLASS, right=F)) %>%  

  
  
  
