

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
  left_join(strat.bio,   by=c("model","stratum_bin","year"))

write.csv(lw.out,"hudson_lw_01.csv",row.names=F)

source("R/REEM_fooddata_functions.R")
strat.diet <- logit_simple(predator="Atka.mackerel", model="AI", months=5:8)







#LW <- test %>%
#  group_by(predator, year, stratum_bin, lbin) %>%
#  dplyr::summarize(
#    specimen_count = length(specimenid), # number of total abundance hauls
#    mean_log_diff = mean(log_diff, na.rm = TRUE),
#    sd_log_diff   = sd(log_diff,na.rm = TRUE),
#    .groups="keep")


########################################


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

  
  
  
