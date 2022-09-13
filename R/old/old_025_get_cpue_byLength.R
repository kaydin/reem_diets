# Task 1: replicate design-based total biomass per stratum that's in the SQL table
#
# Data and script locations -----------------------------------------------
# CPUE table to replicate
# SQL Developer: AFSC--> Other Users --> AI --> Tables --> CPUE

# SQL script to replicate
# G Drive: G:/GOA/biomass_sizecomp_scripts/biomass/biomass.sql

# Stratum areas to use
# SQL Developer: AFSC--> Other Users --> GOA --> Tables --> GOA_STRATA

# This function replicates the tables in RACEBASE GOA --> CPUE

#convert predator list to species codes (RACE) adn save RACE species codes of selected predators that want to calculate diet by stanza/length
#speciescode_list<-LWparam %>%
#  filter(sp_Cname=="walleye pollock"|sp_Cname=="rex sole"|sp_Cname=="arrowtooth flounder"|sp_Cname=="Pacific cod"|sp_Cname=="sablefish"|sp_Cname=="Pacific ocean perch"|sp_Cname=="Pacific halibut"|sp_Cname=="flathead sole")%>% #same list as "01_Diet_dataprep_length.R"
#  select(sp_code)%>%
#  distinct(.keep_all = TRUE) 

speciescode_list<-LWparam %>%
  filter(sp_Cname=="Pacific cod")%>%
  select(sp_code)%>%
  distinct(.keep_all = TRUE) 


get_cpue_Length <- function(racebase_tables = list(
                       cruisedat = cruisedat,
                       haul = haul,
                       catch = catch,
                       lengthdat = lengthdat 
                     ),                             #BF added length file
                     speciescode = speciescode_list$sp_code, # 21741, #predators, # POP
                     #survey_area = "AI") 
                     survey_area = "BS")   {

  #convert predator list to species codes (RACE)
  
  # survey_area is called region in RACEBASE
  cruisedat <- racebase_tables$cruisedat
  
  haul <- racebase_tables$haul
  catch <- racebase_tables$catch
  lengthdat <- racebase_tables$lengthdat #BF added

  sp_catch <- catch %>%
    filter(species_code %in% speciescode)

  dat <- haul %>%
    left_join(cruisedat, by = c("cruisejoin", "region")) %>%
    filter(abundance_haul == "Y" & region == survey_area) %>%
    left_join(sp_catch, by = "hauljoin") %>% 
    replace_na(list( weight = 0, number_fish = 0)) %>%
    left_join(lengthdat, by = c("hauljoin", "species_code")) %>% 

    dplyr::select(
      region,species_code,
      cruisejoin.x, vessel.x, haul.x, hauljoin,
      #haul_type, performance, duration,
      stratum, stationid,
      distance_fished, weight, year,
      frequency,sex,
      weight, number_fish, length, NumSamp_SpHaul, PropLength_Num, weight_Lbin,
      start_latitude, start_longitude,
      gear_temperature, surface_temperature,
      AreaSwept_km2
    ) %>%
    dplyr::rename(
      Lat = start_latitude,
      Lon = start_longitude,
      Catch_KG = weight,
      Vessel = vessel.x,
      fish_length = length,
      gear_temp = gear_temperature,
      Surface_temp = surface_temperature
    )
  # %>%
  # filter(year == survey_yr)
  #x <- dat
  x <- dat %>%
    mutate(
      WGTCPUE = Catch_KG / AreaSwept_km2,
      NUMCPUE = number_fish / AreaSwept_km2,
      survey = survey_area
    ) %>%
    mutate (
      NumLBin_CPUE_km2 = PropLength_Num*NUMCPUE  # Length step: convert proportion of number fish in each length bin to numberCPUE in each length bin (#fish/km2)
    )%>%
    mutate (WgtLBin_CPUE_km2 = NumLBin_CPUE_km2*weight_Lbin) %>% # Length step: calc weight for all fish in Lbin using proportion of CPUE (number fish in Lbin *weight)
    #replace_na(list(species_code = speciescode)) %>% #THIS DOESN"T WORK
    #select(
    #  year, survey, Vessel, haul.x, hauljoin, stationid, performance,
    #  stratum, distance_fished,
    #  species_code, Catch_KG, number_fish,
    #  AreaSwept_km2, WGTCPUE, NUMCPUE, length, NumLBin_CPUE_km2, WgtLBin_CPUE_km2
    #) %>%
    arrange(year) %>%
    dplyr::rename(haul_num=year,
                  num_cpue=NUMCPUE,
                  haul_join=hauljoin
    )

  return(x)
}

# POP: 30060
# walleye pollock: 21740
# sablefish: 20510

# RACEBASE equivalent table: CPUE
x <- get_cpue_Length(survey_area = "BS", speciescode = speciescode_list$sp_code)# 21741) #POP 30060)
write.csv(x,"x_lengthtest.csv")



