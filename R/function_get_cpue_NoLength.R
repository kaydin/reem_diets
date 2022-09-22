# Task 1: replicate design-based total biomass per stratum that's in the SQL table
#
# Data and script locations -----------------------------------------------
# CPUE table to replicate
# SQL Developer: AFSC--> Other Users --> AI --> Tables --> CPUE

# SQL script to replicate
# G Drive: G:/GOA/biomass_sizecomp_scripts/biomass/biomass.sql

# Stratum areas to use
# SQL Developer: AFSC--> Other Users --> GOA --> Tables --> GOA_STRATA

# #FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_DemersalShelfRF.csv") #ModelSpeciesList_GAP_Biomass_EGOA.A.csv")
# #FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_otherskates.csv")
# #FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_shallowFF.csv")
# #FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_slopeRF.csv")
# #FunGrp=read.csv("C:/Users/Bridget.Ferriss/Work/Rpath/GOA_Rpath_git/GOA_Rpath/GOA_biomass/data/ModelSpeciesList_GAP_Biomass_EGOA.B_other.csv")
# head(FunGrp)
# 
# #convert predator list to species codes (RACE) adn save RACE species codes of selected predators that want to calculate diet by stanza/length
# RACE <- FunGrp$RACE

# This function replicates the tables in RACEBASE GOA --> CPUE
get_cpue_noLength <- function(racebase_tables = list(
                       cruisedat = cruisedat,
                       haul = haul,
                       catch = catch
                     ),
                     speciescode = RACE, #speciescode_list$RACE, # 
                     survey_area = "GOA") {

  # survey_area is called region in RACEBASE
  cruisedat <- racebase_tables$cruisedat
  haul <- racebase_tables$haul
  catch <- racebase_tables$catch

  sp_catch <- catch %>%
    filter(species_code %in% speciescode)

  dat <- haul %>%
    left_join(cruisedat,
      by = c("cruisejoin", "region")
    ) %>%
    filter(abundance_haul == "Y" &
      region == survey_area) %>%
    left_join(sp_catch, by = "hauljoin") %>%
    replace_na(list(
      weight = 0,
      number_fish = 0
    )) %>%
    dplyr::select(
      species_code,
      cruisejoin.x, vessel.x, haul.x,
      haul_type, performance, duration,
      stratum, stationid,
      distance_fished, weight, year,
      weight, number_fish,
      start_latitude, start_longitude,
      gear_temperature, surface_temperature,
      AreaSwept_km2
    ) %>%
    dplyr::rename(
      Lat = start_latitude,
      Lon = start_longitude,
      Catch_KG = weight,
      Vessel = vessel.x,
      Bottom_temp = gear_temperature,
      Surface_temp = surface_temperature
    )
  # %>%
  # filter(year == survey_yr)

  x <- dat %>%
    mutate(
      WGTCPUE = Catch_KG / AreaSwept_km2,
      NUMCPUE = number_fish / AreaSwept_km2,
      survey = survey_area
    ) %>%
   # replace_na(list(species_code = speciescode)) %>%
    select(
      year, survey, Vessel, haul.x, stationid,
      stratum, distance_fished,
      species_code, Catch_KG, number_fish,
      AreaSwept_km2, WGTCPUE, NUMCPUE
    ) %>%
    arrange(year)

  return(x)
}

#############################################################################
# This function replicates the tables in RACEBASE GOA --> CPUE
get_cpue_noLength_all_REEM <- function(racebase_tables = list(
  cruisedat = cruisedat,
  haul = haul,
  catch = catch), 
  survey_area = "GOA") {
  
  # survey_area is called region in RACEBASE
  cruisedat <- racebase_tables$cruisedat
  haul <- racebase_tables$haul
  catch <- racebase_tables$catch
  
  sp_catch <- catch # %>%
  #  filter(species_code %in% speciescode)
  
  dat <- haul %>%
    left_join(cruisedat,
              by = c("cruisejoin", "region")
    ) %>%
    filter(abundance_haul == "Y" &
             region == survey_area) %>%
    left_join(sp_catch, by = "hauljoin") %>%
    replace_na(list(
      weight = 0,
      number_fish = 0
    )) %>%
    dplyr::select(
      species_code,
      cruisejoin.x, vessel.x, haul.x,
      haul_type, performance, duration,
      stratum, stationid,
      distance_fished, weight, year,
      weight, number_fish,
      start_latitude, start_longitude,
      gear_temperature, surface_temperature,
      AreaSwept_km2
    ) %>%
    dplyr::rename(
      Lat = start_latitude,
      Lon = start_longitude,
      Catch_KG = weight,
      Vessel = vessel.x,
      Bottom_temp = gear_temperature,
      Surface_temp = surface_temperature
    )
  # %>%
  # filter(year == survey_yr)
  
  x <- dat %>%
    mutate(
      WGTCPUE = Catch_KG / AreaSwept_km2,
      NUMCPUE = number_fish / AreaSwept_km2,
      survey = survey_area
    ) %>%
    # replace_na(list(species_code = speciescode)) %>%
    select(
      year, survey, Vessel, haul.x, stationid,
      stratum, distance_fished,
      species_code, Catch_KG, number_fish,
      AreaSwept_km2, WGTCPUE, NUMCPUE
    ) %>%
    arrange(year)
  
  return(x)
}



# POP: 30060
# walleye pollock: 21740
# sablefish: 20510

# RACEBASE equivalent table: CPUE
# x <- get_cpue_noLength(survey_area = "GOA", speciescode = 30060)






