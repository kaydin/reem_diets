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
get_cpue <- function(racebase_tables = list(
                       cruisedat = cruisedat,
                       haul = haul,
                       catch = catch,
                       lengthdat = lengthdat 
                     ),                             #BF added length file
                     speciescode = 30060, # POP
                     #survey_area = "AI") 
                     survey_area = "GOA")   {

  # survey_area is called region in RACEBASE
  cruisedat <- racebase_tables$cruisedat
  
  haul <- racebase_tables$haul
  catch <- racebase_tables$catch
  lengthdat <- racebase_tables$lengthdat #BF added

  sp_catch <- catch %>%
    filter(species_code == speciescode)

  dat <- haul %>%
    left_join(cruisedat, by = c("cruisejoin", "region")) %>%
    filter(abundance_haul == "Y" & region == survey_area) %>%
    left_join(sp_catch, by = "hauljoin") %>% 
    replace_na(list( weight = 0, number_fish = 0)) %>%
    left_join(lengthdat, by = c("hauljoin", "species_code")) %>% 

    dplyr::select(
      species_code,
      cruisejoin.x, vessel.x, haul.x,
      haul_type, performance, duration,
      stratum, stationid,
      distance_fished, weight, year,
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
    mutate (
      NumLBin_CPUE_km2 = PropLength_Num*NUMCPUE  # Length step: convert proportion of number fish in each length bin to numberCPUE in each length bin (#fish/km2)
    )%>%
    mutate (WgtLBin_CPUE_km2 = NumLBin_CPUE_km2*weight_Lbin) %>% # Length step: calc weight for all fish in Lbin using proportion of CPUE (number fish in Lbin *weight)
    replace_na(list(species_code = speciescode)) %>%
    select(
      year, survey, Vessel, haul.x, stationid,
      stratum, distance_fished,
      species_code, Catch_KG, number_fish,
      AreaSwept_km2, WGTCPUE, NUMCPUE, length, NumLBin_CPUE_km2, WgtLBin_CPUE_km2
    ) %>%
    arrange(year)

  return(x)
}

# POP: 30060
# walleye pollock: 21740
# sablefish: 20510

# RACEBASE equivalent table: CPUE
x <- get_cpue(survey_area = "GOA", speciescode = 30060)
head(x)



