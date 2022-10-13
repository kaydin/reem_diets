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
                       catch = catch
                     ),
                     # KYA changed
                       predator = "P.cod", #speciescode = 30060, # POP
                       model    = "EBS"    #survey_area = "AI"
                     ){ 

  # survey_area is called region in RACEBASE
  cruisedat <- racebase_tables$cruisedat
  haul <- racebase_tables$haul
  catch <- racebase_tables$catch
  # KYA added
    speciescode = as.integer(pred_params[[predator]]$race)
    model_name    <- model    # renamed to avoid name confusion during lookup
    predator_name <- predator # renamed to avoid name confusion during lookup
  
  sp_catch <- catch %>%
    filter(species_code == speciescode)

  # KYA added
    stratbins    <- strata_lookup    %>% mutate(stratum_bin = .data[[stratbin_col]])
    model_haul <- haul %>%
       left_join(stratbins, by=c("region"="survey","stratum"="stratum"))
    
  dat <- model_haul %>%
    left_join(cruisedat,
      by = c("cruisejoin", "region")
    ) %>%
    filter(abundance_haul == "Y" &
      #region == survey_area) %>%
      model == model_name) %>% # KYA changed
    left_join(sp_catch, by = "hauljoin") %>%
    replace_na(list(
      weight = 0,
      number_fish = 0
    )) %>%
    dplyr::select(
      species_code,model,stratum_bin,region.x,#KYA added model, stratum_bin
      cruisejoin.x, vessel.x, haul.x, hauljoin,
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
      catch_kg = weight,
      Vessel = vessel.x,
      region = region.x,
      Bottom_temp = gear_temperature,
      Surface_temp = surface_temperature
    )
  # %>%
  # filter(year == survey_yr)

  x <- dat %>%
    mutate(
      wgtcpue = catch_kg / AreaSwept_km2,
      numcpue = number_fish / AreaSwept_km2,
      survey = region,
      species_name=predator,
    ) %>%
    replace_na(list(species_code = speciescode)) %>%
    select(
      year, model, stratum_bin,species_name, survey, Vessel, haul.x, hauljoin,
      stationid,
      stratum, distance_fished,
      species_code, catch_kg, number_fish,
      AreaSwept_km2, wgtcpue, numcpue # wgtcpue is kg per km2
    ) %>%
    rename(haul = haul.x) %>%
    arrange(year)

  return(x)
}

# POP: 30060
# walleye pollock: 21740
# sablefish: 20510

# RACEBASE equivalent table: CPUE
# x <- get_cpue(survey_area = "GOA", speciescode = 30060)

# profvis(get_cpue(survey_area = "GOA", speciescode = 30060))






