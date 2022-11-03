# get_biomass_stratum

#' Calculate index of total biomass per stratum
#'
#' @param racebase_tables a list of the cruisedat, haul, and catch tables from RACEBASE (all regions, all years)
#' @param speciescode five-digit numeric species code from the GAP species guides
#' @param survey_area a character code for the survey area, either "GOA" or "AI"
#' @param vulnerability the vulnerability of the species to the survey (defaults to 1)
#' @param strata a dataframe with the number and areas of the strata. Defaults to the strata table from RACEBASE.
#'
#' @return a dataframe containing the mean weighted cpue, 
#' @export
#'
#' @examples
#' x <- get_biomass_stratum(speciescode = 30060,survey_area = "GOA")
#' head(x)
get_biomass_stratum_Length <- function(racebase_tables = list(
                                  cruisedat = cruisedat,
                                  haul = haul,
                                  catch = catch,
                                  lengthdat = lengthdat #BF added length file
                                ),
                                speciescode = speciescode_list$sp_code, #30060, # POP
                                #survey_area = "AI",
                                survey_area = "GOA",
                                vulnerability = 1,
                                strata = region_strata #switch(survey_area,  "GOA" = goa_strata,  "AI" = ai_strata)
                                ) {
  At <- sum(strata$area)

  # Get cpue table
  x <- get_cpue_Length(
    racebase_tables = racebase_tables,
    survey_area = survey_area,
    speciescode = speciescode
  )

  # Total CPUE for species, year, stratum
  # no RACEBASE equivalent (building block of BIOMASS_STRATUM)
  
  x2 <- x %>%
    #group_by(year, stratum, length) %>%  #includes year
    group_by(stratum, length) %>%  
    dplyr::summarize(
      haul_count = length(unique(stationid)), # number of total abundance hauls
      mean_wgtLbin_cpue = mean(WgtLBin_CPUE_km2), #Length step - average biomass (CPUE-/km2) per length bin across hauls
      var_wgtLbin_cpue = ifelse(haul_count <= 1, NA, var(WgtLBin_CPUE_km2) / haul_count),
      mean_numLbin_cpue =mean(NumLBin_CPUE_km2),
      var_numLbin_cpue = ifelse(haul_count <= 1, NA, var(NumLBin_CPUE_km2) / haul_count),
      catch_count = length(which(Catch_KG > 0)) # number of hauls with nonzero catch
    ) %>%
    dplyr::ungroup() %>%
    select(
      stratum,length,
      haul_count, catch_count,
      mean_wgtLbin_cpue, var_wgtLbin_cpue,
      mean_numLbin_cpue, var_numLbin_cpue #year
    )

  if (all(x2$catch_count <= x2$haul_count)) {
    print("Number of hauls with positive catches is realistic.")
  }

  # RACEBASE equivalent table: BIOMASS_STRATUM
  biomass_stratum.Lbin <- x2 %>%
    dplyr::left_join(strata) %>%
    rowwise() %>% # for applying ifelse() by row
    mutate(
      stratum_biomass.Lbin = area * mean_wgtLbin_cpue / vulnerability * 0.001, # kg --> mt
      stratum_ratio.Lbin = area / At,
      biomass_var.Lbin = area^2 * var_wgtLbin_cpue * 1e-6, # kg--> mt, square it because it's variance
      qt_size = ifelse(haul_count <= 1, 0,
        qt(p = 0.025, df = haul_count - 1, lower.tail = F)
      ),
      min_biomass.Lbin = stratum_biomass.Lbin - qt_size * sqrt(biomass_var.Lbin),
      max_biomass.Lbin = stratum_biomass.Lbin + qt_size * sqrt(biomass_var.Lbin),
      stratum_pop.Lbin = area * mean_numLbin_cpue, # not sure why this calculation would be different from the stratum_biomass
      pop_var.Lbin = area^2 * var_numLbin_cpue,
      min_pop.Lbin = stratum_pop.Lbin - qt_size * sqrt(pop_var.Lbin),
      max_pop.Lbin = stratum_pop.Lbin + qt_size * sqrt(pop_var.Lbin)
    ) %>%
    mutate(
      min_biomass.Lbin = ifelse(min_biomass.Lbin < 0, 0, min_biomass.Lbin),
      min_pop.Lbin = ifelse(min_pop.Lbin < 0, 0, min_pop.Lbin)
    ) %>% # set low CI to zero if it's negative
    add_column(RACE=speciescode) %>%
    select(survey, RACE, stratum, stratum_ratio.Lbin, haul_count, catch_count, length, mean_wgtLbin_cpue, var_wgtLbin_cpue, mean_numLbin_cpue, var_numLbin_cpue, stratum_biomass.Lbin, biomass_var.Lbin, min_biomass.Lbin, max_biomass.Lbin, stratum_pop.Lbin, pop_var.Lbin, min_pop.Lbin, max_pop.Lbin, area) %>% #removed year
    mutate(
      Ni = area / 0.01,
      fi = (Ni * (Ni - haul_count)) / haul_count
    )
 
  return(biomass_stratum.Lbin)
}

# #example to check working (POP)
# x <- get_biomass_stratum_Length(speciescode = 30060,survey_area = "GOA") #these are RACE species codes (not NODC)
# head(x)





