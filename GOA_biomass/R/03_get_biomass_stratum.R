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
get_biomass_stratum <- function(racebase_tables = list(
                                  cruisedat = cruisedat,
                                  haul = haul,
                                  catch = catch
                                ),
                                speciescode = 30060, # POP
                                survey_area = "AI",
                                vulnerability = 1,
                                strata = switch(survey_area,
                                  "GOA" = goa_strata,
                                  "AI" = ai_strata
                                )) {
  At <- sum(strata$area)

  # Get cpue table
  x <- get_cpue(
    racebase_tables = racebase_tables,
    survey_area = survey_area,
    speciescode = speciescode
  )

  # Total CPUE for species, year, stratum
  # no RACEBASE equivalent (building block of BIOMASS_STRATUM)
  x2 <- x %>%
    group_by(year, stratum) %>%
    dplyr::summarize(
      haul_count = length(unique(stationid)), # number of total abundance hauls
      mean_wgt_cpue = mean(WGTCPUE),
      var_wgt_cpue = ifelse(haul_count <= 1, NA, var(WGTCPUE) / haul_count),
      mean_num_cpue = mean(NUMCPUE),
      var_num_cpue = ifelse(haul_count <= 1, NA, var(NUMCPUE) / haul_count),
      catch_count = length(which(Catch_KG > 0)) # number of hauls with nonzero catch
    ) %>%
    dplyr::ungroup() %>%
    select(
      year, stratum,
      haul_count, catch_count,
      mean_wgt_cpue, var_wgt_cpue,
      mean_num_cpue, var_num_cpue
    )

  if (all(x2$catch_count <= x2$haul_count)) {
    print("Number of hauls with positive catches is realistic.")
  }

  # RACEBASE equivalent table: BIOMASS_STRATUM
  biomass_stratum <- x2 %>%
    dplyr::left_join(strata) %>%
    rowwise() %>% # for applying ifelse() by row
    mutate(
      stratum_biomass = area * mean_wgt_cpue / vulnerability * 0.001, # kg --> mt
      stratum_ratio = area / At,
      biomass_var = area^2 * var_wgt_cpue * 1e-6, # kg--> mt, square it because it's variance
      qt_size = ifelse(haul_count <= 1, 0,
        qt(p = 0.025, df = haul_count - 1, lower.tail = F)
      ),
      min_biomass = stratum_biomass - qt_size * sqrt(biomass_var),
      max_biomass = stratum_biomass + qt_size * sqrt(biomass_var),
      stratum_pop = area * mean_num_cpue, # not sure why this calculation would be different from the stratum_biomass
      pop_var = area^2 * var_num_cpue,
      min_pop = stratum_pop - qt_size * sqrt(pop_var),
      max_pop = stratum_pop + qt_size * sqrt(pop_var)
    ) %>%
    mutate(
      min_biomass = ifelse(min_biomass < 0, 0, min_biomass),
      min_pop = ifelse(min_pop < 0, 0, min_pop)
    ) %>% # set low CI to zero if it's negative
    select(survey, year, stratum, stratum_ratio, haul_count, catch_count, mean_wgt_cpue, var_wgt_cpue, mean_num_cpue, var_num_cpue, stratum_biomass, biomass_var, min_biomass, max_biomass, stratum_pop, pop_var, min_pop, max_pop, area) %>%
    mutate(
      Ni = area / 0.01,
      fi = (Ni * (Ni - haul_count)) / haul_count
    )
  return(biomass_stratum)
}





