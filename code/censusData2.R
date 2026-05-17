

# Code using API to pull from the census bureau info about income, housing and education.
# Clean and make tables, match to precinct and district then test plots
# need your own API Key


library(tidyverse)   
library(tidycensus) # ACS
library(sf)
library(stringr)
library(ggplot2)


cleanMap <- st_read("data/cleanMapInfo.geojson")

# census_api_key("___YOUR_KEY__", install = TRUE, overwrite = TRUE)


# ACS parameters
year <- 2024
state_fips <- "41"   # oregon


# counties to pull
counties <- c(
  "051",   # multnomah
  "067",   # washington
  "005"    # clackamas
)


## function to pull and build county data
buildCountyACS <- function(county_fips) {
  
  # pull acs tables and clean
  getACS <- function(table_id) {
    
    var_dataset <- if (startsWith(table_id, "S")) "acs5/subject" else "acs5"   # choose correct ACS variable set
    
    vars <- load_variables(year, var_dataset, cache = TRUE)    # load variable metadata
    
    # pull ACS data
    acs_tidy <- get_acs(
      geography = "tract",
      table = table_id,
      state = state_fips,
      county = county_fips,
      year = year,
      survey = "acs5",
      geometry = TRUE,   # gis
      output = "tidy"
    )
    
    # clean labels
    acs_labeled <- acs_tidy |>
      left_join(vars, by = c("variable" = "name")) |>    # attach labels
      mutate(
        clean_label = label |>      # clean the label text
          str_replace_all("Estimate!!", "") |>
          str_replace_all("!!", " - ") |>
          str_squish()
      )
    
    acs_wide <- acs_labeled |>
      select(GEOID, NAME, geometry, clean_label, estimate) |>   # keep needed columns
      pivot_wider(
        names_from = clean_label,       # pivot labels to columns
        values_from = estimate
      ) |>
      st_as_sf()      # return as sf object
    
    acs_wide
  }
  
  
  ## pull these ACS tables
  b19001_clean <- getACS("B19001")   # income distribution
  s1501_clean  <- getACS("S1501")    # education
  s1101_clean  <- getACS("S1101")    # housing + households
  
  
  
  #### attach precinct and district info from cleanMap
  cleanMap_local <- cleanMap |>
    st_transform(st_crs(b19001_clean))   # ensure CRS matches ACS data
  
  attach_precincts <- function(acs_sf) {
    
    acs_sf <- acs_sf |>
      mutate(orig_area = st_area(geometry))    #compute original tract area
    
    inter <- st_intersection(acs_sf, cleanMap_local)  # intersect with precinct polygons
    
    inter |>
      mutate(
        intersect_area = st_area(geometry),               # intersected area
        weight = as.numeric(intersect_area / orig_area)        # get area weight
      ) |>
      select(
        GEOID,NAME,geometry,     # keep identifiers + geometry
        PRECINCT, DISTRICT,
        weight,
        everything()          # keep all ACS columns
      )
  }
  
  
  # join precincts to all tables
  b19001_precinct<- attach_precincts(b19001_clean)
  s1501_precinct  <- attach_precincts(s1501_clean)
  s1101_precinct  <-attach_precincts(s1101_clean)
  
  
  
  
  #### cleaning
  
  # remove empty cols, keep non NA
  dropNAs <- function(df) {
    df |> select(where(~ !all(is.na(.x))))
  }
  
  b19001_cleaned <- dropNAs(b19001_precinct)
  s1501_cleaned  <- dropNAs(s1501_precinct)
  s1101_cleaned  <- dropNAs(s1101_precinct)
  
  
  
  # keeping housing info to use
  s1101_housing <- s1101_cleaned |>
    select(
      GEOID,
      PRECINCT,
      DISTRICT,
      `Total...HOUSEHOLDS...Total.households`,
      `Total...HOUSEHOLDS...Average.household.size`,
      `Total...FAMILIES...Total.families`,
      `Total...FAMILIES...Average.family.size`,
      `Total...Total.households...HOUSING.TENURE...Owner.occupied.housing.units`,
      `Total...Total.households...HOUSING.TENURE...Renter.occupied.housing.units`
    )
  
  s1101_housing_nogeo <- s1101_housing |>st_drop_geometry()      # drop geometry for joining
  
  
  
  # keeping edu info to use
  s1501_edu <- s1501_cleaned |>
    transmute(
      GEOID,
      PRECINCT,
      DISTRICT,
      geometry,  # keep geometry temporarily
      
      edu_pop_18_24 =`Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.18.to.24.years`,
      
      edu_pop_25plus = `Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.25.years.and.over`,
      
      edu_pop_18plus = edu_pop_18_24 + edu_pop_25plus,       # combined population
      
      edu_hs_total =
        `Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.18.to.24.years...High.school.graduate..includes.equivalency.` +
        `Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.25.years.and.over...High.school.graduate..includes.equivalency.`,
      
      edu_somecollege_total =
        `Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.18.to.24.years...Some.college.or.associate.s.degree` +
        `Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.25.years.and.over...Some.college..no.degree` +
        `Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.25.years.and.over...Associate.s.degree`,
      
      edu_bach_total =
        `Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.18.to.24.years...Bachelor.s.degree.or.higher` +
        `Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.25.years.and.over...Bachelor.s.degree`,
      
      edu_grad_total =
        `Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.25.years.and.over...Graduate.or.professional.degree`
    )
  
  s1501_edu_nogeo <- s1501_edu |> st_drop_geometry()       # drop geom for joining
  

  #### leaving income as it is, so don't need to clean
  
  
  # build county table 
  county_table <- b19001_cleaned |>
    left_join(
      s1101_housing_nogeo,
      by = c("GEOID", "PRECINCT", "DISTRICT")
    ) |>
    left_join(
      s1501_edu_nogeo,
      by = c("GEOID", "PRECINCT", "DISTRICT")
    )
  
  
  
  ##rename housing cols
  county_table <- county_table |>
    rename(
      total_households =`Total...HOUSEHOLDS...Total.households`,
      
      avg_household_size =`Total...HOUSEHOLDS...Average.household.size`,
      
      total_families =`Total...FAMILIES...Total.families`,
      
      avg_family_size =`Total...FAMILIES...Average.family.size`,
      
      total_homeowners =`Total...Total.households...HOUSING.TENURE...Owner.occupied.housing.units`,
      
      total_renters =`Total...Total.households...HOUSING.TENURE...Renter.occupied.housing.units`
    )
  
  
  county_table
}



# build all counties
county_tables <- lapply(counties, buildCountyACS)

# combine all counties into one main table
censusAll1 <- bind_rows(county_tables)



# fix precinct
censusAll1$PRECINCT <- as.character(censusAll1$PRECINCT)
censusAll1$PRECINCT <- stringr::str_extract(censusAll1$PRECINCT, "\\d+")
censusAll1$PRECINCT <- as.integer(censusAll1$PRECINCT)




### GIS test map plots
ggplot(censusAll1) +
  geom_sf(aes(fill = as.factor(PRECINCT)), color = "white", size = 0.1) +   # map by precinct
  labs(
    title = "Precinct Boundaries",
    fill = "Precinct"
  ) +
  theme_minimal()


ggplot(censusAll1) +
  geom_sf(aes(fill = DISTRICT), color = "black", size = 0.15) +  # map by district
  labs(
    title = "District Boundaries",
    fill = "District"
  ) +
  theme_minimal()
