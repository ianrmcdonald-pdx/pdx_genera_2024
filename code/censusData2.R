

# Code using API to pull from the census bureau info about income, housing and education.
# Clean and make tables, match to precinct and district then test plots
# need your own API Key

library(tidyverse)   
library(tidycensus)  # ACS
library(sf)
library(stringr)
library(ggplot2)


# census_api_key("____YOUR_KEY__", install = TRUE, overwrite = TRUE)


# ACS parameters
year <- 2024          # dataset year
state_fips <- "41"    # oregon
county_fips <- "051"  # Multnomah County


# pull acs tables and clean
getACS <- function(table_id) {
  
  var_dataset <- if (startsWith(table_id, "S")) "acs5/subject" else "acs5"   # choose correct ACS variable set
  
  vars <- load_variables(year, var_dataset, cache = TRUE)   # load variable metadata
  
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


## pull these specific tables
b19001_clean <- getACS("B19001")   # income distribution
s1501_clean  <- getACS("S1501")    # education
s1101_clean  <- getACS("S1101")    # housing + households




#### attach precinct and district info from cleanMap table
cleanMap <- st_transform(cleanMap, st_crs(b19001_clean))   # ensure CRS matches ACS data

attach_precincts <- function(acs_sf) {
  
  acs_sf <-acs_sf |>
    mutate(orig_area = st_area(geometry))    #compute original tract area
  
  inter <- st_intersection(acs_sf, cleanMap)  # intersect with precinct polygons
  
  inter |>
    mutate(
      intersect_area =st_area(geometry),               # intersected area
      weight = as.numeric(intersect_area / orig_area)        # get area weight
    ) |>
    select(
      GEOID, NAME, geometry,     # keep identifiers + geometry
      PRECINCT, DISTRICT,           # precinct + district
      weight,
      everything()          # keep all ACS columns
    )
}


# join precinct to all tables
b19001_precinct <- attach_precincts(b19001_clean)
s1501_precinct  <-attach_precincts(s1501_clean)
s1101_precinct  <- attach_precincts(s1101_clean)





## check, count precincts and disct
b19001Count <- b19001_precinct |> distinct(PRECINCT) |> nrow()  
s1501Count<- s1501_precinct  |> distinct(PRECINCT) |> nrow() 
s1101Count  <- s1101_precinct  |> distinct(PRECINCT) |> nrow() 


b19001Count
s1501Count
s1101Count


countingDiscts <- function(df) {
  df |>
    st_drop_geometry() |>
    distinct(PRECINCT, DISTRICT)|>       # unique precinct district combos
    count(DISTRICT, name = "n_precincts")       # count precincts per district
}

b19001_disctCounts <- countingDiscts(b19001_precinct)
s1501_disctCounts  <- countingDiscts(s1501_precinct)
s1101_disctCounts <-countingDiscts(s1101_precinct)

b19001_disctCounts
s1501_disctCounts
s1101_disctCounts

missing_precincts <- setdiff(cleanMap$PRECINCT, b19001_precinct$PRECINCT)   # precincts missing from ACS



#### cleaning

# remove empty cols
dropNAs <- function(df) {
  df |> select(where(~ !all(is.na(.x))))  # keep only columns with any non NA
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
    `Total...HOUSEHOLDS...Total.households`,          # total households
    `Total...HOUSEHOLDS...Average.household.size`,    # avg household size
    `Total...FAMILIES...Total.families`,             # total families
    `Total...FAMILIES...Average.family.size`,       # avg family size
    `Total...Total.households...HOUSING.TENURE...Owner.occupied.housing.units`,  # homeowners
    `Total...Total.households...HOUSING.TENURE...Renter.occupied.housing.units`  # renters
  )

s1101_housing_nogeo <- s1101_housing |> st_drop_geometry()       # drop geometry for joining


# keeping edu info to use
s1501_edu <- s1501_cleaned |>
  transmute(
    GEOID,
    PRECINCT,
    DISTRICT,
    geometry,  # keep geometry temporarily
    
    edu_pop_18_24 = `Total...AGE.BY.EDUCATIONAL.ATTAINMENT...Population.18.to.24.years`,
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

s1501_edu_nogeo<-s1501_edu |> st_drop_geometry()       # drop geom for joining



# build final table
censusMain <- b19001_cleaned |>
  left_join(s1101_housing_nogeo, by= c("GEOID", "PRECINCT", "DISTRICT")) |>   # add housing
  left_join(s1501_edu_nogeo, by = c("GEOID", "PRECINCT", "DISTRICT"))        # add education


# rename housing cols to clean names
censusMain <- censusMain |>
  rename(
    total_households = `Total...HOUSEHOLDS...Total.households`,
    avg_household_size = `Total...HOUSEHOLDS...Average.household.size`,
    total_families = `Total...FAMILIES...Total.families`,
    avg_family_size = `Total...FAMILIES...Average.family.size`,
    total_homeowners = `Total...Total.households...HOUSING.TENURE...Owner.occupied.housing.units`,
    total_renters = `Total...Total.households...HOUSING.TENURE...Renter.occupied.housing.units`
  )



# GIS test map plots
ggplot(censusMain) +
  geom_sf(aes(fill = PRECINCT), color = "white", size = 0.1) +   # map by precinct
  labs(
    title = "Precinct Boundaries",
    fill = "Precinct"
  ) +
  theme_minimal()

ggplot(censusMain) +
  geom_sf(aes(fill = DISTRICT), color = "black", size = 0.15) +  # map by district
  labs(
    title = "District Boundaries",
    fill = "District"
  ) +
  theme_minimal()
