library(tidycensus)
library(tidyverse)
options(tigris_use_cache = TRUE)


# Set your API key


voter_precincts <- st_read("data/shapefiles/Voter_Precincts/Voter_Precincts.shp") |> 
  st_transform(4326)

# Fetch block group median home value for a specific county
home_values_mult <- get_acs(
  geography = "block group",
  variables = "B25077_001",
  state = "OR",
  county = "Multnomah",
  year = 2024,
  geometry = TRUE
) |> 
  st_transform(4326)


home_values_clack <- get_acs(
  geography = "block group",
  variables = "B25077_001",
  state = "OR",
  county = "Clackamas",
  year = 2024,
  geometry = TRUE
) |> 
  st_transform(4326)


home_values_wash <- get_acs(
  geography = "block group",
  variables = "B25077_001",
  state = "OR",
  county = "Washington",
  year = 2024,
  geometry = TRUE
) |> 
  st_transform(4326)


plot(home_values_mult["estimate"])

#spatial join block groups and precincts
sf::sf_use_s2(FALSE)

p1 <- st_join(home_values_mult, voter_precincts) |> 
  filter(PRECINCTID %in% c("M2803","M2804","M2805","M2806", "M2801"))
         
plot(p1["estimate"])
