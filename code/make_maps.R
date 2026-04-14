library(sf)
library(tidyverse)
library(vote)


#map graphics:  a few examples

pdx_gen <- st_read(
  "Multnomah_Elections_Precinct_Split_2024/Multnomah_Elections_Precinct_Split_2024.shp")

pdx_gen <- st_transform(pdx_gen, crs=4269)

pcc_dist <- st_read("Portland_City_Council_Districts/Portland_City_Council_Districts.shp")
pcc_dist <- st_transform(pcc_dist, crs=4269)
st_crs(pcc_dist)

home_values <- st_transform(home_values, crs=4269)
voter_precincts <- st_read("data/shapefiles/Voter_Precincts/Voter_Precincts.shp")

delete <- st_join(home_values, pdx_gen_d4)
delete <- st_intersection(pdx_gen_d4, home_values)
plot(delete["estimate"])

voter_precincts |> ggplot() + geom_sf()

pdx_gen_d4 <- pdx_gen %>% 
  filter(CoP_Dist == 4) 

sf_use_s2(FALSE)

delete <- st_intersection(pdx_gen, pdx_gen_d4)
delete %>% 
  ggplot() + geom_sf()

plot(voter_precincts["COUNTY"])

pdx_gen_d4 %>% 
  ggplot() + geom_sf()

voter_precincts %>% 
  ggplot() + geom_sf()
