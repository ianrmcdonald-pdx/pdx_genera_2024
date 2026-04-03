library(sf)
library(tidyverse)
library(vote)


#map graphics
pdx_gen <- st_read(
  "Multnomah_Elections_Precinct_Split_2024/Multnomah_Elections_Precinct_Split_2024.shp")

pdx_gen <- st_transform(pdx_gen, crs=4269)

pcc_dist <- st_read("Portland_City_Council_Districts/Portland_City_Council_Districts.shp")
pcc_dist <- st_transform(pcc_dist, crs=4269)
st_crs(pcc_dist)

home_values <- st_transform(home_values, crs=4269)
voter_precincts <- st_read("Voter_Precincts/Voter_Precincts.shp")

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

d4 <- read_csv("City_of_Portland__Councilor__District_4_2024_11_29_17_26_12.cvr.csv")


d4_file <- "Precinct-Level Results/D4/2024-12-16_13-56-40_rctab_cvr.csv"
d4_votes <- read_csv(d4_file)
#d4_colnames <- colnames(d4)

# d4_locate_0 <- d4_colnames[11:length(d4_colnames)] |> 
#   str_locate("District 4:")
# 
# d4_locate_1 <- d4_colnames[11:length(d4_colnames)] |> 
#   str_locate("Winners 3:")
# 
# d4_locate_2 <- d4_colnames[11:length(d4_colnames)] |> 
#   str_locate(":NON")
# 
# d4_locate_3 <- bind_cols(d4_locate_1, cname = d4_colnames[11:214]) |> 
#   mutate(candidate = str_sub(cname, end + 1, -5)) |> 
#   mutate(choice = str_sub(cname, d4_locate_0[,"end"] + 1, d4_locate_0[,"end"] + 1)) |> 
#   select(cname, candidate, choice)

experiment <- d4
experiment_1 <- experiment |> 
  pivot_longer(cols = !c(RowNumber:Remade), 
               names_to = "choice", values_to = "choice_num")  |> 
  filter(choice_num == 1) 

experiment_2 <- experiment_1 |> 
  mutate(rank_cand_loc= str_locate(choice, "District 4:")[,"end"]) |> 
  mutate(rank_cand = str_sub(choice, rank_cand_loc + 1, rank_cand_loc + 1)) |> 
  mutate(cand_loc = str_locate(choice, "Winners 3:")[,"end"]) |> 
  mutate(cand = str_sub(choice, cand_loc + 1, -5)) |> 
  select(!(c(rank_cand_loc,cand_loc, choice_num))) |> 
  group_by(BallotID, PrecinctID, PrecinctStyleName, cand) |> 
  slice_min(rank_cand) |> 
  ungroup() |> 
  mutate(rank_cand =as.integer(rank_cand))

#find duplicatees and write a routine that eliminates them 
experiment_2 |>
  dplyr::summarise(n = dplyr::n(), .by = c(BallotID, PrecinctID, 
                                           PrecinctStyleName, cand)) |>
  dplyr::filter(n > 1L) 

experiment_3 <- experiment_2   |> 
  pivot_wider(id_cols = c("BallotID","PrecinctID","PrecinctStyleName"),
              names_from = cand,
              values_from = rank_cand) 

p2805 <- experiment_3 |> 
  filter(PrecinctID == 57) |> 
  select(!c(BallotID, PrecinctID, PrecinctStyleName))

experiment_4 <- experiment_3 |> 
  select(!c(BallotID, PrecinctID, PrecinctStyleName))

stv_pdx_d4<- stv(experiment_4, nseats = 3, eps = 1, invalid.partial = TRUE)

plot(stv_pdx_d4)

stv_pdx_d4_p2805<- stv(p2805, nseats = 3, eps = 1, invalid.partial = TRUE)
stv_pdx_d4_p2804<- stv(p2804, nseats = 3, eps = 1, invalid.partial = TRUE)
plot(stv_pdx_d4_p2804)

table(experiment_3$PrecinctID)
p2804 <- experiment_3 |> 
  filter(PrecinctID == 6) |> 
  select(!c(BallotID, PrecinctID, PrecinctStyleName))
stv_pdx_d4_p2804<- stv(p2804, nseats = 3, eps = 1, invalid.partial = TRUE)

(x <- complete.ranking(stv_pdx_d4_p2804))
plot(stv_pdx_d4_p2804)

View(stv_pdx_d4_p2805$preferences)

write_csv(experiment_4,"experiment_4.csv")

#Only the highest ranking that you give that candidate will be accepted and each lower ranking for the same candidate is ignored as if you had skipped that ranking.

#everyone ranked needs to be ranked a minimum value....no 2's if there is no 1, etc.  


#try to find discrepancies with report

