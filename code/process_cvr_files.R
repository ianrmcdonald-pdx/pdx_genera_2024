library(sf)
library(tidyverse)
library(vote)


#precinctID reference

precinct_reference <- read_csv("Precinct_ID_Reference.csv") |> 
  mutate(id_number = str_trim(id_number), precinct = str_trim(precinct))


#city of portland version
d4_file_pdx_version <- "City_of_Portland__Councilor__District_4_2024_11_29_17_26_12.cvr.csv"
d4_votes_pdx_version <- read_csv(d4_file_pdx_version)

#multnomah county_version
#https://multco.us/info/maps-and-data-multnomah-county-elections
#there are a lot of undervotes on these ballots
d4_file_mc_version <- "Precinct-Level Results/D4/2024-12-16_13-56-40_rctab_cvr.csv"
d4_votes_mc_version <- read_csv(d4_file_mc_version)

d4_first_revision <- d4_votes_pdx_version |> 
  pivot_longer(cols = !c(RowNumber:Remade), 
               names_to = "choice", values_to = "choice_num")  |> 
  filter(choice_num == 1) 

d4_second_revision <- d4_first_revision |> 
  mutate(rank_cand_loc= str_locate(choice, "District 4:")[,"end"]) |> 
  mutate(rank_cand = str_sub(choice, rank_cand_loc + 1, rank_cand_loc + 1)) |> 
  mutate(cand_loc = str_locate(choice, "Winners 3:")[,"end"]) |> 
  mutate(cand = str_sub(choice, cand_loc + 1, -5)) |> 
  select(!(c(rank_cand_loc,cand_loc, choice_num))) |> 
  group_by(BallotID, PrecinctID, PrecinctStyleName, cand) |> 
  #this captures situations where a candidate is chosen more than once on the same ballot
  slice_min(rank_cand) |> 
  ungroup() |> 
  mutate(rank_cand = as.integer(rank_cand))

#find duplicates and write a routine that eliminates them 
#the stv function does a pretty good job of eliminating invalids but who knows
#what its really doing.  What abour scenario where there is no 1 but a 2?

#deal with this problem at some point.  Ballot ID RCV-0033+10091 is an example

d4_second_revision |>
  dplyr::summarise(n = dplyr::n(), .by = c(BallotID, PrecinctID, 
                                           PrecinctStyleName, cand)) |>
  dplyr::filter(n > 1L) 

d4_third_revision <- d4_second_revision   |> 
  pivot_wider(id_cols = c("BallotID","PrecinctID","PrecinctStyleName"),
              names_from = cand,
              values_from = rank_cand) 

#this is the format that works in the vote package
d4_fourth_revision <- d4_third_revision |> 
  select(!c(BallotID, PrecinctID, PrecinctStyleName))

#apply stv function and loop it
stv_pdx_d4_all <- stv(d4_fourth_revision, nseats = 3, eps = 1, invalid.partial = TRUE)
s_func <- summary(stv_pdx_d4_all)
c_rank <- complete.ranking(stv_pdx_d4_all)

precinctIDs <- as.integer(unique(d4_third_revision$PrecinctID))


precinct_votes <- list()
for(i in 1:length(precinctIDs)) {
  precinct_votes[[i]] <- d4_third_revision |> 
    filter(PrecinctID == precinctIDs[i]) |> 
    select(!c(BallotID, PrecinctID, PrecinctStyleName))
}

stv_pdx_04 <- list()
summary_d4 <- list()
c_rank_d4 <- list()

for(i in 1:length(precinctIDs)) {
  print(i)
  if(nrow(precinct_votes[[i]]) >= 10) {
    stv_pdx_04[[i]] <- stv(precinct_votes[[i]], nseats = 3, eps = 1, invalid.partial = TRUE)
    summary_d4[[i]] <- summary(stv_pdx_04[[i]])
    c_rank_d4[[i]] <- complete.ranking(stv_pdx_04[[i]]) %>% 
      mutate(c_precinct = precinctIDs[i])
  }
}
summary_d4_all <- summary(stv_pdx_d4_all)
review <- summary_d4_all[,59:65]

c_rank_all <- bind_rows(c_rank_d4)




#olivia clark voter analysis

oc <- d4_third_revision |> 
  filter(`Olivia Clark` <= 1, na.rm=TRUE) |> 
  mutate(mitch = ifelse(!is.na(`Mitch Green`),1,0),
         eric = ifelse(!is.na(`Eric Zimmerman`),1,0))

table(oc$mitch)
table(oc$eric)


mg <- d4_third_revision |> 
  filter(`Mitch Green` <= 1, na.rm=TRUE) |> 
  mutate(olivia = ifelse(!is.na(`Olivia Clark`),1,0),
         eric = ifelse(!is.na(`Eric Zimmerman`),1,0),
         sarah = ifelse(!is.na(`Sarah Silkie`),1,0),
         lisa = ifelse(!is.na(`Lisa Freeman`),1,0),
         eli = ifelse(!is.na(`Eli Arnold`),1,0))

ea <- d4_third_revision |> 
  filter(`Eli Arnold` <= 3, na.rm=TRUE) |> 
  mutate(olivia = ifelse(!is.na(`Olivia Clark`),1,0),
         eric = ifelse(!is.na(`Eric Zimmerman`),1,0),
         sarah = ifelse(!is.na(`Sarah Silkie`),1,0),
         lisa = ifelse(!is.na(`Lisa Freeman`),1,0),
         mitch = ifelse(!is.na(`Mitch Green`),1,0))

ss <- d4_third_revision |> 
  filter(`Sarah Silkie` <= 3, na.rm=TRUE) |> 
  mutate(olivia = ifelse(!is.na(`Olivia Clark`),1,0),
         eric = ifelse(!is.na(`Eric Zimmerman`),1,0),
         eli = ifelse(!is.na(`Eli Arnold`),1,0),
         lisa = ifelse(!is.na(`Lisa Freeman`),1,0),
         mitch = ifelse(!is.na(`Mitch Green`),1,0))

mg4 <- d4_fourth_revision |> 
  filter(`Mitch Green` == 1 | `Mitch Green` == 2 | `Mitch Green` == 3) |> 
  stv(nseats = 3, eps = 1, invalid.partial = TRUE)

mg_ranked <- d4_fourth_revision |> 
  filter(!is.na(`Mitch Green`))

ez_ranked <- d4_fourth_revision |> 
  filter(!is.na(`Eric Zimmerman`))

ea_ranked <- d4_fourth_revision |> 
  filter(!is.na(`Eli Arnold`))

oc_ranked <- d4_fourth_revision |> 
  filter(!is.na(`Olivia Clark`))
  
oc4 <- d4_fourth_revision |> 
  filter(`Olivia Clark` == 1) |> 
  stv(nseats = 3, eps = 1, invalid.partial = TRUE)
plot(oc4)

ea4 <- d4_fourth_revision |> 
  filter(`Eli Arnold` ==1) |> 
  stv(nseats = 3, eps = 1, invalid.partial = TRUE)

ss4 <- d4_fourth_revision |> 
  filter(`Sarah Silkie` ==1) |> 
  #filter(!is.na(`Sarah Silkie`)) |> 
  stv(nseats = 3, eps = 1, invalid.partial = TRUE)

mg4 <- d4_fourth_revision |> 
  filter(`Mitch Green` == 1) |> 
  stv(nseats = 3, eps = 1, invalid.partial = TRUE)


oc4_summ <- summary(oc4)
ss4_summ <- summary(ss4)
ea4_summ <- summary(ea4)

mg4_summ <- summary(mg4) #|> 
  #mutate(cand = row.names(mg4_summ)) #|> 
  #filter(cand %in% sankey_cands)
  

  
x <- summary(ea4)
#plot(stv_pdx_d4_p2804)


#Only the highest ranking that you give that candidate will be accepted and each lower ranking for the same candidate is ignored as if you had skipped that ranking.

#everyone ranked needs to be ranked a minimum value....no 2's if there is no 1, etc.  


#try to find discrepancies with report

