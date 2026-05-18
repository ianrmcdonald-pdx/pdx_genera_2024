# loads clean data from workspace
load("votingdata.RData")

# libraries
library(dplyr)
library(ggplot2)

############# FIRST-CHOICE VARIABLES ###########################################

###### MAYOR ######
# eliminates columns and leaves matrix of ranked candidates
mayor_candidates = mayor |>
  select(-X, -recordID, -precinct)

# runs through function for each voter
mayor_first = apply(mayor_candidates, 1, function(x) {
  
  # if voter ranked nobody --> return NA
  if(all(is.na(x))) return(NA)
  
  # converts ranked ballots into single "first choice" per voter
  names(x)[which.min(x)]
})

# reattaches removed columns and adds new first-choice mayor candidate column
mayor_top = mayor |> 
  select(recordID, precinct) |>
  mutate(mayor_choice = mayor_first)

###### DISTRICT 1 ######
d1_candidates = council_d1 |>
  select(-X, -recordID, -precinct)

d1_first = apply(d1_candidates, 1, function(x) {
  
  if(all(is.na(x))) return(NA)
  
  names(x)[which.min(x)]
})

d1_top = council_d1 |>
  select(recordID, precinct) |>
  mutate(council_choice = d1_first, district = "D1")

###### DISTRICT 2 ######
d2_candidates = council_d2 |>
  select(-X, -recordID, -precinct)

d2_first = apply(d2_candidates, 1, function(x) {
  
  if(all(is.na(x))) return(NA)
  
  names(x)[which.min(x)]
})

d2_top = council_d2 |>
  select(recordID, precinct) |>
  mutate(council_choice = d2_first, district = "D2")

###### DISTRICT 3 ######
d3_candidates = council_d3 |>
  select(-recordID, -precinct)

d3_first = apply(d3_candidates, 1, function(x) {
  
  if(all(is.na(x))) return(NA)
  
  names(x)[which.min(x)]
})

d3_top = council_d3 |>
  select(recordID, precinct) |>
  mutate(council_choice = d3_first, district = "D3")

###### DISTRICT 4 ######
d4_candidates = council_d4 |>
  select(-X, -recordID, -precinct)

d4_first = apply(d4_candidates, 1, function(x) {
  
  if(all(is.na(x))) return(NA)
  
  names(x)[which.min(x)]
})

d4_top = council_d4 |>
  select(recordID, precinct) |>
  mutate(council_choice = d4_first, district = "D4")

# combines datasets by voter --> keeps only voters who are in both datasets
# each row = one voter with mayor choice, council choice, and district
d1_merge = inner_join(mayor_top, d1_top, by="recordID")
d2_merge = inner_join(mayor_top, d2_top, by="recordID")
d3_merge = inner_join(mayor_top, d3_top, by="recordID")
d4_merge = inner_join(mayor_top, d4_top, by="recordID")

############# CONTINGENCY TABLES ###############################################

###### DISTRICT 1 ######
# builds matrix
tab_d1 = table(
  d1_merge$mayor_choice, 
  d1_merge$council_choice
)

###### DISTRICT 2 ######
tab_d2 = table(
  d2_merge$mayor_choice, 
  d2_merge$council_choice
)

###### DISTRICT 3 ######
tab_d3 = table(
  d3_merge$mayor_choice, 
  d3_merge$council_choice
)

###### DISTRICT 4 ######
tab_d4 = table(
  d4_merge$mayor_choice, 
  d4_merge$council_choice
)

############# CHI-SQUARE TESTS #################################################
# tests whether mayor choice and council choice are independent
# if independent there is no clustering
# if dependent voters group candidates together
chi_d1 = chisq.test(tab_d1)
chi_d2 = chisq.test(tab_d2)
chi_d3 = chisq.test(tab_d3)
chi_d4 = chisq.test(tab_d4)

############# REDUCE CANDIDATES TO >500 VOTES ##################################
# for chi-square test to be more accurate, I'm dropping candidates with >500 votes

###### DISTRICT 1 ######
# counts votes per candidate, keeps only "major" candidates (500+) and extracts names
# for major mayor candidates
major_mayors_d1 = names(table(d1_merge$mayor_choice)) [
  table(d1_merge$mayor_choice) > 500
]

# for major council candidates
major_council_d1 = names(table(d1_merge$council_choice)) [
  table(d1_merge$council_choice) > 500
]

# only keeps rows where both candidates are "major"
d1_filtered = d1_merge |>
  filter(
    mayor_choice %in% major_mayors_d1,
    council_choice %in% major_council_d1
  )

# creates new filtered contingency table --> fewer candidates, better chi-square validity
tab_d1 = table(d1_filtered$mayor_choice,
               d1_filtered$council_choice)

###### DISTRICT 2 ######
major_mayors_d2 = names(table(d2_merge$mayor_choice)) [
  table(d2_merge$mayor_choice) > 500
]

major_council_d2 = names(table(d2_merge$council_choice)) [
  table(d2_merge$council_choice) > 500
]

d2_filtered = d2_merge |>
  filter(
    mayor_choice %in% major_mayors_d2,
    council_choice %in% major_council_d2
  )

tab_d2 = table(d2_filtered$mayor_choice,
               d2_filtered$council_choice)

###### DISTRICT 3 ######
major_mayors_d3 = names(table(d3_merge$mayor_choice)) [
  table(d3_merge$mayor_choice) > 500
]

major_council_d3 = names(table(d3_merge$council_choice)) [
  table(d3_merge$council_choice) > 500
]

d3_filtered = d3_merge |>
  filter(
    mayor_choice %in% major_mayors_d3,
    council_choice %in% major_council_d3
  )

tab_d3 = table(d3_filtered$mayor_choice, 
               d3_filtered$council_choice)

###### DISTRICT 4 ######
major_mayors_d4 = names(table(d4_merge$mayor_choice)) [
  table(d4_merge$mayor_choice) > 500
]

major_council_d4 = names(table(d4_merge$council_choice)) [
  table(d4_merge$council_choice) > 500
]

d4_filtered = d4_merge |>
  filter(
    mayor_choice %in% major_mayors_d4,
    council_choice %in% major_council_d4
  )

tab_d4 = table(d4_filtered$mayor_choice,
               d4_filtered$council_choice)

############# CHI-SQUARE TESTS FOR >500 VOTES ##################################
chi_d1 = chisq.test(tab_d1)
chi_d1
chi_d2 = chisq.test(tab_d2)
chi_d2
chi_d3 = chisq.test(tab_d3)
chi_d3
chi_d4 = chisq.test(tab_d4)
chi_d4

# rounded results --> standardized residuals
rounded_chi_d1 = round(chi_d1$stdres, 2)
rounded_chi_d1
rounded_chi_d2 = round(chi_d2$stdres, 2)
rounded_chi_d2
rounded_chi_d3 = round(chi_d3$stdres, 2)
rounded_chi_d3
rounded_chi_d4 = round(chi_d4$stdres, 2)
rounded_chi_d4

############# HEATMAPS FOR STANDARD RESIDUALS >500 #############################

###### DISTRICT 1 ######
# turns matrix into dataframe
d1_heat = as.data.frame(chi_d1$stdres)
# builds grid of heatmap --> each tile is one candidate pairing and color is strength
ggplot(d1_heat,
       aes(x=Var2, y=Var1, fill=Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low="blue", # negative association
    mid="white", # neutral
    high="red", # positive association
    midpoint=0
    ) +
  labs(
    title="District 1 Standardized Residuals (>500 Vote Threshold)",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Std Residual"
  ) +
  theme_minimal()

###### DISTRICT 2 ######
d2_heat = as.data.frame(chi_d2$stdres)
ggplot(d2_heat,
       aes(x=Var2, y=Var1, fill=Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low="blue",
    mid="white",
    high="red",
    midpoint=0
  ) +
  labs(
    title="District 2 Standardized Residuals (>500 Vote Threshold)",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Std Residual"
  ) +
  theme_minimal() 

###### DISTRICT 3 ######
d3_heat = as.data.frame(chi_d3$stdres)
ggplot(d3_heat,
       aes(x=Var2, y=Var1, fill=Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low="blue",
    mid="white",
    high="red",
    midpoint=0
  ) +
  labs(
    title="District 3 Standardized Residuals (>500 Vote Threshold)",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Std Residual"
  ) +
  theme_minimal() 

###### DISTRICT 4 ######
d4_heat = as.data.frame(chi_d4$stdres)
ggplot(d4_heat,
       aes(x=Var2, y=Var1, fill=Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low="blue",
    mid="white",
    high="red",
    midpoint=0
  ) +
  labs(
    title="District 4 Standardized Residuals (>500 Vote Threshold)",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Std Residual"
  ) +
  theme_minimal() 
  
############# REDUCE CANDIDATES TO >1000 VOTES ##################################
# trade details for reliability --> more accurate chi-square tests

###### DISTRICT 1 ######
major_mayors_d1_1000 = names(table(d1_merge$mayor_choice)) [
  table(d1_merge$mayor_choice) > 1000
]

major_council_d1_1000 = names(table(d1_merge$council_choice)) [
  table(d1_merge$council_choice) > 1000
]

d1_filtered_1000 = d1_merge |>
  filter(
    mayor_choice %in% major_mayors_d1_1000,
    council_choice %in% major_council_d1_1000
  )

tab_d1_1000 = table(
  d1_filtered_1000$mayor_choice,
  d1_filtered_1000$council_choice
)

chi_d1_1000 = chisq.test(tab_d1_1000)

###### STANDARD RESIDUAL HEATMAP FOR DISTRICT 1 ######
d1_heat_1000 = as.data.frame(chi_d1_1000$stdres)

d1_heat_1000$Freq = pmax(pmin(d1_heat_1000$Freq, 10), -10)

ggplot(d1_heat_1000,
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = "District 1 Standardized Residuals (>1000 Vote Threshold)",
    subtitle = "Residuals truncated to [-10, 10]", 
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Std Residual"
  ) +
  coord_fixed() + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  )

###### DISTRICT 2 ######
major_mayors_d2_1000 = names(table(d2_merge$mayor_choice)) [
  table(d2_merge$mayor_choice) > 1000
]

major_council_d2_1000 = names(table(d2_merge$council_choice)) [
  table(d2_merge$council_choice) > 1000
]

d2_filtered_1000 = d2_merge |>
  filter(
    mayor_choice %in% major_mayors_d2_1000,
    council_choice %in% major_council_d2_1000
  )

tab_d2_1000 = table(
  d2_filtered_1000$mayor_choice,
  d2_filtered_1000$council_choice
)

chi_d2_1000 = chisq.test(tab_d2_1000)

###### STANDARD RESIDUAL HEATMAP FOR DISTRICT 2 ######
d2_heat_1000 = as.data.frame(chi_d2_1000$stdres)

d2_heat_1000$Freq = pmax(pmin(d2_heat_1000$Freq, 10), -10)

ggplot(d2_heat_1000,
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = "District 2 Standardized Residuals (>1000 Vote Threshold)",
    subtitle = "Residuals truncated to [-10, 10]",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Std Residual"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  )

###### DISTRICT 3 ######
major_mayors_d3_1000 = names(table(d3_merge$mayor_choice)) [
  table(d3_merge$mayor_choice) > 1000
]

major_council_d3_1000 = names(table(d3_merge$council_choice)) [
  table(d3_merge$council_choice) > 1000
]

d3_filtered_1000 = d3_merge |>
  filter(
    mayor_choice %in% major_mayors_d3_1000,
    council_choice %in% major_council_d3_1000
  )

tab_d3_1000 = table(
  d3_filtered_1000$mayor_choice,
  d3_filtered_1000$council_choice
)

chi_d3_1000 = chisq.test(tab_d3_1000)

###### STANDARD RESIDUAL HEATMAP FOR DISTRICT 3 ######
d3_heat_1000 = as.data.frame(chi_d3_1000$stdres)

d3_heat_1000$Freq = pmax(pmin(d3_heat_1000$Freq, 10), -10)

ggplot(d3_heat_1000,
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = "District 3 Standardized Residuals (>1000 Vote Threshold)",
    subtitle = "Residuals truncated to [-10, 10]",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Std Residual"
  ) +
  coord_fixed()+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  )

###### DISTRICT 4 ######
major_mayors_d4_1000 = names(table(d4_merge$mayor_choice)) [
  table(d4_merge$mayor_choice) > 1000
]

major_council_d4_1000 = names(table(d4_merge$council_choice)) [
  table(d4_merge$council_choice) > 1000
]

d4_filtered_1000 = d4_merge |>
  filter(
    mayor_choice %in% major_mayors_d4_1000,
    council_choice %in% major_council_d4_1000
  )

tab_d4_1000 = table(
  d4_filtered_1000$mayor_choice,
  d4_filtered_1000$council_choice
)

chi_d4_1000 = chisq.test(tab_d4_1000)

###### STANDARD RESIDUAL HEATMAP FOR DISTRICT 4 ######
d4_heat_1000 = as.data.frame(chi_d4_1000$stdres)

d4_heat_1000$Freq = pmax(pmin(d4_heat_1000$Freq, 10), -10)

ggplot(d4_heat_1000,
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = "District 4 Standardized Residuals (>1000 Vote Threshold)",
    subtitle = "Residuals truncated to [-10, 10]",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Std Residual"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  )

############# RAW VOTE HEATMAPS >1000 VOTES ####################################

###### DISTRICT 1 ######
# raw frequencies
# shows actual voting patterns, not statistical deviation
d1_counts = as.data.frame(tab_d1_1000)
ggplot(d1_counts, 
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white", # low co-occurrence
    high = "darkred" # high co-occurrence
  ) +
  
  labs(
    title = "District 1 Raw Vote Counts (>1000 Vote Threshold)",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Votes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  )

###### DISTRICT 2 ######
d2_counts = as.data.frame(tab_d2_1000)
ggplot(d2_counts, 
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white",
    high = "darkred"
  ) +
  
  labs(
    title = "District 2 Raw Vote Counts (>1000 Vote Threshold)",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Votes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  )

###### DISTRICT 3 ######
d3_counts = as.data.frame(tab_d3_1000)
ggplot(d3_counts, 
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white",
    high = "darkred"
  ) +
  
  labs(
    title = "District 3 Raw Vote Counts (>1000 Vote Threshold)",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Votes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  )

###### DISTRICT 4 ######
d4_counts = as.data.frame(tab_d4_1000)
ggplot(d4_counts, 
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white",
    high = "darkred"
  ) +
  
  labs(
    title = "District 4 Raw Vote Counts (>1000 Vote Threshold)",
    x = "Council Candidate",
    y = "Mayor Candidate",
    fill = "Votes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  )