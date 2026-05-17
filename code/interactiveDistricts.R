

# makes interactive maps using leaflet by distrct level info only. 
# click on any district and it opens popup of information. map bounds based on coordinates.


library(sf)
library(dplyr)
library(leaflet)   # interactive map
library(htmltools) # easier text in popups
library(scales)


# read in data if necessary
# cleanMap <- st_read("data/cleanMapInfo.geojson")
# censusAll1 <- st_read("data/censusAll1.geojson")



# popup for housing
dist_housing_popup <- function(data){
  
  paste0(
    "<strong>District:</strong> ", data$DISTRICT, "<br><br>",
    
    "<strong>Total Households:</strong> ",
    comma(data$total_households), "<br>",
    
    "<strong>Homeowners:</strong> ",
    round(data$homeowner_pct, 1), "%<br>",
    
    "<strong>Renters:</strong> ",
    round(data$renter_pct, 1), "%<br>",
    
    "<strong>Average Household Size:</strong> ",
    round(data$avg_household_size, 2), "<br>",
    
    "<strong>Total Families:</strong> ",
    comma(data$total_families), "<br>",
    
    "<strong>Average Family Size:</strong> ",
    round(data$avg_family_size, 2)
  )
}



# popup for education
dist_education_popup <- function(data){
  
  paste0(
    "<strong>District:</strong> ", data$DISTRICT, "<br><br>",
    
    "<strong>Population 18-24:</strong> ",
    comma(data$edu_pop_18_24), "<br>",
    
    "<strong>Population 25+:</strong> ",
    comma(data$edu_pop_25plus), "<br>",
    
    "<strong>High School:</strong> ",
    comma(data$edu_hs_total), "<br>",
    
    "<strong>Some College:</strong> ",
    comma(data$edu_somecollege_total), "<br>",
    
    "<strong>Bachelor's:</strong> ",
    comma(data$edu_bach_total), "<br>",
    
    "<strong>Graduate Degree:</strong> ",
    comma(data$edu_grad_total)
  )
}



# popup for income
dist_income_popup <-function(data){
  
  paste0(
    "<strong>District:</strong> ", data$DISTRICT, "<br><br>",
    
    "<strong>Total Households:</strong> ",
    comma(data$Total.), "<br><br>",
    
    "<strong>10k - 15k:</strong> ",
    comma(data$Total.....10.000.to..14.999), "<br>",
    
    "<strong>15k - 20k:</strong> ",
    comma(data$Total.....15.000.to..19.999), "<br>",
    
    "<strong>20k - 25k:</strong> ",
    comma(data$Total.....20.000.to..24.999), "<br>",
    
    "<strong>25k - 30k:</strong> ",
    comma(data$Total.....25.000.to..29.999), "<br>",
    
    "<strong>30k - 35k:</strong> ",
    comma(data$Total.....30.000.to..34.999), "<br>",
    
    "<strong>35k - 40k:</strong> ",
    comma(data$Total.....35.000.to..39.999), "<br>",
    
    "<strong>40k - 45k:</strong> ",
    comma(data$Total.....40.000.to..44.999), "<br>",
    
    "<strong>45k - 50k:</strong> ",
    comma(data$Total.....45.000.to..49.999), "<br>",
    
    "<strong>50k - 60k:</strong> ",
    comma(data$Total.....50.000.to..59.999), "<br>",
    
    "<strong>60k - 75k:</strong> ",
    comma(data$Total.....60.000.to..74.999), "<br>",
    
    "<strong>75k - 100k:</strong> ",
    comma(data$Total.....75.000.to..99.999), "<br>",
    
    "<strong>100k - 125k:</strong> ",
    comma(data$Total.....100.000.to..124.999), "<br>",
    
    "<strong>125k - 150k:</strong> ",
    comma(data$Total.....125.000.to..149.999), "<br>",
    
    "<strong>150k - 200k:</strong> ",
    comma(data$Total.....150.000.to..199.999), "<br>",
    
    "<strong>200k+:</strong> ",
    comma(data$Total.....200.000.or.more)
  )
}




# cleaning and aggregating census data

districtAll <- censusAll1 |>
  
  st_transform(4326) |>                 # convert for leaflet
  st_collection_extract("POLYGON") |>   # ensure polygons
  group_by(DISTRICT) |>
  
  # summarize demographic + income variables by district
  summarize(
    
    total_households = sum(total_households, na.rm = TRUE),
    total_homeowners = sum(total_homeowners, na.rm = TRUE),
    total_renters = sum(total_renters, na.rm = TRUE),
    
    avg_household_size = mean(avg_household_size, na.rm = TRUE),
    
    total_families = sum(total_families, na.rm = TRUE),
    avg_family_size = mean(avg_family_size, na.rm = TRUE),
    
    edu_pop_18_24 = sum(edu_pop_18_24, na.rm = TRUE),
    edu_pop_25plus = sum(edu_pop_25plus, na.rm = TRUE),
    
    edu_hs_total = sum(edu_hs_total, na.rm = TRUE),
    edu_somecollege_total = sum(edu_somecollege_total, na.rm = TRUE),
    edu_bach_total = sum(edu_bach_total, na.rm = TRUE),
    edu_grad_total = sum(edu_grad_total, na.rm = TRUE),
    
    Total. = sum(Total., na.rm = TRUE),  # ACS income total
    across(starts_with("Total...."), ~sum(.x, na.rm = TRUE)), # all brackets
    
    geometry = st_union(geometry),       # merge precincts into district
    .groups = "drop"
  )


# homeowner and renter percents
districtAll <- districtAll |>
  
  mutate(
    homeowner_pct =
      100 * total_homeowners /
      (total_homeowners + total_renters),
    
    renter_pct =
      100 * total_renters /
      (total_homeowners + total_renters)
  )



#### map plots

district_housing_map <- make_map(
  districtAll,
  dist_housing_popup(districtAll),
  title = "Housing by District"
)
district_housing_map


district_education_map <- make_map(
  districtAll,
  dist_education_popup(districtAll),
  title = "Education by District"
)
district_education_map


district_income_map <- make_map(
  districtAll,
  dist_income_popup(districtAll),
  title ="Income by District"
)
district_income_map






# assuming the other file, " interactivePrecints.R " already loaded where this function was built
# put here for easy access, if that was not the case
# 
# 
# ### map builder function
# 
# # leaflet map with fixed bounds, colored by district with popups and legend
# make_map <- function(data, popup_data, title = "Map") {
#   
#   # color by district
#   pal <- colorFactor(
#     palette = "Set1",
#     domain = data$DISTRICT
#   )
#   
#   # map coord bounds, to not be so free roam
#   xmin <- -123.8313  # west  ,Astoria
#   ymin <-  45.1231   # south , McMinnville
#   xmax <- -122.0437  # east  , Ripplebrook
#   ymax <-  46.1879   # north  , Camas
#   
#   
#   # leaflet map with locked zoom
#   leaflet(
#     data,
#     options = leafletOptions(minZoom = 10)   # lock max zoom out
#   )|>
#     
#     addProviderTiles("CartoDB.Positron") |>  # basemap
#     
#     # visible map region
#     fitBounds(xmin, ymin, xmax, ymax) |>     # initial
#     setMaxBounds(xmin, ymin, xmax, ymax) |>  # max
#     
#     # draw precinct gis
#     addPolygons(
#       fillColor = ~pal(DISTRICT),  # district color
#       color ="black",        # borders
#       weight = 1,       # border width
#       fillOpacity= 0.7,
#       popup = popup_data,   # attach popup HTML
#       
#       # highlight on hover
#       highlightOptions =highlightOptions(
#         weight = 3,
#         color = "#666",
#         fillOpacity = 0.9,
#         bringToFront = TRUE
#       )
#     )|>
#     
#     # legend for district colors
#     addLegend(
#       "bottomright",
#       pal =pal,
#       values = ~DISTRICT,
#       title = title,
#       opacity = 1
#     )
# }
# 
