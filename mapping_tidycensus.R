library(tidyverse)
library(tidycensus)
library(stringr)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(crsuggest)

## Load variables of the acs5 dataset (American Community Survey) ##
v15 <- load_variables(2019, "acs5")

glimpse(v15)

austin_home_value <- get_acs(geography = "tract", 
                             variables = "B25077_001", 
                             state = "TX",
                             county = "Travis County",
                             geometry = TRUE) 

glimpse(austin_home_value)
## Suggest a coordinate for Austin, TX ##
atx_crs <- suggest_top_crs(austin_home_value)

## Plot a map that represents home values (by colors) for different census tracts of Austin, TX ##
ggplot(austin_home_value, aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = atx_crs) + 
  scale_fill_viridis_c()

## Now we go back to the project: Crash counts at intersections in Texas ##
## Compute average crash count for each census tract in Texas (each census tract has a unique GEOID) ##
crash$GEOID <- as.character(crash$GEOID)
crash_tract <- crash %>% group_by(GEOID)
crash_tract <- crash_tract %>% summarise(
  avg_crash = mean(tot_crash_count)
)

## Consider the whole Texas ##
tx_pop <- get_acs(geography = "tract", 
                  variables = "B01003_001", 
                  state = "TX",
                  geometry = TRUE)
tx_pop$GEOID <- as.character(tx_pop$GEOID)
crash_tract <- dplyr::inner_join(crash_tract,tx_pop, by="GEOID")
crash_tract <- st_as_sf(crash_tract)

tx_crs <- suggest_top_crs(tx_pop)

## The colors represent the percentiles (average crashes) for individual census tracts ##
ggplot(crash_tract, aes(fill = percent_rank(avg_crash))) + 
  geom_sf(color = NA) + 
  coord_sf(crs = tx_crs, datum = NA) + 
  scale_fill_viridis_c(name="Percent rank (Average crash)")

## Compute population density, job density, median income, and median age for each census tract ##
tract_var<-crash %>% group_by(GEOID)
tract_var <- tract_var %>% summarise(
  pop_den = mean(pop_den),
  employ_den = mean(employ_den),
  med_income = mean(cen_tr_income),
  med_age = mean(cen_tr_age)
)
tract_var <- dplyr::inner_join(tract_var,tx_pop, by="GEOID")
tract_var <- st_as_sf(tract_var)

ggplot(tract_var, aes(fill = percent_rank(pop_den))) + 
  geom_sf(color = NA) + 
  coord_sf(crs = tx_crs, datum = NA) + 
  scale_fill_viridis_c(name="Percent rank (Population density)")

ggplot(tract_var, aes(fill = percent_rank(employ_den))) + 
  geom_sf(color = NA) + 
  coord_sf(crs = tx_crs, datum = NA) + 
  scale_fill_viridis_c(name="Percent rank (Employment density)")

ggplot(tract_var, aes(fill = med_income)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = tx_crs, datum = NA) + 
  scale_fill_viridis_c(limits=c(2499,74000), name="Median income",labels=scales::dollar)

ggplot(tract_var, aes(fill = med_age)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = tx_crs, datum = NA) + 
  scale_fill_viridis_c(name="Median age")


## Consider the Austin metropolitan area that consists of five counties ##
atx_pop <- get_acs(geography = "tract", 
                   variables = "B01003_001", 
                   state = "TX",
                   county = c("Travis County","Bastrop County","Caldwell County","Hays County","Williamson County"),
                   geometry = TRUE)
atx_pop$GEOID <- as.character(atx_pop$GEOID)
crash_tract <- dplyr::inner_join(crash_tract,atx_pop, by="GEOID")
crash_tract <- st_as_sf(crash_tract)

atx_crs <- suggest_top_crs(atx_pop)

ggplot(crash_tract, aes(fill = percent_rank(avg_crash))) + 
  geom_sf(color = NA) + 
  coord_sf(crs = atx_crs, datum = NA) + 
  scale_fill_viridis_c(name="Percent rank (Average crash)")
