library(tidyverse)
library(tidycensus)
library(stringr)
library(leaflet)
library(sf)
library(tigris)
library(sp)
library(rgdal)
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

## Obtain shapefile of Texas ##
tx <- readOGR(dsn="shp_new", layer="tl_2016_48_cousub")
plot(tx)

# Extract the longitude and latitude columns from the dataset that contains only the high-crash intersections
coords <- highcrash[c("lon", "lat")]

# Making sure we are working with rows that don't have any blanks
coords <- coords[complete.cases(coords),]

# Letting R know that these are specifically spatial coordinates
sp <- SpatialPoints(coords)

## Plot the Texas map and red dots that indicate the high-crash intersections
tx_fortify <- fortify(tx)

gg <- ggplot()
gg <- gg + geom_polygon(data=tx_fortify, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill=NA, size=0.5) 
gg <- gg + geom_point(data=highcrash, aes(x=lon, y=lat, color="red",shape="."))
gg <- gg +  coord_map()
gg <- gg + theme(legend.position="none",axis.title = element_blank(),axis.ticks = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank())
gg

## Now we go back to the project: Crash counts at intersections in Texas ##
## Compute average crash count for each census tract in Texas (each census tract has a unique GEOID) ##
crash$GEOID <- as.character(crash$GEOID)
crash$cen_tr_pop[which(crash$cen_tr_pop==0)]<-1
crash_tract <- crash %>% group_by(GEOID)

crash_tract <- crash_tract %>% summarise(
  avg_crash = sum(tot_crash_count)/mean(cen_tr_pop)
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
  scale_fill_viridis_c(name="Percent rank (Crash per capita)") +
  theme(legend.title = element_text(size=25),
        legend.text = element_text(size=25))

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


## Consider the Austin metropolitan area that consists of six counties ##
atx_pop <- get_acs(geography = "tract", 
                  variables = "B01003_001", 
                  state = "TX",
                  county = c("Travis County","Bastrop County","Burnet County","Caldwell County","Hays County","Williamson County"),
                  geometry = TRUE)
atx_pop$GEOID <- as.character(atx_pop$GEOID)
crash_tract <- dplyr::inner_join(crash_tract,atx_pop, by="GEOID")
crash_tract <- st_as_sf(crash_tract)

atx_crs <- suggest_top_crs(atx_pop)

ggplot(crash_tract, aes(fill = percent_rank(avg_crash))) + 
  geom_sf(color = NA) + 
  coord_sf(crs = atx_crs, datum = NA) + 
  scale_fill_viridis_c(name="Percent rank (Crash per capita)") +
  theme(legend.title = element_text(size=25),
        legend.text = element_text(size=25))
