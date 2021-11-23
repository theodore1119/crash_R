library(tidyverse)
library(tidycensus)
library(tigris)

## Load variables in acs5 dataset (American Community Survey) ##
v1 <- load_variables(2019, "acs5")
View(v1)

## Obtain the median income, median age, population, and employment for census tracts across Texas ##
tract_medincome <- get_acs(geography = "tract", 
                           variables = c(medincome = "B06011_001"), 
                           state = "TX", 
                           year = 2019)
tract_medincome

tract_medage <- get_acs(geography = "tract", 
                        variables = c(medage = "B01002_001"), 
                        state = "TX", 
                        year = 2019)
tract_medage

tract_pop <- get_acs(geography = "tract", 
                     variables = c(population = "B01003_001"), 
                     state = "TX", 
                     year = 2019)
tract_pop

tract_employment <- get_acs(geography = "tract", 
                            variables = c(employment = "B23001_001"), 
                            state = "TX", 
                            year = 2019)
tract_employment

## Obtain the 11-digit GEOID from the latitudes and longitudes of the intersections, and GEOIDs represent the census tracts ##
coord<-crash[,c("lat","lon")]
coord$census_code <- apply(coord, 1, function(row) call_geolocator_latlon(row['lat'], row['lon']))
coord$census_tract <- substr(coord$census_code, 1, 11)
coord

census_block <- list()
for (i in 1:nrow(crash)) {
  census_block[[i]] <- latlong2fips(crash$lat[i], crash$lon[i], i)
}
# Row Bind the list into a data.frame object #
fips <- rbindlist(census_block)
fips$census_tract <- substr(fips$FIP, 1, 11)
fips
