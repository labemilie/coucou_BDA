######################################################################################################
# Merge data from Gbif and iNaturalist databases

# packages
# packages
library(rgbif)         # For accessing GBIF occurrence data
library(rnaturalearth) # For obtaining spatial data
library(ggplot2)       # For data visualization
library(rinat)         # For accessing iNaturalist occurrence data
library(sf)            # For spatial operations
sf_use_s2(FALSE)
library(tidyverse)     # For data manipulation

## Species1 = Lepus timidus in Switzerland

# Extract relevant data from GBIF occurences (filter the "occur1" matrix)
  #species, decimalLatitude, decimalLongitude, stateProvince, eventDate
species <- occur1$species
latitude <- occur1$decimalLatitude
longitude <- occur1$decimalLongitude
canton <- occur1$stateProvince
date <- occur1$eventDate
source <- rep("gbif", length(species))
# create a data frame for GBIF data with the data that I chose
filtered_gbif1 <- data.frame(species, latitude, longitude, canton, date, source)

#
# Extract relevant data from iNaturalist occurences (filter the "inat_occur1" matrix)
# scientific_name, latitude, longitude, place_guess, observed_on
species <- inat_occur1$scientific_name
latitude <- inat_occur1$latitude
longitude <- inat_occur1$longitude
canton <- inat_occur1$place_guess
date <- inat_occur1$observed_on
source <- rep("inat", length(species))
# Create a data frame for iNaturalist data
filtered_inat1 <- data.frame(species, latitude, longitude, canton, date, source)

##############################################################################################################################
## Species2 = Lepus europaeus in Switzerland

# Extract relevant data from GBIF occurences (filter the "occur2" matrix)
#species, decimalLatitude, decimalLongitude, stateProvince, eventDate
species <- occur2$species
latitude <- occur2$decimalLatitude
longitude <- occur2$decimalLongitude
canton <- occur2$stateProvince
date <- occur2$eventDate
source <- rep("gbif", length(species))
# create a data frame for GBIF data with the data that I chose
filtered_gbif2 <- data.frame(species, latitude, longitude, canton, date, source)

#
# Extract relevant data from iNaturalist occurences (filter the "inat_occur2" matrix)
# scientific_name, latitude, longitude, place_guess, observed_on
species <- inat_occur2$scientific_name
latitude <- inat_occur2$latitude
longitude <- inat_occur2$longitude
canton <- inat_occur2$place_guess
date <- inat_occur2$observed_on
source <- rep("inat", length(species))
# Create a data frame for iNaturalist data
filtered_inat2 <- data.frame(species, latitude, longitude, canton, date, source)

#################################################################################################
## Species1 = Lepus timidus in Austria

# Extract relevant data from GBIF occurences (filter the "occur3" matrix)
  #species, decimalLatitude, decimalLongitude, stateProvince, eventDate
species <- occur3$species
latitude <- occur3$decimalLatitude
longitude <- occur3$decimalLongitude
canton <- occur3$stateProvince
date <- occur3$eventDate
source <- rep("gbif", length(species))
# create a data frame for GBIF data with the data that I chose
filtered_gbif3 <- data.frame(species, latitude, longitude, canton, date, source)

#
# Extract relevant data from iNaturalist occurences (filter the "inat_occur3" matrix)
# scientific_name, latitude, longitude, place_guess, observed_on
species <- inat_occur3$scientific_name
latitude <- inat_occur3$latitude
longitude <- inat_occur3$longitude
canton <- inat_occur3$place_guess
date <- inat_occur3$observed_on
source <- rep("inat", length(species))
# Create a data frame for iNaturalist data
filtered_inat3 <- data.frame(species, latitude, longitude, canton, date, source)

## Species2 = Lepus europaeus in Austria

# Extract relevant data from GBIF occurences (filter the "occur4" matrix)
#species, decimalLatitude, decimalLongitude, stateProvince, eventDate
species <- occur4$species
latitude <- occur4$decimalLatitude
longitude <- occur4$decimalLongitude
canton <- occur4$stateProvince
date <- occur4$eventDate
source <- rep("gbif", length(species))
# create a data frame for GBIF data with the data that I chose
filtered_gbif4 <- data.frame(species, latitude, longitude, canton, date, source)

#
# Extract relevant data from iNaturalist occurences (filter the "inat_occur4" matrix)
# scientific_name, latitude, longitude, place_guess, observed_on
species <- inat_occur4$scientific_name
latitude <- inat_occur4$latitude
longitude <- inat_occur4$longitude
canton <- inat_occur4$place_guess
date <- inat_occur4$observed_on
source <- rep("inat", length(species))
# Create a data frame for iNaturalist data
filtered_inat4 <- data.frame(species, latitude, longitude, canton, date, source)

#
# Combine GBIF and iNaturalist data frames
matrix_full <- rbind(filtered_gbif1, filtered_inat1, filtered_gbif3, filtered_inat3, filtered_gbif2, filtered_inat2, filtered_gbif4, filtered_inat4)
matrix_full <- matrix_full [!grepl("Lepus timidus varronis", matrix_full$species), ]
view(matrix_full)

#
# Plot combined data for Species1 on a map of Switzerland and Austria
countries <- ne_countries(scale = "medium", returnclass = "sf", country = c("Switzerland", "Austria"))
    map_matrix_full <- ggplot(countries) +
      geom_sf() +
      geom_point(data = matrix_full, aes(x = longitude, y = latitude, fill = source), size = 3, 
      shape = 23) + theme_classic()
print(map_matrix_full)