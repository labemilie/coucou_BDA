##############################################################################################
# Load data for Lepus timidus (Species1) and Lepus europaeus (Species2) from GBIF

# packages
library(rgbif)
library(rnaturalearth)
library(ggplot2)
library(gridExtra)

#
# The two species of interest
Species1 <- c("Lepus timidus")  # darkgreen in maps
Species2 <- c("Lepus europaeus") # darkred in maps

#
# load the species data from Gbif

# 'CH' for Switzerland - 2 letters code for retrieving gbif data
  gbif_occur1 <- occ_data(scientificName = Species1, hasCoordinate = TRUE, limit = 200, country = "CH") # ~6000 occurences in Switzerland
  gbif_occur2 <- occ_data(scientificName = Species2, hasCoordinate = TRUE, limit = 200, country = "CH") # ~80 000 occurences in Switzerland

# 'AT' for Austria - 2 letters code for retrieving gbif data
  gbif_occur3 <- occ_data(scientificName = Species1, hasCoordinate = TRUE, limit = 200, country = "AT") # ~350 occurences in Austria
  gbif_occur4 <- occ_data(scientificName = Species2, hasCoordinate = TRUE, limit = 200, country = "AT") # ~9000 occurences in Austria
    
    occur1 <- gbif_occur1$data
    occur2 <- gbif_occur2$data
    occur3 <- gbif_occur3$data
    occur4 <- gbif_occur4$data

# Plot occurence data for Species1 in Switzerland
# modified the size to have smaller occurence points
  CH <- ne_countries(scale = "medium", returnclass = "sf",country = "Switzerland")
    map_gbif_occur1 <- ggplot(data = CH) +
      geom_sf()   +
      geom_point(data = occur1, aes(x = decimalLongitude, y = decimalLatitude), size = 3, 
      shape = 23, fill = "darkgreen") + theme_classic()
  # print(map_gbif_occur1)

# Plot occurence data for Species2 in Switzerland
# modified size to have smaller occurence points
  CH <- ne_countries(scale = "medium", returnclass = "sf",country ="Switzerland")
    map_gbif_occur2 <- ggplot(data = CH) +
      geom_sf()   +
      geom_point(data = occur2, aes(x = decimalLongitude, y = decimalLatitude), size = 3, 
      shape = 23, fill = "darkred") + theme_classic()
  # print(map_gbif_occur2)

# Plot occurence data for Species1 in Austria
# modified size to have smaller occurence points
  AT <- ne_countries(scale = "medium", returnclass = "sf",country ="Austria")
    map_gbif_occur3 <- ggplot(data = AT) +
      geom_sf()   +
      geom_point(data = occur3, aes(x = decimalLongitude, y = decimalLatitude), size = 3, 
      shape = 23, fill = "darkgreen") + theme_classic()
  # print(map_gbif_occur3)

# Plot occurence data for Species2 in Austria
# modified size to have smaller occurence points
  AT <- ne_countries(scale = "medium", returnclass = "sf",country ="Austria")
    map_gbif_occur4 <- ggplot(data = AT) +
      geom_sf()   +
      geom_point(data = occur4, aes(x = decimalLongitude, y = decimalLatitude), size = 3, 
      shape = 23, fill = "darkred") + theme_classic()
  # print(map_gbif_occur4)

# To have a rapid overview of the 4 maps... to check for the right distribution of occurence points
# ?grid.arrange --> respect = TRUE for better fitting of the 4 figures together
gbif_map_grid <- grid.arrange(map_gbif_occur1, map_gbif_occur2, map_gbif_occur3, map_gbif_occur4, ncol = 2, respect = TRUE)
print(gbif_map_grid)

##################################################################################################################
##################################################################################################################
##################################################################################################################
##############################################################################
# Load data for Lepus timidus (Species1) and Lepus europaeus (Species2) from iNaturalist

# packages
library(ggplot2)
library(rinat)
library(rnaturalearth)
library(raster)

#
# load the species data from iNaturalist in Switzerland
inat_occur1 <- get_inat_obs(query = Species1, place_id = "switzerland", maxresults = 100) # ~30 occurences in Switzerland
inat_occur2 <- get_inat_obs(query = Species2, place_id = "switzerland", maxresults = 100) # ~120 occurences in Switzerland

# load the species data from iNaturalist in Austria
inat_occur3 <- get_inat_obs(query = Species1, place_id = "austria", maxresults = 100) # ~45 occurences in Austria
inat_occur4 <- get_inat_obs(query = Species2, place_id = "austria", maxresults = 100) # ~1500 occurences in Austria

# Plot occurence data for Species1 in Switzerland
dat1 <- data.frame(as.numeric(inat_occur1$longitude),as.numeric(inat_occur1$latitude))
colnames(dat1) <- c("longitude", "latitude")
dat1 <- na.omit(dat1)
dat1 <- data.frame(dat1)

spatial_coord <- SpatialPoints(dat1)
plot(spatial_coord)
  switzerland <- ne_countries(scale = "medium", returnclass = "sf", country ="switzerland" )
    map_inat_occur1 <- ggplot(data = switzerland) +
      geom_sf() +
      geom_point(data = inat_occur1, aes(x = longitude, y = latitude), size = 3, 
      shape = 23, fill = "darkgreen") + theme_classic() 
  #print(map_inat_occur1)

# Plot occurence data for Species2 in Switzerland
dat2 <- data.frame(as.numeric(inat_occur2$longitude),as.numeric(inat_occur2$latitude))
colnames(dat2) <- c("longitude", "latitude")
dat2 <- na.omit(dat2)
dat2 <- data.frame(dat2)

spatial_coord <- SpatialPoints(dat2)
plot(spatial_coord)
  switzerland <- ne_countries(scale = "medium", returnclass = "sf",country ="switzerland" )
    map_inat_occur2 <- ggplot(data = switzerland) +
      geom_sf()   +
      geom_point(data = inat_occur2, aes(x = longitude, y = latitude), size = 3, 
      shape = 23, fill = "darkred") + theme_classic() 
  #print(map_inat_occur2)

# Plot occurence data for Species1 in Austria
dat3 <- data.frame(as.numeric(inat_occur3$longitude),as.numeric(inat_occur3$latitude))
colnames(dat3) <- c("longitude", "latitude")
dat3 <- na.omit(dat3)
dat3 <- data.frame(dat3)

spatial_coord <- SpatialPoints(dat3)
plot(spatial_coord)
  Austria <- ne_countries(scale = "medium", returnclass = "sf", country ="Austria" )
    map_inat_occur3 <- ggplot(data = Austria) +
      geom_sf() +
      geom_point(data = inat_occur3, aes(x = longitude, y = latitude), size = 3, 
      shape = 23, fill = "darkgreen") + theme_classic() 
  #print(map_inat_occur3)

# Plot occurence data for Species2 in Austria
dat4 <- data.frame(as.numeric(inat_occur4$longitude),as.numeric(inat_occur4$latitude))
colnames(dat4) <- c("longitude", "latitude")
dat4 <- na.omit(dat4)
dat4 <- data.frame(dat4)

spatial_coord <- SpatialPoints(dat2)
plot(spatial_coord)
  Austria <- ne_countries(scale = "medium", returnclass = "sf",country ="Austria" )
    map_inat_occur4 <- ggplot(data = Austria) +
      geom_sf()   +
      geom_point(data = inat_occur4, aes(x = longitude, y = latitude), size = 3, 
      shape = 23, fill = "darkred") + theme_classic() 
  #print(map_inat_occur4)

# To have a rapid overview of the 4 maps... to check for the right distribution of occurence points
# ?grid.arrange --> respect = TRUE for better fitting of the 4 figures together
inat_map_grid <- grid.arrange(map_inat_occur1, map_inat_occur2, map_inat_occur3, map_inat_occur4, ncol = 2, respect = TRUE)
print(inat_map_grid)