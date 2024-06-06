###########################################################################################
# Packages
library(remotes)
#remotes::install_github("wmgeolab/rgeoboundaries")
library(rgeoboundaries)
library(sf)
library(MODIStsp)
library(raster)

if(FALSE) {
# I did this code two times: one for Swizterland and one for Austria

# Downloading the country boundary of switzerland or Austria
map_boundary <- geoboundaries("Austria")

dir.create("./data/modis", recursive = TRUE)

# Defining filepath to save downloaded spatial file
spatial_filepath <- "./data/modis/austria.shp"
# Saving downloaded spatial file on to our computer
st_write(map_boundary, paste0(spatial_filepath))

#### check available data
#MODIStsp_get_prodlayers("M*D13Q1") # related to the greeness and productivity (chlorophyll)

MODIStsp(
 gui = FALSE,
  out_folder = "./data/modis",
  out_folder_mod = "./data/modis",
  selprod = "Vegetation Indexes_16Days_250m (M*D13Q1)",
  bandsel = "NDVI",
  user = "mstp_test",
  password = "MSTP_test_01",
  start_date = "2020.06.01",
  end_date = "2020.06.01",
  verbose = FALSE,
  spatmeth = "file",
  spafile = spatial_filepath,
  out_format = "GTiff"
)

# Downloading the boundary of switzerland or Austria
map_boundary <- geoboundaries("Austria")
map_boundary <- geoboundaries ("Switzerland")
}

# Reading in the downloaded NDVI raster data
NDVI_raster_CH <- raster("./data/CH_NDVI_2020_153.tif")
NDVI_raster_AUT <- raster("./data/AUT_NDVI_2020_153.tif")

# merge
NDVI_raster <- merge(NDVI_raster_CH, NDVI_raster_AUT)

# Transforming the data
NDVI_raster <- projectRaster(NDVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#plot(NDVI_raster)

# Cropping the data
map_boundary_cropped <- geoboundaries(list_countries)
NDVI_raster <- raster::mask(NDVI_raster, as_Spatial(map_boundary_cropped))
plot(NDVI_raster)

# Dividing values by 10000 to have NDVI values between -1 and 1
gain(NDVI_raster) <- 0.0001

# Assuming matrix_full is your data frame with latitude and longitude columns
spatial_points <- SpatialPoints(coords = matrix_full_elev_eco_clim[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(spatial_points,add=T)

# Extract values
NDVI <- raster::extract(NDVI_raster, spatial_points)
matrix_full_elev_eco_clim_ndvi <- data.frame(matrix_full_elev_eco_clim, NDVI)
view(matrix_full_elev_eco_clim_ndvi)