#Prep data for datasandbox for 2023 Forest Resiliency Working Group
#Tyler McIntosh, 2/6/2023


#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

#######Libraries
library(here) #Relative path best practices
library(terra) #New raster library
library(sf) #New vector library

#####Clean workspace
rm(list=ls()) #Ensure empty workspace
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 10) #Set standard decimal print output


##################### SET ANALYSIS VARIABLES ########################

#Load EPA regions lvl 3 & select area of interest
aoiShp <- st_read("data/aoi/NA_CEC_Eco_Level3.shp")
unique(aoiShp$NA_L3NAME)
aoi <- aoiShp %>% filter(NA_L3CODE == "6.2.14") #SET THIS

#Set project projection
chosenCRS <- 'EPSG:32613' #SET THIS


#Set output folder
folder <- "new_sandbox" #SET THIS

########### ANALYSIS

#Reproject EPA lvl 3
aoi <- st_transform(aoi, chosenCRS)
st_write(aoi, here("data", folder, "EPA_lvl3_SRockies_epsg32613.shp"))

#Reproject EPA lvl 4 to match
epa4 <- st_read("data/aoi/EPA_lvl4_SRockies_raw3.shp")
epa4 <- st_transform(epa4, chosenCRS)
st_write(epa4, here("data", folder, "EPA_lvl4_SRockies_epsg32613.shp"))

#######Load topography data and write as single tif
topoFileNames <- list.files(here("data", folder), pattern = "USGS_SRTM*", full.names=TRUE)
topo_SRockies <- rast(topoFileNames)
writeRaster(topo_SRockies, here("data", folder, "topography_southern_rockies.tif"))

#########Load nlcd data & mosaic
nlcdFileNames <- list.files(here("data", folder),
                            pattern = "NLCD*", full.names = TRUE)
nlcdCollection <- sprc(nlcdFileNames) #Create as SpatRasterCollection since they aren't of the same area
nlcd <- mosaic(nlcdCollection) #Mosaic the SpatRasterCollection
writeRaster(nlcd_southern_rockies, here("data", folder, "nlcd_southern_rockies.tif"))

##########Clip, Mask, & Project the Disturbance Stack

#Load disturbance stack & fix names
distStackFileNames <- list.files(here("data", "data_stack_western_conus_v1", "western-conus", "western-conus"),
                                 pattern = "*.tif", full.names = TRUE)
distStack <- rast(distStackFileNames)

#Create disturbance layer names
dNamesRoot <- "forest-disturbance-s-rockies-"
dNums <- as.character(seq(1999, 2020))
dNames <- paste(dNamesRoot, dNums, sep="")
names(distStack) <- dNames

#Function to prepare raster data for analysis by setting CRS, clipping to a polygon,
#or if have the same extent, only mask.
#This will:
#1) change the raster and vector CRS to match the CRS set as 'setCRS'.
#2) clip and mask a SpatRaster to the extent & outline of the reprojected vector, and
#3) return a SpatRaster
#Arguments:
#raster - a SpatRaster
#vector - a SpatVector
#setCRS - a CRS in wkt character or EPSG formats
clipmask.raster.to.vector <- function(raster, vector, setCRS) {
  if (crs(vector) != setCRS) {
    print("Projecting vector")
    vector <- st_transform(vector, setCRS) 
    print("Vector projected")
  } else {
    print("Vector already in chosen CRS")
  }
  if (crs(raster) != setCRS) {
    print("Projecting raster")
    raster <- project(raster, setCRS)
    print("Raster projected")
  } else {
    print("Raster already in chosen CRS")
  }
  if(ext(raster) != ext(vector)) {
    print("Cropping raster to vector")
    raster <- crop(raster, ext(vector))
    print("Cropped")
  } else {
    print("Extents already match")
  }
  print("Masking raster to vector")
  rMask <- mask(raster, vector)
  print("Masked")
  print("Done")
  return(rMask)
}

#Run function
disturbance_stack_southern_rockies <- clipmask.raster.to.vector(distStack, aoi, chosenCRS)
writeRaster(disturbance_stack_southern_rockies, here("data", folder, "disturbance_stack_southern_rockies.tif"))
