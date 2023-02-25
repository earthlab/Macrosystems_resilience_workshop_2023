#Clean & clip data for 2023 Forest Resiliency Working Group
#Tyler McIntosh, 2/6/2023

#This script takes in data and clips it to an AOI
#You need to choose: An AOI (needs to overlap with the incoming rasters), as well as a CRS
#This is most easily used with the GEE script "DownloadStandardData" to pull these data layers from GEE
#In both scripts, select the same projection and the same AOI identifier


#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

############ SETUP WORKSPACE ############

###Standard libraries, unneeded libraries commented out

#####Standard libraries
library(tidyverse) #Tidyverse!
library(here) #Relative path best practices
#library(googlesheets4) #Access data from google sheets
#library(installr) #Used on Windows to update R via command updateR()

#####Visualization libraries
library(patchwork) #Extends use of ggplot2 for multi-plot layout
#library(kableExtra) #Table creation
#library(knitr) #For use with R markdown
library(RColorBrewer) #color palettes.
# + scale_fill_brewer() for box plot, bar plot, violin plot, dot plot, etc
# + scale_color_brewer() for lines and points
display.brewer.all(colorblindFriendly = TRUE) #Show all colorblind-friendly palettes
library(ggthemes) #GG themes
#library(lattice) #Easy visualization of multidimensional data, e.g. scatterplot matrix: splom()
#library(scatterplot3d) #Visualization of three-dimensional point cloud. Example below:
#prepare 3-D scatterplot
#s3d <-with(trees, scatterplot3d(Girth, Height, Volume, pch=16, highlight.3d = TRUE, angle=60))
#fit <-lm(Volume ~ Girth + Height, data  =trees) # MLR regression model
#s3d$plane3d(fit) #produce 3-D plot/ of "best fit" plane through the 3D scatterplot

#####Geographic libraries
library(terra) #New raster data package, documentation pdf here: https://cran.r-project.org/web/packages/terra/terra.pdf
library(sf) #New vector data package
library(tmap) #Thematic mapping; static & interactive maps
library(tmaptools) #Supports Tmap
library(leaflet) #interactive maps
library(raster) #Old raster data package--often needed to use older packages
#library(landscapemetrics) #Fragstats alternative

#####Statistics & Mathematics
#library(modeest) #Modes of data
#library(moments) #Skewness & kurtosis
#library(REdaS) #Conversion between degrees & radians

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

#######Set project projection
chosenCRS <- 'EPSG:32613' #SET THIS

#Set output folder
folder <- "data_sandbox_masked2"

aoi <- st_transform(aoi, chosenCRS)
#st_write(aoi, "data/aoi/EPA_lvl3_SRockies_epsg32613.shp")

################## LOAD DATASETS #################

########Load disturbance stack & fix names
distStackFileNames <- list.files("data/data_stack_western_conus_v1/western-conus/western-conus",
                                 pattern = "*.tif", full.names = TRUE)
distStack <- rast(distStackFileNames)

#Create disturbance layer names
dNamesRoot <- "forest-disturbance-s-rockies-"
dNums <- as.character(seq(1999, 2020))
dNames <- paste(dNamesRoot, dNums, sep="")
names(distStack) <- dNames

#Rough-crop disturbance stack to smaller area to make re-projection faster
#########################
########ADD THIS#########
#########################

#######Load topography data
topoFileNames <- list.files("data/data_sandbox", pattern = "USGS_SRTM*", full.names=TRUE)
topo <- rast(topoFileNames)

######Load additional drought data & fix names
pdsi <- rast("data/data_sandbox/pdsi_Annual.tif")
spei1y <- rast("data/data_sandbox/spei1y_Annual.tif")
spei5y <- rast("data/data_sandbox/spei5y_Annual.tif")
spei30d <- rast("data/data_sandbox/spei30d_Annual.tif")

#Function to substring from right; requires string & 'n' characters to keep from right of string
substrRight <- function(str, n) {
  return (substr(str, nchar(str)-n+1, nchar(str)))
}

#Rename bands and create list of all drought SpatRasters
names(pdsi) <- names(pdsi) %>% substrRight(21)
names(spei30d) <- names(spei30d) %>% substrRight(24)
names(spei1y) <- names(spei1y) %>% substrRight(23)
names(spei5y) <- names(spei5y) %>% substrRight(23)
drought <- list(pdsi, spei30d, spei1y, spei5y)
names(drought) <- c("pdsi", "spei30d", "spei1y", "spei5y")


#######Load modis data & fix names for easier use
modisFileNames <- list.files("data/data_sandbox", pattern = "Modis*", full.names=TRUE)
modisShortNames <- list.files("data/data_sandbox", pattern = "Modis*") %>%
  lapply(function(nm) {substr(nm,1,nchar(nm)-4)})
modis <- modisFileNames %>% lapply(function(file) {rast(file)})
names(modis) <- modisShortNames

#Function to fix modis names
fixModisNm <- function(str) {
  date <- str %>% substr(1, 10)
  fileNm <- str %>% substrRight(nchar(str)-11)
  newStr <- paste(fileNm, date, sep = "_")
  return(newStr)
}

#Fix all band names in modis list
modis <- modis %>% lapply(function(rast) {
  bandNms <- names(rast) %>% fixModisNm()
  names(rast) <- bandNms
  return(rast)
})

#########Load nlcd data & mosaic
nlcdFileNames <- list.files("data/data_sandbox",
                            pattern = "NLCD*", full.names = TRUE)
nlcdCollection <- sprc(nlcdFileNames) #Create as SpatRasterCollection since they aren't of the same area
nlcd <- mosaic(nlcdCollection) #Mosaic the SpatRasterCollection


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



##############RUN FUNCTION ON DATASETS & WRITE#############

topo_SRockies <- clipmask.raster.to.vector(topo, aoi, chosenCRS)
writeRaster(topo_SRockies, here("data", folder, "topography_southern_rockies.tif"))

disturbance_stack_southern_rockies <- clipmask.raster.to.vector(distStack, aoi, chosenCRS)
writeRaster(disturbance_stack_southern_rockies, here("data", folder, "disturbance_stack_southern_rockies.tif"))

nlcd_southern_rockies <- clipmask.raster.to.vector(nlcd, aoi, chosenCRS)
writeRaster(nlcd_southern_rockies, here("data", folder, "nlcd_southern_rockies.tif"))

#Use lapply to do this efficiently for the drought & modis data
drought_southern_rockies <- drought %>% lapply(function(dLayer){clipmask.raster.to.vector(dLayer, aoi, chosenCRS)})
modis_southern_rockies <- modis %>% lapply(function(dLayer){clipmask.raster.to.vector(dLayer, aoi, chosenCRS)})


#Export function: This function takes in a spatraster and a name of the layer, then exports
exportDats <- function(spatR, rNm) {
  fileName <- paste(rNm, "_southern_rockies.tif", sep="")
  writeRaster(spatR, here("data", folder, fileName))
}

#Use mapply to do this efficiently for the drought & modis data
mapply(exportDats, drought_southern_rockies, names(drought_southern_rockies))
mapply(exportDats, modis_southern_rockies, names(modis_southern_rockies))


#################REMOVE THIS ONCE FIX UP TOP###############

#re-load processed disturbance stack
distStackSRockies <- rast(here("data", "data_sandbox_masked", "disturbance_stack_southern_rockies.tif"))
crs(distStackSRockies)
reprojSRDS <- project(distStackSRockies, chosenCRS)

writeRaster(reprojSRDS, here("data", folder, "disturbance_stack_southern_rockies_EPSG32613.tif"))


#EPA LVL 4
epa4 <- st_read("data/aoi/EPA_lvl4_SRockies_raw3.shp")
epa4 <- st_transform(epa4, chosenCRS)
st_write(epa4, "data/aoi/new_EPA_lvl4_SRockies_epsg32613.shp")

plot(topo$elevation)
plot(epa4$geometry)
unique(epa4$US_L4NAME)
epa4_select <- epa4 %>% filter(US_L4NAME == "Crystalline Mid-Elevation Forests") #SET THIS
mini <- terra::crop(topo_SRockies, ext(epa4_select))
mini <- terra::mask(mini, epa4_select)
terra::plot(mini$elevation)
terra::hist(mini$elevation)



