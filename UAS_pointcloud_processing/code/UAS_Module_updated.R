#check the required libraries
list.of.packages <- c("tidyverse","lidR","terra","raster","rgdal","ForestTools","RCSF","sp","sf","stars","rgl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(lidR)
library(terra)
library(raster)
library(rgdal)
library(ForestTools)
library(RCSF)
library(sp)
library(sf)
library(stars)
library(rgl)


# set working environment
setwd("D:/Carbon_dynamics/UAS-processing/Test")

# explore the raw data
file_list = dir("D:/Carbon_dynamics/UAS-processing/Test", pattern = ".laz", full.names = FALSE, ignore.case = TRUE) 
names_to_cloud = substr(file_list, 2, nchar(file_list)-4) 

#get the raw point cloud data from the data storage and display the point cloud
#read digital surface model in raster format
cropped_dense_point_cloud_fname <- file.path(paste0(file_list[2]))
dense_point_cloud <- lidR::readLAS(cropped_dense_point_cloud_fname)
plot(dense_point_cloud,color = "Z")


main_dsm = raster("D:/Carbon_dynamics/UAS-processing/Test/dsm_bigelk_1.tif")
test_dsm = crop(main_dsm,dense_point_cloud)
plot(test_dsm, main="Digital Surface Model")




#assign variables and file saving location
site_name <- "Test"
flight_datetime <- "07-26-2022"

# directories to be created in this script
new_dir <- file.path("data", site_name, flight_datetime)

  if(!dir.exists(new_dir)) {
    dir.create(new_dir, recursive = TRUE)
  }

classified_dense_pc <- file.path("data", site_name,flight_datetime, paste0( names_to_cloud[2],"_classified2.las"))
  
cropped_dtm_fname <- file.path("data", site_name,flight_datetime, paste0(names_to_cloud[2], "_dtm2.tif"))
cropped_chm_fname <- file.path("data", site_name,flight_datetime,paste0(names_to_cloud[2], "_chm2.tif"))

cropped_ttops_fname <- file.path("data", site_name, flight_datetime, paste0(names_to_cloud[2], "_ttops_cropped2.gpkg"))
cropped_crowns_fname <- file.path("data", site_name, flight_datetime,paste0(names_to_cloud[2], "_crowns_cropped2.gpkg"))





# create digital terrain model
dense_point_cloud[["X scale factor"]] = 0.001
dense_point_cloud[["Y scale factor"]] = 0.001

classified_dense_point_cloud <- lidR::classify_ground(las = dense_point_cloud,
                                                      algorithm = csf(sloop_smooth = TRUE,
                                                                      class_threshold = 0.25,
                                                                      cloth_resolution = 0.50, # was 0.5 before,
                                                                      rigidness = 1,
                                                                      iterations = 500,
                                                                      time_step = 0.65))

plot(classified_dense_point_cloud,color = "Classification")


if(!file.exists(classified_dense_pc)) {
  lidR::writeLAS(las = classified_dense_point_cloud, file = classified_dense_pc)
}


# create canopy height model
dtm <- lidR::grid_terrain(las = classified_dense_point_cloud,
                          res = 0.10, # was 0.5 and 0.25 before
                          algorithm = tin())

#raster::writeRaster(x = dtm, filename = cropped_dtm_fname, overwrite = TRUE)
terra::writeRaster(x = dtm, filename = cropped_dtm_fname, overwrite = TRUE)

dtm_resamp <- raster::resample(x = dtm, y = test_dsm, method = "bilinear")

plot(dtm_resamp, color =  "values",main = "Digital terrain model")


chm <- test_dsm - dtm_resamp
  
chm_smooth <- raster::focal(chm, w = matrix(1, 3, 3), mean)
chm_smooth[raster::getValues(chm_smooth) < 0] <- 0

terra::writeRaster(x = chm_smooth, filename = cropped_chm_fname, overwrite = TRUE)
plot(chm_smooth,color = "values", main =  "Canopy height model")



# detecting tree tops

chm = terra::rast(cropped_chm_fname)
max_chm = chm@ptr$range_max
print(max_chm)


a <- 0.3 # was 0.3
b <- 0.04 #0.04
c <- 0 #0

lin <- function(x){x^2*c + x*b + a} # window filter function to use in next step

ttops <- ForestTools::vwf(CHM = raster::raster(chm), 
                          winFun = lin, 
                          minHeight = 0.5,# chnage min height based on research objectives 
                          maxWinDiameter = 99) %>% 
  sf::st_as_sf()
    
ttop_crd = ttops %>%
    mutate(x = unlist(map(ttops$geom,1)),
           y = unlist(map(ttops$geom,2)))


plot(chm_smooth,color = "values", main =  "Canopy height model")
points(ttop_crd$x,ttop_crd$y)

if(!file.exists(cropped_ttops_fname)) {
  
  sf::st_write(obj = ttops, dsn = cropped_ttops_fname, delete_dsn = TRUE)
}
  

# tree crown delineation

  
  # if (!file.exists(cropped_ttops_fname)){
  #   next
  # }
  # 
  # read necessary data products
ttops <- sf::st_read(cropped_ttops_fname)
  # the {terra} package will be replacing {raster}, so here's how this is done in {terra}
  
  

non_spatial_ttops <-
  ttops %>%
  dplyr::mutate(x = st_coordinates(.)[, 1],
                y = st_coordinates(.)[, 2]) %>%
  sf::st_drop_geometry()
  
crowns <-
  ttops %>%
  ForestTools::mcws(CHM = raster::raster(chm), minHeight = 0.5, format = "raster") %>% # minheight was 1 m before
  setNames(nm = "treeID") %>%
  st_as_stars() %>%
  st_as_sf(merge = TRUE) %>%
  dplyr::left_join(non_spatial_ttops, by = "treeID") %>% 
  sf::st_make_valid()

for (i in 1:nrow(crowns)) {
  this_point <- sf::st_drop_geometry(crowns[i, ]) %>% sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(crowns))
  crowns[i, "point_in_crown"] <- as.vector(sf::st_intersects(x = this_point, y = crowns[i, ], sparse = FALSE))
}

crowns <-
  crowns %>% 
  dplyr::filter(point_in_crown) %>% 
  dplyr::select(-point_in_crown)

if(!file.exists(cropped_crowns_fname)) {
  sf::st_write(obj = crowns, dsn = cropped_crowns_fname, delete_dsn = TRUE)
}


 
