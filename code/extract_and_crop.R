library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
library(sp)
# library(rgeos)
library(raster)
library(rgdal)
# library(scales)
# library(units)
# library(viridis)

#---------------------------------------------------------------------------- 

########################################
## FUNCTIONS
########################################


extract_tif_file <- function(ELEMENT, YEAR) {
  
  # name of zip file
  zip_file <- paste("raw_data/",
                    ELEMENT,
                    "/",
                    ELEMENT,
                    "_dep_",
                    YEAR,
                    ".zip",
                    sep = "") 
  
  
  # unzip and extract tif file for each element x year
  
  tif_name <- ifelse(YEAR > 2011, 
                     paste(ELEMENT,
                           "_dep_",
                           YEAR,
                           "/",
                           ELEMENT,
                           "_dep_",
                           YEAR,
                           ".tif",
                           sep = ""),
                     paste(ELEMENT,
                           "_dep_",
                           YEAR,
                           "/dep_",
                           tolower(ELEMENT),
                           "_",
                           YEAR,
                           ".tif",
                           sep = ""))
  
  # extract directry path
  ex_path <- paste("raw_data/",
                    ELEMENT,
                    "_tif", 
                   sep = "")  
  
  # print(zip_file)
  # print(tif_name)
  # print(ex_path)
  
  unzip(zipfile = zip_file,
        files = tif_name,
        exdir = ex_path)
  
}  



extract_tif_file_n <- function(ELEMENT, YEAR) {
  
  # name of zip file
  zip_file <- paste("raw_data/",
                    ELEMENT,
                    "/",
                    ELEMENT,
                    "_dep_",
                    YEAR,
                    ".zip",
                    sep = "") 
  
  
  # unzip and extract tif file for each element x year
  
  tif_name <- ifelse(YEAR > 2011, 
                     paste(ELEMENT,
                           "_dep_",
                           YEAR,
                           "/",
                           ELEMENT,
                           "_dep_",
                           YEAR,
                           ".tif",
                           sep = ""),
                     paste("TotalN_dep_",
                           YEAR,
                           "/dep_totalN_",
                           YEAR,
                           ".tif",
                           sep = ""))
  
  # extract directry path
  ex_path <- paste("raw_data/",
                   ELEMENT,
                   "_tif", 
                   sep = "")  
  
  # print(zip_file)
  # print(tif_name)
  # print(ex_path)
  # 
  unzip(zipfile = zip_file,
        files = tif_name,
        exdir = ex_path)

}  




extract_tif_file_hg <- function(ELEMENT, YEAR) {
  
  # name of zip file
  zip_file <- paste("raw_data/",
                    ELEMENT,
                    "/",
                    ELEMENT,
                    "_dep_",
                    YEAR,
                    ".zip",
                    sep = "") 
  
  
  # unzip and extract tif file for each element x year
  
  
  tif_name <- paste(ELEMENT,
                           "_dep_",
                           YEAR,
                           "/",
                           ELEMENT,
                           "_dep_",
                           YEAR,
                           ".tif",
                           sep = "")
  # extract directry path
  ex_path <- paste("raw_data/",
                   ELEMENT,
                   "_tif", 
                   sep = "")  
  
  print(zip_file)
  print(tif_name)
  print(ex_path)
  # 
  # unzip(zipfile = zip_file,
  #       files = tif_name,
  #       exdir = ex_path)

}  






#----------------------------------------------------------------------------


########################################
## EXTRACT TIF FILES 
########################################

years <- seq(1985, 2015, 1)
hg_years <- seq(2003, 2015, 1)
# so4
lapply(years, function(x) {
  extract_tif_file("SO4", x)
})

# no3
lapply(years, function(x) {
  extract_tif_file("NO3", x)
})

# nh4
lapply(years, function(x) {
  extract_tif_file("NH4", x)
})

# s+n
lapply(years, function(x) {
  extract_tif_file("SplusN", x)
})

# n

lapply(years, function(x) {
  extract_tif_file_n("N", x)
})


# hg

lapply(hg_years, function(x) {
  extract_tif_file_hg("Hg", x)
})


# 2016
extract_tif_file("NO3", 2016)
extract_tif_file("NH4", 2016)
extract_tif_file_n("N", 2016)
extract_tif_file("SO4", 2016) 
extract_tif_file("SplusN", 2016) 



# 2017
extract_tif_file("NO3", 2017)
extract_tif_file("NH4", 2017)
extract_tif_file_n("N", 2017)
extract_tif_file("SO4", 2017) 
extract_tif_file("SplusN", 2017) 

########################################
## CREATE RASTERS
########################################

raster_list <- function(ELEMENT) {
  element_tif <- paste("raw_data/",
                       ELEMENT,
                       "_tif/",
                       list.files(paste("raw_data/",
                                        ELEMENT,
                                        "_tif/", 
                                        sep = ""),
                                  recursive = TRUE),
                       sep = "")
  
  element_rasters <- lapply(element_tif, function(x) raster(x))

  names(element_rasters) <- paste(ELEMENT,
                                  seq(1985,2017,1),
                                  sep = "_")

  return(element_rasters)

}



raster_list_hg <- function(ELEMENT) {
  element_tif <- paste("raw_data/",
                       ELEMENT,
                       "_tif/",
                       list.files(paste("raw_data/",
                                        ELEMENT,
                                        "_tif/", 
                                        sep = ""),
                                  recursive = TRUE),
                       sep = "")
  
  element_rasters <- lapply(element_tif, function(x) raster(x))
  
  names(element_rasters) <- paste(ELEMENT,
                                  seq(2003,2015,1),
                                  sep = "_")
  
  return(element_rasters)
  
}

# so4
# so4_tif <- paste("raw_data/SO4_tif/",
#                  list.files("raw_data/SO4_tif", recursive = TRUE),
#                  sep = "")
# 
# so4_rasters <- lapply(so4_tif, function(x) raster(x))
# names(so4_rasters) <- paste("so4", 
#                             seq(1985,2015,1),
#                             sep = "_")


so4_rasters <- raster_list("SO4")
saveRDS(so4_rasters, "data/so4_rasters.RDS")
so4_rasters <- readRDS("data/so4_rasters.RDS")

# no3
no3_rasters <- raster_list("NO3")
saveRDS(no3_rasters, "data/no3_rasters.RDS")
no3_rasters <- readRDS("data/no3_rasters.RDS")

# nh4
nh4_rasters <- raster_list("NH4")
saveRDS(nh4_rasters, "data/nh4_rasters.RDS")
nh4_rasters <- readRDS("data/nh4_rasters.RDS")


# nh4
splusn_rasters <- raster_list("SplusN")
saveRDS(splusn_rasters, "data/splusn_rasters.RDS")
splusn_rasters <- readRDS("data/splusn_rasters.RDS")


# nh4
n_rasters <- raster_list("N")
saveRDS(n_rasters, "data/n_rasters.RDS")
n_rasters <- readRDS("data/n_rasters.RDS")


# hg
hg_rasters <- raster_list_hg("Hg")
saveRDS(hg_rasters, "data/hg_rasters.RDS")
hg_rasters <- readRDS("data/hg_rasters.RDS")




#----------------------------------------------------------------------------

########################################
## CROP DATA -- not working because need to read in TFW files
########################################

mnf <- readOGR("gis/mon_nf")

# so4 cropping
so4_mnf <- lapply(so4_rasters, function(x) {
  temp_1 <- crop(x, extent(mnf))
  temp_2 <- mask(temp_1, mnf)
  return(temp_1)
})

saveRDS(so4_mnf, "data/so4_mnf.RDS")


# no3 cropping
no3_mnf <- lapply(no3_rasters, function(x) {
  temp_1 <- crop(x, extent(mnf))
  temp_2 <- mask(temp_1, mnf)
  return(temp_1)
})
saveRDS(no3_mnf, "data/no3_mnf.RDS")


# nh4 cropping
nh4_mnf <- lapply(nh4_rasters, function(x) {
  temp_1 <- crop(x, extent(mnf))
  temp_2 <- mask(temp_1, mnf)
  return(temp_1)
})
saveRDS(nh4_mnf, "data/nh4_mnf.RDS")


#----------------------------------------------------------------------------

wayne <- readOGR("gis/wayne_airshed")
crs_new <- proj4string(nh4_rasters[[1]])
wayne <- sp::spTransform(wayne, crs_new)


# so4 cropping
so4_wayne <- lapply(so4_rasters, function(x) {
  temp_1 <- crop(x, extent(wayne))
  temp_2 <- mask(temp_1, wayne)
  return(temp_1)
})

saveRDS(so4_wayne, "data/so4_wayne.RDS")


# no3 cropping
no3_wayne <- lapply(no3_rasters, function(x) {
  temp_1 <- crop(x, extent(wayne))
  temp_2 <- mask(temp_1, wayne)
  return(temp_1)
})
saveRDS(no3_wayne, "data/no3_wayne.RDS")


# nh4 cropping
nh4_wayne <- lapply(nh4_rasters, function(x) {
  temp_1 <- crop(x, extent(wayne))
  temp_2 <- mask(temp_1, wayne)
  return(temp_1)
})
saveRDS(nh4_wayne, "data/nh4_wayne.RDS")


#----------------------------------------------------------------------------
# 
# r9 <- readOGR("gis/region_9")
# crs_new <- proj4string(n_rasters[[1]])
# r9 <- sp::spTransform(r9, crs_new)
# 
# 
# 
# # splusn cropping
# sn_r9 <- lapply(splusn_rasters, function(x) {
#   temp_1 <- crop(x, extent(r9))
#   temp_2 <- mask(temp_1, r9)
#   return(temp_1)
# })
# saveRDS(sn_r9, "data/sn_r9.RDS")
# 
# 
# 
# # n cropping
# n_r9 <- lapply(n_rasters, function(x) {
#   temp_1 <- crop(x, extent(r9))
#   temp_2 <- mask(temp_1, r9)
#   return(temp_1)
# })
# saveRDS(n_r9, "data/n_r9.RDS")
# 
# 
# 
# # so4 cropping
# so4_r9 <- lapply(s04_rasters, function(x) {
#   temp_1 <- crop(x, extent(r9))
#   temp_2 <- mask(temp_1, r9)
#   return(temp_1)
# })
# saveRDS(so4_r9, "data/so4_r9.RDS")


