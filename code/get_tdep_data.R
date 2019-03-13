library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
# library(sp)
# library(rgeos)
# library(raster)
# library(rgdal)
# library(scales)
# library(units)
# library(viridis)
library(RCurl)
library(curl)
#----------------------------------------------------------------------------

########################################
## FUNCTIONS
########################################

tdep_to_disk <- function(ELEMENT, YEAR) {
  # create url for zip files of gridded data
  url_path <- paste("http://nadp.slh.wisc.edu/maplib/grids/", 
                    YEAR, 
                    "/", 
                    ELEMENT, 
                    "_dep_",
                    YEAR, 
                    ".zip",
                    sep = "")
  
  # create name of destfile
  destfile_name <- paste("raw_data/",
                         ELEMENT,
                         "/",
                         ELEMENT,
                         "_dep_",
                         YEAR,
                         ".zip",
                         sep = "")
  
  # download statement
  curl_download(url_path, destfile = destfile_name)
  
}



tdep_to_disk_total_n <- function(ELEMENT, YEAR) {
  # create url for zip files of gridded data
  url_path <- ifelse(YEAR < 2011, 
                 paste("http://nadp.slh.wisc.edu/maplib/grids/", 
                    YEAR, 
                    "/TotalN_dep_",
                    YEAR, 
                    ".zip",
                    sep = ""),
                 paste("http://nadp.slh.wisc.edu/maplib/grids/", 
                       YEAR, 
                       "/", 
                       ELEMENT, 
                       "_dep_",
                       YEAR, 
                       ".zip",
                       sep = ""))
                 
  
  # create name of destfile
  destfile_name <- paste("raw_data/",
                         ELEMENT,
                         "/",
                         ELEMENT,
                         "_dep_",
                         YEAR,
                         ".zip",
                         sep = "")
  
  # download statement
  curl_download(url_path, destfile = destfile_name)
  
}



tdep_to_disk_hg <- function(ELEMENT, YEAR) {
  # create url for zip files of gridded data
  url_path <- paste("http://nadp.slh.wisc.edu/maplib/grids/MDN/Hg_Dep_", 
                           YEAR, 
                           ".zip",
                           sep = "")
  
  
  # create name of destfile
  destfile_name <- paste("raw_data/Hg/",
                         ELEMENT,
                         "_dep_",
                         YEAR,
                         ".zip",
                         sep = "")
  
  # download statement
  curl_download(url_path, destfile = destfile_name)
  
}



#---------------------------------------------------------------------------

########################################
## DOWNLOAD DATA
########################################

years <- seq(1985, 2016, 1)
hg_years <- seq(2003,2015,1)

# SO4
# lapply(years, function(x) {tdep_to_disk("SO4", x)})
# 
# 
# # NO3
# lapply(years, function(x) {tdep_to_disk("NO3", x)})
# 
# 
# # NH4
# lapply(years, function(x) {tdep_to_disk("NH4", x)})
# 
# 
# # SplusN
# lapply(years, function(x) {tdep_to_disk("SplusN", x)})
# 
# 
# # N
# lapply(years, function(x) {tdep_to_disk_total_n("N", x)})
#  
# # Hg
# lapply(hg_years, function(x) {tdep_to_disk_hg("Hg", x)})



# 2016
# SO4
tdep_to_disk("SO4", 2016)

# NO3
tdep_to_disk("NO3", 2016)


# NH4
tdep_to_disk("NH4", 2016)


# SplusN
tdep_to_disk("SplusN", 2016)


# N
tdep_to_disk_total_n("N", 2016)





# 2017
# SO4
tdep_to_disk("SO4", 2017)

# NO3
tdep_to_disk("NO3", 2017)


# NH4
tdep_to_disk("NH4", 2017)


# SplusN
tdep_to_disk("SplusN", 2017)


# N
tdep_to_disk_total_n("N", 2017)


