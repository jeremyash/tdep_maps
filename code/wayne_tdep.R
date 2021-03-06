## SPATIAL
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(maptools)

## DATA MANAGEMENT
library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
# library(zoo)

## PLOTTING
library(scales)
library(units)
library(viridis)
library(extrafont)
library(gtable)
library(grid)
#----------------------------------------------------------------------------

# 
# library(Hmisc)
# library(tidyverse)
# library(raster)
# library(rasterVis)
# library(rgdal)
# library(grid)
# library(scales)
# library(viridis) 
# library(maptools)
# #----------------------------------------------------------------------------

########################################
## FUNCTIONS
########################################

theme_map <- function(base_size = 9, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank(),
          legend.justification = c(0, 0),
          legend.position = c(0, 0))
}

#----------------------------------------------------------------------------

########################################
## LOAD AND TRANSFORM DATA
########################################


# usa_bound <- map_data("state") 

e00_to_df <- function(e00) {
  test <- raster(e00)
  test_spdf <- as(test, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  colnames(test_df) <- c("value", "x", "y")
  return(test_df)
}

# # total n 
# n_01 <- e00_to_df("raw_data/tdep_n/n_tw-0002/n_tw-0002.e00")
# n_15 <- e00_to_df("raw_data/tdep_n/n_tw-1416/n_tw-1416.e00")
# 
# # total s
# s_01 <- e00_to_df("raw_data/tdep_s/s_tw-0002/s_tw-0002.e00")
# s_15 <- e00_to_df("raw_data/tdep_s/s_tw-1416/s_tw-1416.e00")


read_crs <- function(FILEPATH) {
  dat <- raster(FILEPATH)
  crs(dat) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  return(dat)
}

# total n 
n_01 <- read_crs("raw_data/tdep_n/n_tw-0002/n_tw-0002.e00")
# crs(n_01) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" #correct CRS!
n_15 <- read_crs("raw_data/tdep_n/n_tw-1416/n_tw-1416.e00")

# total s
s_01 <- read_crs("raw_data/tdep_s/s_tw-0002/s_tw-0002.e00")
s_15 <- read_crs("raw_data/tdep_s/s_tw-1416/s_tw-1416.e00")

crs_new <- proj4string(n_01)
# wayne
wayne <- readOGR("gis/wayne_nf_simple")
wayne <- spTransform(wayne, crs_new)
wayne_sf <- sf::st_as_sf(wayne)


#############################################################################
## overlay wayne and extract tdep
#############################################################################


crop_and_summ <- function(SHAPEFILE, RASTER) {
  cr <- crop(RASTER, extent(SHAPEFILE), snap="out")                    
  fr <- rasterize(SHAPEFILE, cr)   
  lr <- mask(x=cr, mask=fr)  
  print(summary(lr))
}


crop_and_summ(wayne, n_01)
crop_and_summ(wayne, n_15)



crop_and_summ(wayne, s_01)
crop_and_summ(wayne, s_15)













########################################
## PLOTTING -- GENERIC W/O BINS  
########################################

# variables and units found here: ftp://ftp.epa.gov/castnet/tdep/Total_Deposition_Documentation_current.pdf



nadp_plot <- function(nadp_df, nadp_title, nadp_units) {
  ggplot() +  
    geom_tile(data=nadp_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
    # geom_polygon(data=usa_bound, aes(x=long, y=lat, group=group), 
    #              fill=NA, color="grey50", size=0.25) +
    # scale_fill_viridis() +
    scale_fill_distiller(palette = "Spectral", direction = -1) + 
    labs(title = nadp_title, fill = nadp_units) +
    coord_equal() +
    theme_map() +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm")) +
    theme(plot.title = element_text(size = 18, hjust = 0.5)) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.title = element_text(size = 12)) 
  
  
}

# nadp_plot(n_total_2015, "Total (wet + dry) deposition of nitrogen \n2015", "kg-N/ha")
# 
# 
# nadp_plot(s_total_2015, "Total (wet + dry) deposition of sulfur \n2015", "kg-S/ha")



nadp_plot(s_total_1315, "Total (wet + dry) deposition of sulfur \n2013 - 2015", "kg-S/ha")


nadp_plot(n_total_1315, "Total (wet + dry) deposition of nitrogen \n2013 - 2015", "kg-N/ha")
#----------------------------------------------------------------------------

########################################
## PLOTTING WITH BINNED VALUES
########################################

#so2 

hist(s_total_1315$value)

# binning
s_total_1315$value <- cut2(s_total_1315$value, cuts = c(0,2,4,6,8,10,20))

levels(s_total_1315$value)[1] <- "<2  "
levels(s_total_1315$value)[2] <- "2-4  "
levels(s_total_1315$value)[3] <- "4-6  "
levels(s_total_1315$value)[4] <- "6-8  "
levels(s_total_1315$value)[5] <- "8-10  "
levels(s_total_1315$value)[6] <- ">10  "


#plotting
ggplot() +  
  geom_tile(data=s_total_1315, aes(x=x, y=y, fill=value)) + 
  # geom_polygon(data=usa_bound, aes(x=long, y=lat, group=group), 
  #              fill=NA, color="grey50", size=0.25) +
  # scale_fill_viridis() +
  scale_fill_brewer(palette = "Spectral", direction = -1) + 
  labs(title = "Total (wet + dry) deposition of sulfur \n2013 - 2015", fill = "kg-S/ha") +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12)) 
#---

#nox

hist(n_total_1315$value)

# binning
n_total_1315$value <- cut2(n_total_1315$value, cuts = c(0,4,8,12,16,20,72))

levels(n_total_1315$value)[1] <- "<4  "
levels(n_total_1315$value)[2] <- "4-8  "
levels(n_total_1315$value)[3] <- "8-12  "
levels(n_total_1315$value)[4] <- "12-16  "
levels(n_total_1315$value)[5] <- "16-20  "
levels(n_total_1315$value)[6] <- ">20  "


#plotting
ggplot() +  
  geom_tile(data=n_total_1315, aes(x=x, y=y, fill=value)) + 
  # geom_polygon(data=usa_bound, aes(x=long, y=lat, group=group), 
  #              fill=NA, color="grey50", size=0.25) +
  # scale_fill_viridis() +
  scale_fill_brewer(palette = "Spectral", direction = -1) + 
  labs(title = "Total (wet + dry) deposition of nitrogen \n2013 - 2015", fill = "kg-N/ha") +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  theme(plot.title = element_text(size = 18, hjust = 0.5)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12)) 
#---

