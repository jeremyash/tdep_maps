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
library(ggspatial)
library(gridExtra)
library(ggpubr)
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
n_16 <- read_crs("raw_data/tdep_n/n_tw-1517/n_tw-1517.e00")

# total s
s_01 <- read_crs("raw_data/tdep_s/s_tw-0002/s_tw-0002.e00")
s_16 <- read_crs("raw_data/tdep_s/s_tw-1517/s_tw-1517.e00")

crs_new <- proj4string(n_01)


# mon
mon <- readOGR("gis/mon_nf")
mon_buffer <- gBuffer(mon, width = 200000)

mon <- spTransform(mon, crs_new)
mon_sf <- sf::st_as_sf(mon)

mon_buffer <- spTransform(mon_buffer, crs_new)
mon_buffer_sf <- sf::st_as_sf(mon_buffer)

# gauley integrated spruce restoration
gisr <- readOGR("gis/GISR_ProjectBoundary_062419")
gisr <- spTransform(gisr, crs_new)

#############################################################################
## overlay mon and extract tdep
#############################################################################


crop_and_summ <- function(SHAPEFILE, RASTER) {
  cr <- crop(RASTER, extent(SHAPEFILE), snap="out")                    
  fr <- rasterize(SHAPEFILE, cr)   
  lr <- mask(x=cr, mask=fr)  
  print(summary(lr))
}


crop_and_summ(mon_buffer, n_01)
crop_and_summ(mon_buffer, n_16)



crop_and_summ(mon_buffer, s_01)
crop_and_summ(mon_buffer, s_16)


#############################################################################
## crop and plot with grids
#############################################################################

crop_raster <- function(SHAPEFILE, RASTER) {
  # cr <- crop(RASTER, extent(SHAPEFILE), snap="out")                    
  # fr <- rasterize(SHAPEFILE, cr)   
  # lr <- mask(x=cr, mask=fr)  
  # val <- getValues(lr)
  spdf <- as(RASTER, "SpatialPixelsDataFrame")
  df <- as.data.frame(spdf)
  colnames(df) <- c("value", "x", "y")
  df <- df %>% 
    filter(between(x, 1270000, 1490000)) %>% 
    filter(between(y, 1720000, 1980000))
  # lr_df <- broom::tidy(lr)
}


# total n in buffer
mon_n_01 <- crop_raster(mon_buffer, n_01)
colnames(mon_n_01)[1] <- "n_01"
mon_n_16 <- crop_raster(mon_buffer, n_16)
colnames(mon_n_16)[1] <- "n_16"
mon_n <- left_join(mon_n_01, mon_n_16) 



# total s in buffer
mon_s_01 <- crop_raster(mon_buffer, s_01)
colnames(mon_s_01)[1] <- "s_01"
mon_s_16 <- crop_raster(mon_buffer, s_16)
colnames(mon_s_16)[1] <- "s_16"
mon_s <- left_join(mon_s_01, mon_s_16) 





# crop rasters to GISR shapefile centroid

gCentroid(gisr)
gisr_n <- crop(n_16, y = extent(c(1310000, 1390000, 1760000, 1840000)))
plot(gisr_n)
plot(gisr, add = TRUE)


writeRaster(gisr_n,
            filename = "gis/gisr_n_dep_15_17.tif")


gisr_s <- crop(s_16, y = extent(c(1310000, 1390000, 1760000, 1840000)))
plot(gisr_s)
plot(gisr, add = TRUE)


writeRaster(gisr_s,
            filename = "gis/gisr_s_dep_15_17.tif")

##-------------
## nitrogen plotting
##-------------

# 2001
summary(mon_n)

n_dep_plot_01 <- ggplot() +
  geom_tile(aes(x,y, fill = n_01), color = NA, data = mon_n) +
  theme_minimal() +
  scale_fill_viridis(direction = 1, name = "Total N \n(kg-N/ha)", limits = c(5,18)) + 
  geom_sf(fill = NA, color = "white", data = mon_sf) +
  labs(title = "2000-2002") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        # plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = -5, size = 14),
        plot.margin = margin(0,2,0,0),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.key.height = unit(0.5, "in")) 

# 2016
n_dep_plot_16 <- ggplot() +
  geom_tile(aes(x,y, fill = n_16), color = NA, data = mon_n) +
  theme_minimal() +
  scale_fill_viridis(direction = 1, name = "Total N \n(kg-N/ha)", limits = c(5,14)) + 
  geom_sf(fill = NA, color = "white", data = mon_sf) +
  labs(title = "2015-2017") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        # plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = -5, size = 14),
        plot.margin = margin(0,2,0,0),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.key.height = unit(0.5, "in")) 

n_figs <- grid.arrange(n_dep_plot_01, n_dep_plot_16, nrow = 1)
n_figs <- annotate_figure(n_figs,
                fig.lab = "Nitrogen Deposition across Monongahela NF",
                fig.lab.size = 20,
                fig.lab.pos = "top.left")

pdf("figures/n_dep_mon_nf.pdf", height = 6, width = 11)
n_figs
dev.off()

##-------------
## sulfur plotting
##-------------

# 2001
summary(mon_s)

s_dep_plot_01 <- ggplot() +
  geom_tile(aes(x,y, fill = s_01), color = NA, data = mon_s) +
  theme_minimal() +
  scale_fill_viridis(direction = 1, name = "Total S \n(kg-S/ha)", limits = c(8,45)) + 
  geom_sf(fill = NA, color = "white", data = mon_sf) +
  labs(title = "2000-2002") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        # plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = -5, size = 14),
        plot.margin = margin(0,2,0,0),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.key.height = unit(0.5, "in")) 

# 2016
s_dep_plot_16 <- ggplot() +
  geom_tile(aes(x,y, fill = s_16), color = NA, data = mon_s) +
  theme_minimal() +
  scale_fill_viridis(direction = 1, name = "Total S \n(kg-S/ha)", limits = c(1,8)) + 
  geom_sf(fill = NA, color = "white", data = mon_sf) +
  labs(title = "2015-2017") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        # plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = -5, size = 14),
        plot.margin = margin(0,2,0,0),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.key.height = unit(0.5, "in")) 

s_figs <- grid.arrange(s_dep_plot_01, s_dep_plot_16, nrow = 1)
s_figs <- annotate_figure(s_figs,
                          fig.lab = "Sulfur Deposition across Monongahela NF",
                          fig.lab.size = 20,
                          fig.lab.pos = "top.left")

pdf("figures/s_dep_mon_nf.pdf", height = 6, width = 11)
s_figs
dev.off()


#############################################################################
## crop and plot with facets
#############################################################################

crop_raster <- function(SHAPEFILE, RASTER) {
  # cr <- crop(RASTER, extent(SHAPEFILE), snap="out")                    
  # fr <- rasterize(SHAPEFILE, cr)   
  # lr <- mask(x=cr, mask=fr)  
  # val <- getValues(lr)
  spdf <- as(RASTER, "SpatialPixelsDataFrame")
  df <- as.data.frame(spdf)
  colnames(df) <- c("value", "x", "y")
  df <- df %>% 
    filter(between(x, 1270000, 1490000)) %>% 
    filter(between(y, 1720000, 1980000))
  # lr_df <- broom::tidy(lr)
 }


# total n in buffer
mon_n_01 <- crop_raster(mon_buffer, n_01)
colnames(mon_n_01)[1] <- "n_01"
mon_n_16 <- crop_raster(mon_buffer, n_16)
colnames(mon_n_16)[1] <- "n_16"
mon_n <- left_join(mon_n_01, mon_n_16) %>% 
  gather(years, n_dep, c("n_01", "n_16"))

n_years_labs <- c("2000-2002", "2015-2017")
names(n_years_labs) <- c("n_01", "n_16") 

# total s in buffer
mon_s_01 <- crop_raster(mon_buffer, s_01)
colnames(mon_s_01)[1] <- "s_01"
mon_s_16 <- crop_raster(mon_buffer, s_16)
colnames(mon_s_16)[1] <- "s_16"
mon_s <- left_join(mon_s_01, mon_s_16) %>% 
  gather(years, s_dep, c("s_01", "s_16"))


s_years_labs <- c("2000-2002", "2015-2017")
names(s_years_labs) <- c("s_01", "s_16") 


# plotting
# nitrogen
n_dep_plot <- ggplot() +
  geom_tile(aes(x,y, fill = n_dep), color = NA, data = mon_n) +
  theme_minimal() +
  scale_fill_viridis(direction = 1, name = "Total N (kg-N/ha)", limits = c(5,18)) + 
  geom_sf(fill = NA, color = "white", data = mon_sf) +
  facet_grid(cols = vars(years), labeller = labeller(years = n_years_labs), scales = "free") +
  labs(title = "Total Nitrogen Deposition across Monongahela NF") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        # plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        title = element_text(size = 16),
        strip.text = element_text(size = 14, margin = margin(6, 0, -1, 0)),
        strip.background = element_blank(),
        # strip.placement = "outside",
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.5, "in")) 

ggsave("figures/n_dep_mon_nf.pdf", n_dep_plot, height = 8.5, width = 11)



# sulfur
s_dep_plot <- ggplot() +
  geom_tile(aes(x,y, fill = s_dep), color = NA, data = mon_s) +
  theme_minimal() +
  scale_fill_viridis(direction = 1, name = "Total S (kg-N/ha)", limits = c(1,45)) + 
  geom_sf(fill = NA, color = "white", data = mon_sf) +
  facet_grid(cols = vars(years), labeller = labeller(years = s_years_labs)) +
  labs(title = "Total Sulfur Deposition across Monongahela NF") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0, "lines"),
        # plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        title = element_text(size = 16),
        strip.text = element_text(size = 14, margin = margin(6, 0, -1, 0)),
        strip.background = element_blank(),
        # strip.placement = "outside",
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.5, "in")) 

ggsave("figures/s_dep_mon_nf.pdf", s_dep_plot, height = 8.5, width = 11)


#############################################################################
## archive
#############################################################################



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



nadp_plot(s_total_1517, "Total (wet + dry) deposition of sulfur \n2015 - 2017", "kg-S/ha")


nadp_plot(n_total_1517, "Total (wet + dry) deposition of nitrogen \n2015 - 2017", "kg-N/ha")
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

