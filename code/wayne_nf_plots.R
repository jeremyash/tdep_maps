library(plyr)
library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(scales)
library(units)
library(rasterVis)
library(viridis)
library(maps)
library(sf)
library(extrafont)
library(grid)

#----------------------------------------------------------------------------

########################################
## FUNCTIONS
########################################


raster_to_df <- function(RASTER) {
  test_spdf <- as(RASTER, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  colnames(test_df) <- c("value", "x", "y")
  # test_df$year <- rep(as.numeric(str_extract_all(names(so4[[1]]), "[0-9]+")[[1]][2]), dim(test_df)[1])
  test_df <- test_df %>% 
    filter(between(x, 1270000, 1490000)) %>% 
    filter(between(y, 1720000, 1980000))
  return(test_df)
}




theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text( color = "black"),
      plot.title = element_text(face = "bold"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.4),
      # panel.grid.major = element_line(color = "#ebebe5", size = 0.8),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      legend.position = "bottom",
      ...
    )
}


# For Windows - in each session
# Adjust the path to match your installation of Ghostscript
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.23/bin/gswin64.exe")

# load font for plotting
windowsFonts(Times=windowsFont("Ubuntu"))





mean_raster_func <- function(DAT) {
  input_raster <- DAT[[1]]
  
  dat_resample <- lapply(DAT, function(x) resample(x, 
                                                   input_raster, 
                                                   method = "bilinear"))
  
  dat_past <- mean(stack(dat_resample[c(19,20,21)]))
  
  dat_recent <- mean(stack(dat_resample[c(30, 31, 32)]))
  
  past_df <- raster_to_df(dat_past) %>% 
    mutate(period = rep("1985-1989", n()))
  
  recent_df <- raster_to_df(dat_recent) %>% 
    mutate(period = rep("2011-2015", n()))
  
  all_df <- bind_rows(past_df, recent_df)
  
}

#----------------------------------------------------------------------------

########################################
## LOAD DATA
########################################

# env data
so4 <- readRDS("data/so4_rasters.RDS")
no3 <- readRDS("data/no3_rasters.RDS")
nh4 <- readRDS("data/nh4_rasters.RDS")
total_n <- readRDS("data/n_rasters.RDS")
sn <- readRDS("data/splusn_rasters.RDS")
hg <- readRDS("data/hg_rasters.RDS")

# wayne data
crs_new <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")


wayne <- readOGR("gis/wayne_airshed")
wayne <- spTransform(wayne, crs_new)
wayne_tidy <- broom::tidy(wayne)

wayne_nf <- readOGR("gis/wayne_nf_simple")
wayne_nf <- spTransform(wayne_nf, crs_new)
wayne_nf_tidy <- broom::tidy(wayne_nf)


wayne_agg   <- aggregate(wayne, dissolve = TRUE)
wayne_tidy <- broom::tidy(wayne_agg)

wayne_states <- readRDS("raw_data/state_sub_spdf.RDS")
wayne_states <- spTransform(wayne_states, crs_new)
wayne_states_df <- broom::tidy(wayne_states)



# change in planning mean

delta_planning_mean_calc <- function(PAST, RECENT, DAT) {
  
  
  # find minimum extent
  ex_crop_df <- plyr::ldply(DAT, function(x) {
    data_frame(xmin = extent(x)[1],
                            xmax = extent(x)[2],
                            ymin = extent(x)[3],
                            ymax = extent(x)[4])
  })
  
  
  ex_vals <- ex_crop_df %>% 
    summarise(xmin = max(xmin),
              xmax = min(xmax),
              ymin = max(ymin),
              ymax = min(ymax))
  
  ex_crop <- c(ex_vals$xmin[1],
               ex_vals$xmax[1],
               ex_vals$ymin[1],
               ex_vals$ymax[1])
  
  # RASTER LIST and crop
  raster_list <- list(DAT[[PAST[1]]],
                      DAT[[PAST[2]]],
                      DAT[[PAST[3]]],
                      DAT[[RECENT[1]]],
                      DAT[[RECENT[2]]],
                      DAT[[RECENT[3]]])

  
  raster_list <- lapply(raster_list, function(x) {
    crop(x, ex_crop, snap = "near")
  })
    

  #create stacks
  past_dat <- stack(raster_list[[1]],
                    raster_list[[2]],
                    raster_list[[3]])

  recent_dat <- stack(raster_list[[4]],
                    raster_list[[5]],
                    raster_list[[6]])
  
  

  #calculate means and delta_means
  past_mean <- mean(past_dat, na.rm = TRUE)
  recent_mean <- mean(recent_dat, na.rm=TRUE)

  delta_mean <- recent_mean - past_mean
  dat_delta <- projectRaster(delta_mean, crs = crs_new)
  
  
  
  cr <- crop(dat_delta, extent(wayne_states), snap="out")                    
  fr <- rasterize(wayne_states, cr)   
  lr <- mask(x=cr, mask=fr)
  return(lr)
# 
#   # crop to wayne area
#   # delta_mean_crop <- crop(delta_mean, extent(650000, 1750000, 1300000, 2400000))
#   delta_mean_int <- intersect(wayne, dat_delta)
  
# 

#   return(dat_delta)
}

delta_sn <- delta_planning_mean_calc(c(20,21,22), c(31,32,33), sn)
delta_so4 <- delta_planning_mean_calc(c(20,21,22), c(31,32,33),so4)
delta_n <- delta_planning_mean_calc(c(20,21,22), c(31,32,33),total_n)
# delta_hg <- delta_planning_mean_calc(c(2,3,4), c(11,12,13),hg)




# individual time periods
planning_mean_calc <- function(INDICES, DAT, CROP_DAT) {
  
  
  # find minimum extent
  ex_crop_df <- plyr::ldply(DAT, function(x) {
    data_frame(xmin = extent(x)[1],
               xmax = extent(x)[2],
               ymin = extent(x)[3],
               ymax = extent(x)[4])
  })
  
  
  ex_vals <- ex_crop_df %>% 
    summarise(xmin = max(xmin),
              xmax = min(xmax),
              ymin = max(ymin),
              ymax = min(ymax))
  
  ex_crop <- c(ex_vals$xmin[1],
               ex_vals$xmax[1],
               ex_vals$ymin[1],
               ex_vals$ymax[1])
  
  # RASTER LIST and crop
  raster_list <- lapply(INDICES, function(x) {
    DAT[[x]]
  })
    
  
  raster_list <- lapply(raster_list, function(x) {
    crop(x, ex_crop, snap = "near")
  })
  
  
  #create stacks
  dat_stack <- stack(raster_list)
  
  
  #calculate means and project
  crs_new <- proj4string(CROP_DAT)
  dat_mean <- mean(dat_stack, na.rm = TRUE)
  
  dat_mean <- projectRaster(dat_mean, crs = crs_new)
  
  
  # crop to shapefile
  cr <- crop(dat_mean, extent(CROP_DAT), snap="out")                    
  fr <- rasterize(CROP_DAT, cr)   
  lr <- mask(x=cr, mask=fr)
  return(lr)
}

# so4
so4_past <- planning_mean_calc(c(21,22,23), so4, wayne_states)
so4_recent <- planning_mean_calc(c(31,32,33), so4, wayne_states)

#############################################################################
## plotting
#############################################################################


##-------------
## so4
##-------------


gplot(delta_so4 ) +
  geom_tile(aes(fill = value)) +
  geom_polygon(aes(long, lat, group = group),
               fill = NA,
               size = 0.6,
               color = "grey15",
               data = wayne_states_df) +
  geom_polygon(aes(long, lat, group = group),
               fill = NA,
               size = 0.8,
               color = "black",
               data = wayne_nf_tidy) +
  geom_polygon(aes(long, lat, group = group),
               fill = NA,
               size = 1,
               color = "cadetblue1",
               data = wayne_tidy) +
  theme_map() +
  theme(
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 14, hjust = 0, color = "black", face = "bold"),
    plot.title = element_text(size = 20, vjust = -3, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 20, color = "black", 
                                 margin = margin(b = 0, 
                                                 t = 0, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 16),
    plot.margin = unit(c(0.2,.2,1,.2), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")) +
  labs(title = "Change in Sulfur Deposition") +
  scale_fill_viridis(option = "viridis", 
                     na.value="transparent",
                     name = expression(paste(SO[4]^{-2}, " kg/ha")),
                     direction = -1,
                     # limits = c(-15, 20),
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(5, units = "mm"),
                       barwidth = unit(100, units = "mm"),
                       draw.ulim = F,
                       title.position = 'left',
                       # some shifting around
                       title.hjust = 0.5,
                       title.vjust = 1,
                       label.hjust = 0.5,
                       label.position = "bottom"
                        )) +
  # scale_x_continuous(limits = c(-86, -77)) +
  # scale_y_continuous(limits = c(36,43)) +
  coord_quickmap()

ggsave(filename = "figures/delta_so4_wayne.jpg",
       height = 4,
       width = 7,
       units = "in")


##-------------
## n
##-------------


gplot(delta_n ) +
  geom_tile(aes(fill = value)) +
  geom_polygon(aes(long, lat, group = group),
               fill = NA,
               size = 0.6,
               color = "grey15",
               data = wayne_states_df) +
  geom_polygon(aes(long, lat, group = group),
               fill = NA,
               size = 0.8,
               color = "black",
               data = wayne_nf_tidy) +
  geom_polygon(aes(long, lat, group = group),
               fill = NA,
               size = 1,
               color = "cadetblue1",
               data = wayne_tidy) +
  theme_map() +
  theme(
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 14, hjust = 0, color = "black", face = "bold"),
    plot.title = element_text(size = 20, vjust = -3, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 20, color = "black", 
                                 margin = margin(b = 0, 
                                                 t = 0, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 16),
    plot.margin = unit(c(0.2,.2,1,.2), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")) +
  labs(title = "Change in Nitrogen Deposition") +
  scale_fill_viridis(option = "viridis", 
                     na.value="transparent",
                     name = expression(paste(NO[3], " & ", NH[4], " kg/ha")),
                     direction = -1,
                     # limits = c(-15, 20),
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(5, units = "mm"),
                       barwidth = unit(100, units = "mm"),
                       draw.ulim = F,
                       title.position = 'left',
                       # some shifting around
                       title.hjust = 0.5,
                       title.vjust = 1,
                       label.hjust = 0.5,
                       label.position = "bottom"
                     )) +
  # scale_x_continuous(limits = c(-86, -77)) +
  # scale_y_continuous(limits = c(36,43)) +
  coord_quickmap()

ggsave(filename = "figures/delta_n_wayne.jpg",
       height = 4,
       width = 7,
       units = "in")


#############################################################################
## crop and write to raster
#############################################################################

crop_and_write <- function(DAT, ELEMENT) {
  dat_crop <- intersect(DAT, wayne)
  file_path <- paste("data/wayne/", ELEMENT, "/" , names(dat_crop), ".tif", sep = "")
  writeRaster(dat_crop, filename = file_path, format="GTiff", overwrite = TRUE)
}


lapply(total_n, function(x) {crop_and_write(x, "total_n")})
lapply(so4, function(x) {crop_and_write(x, "so4")})
lapply(hg, function(x) {crop_and_write(x, "hg")})


#############################################################################
## archive
#############################################################################


sn_03_05_wayne <- crop(sn_03_05, extent(650000, 1750000, 1300000, 2400000))
sn_13_15_wayne <- crop(sn_13_15, extent(650000, 1750000, 1300000, 2400000))

#2003-2005
# #crop value to wisc
# sn_03_05_wayne <- raster::intersect(sn_03_05, wayne)


gplot(sn_03_05_wayne) +
  geom_tile(aes(fill = value)) +
  geom_polygon(aes(long, lat, group = group), 
               fill = NA, 
               size = 1, 
               color = "grey15", 
               data = wayne_tidy) +
  theme_map() +
  theme(
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 14, hjust = 0, color = "#4e4d47", face = "bold"),
    plot.title = element_text(size = 40, hjust = 0.5, vjust = -15, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 30, hjust = 0.5, color = "#4e4d47", 
                                 margin = margin(b = 0, 
                                                 t = 0, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 16),
    plot.margin = unit(c(0.2,.2,1,.2), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")) +
  labs(subtitle = "Mean Sulfur + Nitrogen Wet Deposition, 2003−2005") +
  scale_fill_viridis(option = "viridis", 
                     na.value="transparent",
                     name = "S + N (eq/ha)",
                     direction = -1,
                     limits = c(190, 1400),
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(5, units = "mm"),
                       barwidth = unit(150, units = "mm"),
                       draw.ulim = F,
                       title.position = 'left',
                       # some shifting around
                       title.hjust = 0.5,
                       title.vjust = 1,
                       label.hjust = 0.5,
                       label.position = "bottom"
                     )) +
  coord_equal()


ggsave("figures/wayne_sn_03_05.pdf", height = 8.5, width = 11, units = "in")



#2013-2015

#crop value to wisc


gplot(sn_13_15_wayne) +
  geom_tile(aes(fill = value)) +
  geom_polygon(aes(long, lat, group = group), 
               fill = NA, 
               size = 1, 
               color = "grey15", 
               data = wayne_tidy) +
  theme_map() +
  theme(
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 14, hjust = 0, color = "#4e4d47", face = "bold"),
    plot.title = element_text(size = 40, hjust = 0.5, vjust = -15, color = "black", face = "bold"),
    plot.subtitle = element_text(size = 30, hjust = 0.5, color = "#4e4d47", 
                                 margin = margin(b = 0, 
                                                 t = 0, 
                                                 l = 2, 
                                                 unit = "cm"), 
                                 debug = F),
    legend.title = element_text(size = 16),
    plot.margin = unit(c(0.2,.2,1,.2), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 6, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#939184")) +
  labs(subtitle = "Mean Sulfur + Nitrogen Wet Deposition, 2013−2015") +
  scale_fill_viridis(option = "viridis", 
                     na.value="transparent",
                     name = "S + N (eq/ha)",
                     direction = -1,
                     limits = c(190, 1400),
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(5, units = "mm"),
                       barwidth = unit(150, units = "mm"),
                       draw.ulim = F,
                       title.position = 'left',
                       # some shifting around
                       title.hjust = 0.5,
                       title.vjust = 1,
                       label.hjust = 0.5,
                       label.position = "bottom"
                     )) +
  coord_equal()


ggsave("figures/wayne_sn_13_15.pdf", height = 8.5, width = 11, units = "in")







#############################################################################
## testing
#############################################################################

##-------------
## inset map
##-------------
# 
# inset <- ggplot() +
#   geom_map(data = norwaymap, aes(x = long, y = lat, map_id = region),
#            map = norwaymap, colour = NA, fill = "grey60") +
#   geom_rect(data = data.frame(),
#             aes(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
#             colour = "red", fill = NA) +
#   coord_map(xlim = c(3, 33), ylim = c(57, 72)) +
#   labs(x = NULL, y = NULL)
# 
xlim <- range(wayne_tidy$long) + c(-0.25, 0.25)
ylim <- range(wayne_tidy$lat) + c(-0.25, 0.25)

# airshed map
inset_map <- ggplot(wayne_states_df) +
  geom_polygon(aes(long, lat, group = group), fill = NA, color = "grey15", size = 1) +
  # geom_polygon(aes(long, lat, group = group),
  #             color = "cadetblue3",
  #             fill = "cadetblue3",
  #             # fill = NA,
  #             alpha = 1.0,
  #             size = 2,
  #             show.legend = FALSE,
  #             data=wayne_tidy) +
  # geom_polygon(aes(long, lat, group = group,  fill = "darkgoldenrod1"),
  #              color = NA,
  #              alpha = 0.8,
#              show.legend = TRUE,
#              data = planning_df) +
# geom_polygon(aes(long, lat, group = group, fill = "darkgreen"),
#              color = NA,
#              show.legend = TRUE,
#              data = wayne_df) +
geom_rect(data = data.frame(),
          aes(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
          colour = "red", size = 2.5, fill = NA) +
  theme_minimal() +  
  coord_map() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 2))






# join together
grid.newpage()
vp_b <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vp_a <- viewport(width = 0.4, height = 0.4, x = 0.6, y = 0.8)  # the inset in upper left
print(so4_plot, vp = vp_b)
print(inset_map,  vp = vp_a)

ggsave("figures/wayne_so4_05_16.jpg", height = 8.5, width = 11, units = "in")


#----------------------------------------------------------------------------

# find minimum extent
ex_crop_df <- plyr::ldply(so4, function(x) {
  data_frame(xmin = extent(x)[1],
             xmax = extent(x)[2],
             ymin = extent(x)[3],
             ymax = extent(x)[4])
})


ex_vals <- ex_crop_df %>% 
  summarise(xmin = max(xmin),
            xmax = min(xmax),
            ymin = max(ymin),
            ymax = min(ymax))

ex_crop <- c(ex_vals$xmin[1],
             ex_vals$xmax[1],
             ex_vals$ymin[1],
             ex_vals$ymax[1])

# RASTER LIST and crop
raster_list <- lapply(c(1,5,8), function(x) {
  so4[[x]]
})


raster_list <- lapply(raster_list, function(x) {
  crop(x, ex_crop, snap = "near")
})


#create stacks
dat_stack <- stack(raster_list)


#calculate means and project
crs_new <- proj4string(CROP_DAT)
dat_mean <- mean(dat_stack, na.rm = TRUE)

dat_mean <- projectRaster(delta_mean, crs = crs_new)


# crop to shapefile
cr <- crop(dat_mean, extent(CROP_DAT), snap="out")                    
fr <- rasterize(CROP_DAT, cr)   
lr <- mask(x=cr, mask=fr)
return(lr)





















