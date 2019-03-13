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
    filter(between(x, 256278, 459408)) %>% 
    filter(between(y, 2747709, 2834176))
  return(test_df)
}



theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu", color = "#22211d"),
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
  
  dat_past <- mean(stack(dat_resample[c(1,2,3,4,5)]))
  
  dat_recent <- mean(stack(dat_resample[c(27,28,29,30,31)]))
  
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

so4 <- readRDS("data/so4_rasters.RDS")
totaln <- readRDS("data/n_rasters.RDS")
# nh4 <- readRDS("data/nh4_rasters.RDS")
# # 
# # names(so4)
# 
# sn <- readRDS("data/splusn_rasters.RDS")


# total n
ex_crop <- extent(totaln[[23]])
totaln[[30]] <- crop(totaln[[30]], ex_crop, snap = "near")
totaln[[29]] <- crop(totaln[[29]], ex_crop, snap = "near")
totaln[[31]] <- crop(totaln[[31]], ex_crop, snap = "near")
# totaln[[23]] <- crop(totaln[[23]], ex_crop, snap = "near")
totaln[[24]] <- crop(totaln[[24]], ex_crop, snap = "near")
totaln[[25]] <- crop(totaln[[25]], ex_crop, snap = "near")


# difference in averages
n_13_15 <- (totaln[[29]] + totaln[[30]] + totaln[[31]])/3
n_07_09 <- (totaln[[23]] + totaln[[24]] + totaln[[25]])/3

delta_n <- n_13_15 - n_07_09



# so4
ex_crop <- extent(so4[[23]])
so4[[30]] <- crop(so4[[30]], ex_crop, snap = "near")
so4[[29]] <- crop(so4[[29]], ex_crop, snap = "near")
so4[[31]] <- crop(so4[[31]], ex_crop, snap = "near")
# so4[[23]] <- crop(so4[[23]], ex_crop, snap = "near")
so4[[24]] <- crop(so4[[24]], ex_crop, snap = "near")
so4[[25]] <- crop(so4[[25]], ex_crop, snap = "near")

so4_13_15 <- (so4[[29]] + so4[[30]] + so4[[31]])/3
so4_07_09 <- (so4[[23]] + so4[[24]] + so4[[25]])/3

delta_so4 <- so4_13_15 - so4_07_09





crs_new <- proj4string(delta_so4)


r9 <- readOGR("gis/region_9")
r9 <- spTransform(r9, crs_new)
r9_tidy <- broom::tidy(r9)



# proj4string(r9) <- crs_new
# proj4string(sn_13_15_r9) <- crs_new
#   
identicalCRS(r9, sn_13_15)
#----------------------------------------------------------------------------

########################################
## RASTER TO DFs
########################################


#so4
#crop value to wisc
so4_r9 <- raster::intersect(delta_so4, r9)
# var_crop[var_crop == 0] <- "NA"



gplot(so4_r9 ) +
  geom_tile(aes(fill = value)) +
  geom_polygon(aes(long, lat, group = group), 
               fill = NA, 
               size = 1, 
               color = "grey15", 
               data = r9_tidy) +
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
  labs(subtitle = expression(paste("Change in ", SO[4], " deposition [2008-2014]"))) +
  scale_fill_viridis(option = "viridis", 
                     na.value="transparent",
                     name = expression(paste(SO[4]^{-2}, " kg/ha")),
                     direction = -1,
                     # limits = c(-15, 20),
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
                     )) 


ggsave("figures/r9_so4_08_14.jpg", height = 8.5, width = 11, units = "in")


#total n
#crop value to wisc
n_r9 <- raster::intersect(delta_n, r9)
# var_crop[var_crop == 0] <- "NA"



gplot(n_r9 ) +
  geom_tile(aes(fill = value)) +
  geom_polygon(aes(long, lat, group = group), 
               fill = NA, 
               size = 1, 
               color = "grey15", 
               data = r9_tidy) +
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
  labs(subtitle = expression(paste("Change in inorganic N deposition [2008-2014]"))) +
  scale_fill_viridis(option = "viridis", 
                     na.value="transparent",
                     name = expression(paste(NO[3], " & ", NH[4], " kg/ha")),
                     direction = -1,
                     # limits = c(-15, 20),
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
                     )) 


ggsave("figures/r9_totaln_08_14.jpg", height = 8.5, width = 11, units = "in")




#----------------------------------------------------------------------------
########################################
## total n + s
########################################


#2007-2009
#crop value to wisc
sn_07_09_r9 <- raster::intersect(sn_07_09, r9)
# var_crop[var_crop == 0] <- "NA"



gplot(sn_07_09_r9) +
  geom_tile(aes(fill = value)) +
  geom_polygon(aes(long, lat, group = group), 
               fill = NA, 
               size = 1, 
               color = "grey15", 
               data = r9_tidy) +
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
  labs(subtitle = "Mean Sulfur + Nitrogen Wet Deposition, 2007−2009") +
  scale_fill_viridis(option = "viridis", 
                     na.value="transparent",
                     name = "S + N (eq/ha)",
                     direction = -1,
                     limits = c(150, 950),
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
                     )) 


ggsave("figures/r9_sn_07_09.jpg", height = 8.5, width = 11, units = "in")



#2013-2015

#crop value to wisc
sn_13_15_r9 <- raster::intersect(sn_13_15, r9)
# var_crop[var_crop == 0] <- "NA"



gplot(sn_13_15_r9) +
  geom_tile(aes(fill = value)) +
  geom_polygon(aes(long, lat, group = group), 
               fill = NA, 
               size = 1, 
               color = "grey15", 
               data = r9_tidy) +
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
                     limits = c(150, 950),
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
                     )) 


ggsave("figures/r9_sn_13_15.jpg", height = 8.5, width = 11, units = "in")




#----------------------------------------------------------------------------





