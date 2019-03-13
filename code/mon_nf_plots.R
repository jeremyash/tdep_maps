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
library(viridis)
library(broom)
library(stringr)
library(RColorBrewer)

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

plot_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = 'white'
  color.grid.major = "black"
  color.axis.text = "black"
  color.axis.title = "black"
  color.title = "black"
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    
    # Format the grid
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(axis.line = )
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=13,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=13,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=14,color=color.axis.title, vjust=0, angle = 45)) +
    theme(axis.title.y=element_text(size=14,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}    




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

mnf <- readOGR("gis/mon_nf")
mnf_tidy <- tidy(mnf)

so4 <- readRDS("data/so4_rasters.RDS")
no3 <- readRDS("data/no3_rasters.RDS")
nh4 <- readRDS("data/nh4_rasters.RDS")

names(so4)
#----------------------------------------------------------------------------

########################################
## RASTER TO DFs
########################################


so4_df <- ldply(so4, function(x) raster_to_df(x)) %>% 
  rename(element_year = ".id") %>% 
  separate(element_year, into = c("element", "year"))

saveRDS(so4_df, "data/so4_df.RDS")

no3_df <- ldply(no3, function(x) raster_to_df(x)) %>% 
  rename(element_year = ".id") %>% 
  separate(element_year, into = c("element", "year"))

saveRDS(no3_df, "data/no3_df.RDS")

nh4_df <- ldply(nh4, function(x) raster_to_df(x)) %>% 
  rename(element_year = ".id") %>% 
  separate(element_year, into = c("element", "year"))

saveRDS(nh4_df, "data/nh4_df.RDS")

########################################
## CREATE PLOTS OF RASTERS
########################################

years <- seq(1985,2015,1)

### so4

# determine plotting limits
so4_df_lim <- so4_df %>% 
  summarise(min_dep = min(value),
            max_dep = max(value)) 


so4_plot_func <- function(YEAR) {
  # subset to yearly data
  plot_dat <- so4_df %>% 
    filter(year == YEAR)
  
  # generate plot
  ggplot(plot_dat) +
    geom_raster(aes(x, y, fill = value)) +
    # xlim(1270000, 1490000) +
    # ylim(1720000, 1980000) +
    scale_fill_viridis(name = expression(paste(SO[4]^{-2}, "(kg/ha)", sep = "   ")),
                       limits = c(0,65)) + 
    geom_polygon(data = mnf_tidy, aes(long, lat, group = group), 
                 fill = NA, size = 1.5, color = "black") +
    coord_equal() +
    theme_map() +
    labs(title = YEAR) +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm")) +
    theme(plot.title = element_text(size = 18, hjust = 0.5)) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.title = element_text(size = 12)) 
  
  
  # save plot to file
  
  plot_title <- paste("figures/",
                      unique(plot_dat$element),
                      "_",
                      unique(plot_dat$year),
                      ".pdf",
                      sep = "")
  
  ggsave(plot_title, height = 8.5, width = 11, units = "in")
  
  
}

lapply(years, function(x) so4_plot_func(x))
#------------------------


### no3

# determine plotting limits
no3_df_lim <- no3_df %>% 
  summarise(min_dep = min(value),
            max_dep = max(value)) 


no3_plot_func <- function(YEAR) {
  # subset to yearly data
  plot_dat <- no3_df %>% 
    filter(year == YEAR)
  
  # generate plot
  ggplot(plot_dat) +
    geom_raster(aes(x, y, fill = value)) +
    # xlim(1270000, 1490000) +
    # ylim(1720000, 1980000) +
    scale_fill_viridis(name = expression(paste(NO[3]^{" -"}, "(kg/ha)", sep = "   ")),
                       limits = c(0,35)) + 
    geom_polygon(data = mnf_tidy, aes(long, lat, group = group), 
                 fill = NA, size = 1.5, color = "black") +
    coord_equal() +
    theme_map() +
    labs(title = YEAR) +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm")) +
    theme(plot.title = element_text(size = 18, hjust = 0.5)) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.title = element_text(size = 12)) 
  
  
  # save plot to file
  
  plot_title <- paste("figures/",
                      unique(plot_dat$element),
                      "_",
                      unique(plot_dat$year),
                      ".pdf",
                      sep = "")
  
  ggsave(plot_title, height = 8.5, width = 11, units = "in")
  
  
}

lapply(years, function(x) no3_plot_func(x))
#------------------------

### nh4

# determine plotting limits
nh4_df_lim <- nh4_df %>% 
  summarise(min_dep = min(value),
            max_dep = max(value)) 


nh4_plot_func <- function(YEAR) {
  # subset to yearly data
  plot_dat <- nh4_df %>% 
    filter(year == YEAR)
  
  # generate plot
  ggplot(plot_dat) +
    geom_raster(aes(x, y, fill = value)) +
    # xlim(1270000, 1490000) +
    # ylim(1720000, 1980000) +
    scale_fill_viridis(name = expression(paste(NH[4]^{" +"}, "(kg/ha)", sep = "   ")),
                       limits = c(0,6)) + 
    geom_polygon(data = mnf_tidy, aes(long, lat, group = group), 
                 fill = NA, size = 1.5, color = "black") +
    coord_equal() +
    theme_map() +
    labs(title = YEAR) +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm")) +
    theme(plot.title = element_text(size = 18, hjust = 0.5)) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.title = element_text(size = 12)) 
  
  
  # save plot to file
  
  plot_title <- paste("figures/",
                      unique(plot_dat$element),
                      "_",
                      unique(plot_dat$year),
                      ".pdf",
                      sep = "")
  
  ggsave(plot_title, height = 8.5, width = 11, units = "in")
  
  
}

lapply(years, function(x) nh4_plot_func(x))
#------------------------

#----------------------------------------------------------------------------

########################################
## TRENDS OVER TIME
########################################

so4_mnf <- readRDS("data/so4_mnf.RDS")
no3_mnf <- readRDS("data/no3_mnf.RDS")
nh4_mnf <- readRDS("data/nh4_mnf.RDS")


so4_dat <- plyr::ldply(so4_mnf, function(x) 
  dat <- data_frame(dep = getValues(x)) %>% 
    summarise(min_dep = min(dep),
              max_dep = max(dep),
              mean_dep = mean(dep),
              sd_dep = sd(dep))) %>% 
  rename(element_year = ".id") %>% 
  separate(element_year, into = c("element", "year"))


no3_dat <- plyr::ldply(no3_mnf, function(x) 
  dat <- data_frame(dep = getValues(x)) %>% 
    summarise(min_dep = min(dep),
              max_dep = max(dep),
              mean_dep = mean(dep),
              sd_dep = sd(dep))) %>% 
  rename(element_year = ".id") %>% 
  separate(element_year, into = c("element", "year"))




nh4_dat <- plyr::ldply(nh4_mnf, function(x) 
  dat <- data_frame(dep = getValues(x)) %>% 
    summarise(min_dep = min(dep),
              max_dep = max(dep),
              mean_dep = mean(dep),
              sd_dep = sd(dep))) %>% 
  rename(element_year = ".id") %>% 
  separate(element_year, into = c("element", "year"))





# so4

ggplot(so4_dat, aes(year, mean_dep, group = element)) +
  geom_errorbar(aes(ymin=mean_dep-sd_dep, ymax=mean_dep+sd_dep), colour="black", width=.3) +
  geom_line(color = "darkred", alpha = 0.8, size = 1.2) +
  geom_point(size=3, shape=21, fill="darkred", color = "darkred", alpha = 0.8) +
  theme_bw() +
  labs(x = "Year",
       y = expression(paste(SO[4]^{" -2"}, " (kg/ha)", sep = "   ")),
       title = "Sulfate Ion Wet Deposition on Monongahela National Forest") +
  theme(axis.text.x=element_text(size=13,color="black", angle = 45, vjust = 0.95, hjust = 0.95)) +
  theme(axis.text.y=element_text(size=13,color="black")) +
  theme(axis.title.x=element_text(size=14,color="black", vjust=0)) +
  theme(axis.title.y=element_text(size=14,color="black", vjust=1.25)) +
  theme(plot.title = element_text(size = 18))

ggsave("figures/so4_trend.pdf", height = 8.5, width = 11, units = "in")




# no3

ggplot(no3_dat, aes(year, mean_dep, group = element)) +
  geom_errorbar(aes(ymin=mean_dep-sd_dep, ymax=mean_dep+sd_dep), colour="black", width=.3) +
  geom_line(color = "darkred", alpha = 0.8, size = 1.2) +
  geom_point(size=3, shape=21, fill="darkred", color = "darkred", alpha = 0.8) +
  theme_bw() +
  labs(x = "Year",
       y = expression(paste(NO[3]^{" -"}, " (kg/ha)", sep = "   ")),
       title = "Nitrate Ion Wet Deposition on Monongahela National Forest") +
  theme(axis.text.x=element_text(size=13,color="black", angle = 45, vjust = 0.95, hjust = 0.95)) +
  theme(axis.text.y=element_text(size=13,color="black")) +
  theme(axis.title.x=element_text(size=14,color="black", vjust=0)) +
  theme(axis.title.y=element_text(size=14,color="black", vjust=1.25)) +
  theme(plot.title = element_text(size = 18))

ggsave("figures/no3_trend.pdf", height = 8.5, width = 11, units = "in")


# nh4 
ggplot(nh4_dat, aes(year, mean_dep, group = element)) +
  geom_errorbar(aes(ymin=mean_dep-sd_dep, ymax=mean_dep+sd_dep), colour="black", width=.3) +
  geom_line(color = "darkred", alpha = 0.8, size = 1.2) +
  geom_point(size=3, shape=21, fill="darkred", color = "darkred", alpha = 0.8) +
  theme_bw() +
  labs(x = "Year",
       y = expression(paste(NH[4]^{" +"}, "(kg/ha)", sep = "   ")),
       title = "Ammonium Ion Wet Deposition on Monongahela National Forest") +
  theme(axis.text.x=element_text(size=13,color="black", angle = 45, vjust = 0.95, hjust = 0.95)) +
  theme(axis.text.y=element_text(size=13,color="black")) +
  theme(axis.title.x=element_text(size=14,color="black", vjust=0)) +
  theme(axis.title.y=element_text(size=14,color="black", vjust=1.25)) +
  theme(plot.title = element_text(size = 18))

ggsave("figures/nh4_trend.pdf", height = 8.5, width = 11, units = "in")



#----------------------------------------------------------------------------
########################################
## FIVE YEAR AVERAGES
########################################

# so4
so4_mean <- mean_raster_func(so4)

so4_mean %>% summarise(min_value = min(value), max_value = max(value))

ggplot(so4_mean) +
  geom_raster(aes(x, y, fill = value)) +
  # xlim(1270000, 1490000) +
  # ylim(1720000, 1980000) +
  scale_fill_viridis(name = expression(paste(SO[4]^{-2}, " (kg/ha)", sep = "   ")),
                     limits = c(0,65)) + 
  geom_polygon(data = mnf_tidy, aes(long, lat, group = group), 
               fill = NA, size = 1.5, color = "black") +
  coord_equal() +
  theme_map() +
  labs(title = "Sulfate Ion Wet Deposition on Monongahela National Forest") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  theme(plot.title = element_text(size = 24, hjust = 0.5)) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  facet_grid(. ~period) +
  theme(strip.text.x = element_text(size = 20, margin = margin(b = 0, t = 10))) +
  
  theme(strip.background =  element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

ggsave("figures/so4_five_year_means.pdf", height = 8.5, width = 11)

  
  

# no3

no3_mean <- mean_raster_func(no3)

no3_mean %>% summarise(min_value = min(value), max_value = max(value))

ggplot(no3_mean) +
  geom_raster(aes(x, y, fill = value)) +
  # xlim(1270000, 1490000) +
  # ylim(1720000, 1980000) +
  scale_fill_viridis(name = expression(paste(NO[3]^{" -"}, "(kg/ha)", sep = "   ")),
                     limits = c(0,35)) + 
  geom_polygon(data = mnf_tidy, aes(long, lat, group = group), 
               fill = NA, size = 1.5, color = "black") +
  coord_equal() +
  theme_map() +
  labs(title = "Nitrate Ion Wet Deposition on Monongahela National Forest") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  theme(plot.title = element_text(size = 24, hjust = 0.5)) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  facet_grid(. ~period) +
  theme(strip.text.x = element_text(size = 20, margin = margin(b = 0, t = 10))) +
  
  theme(strip.background =  element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

ggsave("figures/no3_five_year_means.pdf", height = 8.5, width = 11)




 
# nh4

nh4_mean <- mean_raster_func(nh4) 

nh4_mean %>% summarise(min_value = min(value), max_value = max(value))

ggplot(nh4_mean) +
  geom_raster(aes(x, y, fill = value)) +
  # xlim(1270000, 1490000) +
  # ylim(1720000, 1980000) +
  scale_fill_viridis(name = expression(paste(NH[4]^{" +"}, "(kg/ha)", sep = "   ")),
                     limits = c(0,5)) +
  geom_polygon(data = mnf_tidy, aes(long, lat, group = group), 
               fill = NA, size = 1.5, color = "black") +
  coord_equal() +
  theme_map() +
  labs(title = "Ammonium Ion Wet Deposition on Monongahela National Forest") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  theme(plot.title = element_text(size = 24, hjust = 0.5)) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  facet_grid(. ~period) +
  theme(strip.text.x = element_text(size = 20, margin = margin(b = 0, t = 10))) +
  
  theme(strip.background =  element_blank()) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

ggsave("figures/nh4_five_year_means.pdf", height = 8.5, width = 11)

########################################
## TESTING
########################################


so4_1981 <- so4[[1]]
so4_resample <- lapply(so4, function(x) resample(x, so4_1981, method = "bilinear"))
 

so4_past <- stack(so4_resample[c(1,2,3,4,5)])
so4_past_mean <- mean(so4_past)

so4_recent <- stack(so4_resample[c(27,28,29,30,31)])
so4_recent_mean <- mean(so4_recent)





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


so4_means <- mean_raster_func(so4)











