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
library(grid)
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------


#############################################################################
## load data: http://nadp.slh.wisc.edu/data/ntn/ntnAllsites.aspx
#############################################################################

# unit and buffer
unit_sh <- readOGR("gis/hoosier_nf") 
unit_buffer <- gBuffer(unit_sh, width = 2e+05)

unit_sh <- spTransform(unit_sh, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
unit_buffer <- spTransform(unit_buffer, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))


# nadp site ctds
site_ctds_df <-read_csv("raw_data/nadp_site_ctds.csv") %>% 
  dplyr::select(siteID = "Site ID", Latitude, Longitude)


# Annual Precipitation-Weighted Mean Concentrations:
nadp_conc <- read_csv("raw_data/NTN-All-cy.csv") %>% 
  dplyr::select(siteID, yr, Criteria1:Criteria3, pH)

# Annual Depositions ()
nadp_dep <- read_csv("raw_data/NTN-All-cydep.csv") %>% 
  dplyr::select(siteID, yr,  Criteria1:Criteria3,  SO4, totalN)

# NADP dat
nadp_dat <- left_join(nadp_conc, nadp_dep, by = c("siteID", "yr", "Criteria1", "Criteria2", "Criteria3")) %>% 
  filter(Criteria1 >= 75 & Criteria2 >= 90 & Criteria3 >= 75)

# create NA values
nadp_dat[nadp_dat == -9] <- NA

#############################################################################
## overlay sites to get NADP sites in unit and unit buffer
#############################################################################

# nadp monitor locations
site_ctds <- with(site_ctds_df, data.frame(Longitude, Latitude, row.names = siteID)) 
site_ctds <- na.omit(site_ctds)

site_sp_points <- SpatialPoints(site_ctds, proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) 
site_sp_points <- spTransform(site_sp_points, proj4string(unit_sh))


# find nearest monitor
find_nearest_NADP <- function(UNIT) {
  # gis data
  unit_sh_proj <- readOGR(paste("gis/", UNIT, sep = "")) 
  site_sp_points_proj <- spTransform(site_sp_points, proj4string(unit_sh_proj))
  
  unit_site_dist <- as_tibble(gDistance(site_sp_points_proj, unit_sh_proj, byid =TRUE)) %>% 
    gather(siteID, dist_to_unit) %>% 
    arrange(dist_to_unit) %>% 
    slice(1) %>% 
    pull(siteID)
    
  return(unit_site_dist)
}

unit_nadp <- find_nearest_NADP("hoosier_NF")


# get monitors from unit and buffer...NOTE: no site on Hoosier so need to find closest monitor
unit_mons <- nadp_dat %>% 
  filter(siteID == unit_nadp)


buffer_mons <- tibble(siteID = names(over(site_sp_points, unit_buffer)),
                          in_buffer = over(site_sp_points, unit_buffer)) %>% 
  filter(!(is.na(in_buffer))) %>% 
  dplyr::select(siteID) %>% 
  left_join(., nadp_dat, by = "siteID")

buffer_stat_df <- buffer_mons %>% 
  gather(variable, value, pH:totalN) %>% 
  group_by(yr, variable) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE),
            min_value = min(value, na.rm = TRUE),
            max_value = max(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE)
  ) %>% 
  ungroup()

#############################################################################
## plotting wv station with other 
#############################################################################

##-------------
## ph
##-------------

ph_dat <- buffer_stat_df %>% 
  filter(variable == "pH") 

# remove show.legend tro get legend of all
ph_plot <- ggplot(data = ph_dat) +
  geom_ribbon(aes(x = yr, ymin=mean_value-sd_value, ymax=mean_value+sd_value,  fill = "200km buffer"), show.legend = FALSE) +
  geom_line(aes(yr, mean_value, color = "200km buffer", fill = "200km buffer"),  size = 1.2, show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Year", 
       y = "Lab pH", 
       title = "Acidity of Precipitation") +
  
  geom_line(aes(yr, pH, color = "SW Purdue Ag NADP", fill = "SW Purdue Ag NADP"), size = 1.2, show.legend = FALSE, data = unit_mons) +
  scale_color_manual(name = NULL,
                    values = c("SW Purdue Ag NADP" = "darkgreen", "200km buffer" = "grey85"), breaks = c("SW Purdue Ag NADP", "200km buffer")) +
  scale_fill_manual(name = NULL,
                    values = c("200km buffer" = "grey75", "SW Purdue Ag NADP" = "white"), breaks = c("SW Purdue Ag NADP", "200km buffer")) +
  scale_x_continuous(limits = c(1977, 2018),
                     breaks = seq(1977, 2017, 5),
                     minor_breaks = seq(1977, 2017, 1),
                     labels = seq(1977, 2017, 5)) +
  scale_y_continuous(lim = c(4, 5.5), breaks = seq(4,5.5, 0.25), labels = seq(4,5.5, 0.25)) +
  
  theme(axis.title = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(12),
        plot.subtitle = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.background  = element_rect(fill = "white"),
        legend.key  = element_rect(color = "white", fill = "white"),
        legend.position = c(0.23, 0.8),
        legend.margin = margin(0,1.5,1,1),
        plot.margin = margin(0,0,0,0)) +
  guides(color = guide_legend(override.aes = list(size = c(2, 1.2))))
        


ggsave(filename = "figures/hoosier_ph_trend.jpg",
       plot = ph_plot,
       height = 3,
       width = 4.2,
       units = "in")







##-------------
## so4
##-------------

so4_dat <- buffer_stat_df %>% 
  filter(variable == "SO4") 


so4_plot <- ggplot(data = so4_dat) +
  geom_ribbon(aes(x = yr, ymin=mean_value-sd_value, ymax=mean_value+sd_value,  fill = "200km buffer"), show.legend = FALSE) +
  geom_line(aes(yr, mean_value, color = "200km buffer", fill = "200km buffer"),  size = 1.2, show.legend = FALSE) +
  theme_minimal() +
  geom_line(aes(yr, SO4, color = "SW Purdue Ag NADP", fill = "SW Purdue Ag NADP"), size = 1.2, data = unit_mons, show.legend = FALSE) +
  scale_color_manual(name = NULL,
                     values = c("SW Purdue Ag NADP" = "darkgreen", "200km buffer" = "grey85"),
                     breaks = c("SW Purdue Ag NADP", "200km buffer")) +
  scale_fill_manual(name = NULL,
                    values = c("200km buffer" = "grey75", "SW Purdue Ag NADP" = "white"),
                    breaks = c("SW Purdue Ag NADP", "200km buffer")) +
  labs(x = "Year", 
       y = expression(paste(SO[4]^paste("  2", "-"), " (kg/ha)")), 
       title = "Sulfur Deposition") +
  scale_x_continuous(limits = c(1977, 2018),
                     breaks = seq(1977, 2017, 5),
                     minor_breaks = seq(1977, 2017, 1),
                     labels = seq(1977, 2017, 5)) +
  scale_y_continuous(lim = c(0, 55), breaks = seq(0,55, 5)) +
  theme(axis.title = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(12),
        plot.subtitle = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.background  = element_rect(fill = "white"),
        legend.key  = element_rect(color = "white", fill = "white"),
        legend.position = c(0.8, 0.8),
        legend.margin = margin(0,1.5,1,1),
        plot.margin = margin(0,0,0,0)) +
  guides(color = guide_legend(override.aes = list(size = c(2, 1.2))))


ggsave(filename = "figures/hoosier_so4_trend.jpg",
       plot = so4_plot,
       height = 3,
       width = 4.2,
       units = "in")

##-------------
## n
##-------------

n_dat <- buffer_stat_df %>% 
  filter(variable == "totalN") 


n_plot <- ggplot(data = n_dat) +
  geom_ribbon(aes(x = yr, ymin=mean_value-sd_value, ymax=mean_value+sd_value,  fill = "200km buffer")) +
  geom_line(aes(yr, mean_value, color = "200km buffer", fill = "200km buffer"),  size = 1.2) +
  theme_minimal() +
  geom_line(aes(yr, totalN, color = "SW Purdue Ag NADP", fill = "SW Purdue Ag NADP"), size = 1.2, data = unit_mons) +
  scale_color_manual(name = NULL,
                     values = c("SW Purdue Ag NADP" = "darkgreen", "200km buffer" = "grey85"),
                     breaks = c("SW Purdue Ag NADP", "200km buffer")) +
  scale_fill_manual(name = NULL,
                    values = c("200km buffer" = "grey75", "SW Purdue Ag NADP" = "white"),
                    breaks = c("SW Purdue Ag NADP", "200km buffer")) +
  labs(x = "Year", 
       y = expression(paste(NO[3]^" -", " & ", NH[4]^" +", " (kg/ha)")), 
       title = "Nitrogen Deposition") +
  scale_x_continuous(limits = c(1977, 2018),
                     breaks = seq(1977, 2017, 5),
                     minor_breaks = seq(1977, 2017, 1),
                     labels = seq(1977, 2017, 5)) +
  scale_y_continuous(lim = c(0, 10), breaks = seq(0,10, 1)) +
  theme(axis.title = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(12),
        plot.subtitle = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.background  = element_rect(fill = "white"),
        legend.key  = element_rect(color = "white", fill = "white"),
        legend.position = c(0.75, 0.15),
        legend.margin = margin(0,1.5,5,1),
        plot.margin = margin(0,0,0,0)) +
  guides(color = guide_legend(override.aes = list(size = c(2, 1.2))))




ggsave(filename = "figures/hoosier_n_trend.jpg",
       plot = n_plot,
       height = 3,
       width = 4.2,
       units = "in") 




