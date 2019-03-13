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

# eastern domain shapefile
e_domain <- readOGR("gis/eastern_domain")
e_domain <- spTransform(e_domain, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# nadp site ctds
site_ctds_df <-read_csv("raw_data/nadp_site_ctds.csv") %>% 
  dplyr::select(siteID = "Site ID", Latitude, Longitude)


# Annual Precipitation-Weighted Mean Concentrations:
nadp_conc <- read_csv("raw_data/NTN-All-cy.csv") %>% 
  dplyr::select(siteID, yr, pH)

# Annual Depositions ()
nadp_dep <- read_csv("raw_data/NTN-All-cydep.csv") %>% 
  dplyr::select(siteID, yr, SO4, totalN)

# NADP dat
nadp_dat <- left_join(nadp_conc, nadp_dep, by = c("siteID", "yr"))

# create NA values
nadp_dat[nadp_dat == -9] <- NA



#############################################################################
## overlay sites to get NADP sites in airshed
#############################################################################

# nadp monitor locations
site_ctds <- with(site_ctds_df, data.frame(Longitude, Latitude, row.names = siteID)) 
site_ctds <- na.omit(site_ctds)

site_sp_points <- SpatialPoints(site_ctds, proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) 
site_sp_points <- spTransform(site_sp_points, proj4string(e_domain))

# get data from eastern domain monitors
e_domain_mons <- over(site_sp_points, e_domain) %>% 
  mutate(siteID = row.names(.)) %>%
  mutate_if(is.factor, as.character) %>% 
  filter(!(is.na(DOMAIN))) %>% 
  dplyr::select(siteID) %>% 
  left_join(., nadp_dat, by = "siteID") 
  

#######NOTHING CHANGED BELOW HERE#######################################################################
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
  geom_ribbon(aes(x = yr, ymin=mean_value-sd_value, ymax=mean_value+sd_value,  fill = "100km buffer"), show.legend = FALSE) +
  geom_line(aes(yr, mean_value, color = "100km buffer", fill = "100km buffer"),  size = 1.2, show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Year", 
       y = "Lab pH", 
       title = "Acidity of Precipitation") +
  
  geom_line(aes(yr, pH, color = "Parsons NADP", fill = "Parsons NADP"), size = 1.2, show.legend = FALSE, data = wv_mons) +
  scale_color_manual(name = NULL,
                    values = c("Parsons NADP" = "darkgreen", "100km buffer" = "grey85"), breaks = c("Parsons NADP", "100km buffer")) +
  scale_fill_manual(name = NULL,
                    values = c("100km buffer" = "grey75", "Parsons NADP" = "white"), breaks = c("Parsons NADP", "100km buffer")) +
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
        


ggsave(filename = "figures/mon_ph_trend.jpg",
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
  geom_ribbon(aes(x = yr, ymin=mean_value-sd_value, ymax=mean_value+sd_value,  fill = "100km buffer"), show.legend = FALSE) +
  geom_line(aes(yr, mean_value, color = "100km buffer", fill = "100km buffer"),  size = 1.2, show.legend = FALSE) +
  theme_minimal() +
  geom_line(aes(yr, SO4, color = "Parsons NADP", fill = "Parsons NADP"), size = 1.2, data = wv_mons, show.legend = FALSE) +
  scale_color_manual(name = NULL,
                     values = c("Parsons NADP" = "darkgreen", "100km buffer" = "grey85"),
                     breaks = c("Parsons NADP", "100km buffer")) +
  scale_fill_manual(name = NULL,
                    values = c("100km buffer" = "grey75", "Parsons NADP" = "white"),
                    breaks = c("Parsons NADP", "100km buffer")) +
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


ggsave(filename = "figures/mon_so4_trend.jpg",
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
  geom_ribbon(aes(x = yr, ymin=mean_value-sd_value, ymax=mean_value+sd_value,  fill = "100km buffer")) +
  geom_line(aes(yr, mean_value, color = "100km buffer", fill = "100km buffer"),  size = 1.2) +
  theme_minimal() +
  geom_line(aes(yr, totalN, color = "Parsons NADP", fill = "Parsons NADP"), size = 1.2, data = wv_mons) +
  scale_color_manual(name = NULL,
                     values = c("Parsons NADP" = "darkgreen", "100km buffer" = "grey85"),
                     breaks = c("Parsons NADP", "100km buffer")) +
  scale_fill_manual(name = NULL,
                    values = c("100km buffer" = "grey75", "Parsons NADP" = "white"),
                    breaks = c("Parsons NADP", "100km buffer")) +
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
        legend.position = c(0.82, 0.9),
        legend.margin = margin(0,1.5,5,1),
        plot.margin = margin(0,0,0,0)) +
  guides(color = guide_legend(override.aes = list(size = c(2, 1.2))))




ggsave(filename = "figures/mon_n_trend.jpg",
       plot = n_plot,
       height = 3,
       width = 4.2,
       units = "in") 




#############################################################################
## faceted plot
#############################################################################


# get monitors from mon and buffer
wv_mons <- over(site_sp_points, mon) %>% 
  mutate(siteID = row.names(.)) %>%
  mutate_if(is.factor, as.character) %>% 
  filter(!(is.na(NAME))) %>% 
  dplyr::select(siteID) %>% 
  left_join(., nadp_dat, by = "siteID") %>% 
  gather(variable, value, pH:totalN) 

buffer_stat_df <- buffer_mons %>% 
  gather(variable, value, pH:totalN) %>% 
  group_by(yr, variable) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE),
            min_value = min(value, na.rm = TRUE),
            max_value = max(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE)) %>% 
  ungroup()

buffer_stat_df$variable <- factor(buffer_stat_df$variable, 
                                  # levels = c("pH", "SO4", "totalN"),
                                  labels = c("Aciditiy~of~Precipitation~(lab~pH)",
                                             "Sulfur~Deposition~(SO[4]^{~2-{}}~kg/ha)",
                                             "Nitrogen~Deposition~(NO[3]^~-{}~'&'~NH[4]^~+{}~kg/ha)"))
wv_mons$variable <- factor(wv_mons$variable,
                           labels = c("Aciditiy~of~Precipitation~(lab~pH)",
                                      "Sulfur~Deposition~(SO[4]^{~2-{}}~kg/ha)",
                                      "Nitrogen~Deposition~(NO[3]^~-{}~'&'~NH[4]^~+{}~kg/ha)"))

ggplot(data = buffer_stat_df) +
  geom_ribbon(aes(x = yr, ymin=mean_value-sd_value, ymax=mean_value+sd_value,  fill = "Nearby Area")) +
  geom_line(aes(yr, mean_value, color = "100km buffer", fill = "Parsons"),  size = 1.2) +
  theme_minimal() +
  geom_line(aes(yr, value, color = "Monongahela NF", fill = "Monongahela NF"), size = 1.2, data = wv_mons) +
  scale_color_manual(name = NULL,
                     values = c("Monongahela NF" = "darkgreen", "Nearby Area" = "grey85")) +
  scale_fill_manual(name = NULL,
                    values = c("Nearby Area" = "grey75", "Monongahela NF" = "white")) +
  labs(x = "Year", y = NULL) +
       # y = expression(paste(NO[3], " & ", NH[4], " kg/ha")), 
       # title = "Nitrogen Deposition") +
  scale_x_continuous(limits = c(1977, 2018),
                     breaks = seq(1977, 2017, 5),
                     minor_breaks = seq(1977, 2017, 1),
                     labels = seq(1977, 2017, 5)) +
  theme(axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(13),
        plot.subtitle = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.background  = element_blank(),
        legend.key  = element_rect(color = "white", fill = "white"),
        legend.position = "bottom") +
  facet_wrap(vars(variable), 
             nrow = 3, 
             ncol = 1, 
             strip.position = "top",
             labeller = label_parsed,
             scales = "free_y") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        strip.switch.pad.grid = unit(0, "cm"))
 

ggsave("figures/mon_facet_plot.jpg",
       height = 8,
       width = 4,
       units = "in")

















#############################################################################
## plotting all stations
#############################################################################

buffer_stat_df <- buffer_mons %>% 
  gather(variable, value, pH:totalN) %>% 
  group_by(yr, variable) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE),
            min_value = min(value, na.rm = TRUE),
            max_value = max(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE)
  ) %>% 
  ungroup()


##-------------
## ph
##-------------

ph_dat <- buffer_stat_df %>% 
  filter(variable == "pH") 


ggplot(aes(yr, mean_value, group = variable), data = ph_dat) +
  geom_errorbar(aes(ymin=mean_value-sd_value, ymax=mean_value+sd_value), colour="black", width=.6) +
  geom_line(color = "darkred", alpha = 0.8, size = 1.2) +
  geom_point(size=3, shape=21, fill="darkred", color = "darkred", alpha = 0.8) +
  theme_minimal() +
  labs(x = "Year", 
       y = "Lab pH", 
       title = "Acidity of Precipitation in 100km buffer",
       subtitle = "around Monongahela NF") +
   scale_color_manual(values = cols,
                     name = NULL,
  ) +
  # scale_x_continuous(limits = c(min(plot_dat$yr) - 1, max(plot_dat$yr) + 1),
  #                    breaks = seq(min(plot_dat$yr), max(plot_dat$yr), x_breaks)) +
  scale_x_continuous(limits = c(1977, 2018),
                     breaks = seq(1978, 2017, 3),
                     minor_breaks = seq(1978, 2017, 1),
                     labels = seq(1978, 2017, 3)) +
  scale_y_continuous(lim = c(4, 5.5), breaks = seq(4,5.5, 0.25), labels = seq(4,5.5, 0.25)) +
  theme(axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(14),
        plot.subtitle = element_text(size = 14),
        legend.text = element_text(size = 12))



ggsave(filename = "figures/mon_buffer_ph_trend.jpg",
       height = 4,
       width = 6,
       units = "in")


##-------------
## so4
##-------------


s_dat <- buffer_stat_df  %>% 
  filter(variable == "SO4")


ggplot(aes(yr, mean_value, group = variable), data = s_dat) +
  geom_errorbar(aes(ymin=mean_value-sd_value, ymax=mean_value+sd_value), colour="black", width=.6) +
  geom_line(color = "darkred", alpha = 0.8, size = 1.2) +
  geom_point(size=3, shape=21, fill="darkred", color = "darkred", alpha = 0.8) +
  theme_minimal() +
  labs(x = "Year", 
       y = expression(paste(SO[4]^paste("  2", "-"), " kg/ha")), 
       title = "Sulfur Deposition in 100km buffer",
       subtitle = "around Monongahela NF") +
  scale_x_continuous(limits = c(1977, 2018),
                     breaks = seq(1978, 2017, 3),
                     minor_breaks = seq(1978, 2017, 1),
                     labels = seq(1978, 2017, 3)) +
  scale_y_continuous(lim = c(0, 55), breaks = seq(0,55, 5)) +
  theme(axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(14),
        plot.subtitle = element_text(size = 14),
        legend.text = element_text(size = 12))



ggsave(filename = "figures/mon_buffer_so4_trend.jpg",
       height = 4,
       width = 6,
       units = "in")

##-------------
## n
##-------------



n_dat <- buffer_stat_df %>% 
  filter(variable == "totalN") 


ggplot(aes(yr, mean_value, group = variable), data = n_dat) +
  geom_errorbar(aes(ymin=mean_value-sd_value, ymax=mean_value+sd_value), colour="black", width=.6) +
  geom_line(color = "darkred", alpha = 0.8, size = 1.2) +
  geom_point(size=3, shape=21, fill="darkred", color = "darkred", alpha = 0.8) +
  theme_minimal() +
  labs(x = "Year", 
       y = expression(paste(NO[3], " & ", NH[4], " kg/ha")), 
       title = "Nitrogen Deposition in 100km buffer",
       subtitle = "around Monongahela NF") +
  scale_x_continuous(limits = c(1977, 2018),
                     breaks = seq(1978, 2017, 3),
                     minor_breaks = seq(1978, 2017, 1),
                     labels = seq(1978, 2017, 3)) +
  scale_y_continuous(lim = c(0, 9), breaks = seq(0,9, 1)) +
  theme(axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(14),
        plot.subtitle = element_text(size = 14),
        legend.text = element_text(size = 12))






ggsave(filename = "figures/n_trend.jpg",
       height = 4,
       width = 6,
       units = "in") 

