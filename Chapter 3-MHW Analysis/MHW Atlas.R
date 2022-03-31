# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing
library(heatwaveR)
library(ggpubr)

###### PARAMETERS ######

# coord box to plot
latitude <- c(11, 27)
longitude <- c(-86, -60)

# Date download range by start and end dates per year
# Dates are 1982-2020, code can only process 9 years at a time
# NOTE: This will also be the date range plotted
dl_years <- data.frame(date_index = 1:1,
                       start = as.Date(c("1982-01-01")),
                       end = as.Date(c("2019-12-31")))


###### SCRIPT ######
# This function downloads and prepares data based on user provided start and end dates
# Lat. and long. correspond to a specific point in the Caribbean
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(x = "ncdcOisst21Agg_LonPM180", 
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = latitude,
                       longitude = longitude,
                       fields = "sst")$data %>% 
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::rename(t = time, temp = sst) %>% 
    select(lon, lat, t, temp) %>% 
    na.omit()
}

# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup() %>% 
  select(lon, lat, t, temp)

event_only <- function(df){
  # First calculate the climatologies
  clim <- ts2clm(data = df, climatologyPeriod = c(dl_years$start[1], tail(dl_years$end, 1)))
  # Then the events
  event <- detect_event(data = clim)
  # Return only the event metric dataframe of results
  return(event$event)
}

MHW_dplyr <- OISST_data %>% 
  # Then we group the data by the 'lon' and 'lat' columns
  group_by(lon, lat) %>% 
  # Then we run our MHW detecting function on each group
  group_modify(~event_only(.x))

OISST_summarise <- MHW_dplyr %>%
  # Group by point coord
  group_by(lon, lat) %>%
  # Create summary of duration, intensity and number of events for each point
  summarise(duration_mean = mean(duration), intensity_mean = mean(intensity_mean), n = n(), .groups = "drop")

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, plot = FALSE)) %>% 
  dplyr::rename(lon = long)

map_duration <- ggplot(OISST_summarise, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = duration_mean), interpolate = FALSE, alpha = 0.9) +
  scale_fill_gradient2(name = "Mean Duration", high = "red", mid = "deepskyblue3",
                       low = "white", midpoint = 10,
                       guide = guide_colourbar(direction = "horizontal",
                                               title.position = "top")) +
  geom_polygon(data = map_base, aes(group = group), 
               colour = NA, fill = "grey80") +
  coord_fixed(ratio = 1, xlim = longitude, ylim = latitude, expand = FALSE) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom")

map_intensity <- ggplot(OISST_summarise, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity_mean), interpolate = FALSE, alpha = 0.9) +
  scale_fill_gradient2(name = "Mean Intensity", high = "red", mid = "deepskyblue3",
                       low = "white", midpoint = 1,
                       guide = guide_colourbar(direction = "horizontal",
                                               title.position = "top")) +
  geom_polygon(data = map_base, aes(group = group), 
               colour = NA, fill = "grey80") +
  coord_fixed(ratio = 1, xlim = longitude, ylim = latitude, expand = FALSE) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom")

# For the number of events map, may want to change the scale to a discrete (and not green :) )
map_n <- ggplot(OISST_summarise, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = n), interpolate = FALSE, alpha = 0.9) +
  scale_fill_gradient2(name = "Number of Events", high = "red", mid = "deepskyblue3",
                       low = "white", midpoint = 60,
                       guide = guide_colourbar(direction = "horizontal",
                                               title.position = "top")) +
  geom_polygon(data = map_base, aes(group = group), 
               colour = NA, fill = "grey80") +
  coord_fixed(ratio = 1, xlim = longitude, ylim = latitude, expand = FALSE) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom")

map_both <- ggpubr::ggarrange(map_duration, map_intensity, map_n, align = "hv")
map_both
