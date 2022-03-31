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

OISST_n_year <- MHW_dplyr %>%
  # Create a year column
  mutate(year = lubridate::year(date_start)) %>%
  # Group on year
  group_by(year) %>%
  # Get number of events per year
  summarise(n = n(), .groups = "drop")

barplot(height = OISST_n_year$n, names = OISST_n_year$year, xlab = "Year", ylab = "Number of Events", col="#69b3a2", las=2)

