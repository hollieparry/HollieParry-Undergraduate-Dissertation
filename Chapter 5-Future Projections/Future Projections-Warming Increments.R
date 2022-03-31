# The packages we will use

library(raster)
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
latitude <- c(11, 14)
longitude <- c(-87, -83)

model_name <- "duration_intensitycumabs.rds"

csv_name <- "intencumabs_duration.csv"

###### SCRIPT ######

sst_change <- data.frame()

for (i in 1:12){
  # Get sst_change
  file_name <- paste(toString(i),".CMIP6 - Sea Surface Temperature (SST) Change deg C - Warming 1.5°C SSP5-8.5 (rel. to 1981-2010) - ",month.name[i],
    " (26 models).nc",sep="")
  nc_data <- raster(file_name)
  month_sst_change <- raster::as.data.frame(nc_data, xy=TRUE)
  
  month_sst_change <- month_sst_change %>% dplyr::rename(lon=x, lat=y,sst_change=GDAL.Band.Number.1) %>%
    na.omit() %>%
    filter(lat > latitude[1] & lat < latitude[2]) %>%
    filter(lon > longitude[1] & lon < longitude[2])
  
  # Round lon lat to the nearest 0.125 + n(0.25)
  month_sst_change$lon <- round(month_sst_change$lon/0.25)*0.25+0.125
  month_sst_change$lat <- round(month_sst_change$lat/0.25)*0.25+0.125
  
  month_sst_change$month <- i
  
  sst_change <- rbind(sst_change, month_sst_change)
}

# spline interpolate for each lat lon
split_sst <- split(sst_change, interaction(sst_change$lon, sst_change$lat))

interpolated_sst <- data.frame()

for (df in split_sst){
  sst_vector <- df$sst_change
  sst_vector <- c(sst_vector, sst_vector[1])
  sst_interpolated <- spline(sst_vector, method="periodic", n=365)$y
  new_df = data.frame(day=1:365, sst = sst_interpolated, lat=rep(df$lat, 365),lon=rep(df$lon,365))[1:365,]
  interpolated_sst <- rbind(interpolated_sst, new_df)
}

# If u wanna make climatology bigger do it, just include 2010, this is when we are
# projections from
dl_years <- data.frame(date_index = 1:1,
                       start = as.Date(c("2005-01-01")),
                       end = as.Date(c("2010-12-31")))

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
    na.omit()
}

# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
OISST_data <- dl_years %>% 
  group_by(date_index) %>% 
  group_modify(~OISST_sub_dl(.x)) %>% 
  ungroup()

# Get day from oisst                     
OISST_data$t <- strptime(OISST_data$t, format="%Y-%m-%d")
OISST_data$day <- OISST_data$t$yday + 1

OISST_data <- merge(x=OISST_data, y=interpolated_sst, by=c("lat","lon","day"), all.x=TRUE)
OISST_data$sst_proj = OISST_data$temp + OISST_data$sst


OISST_data<-OISST_data %>% dplyr::select("lat", "lon", "t", "sst_proj") %>%
  na.omit() %>%
  dplyr::rename(temp = sst_proj)

event_only <- function(df){
  # First calculate the climatologies
  clim <- ts2clm(data = df, climatologyPeriod = c(dl_years$start[1], tail(dl_years$end, 1)))
  # Then the events
  event <- detect_event(data = clim)
  # Return only the event metric dataframe of results
  return(event$event)
}

OISST_data$t <- as.Date(OISST_data$t)

MHW_dplyr <- OISST_data %>% 
  # Then we group the data by the 'lon' and 'lat' columns
  group_by(lon, lat) %>% 
  # Then we run our MHW detecting function on each group
  group_modify(~event_only(.x))

lm <- readRDS(model_name)

MHW_dplyr$bleach_prob <- predict(lm, MHW_dplyr)
