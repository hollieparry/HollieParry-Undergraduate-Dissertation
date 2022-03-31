library(raster) 
library(dplyr)
library(heatwaveR)

## PARAMS
file_name <- "tos_Oday_MRI-ESM2-0_Caribbean_r1i1p1f1_gr_19500101-21001231.nc"

model_name <- "finalmodel.rds"

nc_data <- brick(file_name)

df <- data.frame()
for (i in 1:nlayers(nc_data)){
  layer <- nc_data[[i]]
  
  date_df <- raster::as.data.frame(layer, xy=TRUE)
  date_df$x <- date_df$x - 180
  
  date_name <- names(nc_data[[i]])
  date_df<-date_df%>% dplyr::rename(temp = date_name, lon = x, lat = y)%>%
    na.omit()
  
  date <- as.Date(substr(date_name,2,100), format = "%Y.%m.%d")
  date_df$t = date
  
  df <- rbind(df, date_df)
}

clim <- ts2clm(data = df, climatologyPeriod = c("1982-01-01", "2010-12-31"))

event_only <- function(df){
  # Then the events
  event <- detect_event(data = clim)
  # Return only the event metric dataframe of results
  return(event$event)
}

MHW_dplyr <- df %>% 
  # Then we group the data by the 'lon' and 'lat' columns
  group_by(lon, lat) %>% 
  # Then we run our MHW detecting function on each group
  group_modify(~event_only(.x))

lm <- readRDS(model_name)

MHW_dplyr$bleach_prob <- predict(lm, MHW_dplyr)
