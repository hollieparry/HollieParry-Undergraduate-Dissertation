library(dplyr)
library(lubridate)

# Load MHW Dataset
MHW <- read.csv("MHW.csv")

# We are going to create a year and month column from date_peak to match bleach data

# Convert date_peak to datetime format
MHW$date_peak <- strptime(MHW$date_peak, format="%Y-%m-%d")

# Round date to nearest 6 months
MHW$date_round <- round_date(MHW$date_peak, unit = "1 year")

# Remove duplicated rows based on lat lon month year
MHW %>% distinct(lon,lat,date_round, .keep_all=TRUE)

# Load bleach dataset
bleach <- read.csv("bleach.csv", encoding = "UTF-8")

# Rename col names to match the MHW col names
names(bleach)[1] <- "lat"
names(bleach)[2] <- "lon"
names(bleach)[3] <- "month"
names(bleach)[4] <- "year"

# Remove columns with month as NA
bleach <- bleach[!is.na(bleach$month),]

# Create a date column so we can round, month year to nearest 6 months
bleach$date <- paste(bleach$year, bleach$month,"01",sep="-")
bleach$date <- strptime(bleach$date, format="%Y-%m-%d")

bleach$date_round <- round_date(bleach$date, unit = "1 year")

# Remove duplicated rows based on lat lon month year
bleach %>% distinct(lon,lat,month,date_round, .keep_all=TRUE)

# Round lon lat to the nearest 0.125 + n(0.25)
bleach$lon <- round(bleach$lon/0.25)*0.25+0.125
bleach$lat <- round(bleach$lat/0.25)*0.25+0.125

# Add column to indicate bleach event
bleach$bleach_event <- 1

# Merge datasets on lon,lat,mon,year
MHW_bleach <- merge(x = MHW, y = bleach, by = c("lon","lat","date_round"), all.x=TRUE)

# Fill events that are not bleached with 0
MHW_bleach$bleach_event[is.na(MHW_bleach$bleach_event)] <- 0

# Create model
mhw_bleach.lm <- glm(formula = bleach_event ~ duration + intensity_cumulative_abs, data = MHW_bleach, family = binomial(link="probit"))

# Save model
saveRDS(mhw_bleach.lm, "finalmodel.rds")
