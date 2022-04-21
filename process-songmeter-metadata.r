####################
## Load libraries

# This one isn't necessary, but it includes a bunch of
# helpful functions that makes the code tidier and easier to read.
library(tidyverse)

# This library makes dealing with date objects a bit easier.
library(lubridate)

# We use this to join recordings with SM deployments
library(fuzzyjoin)

#####################
## Define global constants
# A global constant is an object (string, number, vector, table, etc.) that
#  can be used anywhere in the entire script (global) and doesn't change (is constant)

# Where I stored the data
data.dir <- '../../Data/'

# The name of the two files
recordingLengths.filename <- "songmeter-recording-specs.csv"
deployments.filename <- "songmeter-deployments.csv"
point.metadata.filename <- "point.metadata.csv"


#####################
## Load tables

# The recording lengths file (paste0 is used to stick two strings together)
lengths.raw.df <- read.csv( paste0( data.dir, recordingLengths.filename ) ) %>%
  mutate(timestamp = as.Date(timestamp))

# The deployments file
deps.raw.df <- read.csv( paste0( data.dir, deployments.filename ) ) %>%
  mutate(
    ts.start = as.Date(ts.start),
    ts.end = ifelse(is.na(ts.end) | (length(ts.end) == 0), ceiling_date(ts.start,"year")-days(1), (ts.end)) %>% as.Date(origin = "1970-01-01"),
    ts.end = ifelse(is.na(ts.end) | (length(ts.end) == 0), ceiling_date(ts.start,"year")-days(1), (ts.end)) %>% as.Date(origin = "1970-01-01"))

# Survey point metadata
points.df <- read.csv( paste0( data.dir, point.metadata.filename ) ) %>%
  mutate(routeID = gsub("-[0-9A-z]{2}$","", pointID))

#####################
## Wrangle data
# This is where I will manipulate the data to my liking

# Merge songmeter deployments with recording lengths
# This takes a while since I'm using fuzzy_left_join
recordings.df <- lengths.raw.df %>%
  fuzzy_left_join(deps.raw.df,
                  by = c('deviceID' = 'deviceID',
                         'timestamp' = 'ts.start',
                         'timestamp' = 'ts.end'),
                  match_fun = list(`==`, `>=`, `<=`))

recordings.df %>% select(-deviceID.y) %>% rename(deviceID = deviceID.x) %>% write.csv( paste0( data.dir, 'songmeter-recording-metadata.csv' ), row.names = F )

recordings.noLatLon.df <- recordings.df %>% 
  filter(is.na(lat))

recordings.noWetland.df <- recordings.df %>% 
  filter(is.na(wetland.type))

# This is to verify there aren't any duplicated deployments of songmeters
duplicate.deployments.df <- recordings.df %>%
  rename(deviceID = deviceID.x) %>%
  mutate(deployID = as.integer(as.factor(paste(deviceID, route.name, lat, lon, ts.start, ts.end)))) %>%
  group_by(deployID) %>%
  summarise(n.devices = length(unique(deviceID)))

# Summarise recordings by point.
# ts.min and ts.max represent first and last recordings for the point/year.
recordings.by.point.df <- recordings.df %>%
  rename(deviceID = deviceID.x) %>%
  mutate(year = factor(year(timestamp))) %>%
  group_by(year, routeID, route.name, pointID) %>%
  summarise(driveID = paste0(unique(driveID), collapse = ','),
            wetland.type = paste0(unique(wetland.type), collapse = ','),
            n.minutes = sum(length.secs, na.rm = T) / 60,
            ts.min = min(timestamp, na.rm = T),
            ts.max = max(timestamp, na.rm = T),
            n.days = round(difftime(ts.max, ts.min, units = 'days')),
            n.devices = length(unique(deviceID)),
            deviceID = paste0(unique(deviceID), collapse = ','))

recordings.by.point.df %>% write.csv( paste0( data.dir, 'songmeter-route-summary.csv' ), row.names = F )

# Number of recording minutes with NO METADATA
n.minutes.noMeta <- sum(recordings.by.point.df$n.minutes[which(is.na(recordings.by.point.df$lat))])

# Number of recording minutes with metadata
n.minutes.withMeta <- sum(recordings.by.point.df$n.minutes[which(!is.na(recordings.by.point.df$lat))])

# Percent of recording minutes with NO METADATA
round(100*n.minutes.noMeta/(n.minutes.noMeta+n.minutes.withMeta))

# Make a plot of this summary
# I had to remove NA route names
recordings.by.point.df %>%
  filter(!is.na(route.name)) %>%
  group_by(year, route.name) %>%
  summarise(n.minutes = sum(n.minutes, na.rm = T),
            n.points = length(unique(pointID))) %>%
  ggplot(aes(route.name, year, size = n.points, color = n.minutes))+
  geom_point()+
#  guides(color = T)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5))+
  scale_color_viridis_c()+
  labs(x = 'Route name', y = 'Year', size = 'Number of\n points', color = 'Number of\n minutes', title = 'Number of recording minutes per wetland route per year')
