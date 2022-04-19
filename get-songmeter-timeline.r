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
data.dir <- '../Data/'

# The name of the two files
recordingLengths.filename <- "songmeter_recording_lengths 2012-2020.csv"
deployments.filename <- "SM-DeploymentSchedule-History2014-2020.csv"


#####################
## Load tables  

# The recording lengths file (paste0 is used to stick two strings together)
lengths.raw.df <- read.csv( paste0( data.dir, recordingLengths.filename ) )

# The deployments file
deps.raw.df <- read.csv( paste0( data.dir, deployments.filename ) )


#####################
## Wrangle data
# This is where I will manipulate the data to my liking

# Mutate the file names to separate the device ID from the time stamp
# This is done using 'str_extract' which uses a regular expression (regex) to 
# find the desired text. Regex is notoriously difficult to learn so don't fuss
# too much over what I'm doing. If bugs are found in this code, I can fix it!
lengths.df <- lengths.raw.df %>% 
  mutate(deviceID = as.integer(str_extract(file.location, '(?<=(POPA|SM)( |-)?)[0-9]{1,2}(?=.*)')),
         timestamp.raw = str_extract(filename, '(?<=(POPA|SM)[-]?[0-9]{1,2}_)(.*)(?=.wav)'),
         timestamp.raw = if_else(is.na(timestamp.raw), str_extract(filename, '^(.*)(?=.wav)'), timestamp.raw),
         timestamp = as.POSIXct(timestamp.raw, format = "%Y%m%d_%H%M%S"),
       device.name = paste("POPA-", deviceID, sep = '')) %>%
  # I'm grouping the data by file name because there are lots of duplicates.
  # I also am creating a new variable 'n.copies' for the number of duplicates.
  group_by(filename) %>% 
  summarise(deviceID = deviceID[1],
            device.name = device.name[1],
            timestamp = timestamp[1],
            length.minutes = length.min[1],
            n.copies = n())

# Fix the deployments file so it's ready to be merged with the recording lengths
deps.df <- deps.raw.df %>%
  mutate(ts.start = as.POSIXct(paste(year.in, mon.in, day.in), format = "%Y %b %d"),
         ts.end = as.POSIXct(paste(year.out, month.out, day.out), format = "%Y %b %d"),
         deployment.days = difftime(ts.end, ts.start, units = 'days')) %>%
  select(POPA, routeID = MMMP.Route, route.name = Location, 
         pointID = Site.ID, lat = Latitude, lon = Longitude, ts.start, ts.end, deployment.days, notes = Notes)

# Here I'm just saving a tidied version of the deployments file with column names fixed
deps.df %>% rename(deviceID = POPA) %>% write.csv( paste0( data.dir, deployments.filename, ' - LEB edit.csv' ), row.names = F )

# Merge songmeter deployments with recording lengths
# This takes a while since I'm using fuzzy_left_join
recordings.df <- lengths.df %>%
  fuzzy_left_join(deps.df, 
                  by = c('deviceID' = 'POPA', 
                         'timestamp' = 'ts.start', 
                         'timestamp' = 'ts.end'),
                  match_fun = list(`==`, `>=`, `<=`))

recordings.df %>% select(-POPA) %>% write.csv( paste0( data.dir, 'All Songmeter Recordings, Dates, and Locations.csv' ), row.names = F )

# This is to verify there aren't any duplicated deployments of songmeters
duplicate.deployments.df <- recordings.df %>%
  mutate(deployID = as.integer(as.factor(paste(deviceName, route.name, lat, lon, ts.start, ts.end)))) %>%
  group_by(deployID) %>%
  summarise(n.devices = length(unique(deviceName)))

# Summarise recordings by point.
# ts.min and ts.max represent first and last recordings for the point/year.
recordings.by.point.df <- recordings.df %>%
  mutate(year = factor(year(timestamp))) %>%
  group_by(year, routeID, route.name, pointID) %>%
  summarise(n.devices = length(unique(deviceID)),
            deviceID = paste0(unique(deviceID), collapse = ','),
            n.minutes = sum(length.minutes, na.rm = T),
            ts.min = min(timestamp, na.rm = T),
            ts.max = max(timestamp, na.rm = T),
            n.days = round(difftime(ts.max, ts.min, units = 'days')))


recordings.by.point.df %>% write.csv( paste0( data.dir, 'Songmeter recordings by route.csv' ), row.names = F )

# Number of recording minutes with NO METADATA
n.minutes.noMeta <- sum(recordings.by.point.df$n.minutes[which(is.na(recordings.by.point.df$routeID))])

# Number of recording minutes with metadata
n.minutes.withMeta <- sum(recordings.by.point.df$n.minutes[which(!is.na(recordings.by.point.df$routeID))])

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
