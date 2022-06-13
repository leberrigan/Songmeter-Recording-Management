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
data.dir <- 'data/'

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

recordings.fixed.df <- recordings.df %>%
  left_join(select(points.df, pointID, latitude, longitude, type), by = "pointID") %>%
  mutate(lat = ifelse(is.na(lat), latitude, lat) %>% as.numeric,
         lon = ifelse(is.na(lon), longitude, lon) %>% as.numeric,
         wetland.type = ifelse(is.na(wetland.type), type, wetland.type)) %>%
  rename(deviceID = deviceID.x) %>%
  select(-type, -longitude, -latitude, -deviceID.y)


recordings.fixed.df %>% write.csv( paste0( data.dir, 'songmeter-recording-metadata.csv' ), row.names = F )

# Metadata Completeness
recordings.fixed.df %>%
  filter(year(timestamp) > 2000) %>%
  mutate(year = floor_date(timestamp, "year"),
         no.lat = is.na(lat) & is.na(route.name),
         no.wet = is.na(wetland.type),
         cat = case_when(no.lat ~ "No coords",
                         no.wet ~ "No wetland type",
                         T ~ "Good")) %>%
  ggplot(aes(year, length.secs/3600, group = cat, fill = cat))+
  geom_col()+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
  labs(x = "Year", y = "Hours of recordings", title = "Metadata completeness of SongMeter recordings thru all years")+
  guides(sum = "none")

# Distribution of wetland types
recordings.fixed.df %>%
  filter(year(timestamp) > 2000,
         !is.na(wetland.type)) %>%
  mutate(year = floor_date(timestamp, "year")) %>%
  ggplot(aes(year, length.secs/3600, group = wetland.type, fill = wetland.type))+
  geom_col()+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
  labs(x = "Year", y = "Hours of recordings", title = "Annual distribution of recordings by wetland type")+
  guides(sum = "none")


# Mapping of wetland recordings by year

library(rworldmap)
library(rworldxtra)

# Make a new high resolution map
lakes <- map_data('lakes')
lakes.df <- fortify(lakes)
lakes <- NULL

worldMap <- getMap(resolution = "high")
# Connect up all the points so the polygons are closed
worldMap.df <- fortify(worldMap)
worldMap <- NULL

bounds.view <- list(c(-67.7, -62.25), c(45, 47.5))

recordings.fixed.df %>%
  filter(year(timestamp) > 2000,
         !is.na(lon)) %>%
  mutate(year = floor_date(timestamp, "year") %>% year() %>% as.character()) %>%
  ggplot(aes(lon, lat)) +
  geom_polygon(data = worldMap.df, aes(long, lat,group=group), fill="#AAAAAA", colour="#000000")+
  geom_polygon(data = lakes.df, aes(long, lat,group=group), fill="#d1dbe5", colour="#000000")+
#  geom_point()+
#  geom_jitter(aes(fill = year), size = 3, alpha = 0.5, shape = 21, color = 'black', stroke = 1)+
  stat_summary_2d(aes(z=length.secs/3600), binwidth=0.2, fun="sum")+
  labs(x = 'Longitude', y = 'Latitude', title = "Distribution of SongMeter recordings by year", fill = "Hours")+
  #scale_color_viridis_c()+
  scale_fill_viridis_c()+
#  scale_size_continuous(range = c(4,10))+
  coord_fixed(xlim = bounds.view[[1]], ylim = bounds.view[[2]])+
  theme_minimal()+
  facet_wrap(.~year)
#  guides(size = F)+



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


# Make a plot of this summary
# I had to remove NA route names
recordings.by.point.df %>%
  filter(!is.na(route.name)) %>%
  group_by(year, route.name) %>%
  summarise(n.minutes = sum(n.minutes, na.rm = T),
            n.points = length(unique(pointID))) %>%
  ggplot(aes(year, route.name, size = n.points, color = n.minutes))+
  geom_point()+
#  guides(color = T)+
  scale_color_viridis_c()+
  labs(x = 'Route name', y = 'Year', size = 'Number of\n points', color = 'Number of\n minutes', title = 'Number of recording minutes per wetland route per year')
