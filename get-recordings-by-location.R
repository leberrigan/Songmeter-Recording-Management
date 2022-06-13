
setwd("E:/MMMP/R/Songmeter Recording Management")

library(tidyverse)
library(lubridate)

data.dir <- "data/"

sm.meta.file <- "songmeter-recording-metadata.csv"

sm.meta.df <- read.csv(paste0(data.dir, sm.meta.file))

phil.files.df <- sm.meta.df %>%
  filter(lon > -65, lat < 46.25) %>%
  filter(year(timestamp) > 2000,
         !is.na(lon)) %>%
  mutate(year = floor_date(as.Date(timestamp), "year") %>% year() %>% as.character())


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

bounds.view <- list(lons = c(-67.7, -62.25), lats = c(45, 47.5))

phil.files.df %>%
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
  coord_fixed(xlim = bounds.view$lons, ylim = bounds.view$lats)+
  theme_minimal()

phil.files.df %>%
  group_by(year) %>%
  summarise(size = sum(file.size)/(1e12))
