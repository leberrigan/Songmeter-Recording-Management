library(tidyverse)
library(stringr)

getwd()

selections.2013.files <- list.files("../../Songmeters/Transcriptions/2013", pattern = ".*csv$", full.names = T)
selections.2013 <- lapply(selections.2013.files, read.csv) %>%
  bind_rows()

paths <- selections.2013$Begin.Path %>% unique #%>% str_split("\\\\") 

tibble(fullpath = paths, fullpath2 = paths) %>%
  mutate(fullpath = gsub("(\\\\Songmeter\\\\|\\\\Songmeter_Recordings\\\\)", "\\\\", fullpath)) %>%
  separate(fullpath, c("drive.letter", "year", "location", "dates", "filename"), "\\\\") %>%
  separate(location, c("deviceID", "route.name", "pointID"), "_") %>% View
  separate(dates, c("dt.start", "dt.end"), "-") %>%
  mutate(deviceID = gsub("-[0-9]{2}$","", gsub("POPA","", deviceID)),
         dt.start = as.Date(dt.start, origin = "1970-01-01", format = "%Y%m%d"),
         dt.end = as.Date(dt.end, origin = "1970-01-01", format = "%Y%m%d"),
         routeID = gsub("-[0-9A-z]{2}$","", pointID),
         lat = NA,
         lon = NA,
         wetland.type = "") %>%
  View()
  select(deviceID, routeID, route.name, pointID, wetland.type, lat, lon, dt.start, dt.end) %>%
  distinct() %>%
  filter(!is.na(deviceID)) %>%
  write.csv("2013_songmeter_deployments_LEB_constructed.csv", row.names = F)


