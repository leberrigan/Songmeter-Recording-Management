# Summarise drive recordings
library(tidyverse)
library(stringr)
library(lubridate)

data.folder <- 'songmeter_recording_info/'

recording.info.filenames <- list.files(data.folder, pattern="csv$", full.names = T)

recording.info.df <- lapply(recording.info.filenames, read.csv) %>%
  bind_rows() %>%
  mutate(
    length.secs = ifelse(is.na(length.secs), length, length.secs)
  )

# How to recognize the timestamps
timestamp.regex <- "[0-9_]{15}(?=\\.wav)"

# How to recognize the timestamps
device.id.regex <- "(?<=(POPA|SM)-?)[0-9]{2}"

expanded.recording.info <- recording.info.df %>%
  filter(!grepl("://BITH", file.location, fixed=T),
         !is.na(device.id)) %>%
  group_by(filename) %>%
  mutate(n.copies = length(unique(drive.id))) %>%
  ungroup()

# Removed recordings unrelated to MMMP
nrow(recording.info.df) - nrow(expanded.recording.info)

# Number of recordings where the timestamp is wrong
expanded.recording.info %>% filter(year(timestamp) < 2012) %>% nrow

# Number of copies of recordings
expanded.recording.info %>%
  group_by(n.copies) %>%
  summarise(n = n()) %>%
  ggplot(aes("", n, fill = as.character(n.copies)))+
  geom_bar(width = 1, stat='identity')+
  coord_polar("y")

# Plot which drives have unique recordings (so we can back them up)
expanded.recording.info %>%
  filter(year(timestamp) >= 2012) %>%
  ggplot(aes(fct_reorder(drive.id %>% as.character,drive.id), fill = (n.copies == 1)))+
  geom_bar()+
  ggtitle("Number of unbacked up recordings by hard drive")

# Plot which drives have unique recordings (so we can back them up)
expanded.recording.info %>%
  filter(year(timestamp) >= 2012) %>%
  ggplot(aes(fct_reorder(drive.id %>% as.character,drive.id), length.secs / 3600, fill = (n.copies == 1)))+
  geom_bar(stat="sum")+
  ggtitle("Hours of unbacked up recordings by hard drive")+
  labs(y = "Hours", x = "Drive ID")

# Figure out which recordings/hard drives need to be backed up
requiring.backup.df <- expanded.recording.info %>%
  filter(year(timestamp) >= 2012,
         n.copies == 1)

requiring.backup.df %>%
  ggplot(aes(year(timestamp)))+
  geom_bar()+
  ggtitle("Number of unbacked up recordings by year")

requiring.backup.df %>% write.csv("recordings-requiring-backup.csv", row.names = F)

expanded.recording.info %>%
  mutate(valid.ts = year(timestamp) >= 2012,
         n.copies == 1) %>%
  group_by(drive.id) %>%
  summarise() %>%
    write.csv("drive-inventory.csv", row.names = F)

expanded.recording.info %>%
  rename(driveID = drive.id, deviceID = device.id) %>%
  write.csv("songmeter-recording-specs.csv", row.names = F)


# Plot annual recordings
expanded.recording.info %>%
  filter(year(timestamp) >= 2012) %>%
  ggplot(aes(year(timestamp), length.secs/3600, fill = (n.copies == 1)))+
  geom_bar(stat = "sum")
