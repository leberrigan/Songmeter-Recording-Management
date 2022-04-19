# Backup songmeter recordings
library(tidyverse)
library(stringr)
library(lubridate)

backup.files.df <- read.csv('recordings-requiring-backup.csv')

search.drive <- 'G:/'

backup.drive <- 'F:/'
backup.drive.id <- 17

recording.info.filenames <- list.files(data.folder, pattern="csv$", full.names = T)

recording.info.df <- lapply(recording.info.filenames, read.csv) %>%
  bind_rows