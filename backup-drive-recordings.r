# Backup songmeter recordings
library(tidyverse)
library(stringr)
library(lubridate)
library(fs)
source("get-drive-recordings-fn.r")
source("backup-drive-recordings-fn.r")

# Path of hard drive with unique files on it
search.drive <- 'F:/'

# Table of files on all drives that need to be backed up
search.files.df <- read.csv('recordings-requiring-backup.csv')

# Backup hard drive path
backup.drive <- 'G:/'
backup.drive.id <- 21

# Updates catalogue once complete
catalogue.dir <- 'songmeter_recording_info/'

backup.recordings(search.files.df, search.drive, backup.drive, backup.drive.id)


# For debugging

filename <- "G:/$RECYCLE.BIN/S-1-5-21-1287527300-93463349-1530313631-1001/$RJWQIKW/MMP-POPA16_20160523_170000.wav"

grepl(paste0("^",search.drive,"\\$RECYCLE\\.BIN"), filename)
