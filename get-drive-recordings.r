library(tidyverse)
library(tuneR)

drive.id <- '3'

drive.dir <- 'F:/'

output.filename <- 'songmeter_recording_lengths.csv'
output.folder <- 'songmeter_recording_info/'

get.drive.recording.info('17', 'F:/', output.folder)



get.drive.recording.info <- function(drive.id, drive.dir, output.folder, default.sample.rate = 44100) {

  files <- paste(drive.dir, list.files(path = drive.dir, pattern = '\\.wav$', recursive = T, include.dirs = F), sep = '/')
  
  files
  
  waveSize <- function(filename) {
    if (file.exists(filename) & file.size(filename) > 1e3) {
      recording <- tryCatch({
        readWave(filename, header = T)
      }, error=function(cond){
        message("Error: ", cond)
        message("At file: ", filename)
        return(NA)
      })
      if (is.na(recording)) {
        message("We'll assume the file is too large to open and estimate the length")
        estimated_samples <- (file.size(filename)/4) - 8192
        recording <-list(samples = estimated_samples, sample.rate = default.sample.rate)
      }
      length.secs <- round(recording$samples / recording$sample.rate, 2)
    } else if (file.exists(filename)) {
      warning("WARNING: File is empty: ", filename)
      length.secs <- 0
    } else {
      warning("WARNING: File does not exist: ", filename)
      length.secs <- NA
    }
    return(length.secs)
  }
  
  message("Gathering recordings for drive #", drive.id)
  new_recording_lengths <- sapply(files, waveSize)
  message("Finished gathering recordings.")  
  
  joined_recording_info<- tibble(drive.id = drive.id, file.location = files, filename = gsub('.*\\/', '', files), length = new_recording_lengths, type = 'other')
  
  joined_recording_info %>% write.csv( paste0(output.folder, "Songmeter_info_drive_", drive.id, ".csv"), row.names = F )
  
}

# for debugging
error.file <- "F://MMMP 2017/Songmeters/SE NB Area/POPA25 2017_07_05 to 2017_07_10 - BOUCTOUCHE V2/MMP-POPA25_20170707_020000.wav"
error.file <- files[8773]
which(files == error.file)
recording <- tryCatch({
  readWave(error.file, header = T)
}, error=function(cond){
  message("Error: ", cond)
  message("At file: ", error.file)
  message("We'll assume the file is too large to open, since")
})
estimated_samples <- (file.size(error.file)/4) - 8192

estimated.length.secs <- round(estimated_samples / default.sample.rate, 2)

length.secs
length.secs <- round(recording$samples / recording$sample.rate, 2)

estimated.length.secs

#lebi_lengths2 <- tibble(file.location = files, filename = gsub('.*\\/', '', files), length = recording_lengths)
#lebi_lengths <- tibble(file.location = files, filename = gsub('.*\\/', '', files), length = recording_lengths)


songmeter_recording_lengths <- rbind(cowe_lengths, cowe_lengths2) %>%
  mutate(type = 'coastal') %>% 
  rbind(mutate(rbind(lebi_lengths, lebi_lengths2), type = 'other'))

rec_lengths %>%
  group_by(filename) %>%
  summarise(length = max(length), type = type[1]) %>%  
  group_by(type) %>%
  summarise(seconds = sum(length), minutes = seconds/60, hours = minutes/60)

length(unique(songmeter_recording_lengths$filename))
