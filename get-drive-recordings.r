library(tidyverse)
library(tuneR)
source("get-drive-recordings-fn.r")


output.folder <- 'songmeter_recording_info/'

get.drive.recording.info('21', 'F:/', output.folder)
get.drive.recording.info('8', 'G:/', output.folder)






###############
# For debugging
#














test <- list(
  list(a="1",b="2",c="3"),
  list(a="4",b="5",c="6"),
  list(a="7",b="8",c="9")
) %>% bind_rows()


cumsum(test$c)


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
