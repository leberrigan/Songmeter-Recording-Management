
source("get-drive-info-fn.r")

# How to recognize the timestamps
timestamp.regex <- "[0-9_]{15}(?=\\.wav)"

# How to recognize the timestamps
device.id.regex <- "(?<=(POPA|SM)-?)[0-9]{2}"

# How to recognize the timestamps
remove.match.regex <- "://BITH"

# Where hard drive metadata will be stored
drive.meta.filename <- "backup-drive-metadata.csv"

# For file headers that cannot be read (such as with very long recordings), what should the default sample rate be?
default.sample.rate = 44100 # Hz

get.drive.recording.info <- function(drive.id, drive.dir, output.folder) {
  require(tidyverse)
  require(tuneR)
  message("Getting a list of all the WAV files on the drive.")
  # Get a list of .wav files off the drive
  files <- paste(drive.dir, list.files(path = drive.dir, pattern = '\\.wav$', recursive = T, include.dirs = F), sep = '/')

  # Function for gathering size of audio files
  waveSize <- function(filename) {

  # For debugging
  #  message("Getting specs for: ", gsub('.*\\/', '', filename))

    if (file.exists(filename) & file.size(filename) > 1e3) {
      recording.specs <- tryCatch( {
          readWave(filename, header = T)
        }, error=function(cond){
          message("Error: ", cond)
          message("At file: ", filename)
          return(NA)
        }
      )

      if ( !"samples" %in% names(recording.specs) ) {
        warning("WARNING: File header is unrecognized, possibly because it is a very large file: ", filename)
        message("We'll assume the header is too large to read and estimate the length based on a default sample rate of ", default.sample.rate, " Hz")
        estimated.samples <- (file.size(filename)/4) - 8192
        recording.specs <-list(
          samples = estimated.samples,
          sample.rate = default.sample.rate
        )
      }
      recording.specs$length.secs <- round(recording.specs$samples / recording.specs$sample.rate, 2)
      recording.specs$file.size <- file.size(filename)

    } else if (file.exists(filename)) {
      warning("WARNING: File is empty: ", filename)
      estimated.samples <- (file.size(filename)/4) - 8192
      recording.specs <- list(
        samples = estimated.samples,
        sample.rate = default.sample.rate,
        length.secs = 0,
        file.size = 0
      )
    } else {
      warning("WARNING: File does not exist: ", filename)
      recording.specs <- list(
        samples = NA,
        sample.rate = NA,
        length.secs = NA,
        file.size = NA
      )
    }

    recording.specs$file.location <- filename
    recording.specs$filename <- gsub('.*\\/', '', filename)

    # Returns a named list which will become a row after we use "bind_rows"
    return( recording.specs )
  }

  message("Gathering recordings specs for ", length(files), " WAV files on drive #", drive.id)

  # Get the sizes of each file, among other things
  recording.specs <- lapply(files, waveSize) %>%
  # Bind the list of named lists into a data frame
    bind_rows() %>%
    mutate(
      drive.id = drive.id,
    # Get the columns derived from the file name
      device.id = str_match(filename, device.id.regex)[,1],
      timestamp = str_match(filename, timestamp.regex)[,1] %>%
        as.POSIXct(origin="1970-01-01",format="%Y%m%d_%H%M%S")
    ) %>%
    filter(
    # Remove files which have match this pattern. Leave remove.match.regex blank if you don't want to filter anything out based on the name.
    # This is only necessary if there are .wav files on the drive that shouldn't be catalogued
      !grepl(remove.match.regex, file.location, fixed=T)|length(remove.match.regex)==0,
    # Some file names are corrupt and do not contain the device ID.
    # However, if your regex is incorrect it may also filter out valid recordings in this step.
      !is.na(device.id)
    )

  message("Finished gathering recordings.")

  recording.specs %>% write.csv( paste0(output.folder, "Songmeter_info_drive_", drive.id, ".csv"), row.names = F )

  drive.meta <- get.disk.info() %>%
    filter( drive.letter == substr(drive.dir, 1, 2) ) %>%
    mutate(
      id = as.character(drive.id),
      recordings = nrow(recording.specs),
      recording.hours = sum(recording.specs$length.secs, na.rm = T)/3600,
      recording.gbs = sum(recording.specs$file.size, na.rm = T)/(1e9),
      recordings.empty = length(which(recording.specs$file.size == 0)),
      recordings.missing = length(which(is.na(recording.specs$file.size)))
     )

  if (file.exists(drive.meta.filename)) {
    drive.meta <- read.csv(drive.meta.filename) %>%
      mutate(id = as.character(id)) %>%
      filter(id != drive.id) %>%
      bind_rows(drive.meta)
  }

  drive.meta %>% write.csv(drive.meta.filename, row.names = F)

}
