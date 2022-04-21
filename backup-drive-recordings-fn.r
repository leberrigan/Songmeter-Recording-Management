backup.recordings <- function(search.files.df, search.drive, backup.drive, backup.drive.id, backup.dir = "songmeter-backup") {

  backup.dir <- paste0(backup.drive, backup.dir)

  search.drive.files <- list.files(search.drive, pattern = '\\.wav$', recursive = T, full.names = T)
  backup.drive.filenames <- gsub('.*\\/', '', list.files(backup.drive, pattern = '\\.wav$', recursive = T, full.names = T))

  backup.files.df <- tibble(drive.id = backup.drive.id,
                            file.location = search.drive.files,
                            filename = gsub('.*\\/', '', search.drive.files)) %>%
    left_join(select(search.files.df, -file.location), by = "filename") %>%
    filter(
      !is.na(drive.id.y),
      !filename %in% backup.drive.filenames &
      !grepl(paste0("^",search.drive,"\\$RECYCLE\\.BIN"), filename)
    )

  if ( nrow(backup.files.df) > 0 ) {

    message("Found ", nrow(backup.files.df), " files on the search drive that need to be backed up!")

    message("There are ", round( sum(backup.files.df$length) / 3600 ), " hours of recordings that will be copied.")
    message("This will take ~",
            round( ((sum(backup.files.df$length) * 44100 * 4) / (4e7)) / 60 )
            ," minutes at 40 MB/s transfer speeds.")

    if(!dir.exists(backup.dir)) {
      message("Creating backup directory")
      dir.create(backup.dir)
    }

    message("Copying files from ", search.drive, " to ", backup.drive)
    # Faster than a direct system call and file.copy
    file_copy(
        backup.files.df$file.location,
        backup.dir,
        overwrite = T
      )

    # Make a new catalogue of recordings on the backup drive
    message("Creating a new catalogue of recordings on the backed up drive")
    get.drive.recording.info(backup.drive.id, backup.drive, catalogue.dir)

  } else {

    stop("No files were found. Are you sure the search drive is correct?\nSearch drive: ", search.drive)

  }

}
