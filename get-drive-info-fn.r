
get.disk.info <- function() {

  if (Sys.info()['sysname'] == 'Windows') {
    disks <- system("wmic logicaldisk get size,freespace,caption", inter=TRUE)

    disks <- read.fwf(textConnection(disks[1:(length(disks)-1)]),
                      widths=c(9, 14, 14), strip.white=TRUE, stringsAsFactors=FALSE)

    colnames(disks) <- disks[1,]
    disks <- disks[-1,]
    rownames(disks) <- NULL

    return(
      disks %>%
        rename(drive.letter = Caption, free.space = FreeSpace, capacity = Size) %>%
        mutate(
          free.space = as.numeric(free.space),
          capacity = as.numeric(capacity)
        )
      )


  } else if (F) {
    # UNTESTED
    # For Non-Windows

    disk.usage <- function(path = Sys.getenv("HOME")) {
      if(length(system("which df", intern = TRUE, ignore = TRUE))) {
        cmd <- sprintf("df %s", path)
        exec <- system(cmd, intern = TRUE, ignore = TRUE)
        exec <- strsplit(exec[length(exec)], "[ ]+")[[1]]
        exec <- as.numeric(exec[3:4])
        structure(exec, names = c("used", "available"))
      } else {
        stop("'df' command not found")
      }
    }

    du <- disk.usage()
    du[1]/sum(du)
  }
}
