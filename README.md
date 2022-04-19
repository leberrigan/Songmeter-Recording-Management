# Songmeter Audio File Management

## Catalogue recordings on your hard drives
1. Plug in a hard drive
1. Enter the drive letter and ID
1. Run the code

## Summary of catalogue recordings
- Identify which recordings are missing backups
- Estimate the recording schedules based on the device ID and year

## Backup recordings
1. Plug in the hard drive that contains the unique recordings
1. Plug in the hard drive that will hold the backup
1. Enter the drive letter of each both drives and the ID of backup drive
1. Run the code

- It searches for files identified in the summary step.
- It writes a file in the catalogue using the drive ID from the previous step.


## Notes
- There should always be at least 2 copies of each recording on different hard drives.
- If a hard drive dies, remove it from the catalogue folder and re-run the code to summarise the catalogue. This will identify which recordings need to be backed up again.
- There is a regular expression used to identify the relevant recordings as well as to identify the recording unit and the time it was recorded. This can be changed to suit your needs based on the parameters in the scripts - contact me if you want me to do it for you.
- Recordings will be removed if it cannot identify the device ID. However, it will allow recordings with bad timestamps.
