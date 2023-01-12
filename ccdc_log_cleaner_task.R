# Define till which date files should be deleted
print(paste0(Sys.time(),"start log cleaner"))

loggingFolder <- list.dirs("/var/log/ccdc_reporting/current")
backupFolder <- "/var/log/ccdc_reporting/backup"
print(paste0("logginFolder: ", loggingFolder))
print(paste0("backupFolder: ", backupFolder))
print(paste0(Sys.time(),"delete old log files"))
unlink(backupFolder, recursive = TRUE)

print(paste0("create new backupFolder: ", backupFolder))
dir.create(backupFolder)

curlist.of.files <- list.files(loggingFolder, ".log", recursive = FALSE)
print(paste0("logFiles:",curlist.of.files))


print(paste0(Sys.time(),"backup current log files"))
print(paste(loggingFolder, curlist.of.files, sep = "/"))
file.copy(paste(loggingFolder, curlist.of.files, sep = "/"), backupFolder)

print(paste0("delete log files", paste0(loggingFolder, "/", curlist.of.files) ))
file.remove(paste0(loggingFolder, "/", curlist.of.files), recursive = FALSE)