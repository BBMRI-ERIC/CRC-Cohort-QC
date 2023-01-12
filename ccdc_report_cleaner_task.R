# Define till which date files should be deleted
print("start cleaner")
oldDate <- format(Sys.Date()-8, "%Y%m%d")

dirs <- list.dirs()
dirs <- dirs[ grepl("_report", dirs) ]
dirs <- dirs[ grepl(oldDate, dirs) ]
unlink(dirs, recursive = TRUE)
print ("files deleted")