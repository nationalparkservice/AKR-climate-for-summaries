#### This code is for small parks that cannot be run spatially and Daymet / GCM
#### data must be extracted at the park centroid (point-based)
#### Developed by BCR on 5/5/2025

## MET MONTHLY

# Loop over GCMs

for (G in seq_along(GCMs)) {
  gcm <- sub("\\..*", "", GCMs[G])
  rcp <- sub('.*\\.', '', GCMs[G])
  path <- file.path(met.dir, "monthly/BCSD", gcm, rcp)
  file.list <- list.files(path = path, pattern = '.nc$', full.names = TRUE)
  
  hist_filelist <- file.list[grepl(paste(historical.period, collapse = "|"), file.list)]
  fut_filelist  <- file.list[grepl(paste(future.period, collapse = "|"), file.list)]
  
  print(paste0("Extracting monthly GCM for ", GCMs[G]))
  
  # Historical
  hist_list <- lapply(hist_filelist, function(f) {
    s <- read_ncdf(f, curvilinear = c("longitude", "latitude"))
    st_extract(s, at = pt)
  })
  saveRDS(hist_list, file = paste(data.dir,paste0("cropped_st_hist_",gcm,"_",rcp),sep="/")) 
  # saveRDS(hist_list, file = file.path(data.dir, paste0("pt_extract_hist_", gcm, "_", rcp, ".rds")))
  
  # Future
  fut_list <- lapply(fut_filelist, function(f) {
    s <- read_ncdf(f, curvilinear = c("longitude", "latitude"))
    st_extract(s, at = pt)
  })
  saveRDS(fut_list, file = paste(data.dir,paste0("cropped_st_fut_",gcm,"_",rcp),sep="/")) 
  # saveRDS(fut_list, file = file.path(data.dir, paste0("pt_extract_fut_", gcm, "_", rcp, ".rds")))
}

# Loop over Daymet

print("Extracting monthly Daymet")

daymet_files <- list.files(path = file.path(met.dir, "monthly/daymet"), pattern = '.nc$', full.names = TRUE)

daymet_list <- lapply(daymet_files, function(f) {
  s <- read_stars(f, curvilinear = c("longitude", "latitude"))
  st_extract(s, at = pt)
})

saveRDS(daymet_list, file = paste(data.dir,"cropped_st_Daymet",sep="/"))
# saveRDS(daymet_list, file = file.path(data.dir, "pt_extract_Daymet_monthly.rds"))


rm(hist_list,fut_list,daymet_files,daymet_list)
