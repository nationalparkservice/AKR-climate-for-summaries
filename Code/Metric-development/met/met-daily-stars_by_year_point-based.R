#### This code is for small parks that cannot be run spatially and Daymet / GCM
#### data must be extracted at the park centroid (point-based)
#### Developed by BCR on 5/5/2025

print("extracting Daymet")
DF = data.frame()

grid_filelist = list.files(path = paste(met.dir,"daymet",sep='/'), pattern= '.nc', 
                           full.names = TRUE)

for(i in 1:length(grid_filelist)){
  yr = as.POSIXct(sub('.*\\met_', '', sub("\\..*", "", grid_filelist[i])), format="%Y")
  print(yr)
  
  # Read curvilinear stars object
  invisible(capture.output(
    suppressWarnings(
      (grid_star = read_stars(grid_filelist[i], sub=c("tmax","tmin","pcp"), 
                              curvilinear = c("longitude", "latitude"))))))

  # Extract at point
  grid_crop <- st_extract(grid_star, at = pt)
  grid_crop <- drop_units(grid_crop)
  
  # Convert units
  grid_crop <- grid_crop %>%
    mutate(tmax_f = tmax * 9/5 + 32,
           tmin_f = tmin * 9/5 + 32,
           pcp_in = pcp / 25.4)
  
  # Thresholds
  grid_threshold <- grid_crop %>%
    mutate(freeze.thaw = tmax_f > 34 & tmin_f < 28,
           GDD = (tmax_f + tmin_f)/2,
           under32 = tmin < 0,
           over20 = tmax > 20,
           pcp.over.5 = pcp_in > 0.5)
  
  # Aggregate annual totals (no spatial dimensions)
  df <- data.frame(GCM = "Daymet", year = yr)
  df$freeze.thaw = sum(grid_threshold$freeze.thaw, na.rm=TRUE)
  df$GDD         = sum(grid_threshold$GDD, na.rm=TRUE)
  df$under32     = sum(grid_threshold$under32, na.rm=TRUE)
  df$over20      = sum(grid_threshold$over20, na.rm=TRUE)
  df$pcp.over.5  = sum(grid_threshold$pcp.over.5, na.rm=TRUE)
  
  # Winter-specific aggregates
  by_t = "1 month"
  under32.month <- aggregate(grid_threshold %>% select(under32), by = by_t, FUN = sum)
  
  df$WSF.below32 <- sum(under32.month$under32[9:12], na.rm = TRUE)
  df$W.under32   <- sum(under32.month$under32[12], na.rm = TRUE)
  
  DF <- rbind(DF, df)
  
  rm(grid_star, grid_crop, grid_threshold, under32.month)
  gc()
}

for (G in 1:length(GCMs)) {
  gcm <- sub("\\..*", "", GCMs[G])
  rcp <- sub('.*\\.', '', GCMs[G])
  path <- paste(met.dir, gcm, rcp, sep = '/')
  file.list <- list.files(path = path, pattern = '.nc4', full.names = TRUE)
  fut_filelist <- Filter(function(x) grepl(paste(future.period, collapse = "|"), x), 
                         file.list)
  
  print(paste0("extracting ", GCMs[G]))
  
  # Extract centroid in WGS84 to match curvilinear grid
  pt <- st_centroid(st_transform(park, 4326))
  
  for (i in 1:length(fut_filelist)) {
    yr <- as.POSIXct(sub('.*\\met_', '', sub("\\..*", "", fut_filelist[i])), format="%Y")
    print(yr)
    
    # Read GCM data as stars with curvilinear support
    invisible(capture.output(
      suppressWarnings(
        (fut_star = read_ncdf(fut_filelist[i], var=c("tmax","tmin","pcp"), 
                              curvilinear = c("longitude", "latitude"))))))

    # Extract values from nearest grid cell
    fut_crop <- st_extract(fut_star, at = pt)
    fut_crop <- drop_units(fut_crop)
    
    # Convert units
    fut_crop <- fut_crop %>%
      mutate(tmax_f = tmax * 9/5 + 32,
             tmin_f = tmin * 9/5 + 32,
             pcp_in = pcp / 25.4)
    
    # Add threshold variables
    fut_threshold <- fut_crop %>%
      mutate(freeze.thaw = tmax_f > 34 & tmin_f < 28,
             GDD = (tmax_f + tmin_f)/2,
             under32 = tmin < 0,
             over20 = tmax > 20,
             pcp.over.5 = pcp_in > 0.5)
    
    # Aggregate annual values
    df <- data.frame(GCM = GCMs[G], year = yr)
    df$freeze.thaw = sum(fut_threshold$freeze.thaw, na.rm=TRUE)
    df$GDD         = sum(fut_threshold$GDD, na.rm=TRUE)
    df$under32     = sum(fut_threshold$under32, na.rm=TRUE)
    df$over20      = sum(fut_threshold$over20, na.rm=TRUE)
    df$pcp.over.5  = sum(fut_threshold$pcp.over.5, na.rm=TRUE)
    
    # Winter season
    by_t = "1 month"
    under32.month <- aggregate(fut_threshold %>% select(under32), by = by_t, FUN = sum)
    df$WSF.below32 <- sum(under32.month$under32[9:12], na.rm = TRUE)
    df$W.under32   <- sum(under32.month$under32[12], na.rm = TRUE)
    
    # Add to final data frame
    DF <- rbind(DF, df)
    
    rm(fut_star, fut_crop, fut_threshold, under32.month)
    gc()
  }
}

write.csv(DF,paste0(data.dir,"/Daily_met.csv"),row.names=FALSE)
