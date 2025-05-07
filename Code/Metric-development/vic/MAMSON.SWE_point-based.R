#### This code is for small parks that cannot be run spatially and Daymet / GCM
#### data must be extracted at the park centroid (point-based)
#### Developed by BCR on 5/5/2025

library(lubridate)

var <- "MAMSON.SWE"
DF <- data.frame()

# Daymet ----
cropped_st_grid <- readRDS(paste(data.dir, "cropped_st_Daymet_ws", sep = "/"))

all_swe <- lapply(cropped_st_grid, function(s) {
  s <- dplyr::select(s, SWE)
  s <- st_transform(s, st_crs(pt))
  s <- st_extract(s, at = pt) %>% drop_units()
  if ("SWE" %in% names(s)) {
    s$SWEf <- s$SWE / 25.4
    return(s)
  } else {
    return(NULL)
  }
})
all_swe <- Filter(Negate(is.null), all_swe)

swe_vals <- do.call(c, all_swe)
by_t <- "1 year"
swe_agg <- aggregate(swe_vals, by = by_t, FUN = mean)
time_vals <- st_get_dimension_values(swe_agg, "time")

df <- data.frame(Year = time_vals, MAMSON.SWE = as.vector(swe_agg$SWEf), GCM = "Daymet")
df$Year <- substr(df$Year, 1, 4)
DF <- rbind(DF, df)

# GCMs ----
for (G in seq_along(GCMs)) {
  gcm <- sub("\\..*", "", GCMs[G])
  rcp <- sub(".*\\.", "", GCMs[G])
  cropped_st_fut <- readRDS(paste(data.dir, paste0("cropped_st_fut_ws_", gcm, "_", rcp), sep = "/"))
  
  all_fut_swe <- lapply(cropped_st_fut, function(s) {
    s <- dplyr::select(s, SWE)
    s <- st_transform(s, st_crs(pt))
    s <- st_extract(s, at = pt) %>% drop_units()
    if ("SWE" %in% names(s)) {
      s$SWEf <- s$SWE / 25.4
      return(s)
    } else {
      return(NULL)
    }
  })
  all_fut_swe <- Filter(Negate(is.null), all_fut_swe)
  
  swe_vals <- do.call(c, all_fut_swe)
  fut_agg <- aggregate(swe_vals, by = by_t, FUN = mean)
  time_vals <- st_get_dimension_values(fut_agg, "time")
  
  df <- data.frame(Year = time_vals, MAMSON.SWE = as.vector(fut_agg$SWEf), GCM = GCMs[G])
  df$Year <- substr(df$Year, 1, 4)
  DF <- rbind(DF, df)
  
  # Save mean delta
  mean_grid <- mean(unlist(lapply(all_swe, function(x) mean(x$SWEf, na.rm = TRUE))))
  mean_fut <- mean(fut_agg$SWEf, na.rm = TRUE)
  delta <- mean_fut - mean_grid
  saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
}

write.csv(DF, paste0(data.dir, "/", var, "_ANN.csv"), row.names = FALSE)

rm(all_swe, swe_vals, by_t, swe_agg, time_vals, all_fut_swe, fut_agg, mean_grid, 
   mean_fut)
