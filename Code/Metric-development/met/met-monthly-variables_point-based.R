#### This code is for small parks that cannot be run spatially and Daymet / GCM
#### data must be extracted at the park centroid (point-based)
#### Developed by BCR on 5/5/2025

library(lubridate)

monthly.variables <- c("DJF.tmeanF", "MAM.tmeanF", "JJA.tmeanF", "SON.tmeanF",
                       "DJF.tmaxF", "MAM.tmaxF", "JJA.tmaxF", "SON.tmaxF",
                       "DJF.tminF", "MAM.tminF", "JJA.tminF", "SON.tminF",
                       "DJF.precipIn", "MAM.precipIn", "JJA.precipIn", "SON.precipIn")

CF_GCM <- data.frame(CF = c("Climate Future 1", "Climate Future 2"),
                     GCM = c("CCSM4.rcp85", "MRI-CGCM3.rcp85"))

DF.hist <- CF_GCM[,1:2]; DF.hist$Period <- "Historical"; DF.hist[,monthly.variables] <- NA
DF.fut  <- CF_GCM[,1:2]; DF.fut$Period  <- "Future";     DF.fut[,monthly.variables]  <- NA
DF.grid <- data.frame(CF = "Historical", GCM = "Daymet", Period = "Historical")
DF.grid[,monthly.variables] <- NA

# Function to calculate seasonal means from stars object
extract_seasonal_means <- function(star_obj, varname) {
  time_vals <- st_get_dimension_values(star_obj, "time")
  df <- as.data.frame(star_obj)
  df$month <- month(rep(time_vals, each = nrow(df) / length(time_vals)))
  list(
    DJF = mean(df[[varname]][df$month %in% c(12, 1, 2)], na.rm = TRUE),
    MAM = mean(df[[varname]][df$month %in% 3:5], na.rm = TRUE),
    JJA = mean(df[[varname]][df$month %in% 6:8], na.rm = TRUE),
    SON = mean(df[[varname]][df$month %in% 9:11], na.rm = TRUE)
  )
}

# ---- DAYMET ----
print("Extracting Daymet monthly from centroid")
daymet_files <- list.files(path = file.path(met.dir, "monthly/daymet"), pattern = '.nc$', full.names = TRUE)

daymet_results <- lapply(daymet_files, function(f) {
  s <- read_stars(f, curvilinear = c("longitude", "latitude")) %>%
    st_extract(at = pt) %>%
    drop_units()
  
  s <- s %>% mutate(
    tmean   = (tmax + tmin) / 2,
    tmaxF   = tmax * 9/5 + 32,
    tminF   = tmin * 9/5 + 32,
    tmeanF  = tmean * 9/5 + 32,
    precipIn = pcp / 25.4
  )
  
  list(
    tmeanF   = extract_seasonal_means(s, "tmeanF"),
    tmaxF    = extract_seasonal_means(s, "tmaxF"),
    tminF    = extract_seasonal_means(s, "tminF"),
    precipIn = extract_seasonal_means(s, "precipIn")
  )
})

# Extract and save
daymet_last <- daymet_results[[length(daymet_results)]]
for (var in names(daymet_last)) {
  for (season in names(daymet_last[[var]])) {
    val <- daymet_last[[var]][[season]]
    DF.grid[1, paste0(season, ".", var)] <- val
    # saveRDS(val, file = paste0(data.dir, "/", season, ".", var, "_Daymet"))
  }
}

# ---- GCMs ----
for (G in seq_along(GCMs)) {
  gcm <- sub("\\..*", "", GCMs[G])
  rcp <- sub(".*\\.", "", GCMs[G])
  path <- file.path(met.dir, "monthly/BCSD", gcm, rcp)
  file.list <- list.files(path = path, pattern = '.nc$', full.names = TRUE)
  
  hist_files <- file.list[grepl(paste(historical.period, collapse = "|"), file.list)]
  fut_files  <- file.list[grepl(paste(future.period, collapse = "|"), file.list)]
  
  for (period_name in c("Historical", "Future")) {
    file_vec <- if (period_name == "Historical") hist_files else fut_files
    
    for (f in file_vec) {
      s <- read_ncdf(f, curvilinear = c("longitude", "latitude")) %>%
        st_extract(at = pt) %>%
        drop_units()
      
      s <- s %>% mutate(
        tmean = (tmax + tmin)/2,
        tmaxF = tmax * 9/5 + 32,
        tminF = tmin * 9/5 + 32,
        tmeanF = tmean * 9/5 + 32,
        precipIn = pcp / 25.4
      )
      
      for (var in c("tmeanF", "tmaxF", "tminF", "precipIn")) {
        seasonal_vals <- extract_seasonal_means(s, var)
        for (season in names(seasonal_vals)) {
          val <- seasonal_vals[[season]]
          var_name <- paste0(season, ".", var)
          if (period_name == "Historical") {
            DF.hist[G, var_name] <- val
            saveRDS(val, file = paste0(data.dir, "/", var_name, "_", gcm, "_", rcp))
          } else {
            DF.fut[G, var_name] <- val
            saveRDS(val, file = paste0(data.dir, "/", var_name, "_", gcm, "_", rcp))
          }
        }
      }
    }
  }
}

# Combine and save
DF <- bind_rows(DF.grid, DF.hist, DF.fut)

write.csv(DF, paste0(data.dir, "/Monthly_met.csv"), row.names = FALSE)

rm(monthly.variables, DF.hist, DF.fut, DF.grid, daymet_files, daymet_results,
   hist_files, fut_files, val, seasonal_vals, var_name)

gc()
