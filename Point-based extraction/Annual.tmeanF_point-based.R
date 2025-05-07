# Annual Tmean ----
var = "Annual.tmeanF"
DF <- data.frame()

cropped_st_grid <- readRDS(file.path(data.dir, "cropped_st_Daymet"))

# Process each stars object: compute tmean = (tmax + tmin)/2
grid_var <- lapply(cropped_st_grid, function(s) {
  s %>% mutate(tmean = (tmax + tmin) / 2) %>% dplyr::select(tmean)
})

grid_var_stars <- Reduce(c, grid_var)

# Convert to Fahrenheit (safe for units or non-units)
grid_var_stars <- grid_var_stars %>%
  mutate(tmean_f = if ("units" %in% class(tmean)) drop_units(tmean) * 9/5 + 32 else tmean * 9/5 + 32) %>%
  dplyr::select(tmean_f)

# Aggregate to annual mean
by_t = "1 year"
grid <- aggregate(grid_var_stars, by = by_t, FUN = mean, na.rm = TRUE)

# Extract values directly
time_vals <- st_get_dimension_values(grid, "time")
df <- data.frame(
  Year = format(as.Date(time_vals), "%Y"),
  mean = as.numeric(grid$tmean_f),
  GCM = "Daymet"
)
names(df)[2] <- var
DF <- rbind(DF, df)

# Mean grid for map (optional)
mean_grid <- st_apply(grid, "geometry", mean)

# Loop through GCMs
for (G in seq_along(GCMs)) {
  gcm <- sub("\\..*", "", GCMs[G])
  rcp <- sub(".*\\.", "", GCMs[G])
  
  cropped_st_hist <- readRDS(file.path(data.dir, paste0("cropped_st_hist_", gcm, "_", rcp)))
  cropped_st_fut  <- readRDS(file.path(data.dir, paste0("cropped_st_fut_", gcm, "_", rcp)))
  
  # Historical block
  hist_var <- lapply(cropped_st_hist, function(s) {
    s %>% mutate(tmean = (tmax + tmin) / 2) %>% dplyr::select(tmean)
  })
  
  hist_var_stars <- Reduce(c, hist_var) %>%
    mutate(tmean_f = if ("units" %in% class(tmean)) drop_units(tmean) * 9/5 + 32 else tmean * 9/5 + 32) %>%
    dplyr::select(tmean_f)
  
  hist <- aggregate(hist_var_stars, by = "1 year", FUN = mean, na.rm = TRUE)
  
  # Future block
  fut_var <- lapply(cropped_st_fut, function(s) {
    s %>% mutate(tmean = (tmax + tmin) / 2) %>% dplyr::select(tmean)
  })
  
  fut_var_stars <- Reduce(c, fut_var) %>%
    mutate(tmean_f = if ("units" %in% class(tmean)) drop_units(tmean) * 9/5 + 32 else tmean * 9/5 + 32) %>%
    dplyr::select(tmean_f)
  
  fut <- aggregate(fut_var_stars, by = "1 year", FUN = mean, na.rm = TRUE)
  
  fut_time_vals <- st_get_dimension_values(fut, "time")
  df <- data.frame(
    Year = format(as.Date(fut_time_vals), "%Y"),
    mean = as.numeric(fut$tmean_f),
    GCM = GCMs[G]
  )
  names(df)[2] <- var
  DF <- rbind(DF, df)
  
  mean_hist <- st_apply(hist, "geometry", mean)
  mean_fut  <- st_apply(fut,  "geometry", mean)
  delta     <- mean_fut - mean_hist
  
  saveRDS(delta, file = file.path(data.dir, paste(var, gcm, rcp, sep = "_")))
}

write.csv(DF, file.path(data.dir, paste0(var, "_ANN.csv")), row.names = FALSE)

rm(grid_var, grid_var_stars, grid, mean_grid, hist, hist_var, hist_var_stars, mean_hist, fut, fut_var, fut_var_stars, mean_fut)
gc()
