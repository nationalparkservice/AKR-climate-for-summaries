# Annual PrecipIn ----
var = "Annual.precipIn"
DF <- data.frame()

cropped_st_grid <- readRDS(file.path(data.dir, "cropped_st_Daymet"))

# Process each stars object: extract pcp
grid_var <- lapply(cropped_st_grid, function(s) {
  dplyr::select(s, pcp)
})

grid_var_stars <- Reduce(c, grid_var)

# Convert to inches (safe for units or non-units)
grid_var_stars <- grid_var_stars %>%
  mutate(pcp_in = if ("units" %in% class(pcp)) drop_units(pcp) / 25.4 else pcp / 25.4) %>%
  dplyr::select(pcp_in)

# Aggregate to annual sum (converted to monthly total with *30)
by_t = "1 year"
grid <- aggregate(grid_var_stars, by = by_t, FUN = function(x) sum(x) * 30)

# Extract values directly
time_vals <- st_get_dimension_values(grid, "time")
df <- data.frame(
  Year = format(as.Date(time_vals), "%Y"),
  mean = as.numeric(grid$pcp_in),
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
    dplyr::select(s, pcp)
  })
  
  hist_var_stars <- Reduce(c, hist_var) %>%
    mutate(pcp_in = if ("units" %in% class(pcp)) drop_units(pcp) / 25.4 else pcp / 25.4) %>%
    dplyr::select(pcp_in)
  
  hist <- aggregate(hist_var_stars, by = "1 year", FUN = function(x) sum(x) * 30)
  
  # Future block
  fut_var <- lapply(cropped_st_fut, function(s) {
    dplyr::select(s, pcp)
  })
  
  fut_var_stars <- Reduce(c, fut_var) %>%
    mutate(pcp_in = if ("units" %in% class(pcp)) drop_units(pcp) / 25.4 else pcp / 25.4) %>%
    dplyr::select(pcp_in)
  
  fut <- aggregate(fut_var_stars, by = "1 year", FUN = function(x) sum(x) * 30)
  
  fut_time_vals <- st_get_dimension_values(fut, "time")
  df <- data.frame(
    Year = format(as.Date(fut_time_vals), "%Y"),
    mean = as.numeric(fut$pcp_in),
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
