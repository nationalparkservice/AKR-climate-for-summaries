# Updated script: point-based extraction for SWE and runoff

# library(stars)
# library(dplyr)
library(lubridate)

# Define centroid point for extraction
# pt <- st_centroid(buff_shp)

# Variables and output holders
var1 = "SWE"
var2 = "runoff"
DF1 <- data.frame()
DF2 <- data.frame()

print("extracting Daymet")
grid_filelist = list.files(path = paste0(vic.dir,"/daily/daymet"), pattern= '.nc', full.names = TRUE)
wf_grid_filelist <- grid_filelist[grep("wf", grid_filelist)]
ws_grid_filelist <- grid_filelist[grep("ws", grid_filelist)]

# ---- DAYMET extraction ----
for(i in seq_along(wf_grid_filelist)){
  yr = as.POSIXct(sub('.*\\wf_', '', sub("\\..*", "", wf_grid_filelist[i])), format="%Y")
  print(yr)
  
  # Runoff
  grid_star_wf <- read_stars(wf_grid_filelist[i], sub = c("RUNOFF"), curvilinear = c("longitude", "latitude"))
  grid_star_wf <- st_transform(grid_star_wf, st_crs(pt))
  point_wf <- st_extract(grid_star_wf, at = pt) %>% drop_units() %>% mutate(RUNOFF_in = RUNOFF / 25.4)
  df2 <- as.data.frame(point_wf)
  df2$GCM <- "Daymet"
  
  # SWE
  grid_star_ws <- read_stars(ws_grid_filelist[i], sub = c("SWE"), curvilinear = c("longitude", "latitude"))
  grid_star_ws <- st_transform(grid_star_ws, st_crs(pt))
  point_ws <- st_extract(grid_star_ws, at = pt) %>% drop_units() %>% mutate(SWE_in = SWE / 25.4)
  df1 <- as.data.frame(point_ws)
  df1$GCM <- "Daymet"
  
  DF1 <- rbind(DF1, df1)
  DF2 <- rbind(DF2, df2)
}

# ---- FUTURE GCM extraction ----
for (G in seq_along(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  path = file.path(vic.dir, "daily/BCSD", gcm, rcp)
  
  file.list = list.files(path = path, pattern = '.nc', full.names = TRUE)
  fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
  wf_fut_filelist <- fut_filelist[grep("wf", fut_filelist)]
  ws_fut_filelist <- fut_filelist[grep("ws", fut_filelist)]
  
  for(i in seq_along(wf_fut_filelist)){
    yr = as.POSIXct(sub('.*\\wf_', '', sub("\\..*", "", wf_fut_filelist[i])), format="%Y")
    print(yr)
    
    # Runoff
    fut_star_wf <- read_stars(wf_fut_filelist[i], sub = c("RUNOFF"), curvilinear = c("longitude", "latitude"))
    fut_star_wf <- st_transform(fut_star_wf, st_crs(pt))
    point_wf <- st_extract(fut_star_wf, at = pt) %>% drop_units() %>% mutate(RUNOFF_in = RUNOFF / 25.4)
    df2 <- as.data.frame(point_wf)
    df2$GCM <- GCMs[G]
    
    # SWE
    fut_star_ws <- read_stars(ws_fut_filelist[i], sub = c("SWE"), curvilinear = c("longitude", "latitude"))
    fut_star_ws <- st_transform(fut_star_ws, st_crs(pt))
    point_ws <- st_extract(fut_star_ws, at = pt) %>% drop_units() %>% mutate(SWE_in = SWE / 25.4)
    df1 <- as.data.frame(point_ws)
    df1$GCM <- GCMs[G]
    
    DF1 <- rbind(DF1, df1)
    DF2 <- rbind(DF2, df2)
  }
}

# Write final outputs
write.csv(DF1, file.path(data.dir, paste0(var1, "_DAY.csv")), row.names = FALSE)
write.csv(DF2, file.path(data.dir, paste0(var2, "_DAY.csv")), row.names = FALSE)

rm(df1, df2, point_ws, point_wf); gc()
