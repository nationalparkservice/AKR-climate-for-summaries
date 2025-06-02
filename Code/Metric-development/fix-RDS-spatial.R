# Fixes deltas.RDS files so they compare future to Daymet instead of to historical modeled
library(stars);library(dplyr);library(ggplot2);library(ggthemes); library(readxl); library(viridis);library(here);
library(ggrepel);library(rlang);library(units)

rm(list=ls())
root.dir <- "C:/Users/arunyon/OneDrive - DOI/AKR-CFs/"

projections <- read_excel(paste0("C:/Users/arunyon/OneDrive - DOI/AKR-CFs/", "AKCFs-Projections-used.xlsx"))

historical.period <- as.character(seq(1950,1999,1))
future.period <- as.character(seq(2035,2065,1))
daymet.period <- as.character(seq(1980,2016,1))

CFs <- c("Climate Future 1", "Climate Future 2")
cols <- c("#6EB2D4","#CA0020")

###
##
# This is where loop gets starts for parks
SiteID = "ANIA" #Update or loop
park <- SiteID
data.dir <- paste0(root.dir,SiteID,"/Data/") 

# Set projections
GCMs <- unlist(projections[which(projections$SiteID == park),][2:3])
CF_GCM <- data.frame(CF=CFs,GCM=GCMs,CF_col=cols)


#### Tmean
var = "Annual.tmeanF"

## Read in and create mean obj for Daymet
cropped_st_grid <- readRDS(paste(data.dir,"cropped_st_Daymet",sep="/"))
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s %>% mutate(tmean = (tmax + tmin)/2) -> s 
  s = dplyr::select(s, tmean)
  grid_var[[F]] = s[,,,] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmean <- drop_units(grid_var_stars$tmean)
grid_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% dplyr::select(tmean_f) -> grid_var_stars

by_t = "1 year"
grid <- aggregate(grid_var_stars, by = by_t, FUN = mean, na.omit = TRUE) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
grid <- grid[,1:length(daymet.period),,]

mean_grid <- st_apply(grid, c("x", "y"), mean)
saveRDS(mean_grid, file = paste(data.dir,paste(var,"Daymet",sep="_"),sep="/"))


for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  
  cropped_st_fut <- readRDS(paste(data.dir,paste0("cropped_st_fut_",gcm,"_",rcp),sep="/"))
  
  fut_var <- list()
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s %>% mutate(tmean = (tmax + tmin)/2) -> s 
    s = dplyr::select(s, tmean)
    fut_var[[F]] = s[,,,] #all months
  }
  
  fut_var_stars <- Reduce(c, fut_var) 
  fut_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% dplyr::select(tmean_f) -> fut_var_stars

  fut <- aggregate(fut_var_stars, by = by_t, FUN = mean, na.omit = TRUE) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
  fut <- fut[,1:length(future.period),,]

  mean_fut <- st_apply(fut, c("x", "y"), mean)
  mean_fut2 <- mean_fut %>% mutate(grid = as.vector(mean_grid$mean)) %>% dplyr::select(grid) #This is some fuckery that has to be done bececause Daymet grid is flipped somehow, puts it on same grid
  flip <- st_flip(mean_fut2, "y") #flip Daymet
  
  delta <- mean_fut - flip
  saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
}

#
#
#

### Annual PrecipIn ----
var = "Annual.precipIn"

cropped_st_grid <- readRDS(paste(data.dir,"cropped_st_Daymet",sep="/"))
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = dplyr::select(s, pcp)
  grid_var[[F]] = s[,,,] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$pcp <- drop_units(grid_var_stars$pcp)
grid_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% dplyr::select(pcp_in) -> grid_var_stars

# st_get_dimension_values(grid_var_stars,"time") #how get time dimension values
by_t = "1 year"
grid <- aggregate(grid_var_stars, by = by_t, FUN = function(x) sum(x) *30) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
grid <- grid[,1:length(daymet.period),,]

mean_grid <- st_apply(grid, c("x", "y"), mean)
saveRDS(mean_grid, file = paste(data.dir,paste(var,"Daymet",sep="_"),sep="/"))


for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
 
  cropped_st_fut <- readRDS(paste(data.dir,paste0("cropped_st_fut_",gcm,"_",rcp),sep="/"))
  
  fut_var <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s = dplyr::select(s, pcp)
    fut_var[[F]] = s[,,,] #all months
  }
  
 
  fut_var_stars <- Reduce(c, fut_var)
  fut_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% dplyr::select(pcp_in) -> fut_var_stars
  
  fut <- aggregate(fut_var_stars, by = by_t, FUN = function(x) sum(x) *30) # *30 bc mean daily, want mean monthly
  fut <- fut[,1:length(future.period),,]

  mean_fut <- st_apply(fut, c("x", "y"), mean)
  mean_fut2 <- mean_fut %>% mutate(grid = as.vector(mean_grid$mean)) %>% dplyr::select(grid) #This is some fuckery that has to be done bececause Daymet grid is flipped somehow, puts it on same grid
  flip <- st_flip(mean_fut2, "y") #flip Daymet
  
  delta <- mean_fut - flip
  saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
}

#
#
#

# Annual maxSWE ----
var = "max.SWE"

cropped_st_grid <- readRDS(paste(data.dir,"cropped_st_Daymet_ws",sep="/"))
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s = dplyr::select(s, SWE)
  if (is.na(summary(s$SWE)[4])) {
    grid_var[[F]] = grid_var[[F-1]]
    st_dimensions(grid_var[[F]])[3] = st_dimensions(s)[3]
  } else{
    grid_var[[F]] = s[,,,] #all months
  }
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$SWE <- drop_units(grid_var_stars$SWE)
grid_var_stars %>% mutate(SWEf = SWE / 25.4) %>% dplyr::select(SWEf) -> grid_var_stars

# st_get_dimension_values(grid_var_stars,"time") #how get time dimension values
by_t = "1 year"
grid <- aggregate(grid_var_stars, by = by_t, FUN = function(x) max(x)) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.

mean_grid <- st_apply(grid, c("x", "y"), mean)
saveRDS(mean_grid, file = paste(data.dir,paste(var,"Daymet",sep="_"),sep="/"))

for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])

  cropped_st_fut <- readRDS(paste(data.dir,paste0("cropped_st_fut_ws_",gcm,"_",rcp),sep="/"))
  
  fut_var <- list()
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s = dplyr::select(s, SWE)
    if (is.na(summary(s$SWE)[4])) {
      fut_var[[F]] = fut_var[[F-1]]
      st_dimensions(fut_var[[F]])[3] = st_dimensions(s)[3]
    } else{
      fut_var[[F]] = s[,,,] #all months
    }
  }
  
  fut_var_stars <- Reduce(c, fut_var)
  fut_var_stars$SWE <- drop_units(fut_var_stars$SWE)
  fut_var_stars %>% mutate(SWEf = SWE / 25.4) %>% dplyr::select(SWEf) -> fut_var_stars
  
  fut <- aggregate(fut_var_stars, by = by_t, FUN = function(x) max(x)) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
  fut <- fut[,1:length(future.period),,]
 
  mean_fut <- st_apply(fut, c("x", "y"), mean)
  
  delta <- mean_fut - mean_grid
  saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
}

#
#
#

# Annual soil.temp ----
var = "soil.temp"

cropped_st_grid <- readRDS(paste(data.dir,"cropped_st_Daymet_eb",sep="/"))

grid_var <- list()
for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s %>% mutate(soil.temp = (SOIL_TEMP1+SOIL_TEMP2)/2) -> s 
  s = dplyr::select(s, soil.temp)
  if (is.na(summary(s$soil.temp)[4])) {
    grid_var[[F]] = grid_var[[F-1]]
    st_dimensions(grid_var[[F]])[3] = st_dimensions(s)[3]
  } else{
    grid_var[[F]] = s[,,,] #all months
  }
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars %>% mutate(soil.tempf = soil.temp * 9/5 + 32) %>% dplyr::select(soil.tempf) -> grid_var_stars

# st_get_dimension_values(grid_var_stars,"time") #how get time dimension values
by_t = "1 year"
grid <- aggregate(grid_var_stars, by = by_t, FUN = function(x) mean(x)) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
grid1 <- split(grid, "time")

mean_grid <- st_apply(grid, c("x", "y"), mean)
saveRDS(mean_grid, file = paste(data.dir,paste(var,"Daymet",sep="_"),sep="/"))

for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])

  cropped_st_fut <- readRDS(paste(data.dir,paste0("cropped_st_fut_eb_",gcm,"_",rcp),sep="/"))
 
  fut_var <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s %>% mutate(soil.temp = (SOIL_TEMP1+SOIL_TEMP2)/2) -> s 
    if (is.na(summary(s$soil.temp)[4])) {
      fut_var[[F]] = fut_var[[F-1]]
      st_dimensions(fut_var[[F]])[3] = st_dimensions(s)[3]
    } else{
      fut_var[[F]] = s[,,,] #all months
    }
  }
  
  fut_var_stars <- Reduce(c, fut_var)
  fut_var_stars %>% mutate(soil.tempf = soil.temp * 9/5 + 32) %>% dplyr::select(soil.tempf) -> fut_var_stars

  fut <- aggregate(fut_var_stars, by = by_t, FUN = function(x) mean(x)) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
  fut <- fut[,1:length(future.period),,]
 
  mean_fut <- st_apply(fut, c("x", "y"), mean)
  # mean_fut2 <- mean_fut %>% mutate(grid = as.vector(mean_grid$mean)) %>% dplyr::select(grid) #This is some fuckery that has to be done bececause Daymet grid is flipped somehow, puts it on same grid
  # flip <- st_flip(mean_fut2, "y") #flip Daymet
  
  delta <- mean_fut - mean_grid
    saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
}

#
#
#

# Annual water.balance ----
var = "water.balance"

cropped_st_grid <- readRDS(paste(data.dir,"cropped_st_Daymet_wf",sep="/"))
grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s %>% mutate(water.balance = PRCP - EVAP) -> s 
  s = dplyr::select(s, water.balance)
  if (is.na(summary(s$water.balance)[4])) {
    grid_var[[F]] = grid_var[[F-1]]
    st_dimensions(grid_var[[F]])[3] = st_dimensions(s)[3]
  } else{
    grid_var[[F]] = s[,,,] #all months
  }
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars %>% mutate(water.balancef = water.balance / 25.4) %>% dplyr::select(water.balancef) -> grid_var_stars

# st_get_dimension_values(grid_var_stars,"time") #how get time dimension values
by_t = "1 year"
grid <- aggregate(grid_var_stars, by = by_t, FUN = function(x) sum(x)) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.

mean_grid <- st_apply(grid, c("x", "y"), mean)
saveRDS(mean_grid, file = paste(data.dir,paste(var,"Daymet",sep="_"),sep="/"))

for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])
  
  cropped_st_fut <- readRDS(paste(data.dir,paste0("cropped_st_fut_wf_",gcm,"_",rcp),sep="/"))
 
  fut_var <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s %>% mutate(water.balance = PRCP - EVAP) -> s 
    s = dplyr::select(s, water.balance)
    if (is.na(summary(s$water.balance)[4])) {
      fut_var[[F]] = fut_var[[F-1]]
      st_dimensions(fut_var[[F]])[3] = st_dimensions(s)[3]
    } else{
      fut_var[[F]] = s[,,,] #all months
    }
  }
  
  
  fut_var_stars <- Reduce(c, fut_var)
  fut_var_stars %>% mutate(water.balancef = water.balance / 25.4) %>% dplyr::select(water.balancef) -> fut_var_stars
  
  fut <- aggregate(fut_var_stars, by = by_t, FUN = function(x) sum(x)) # Doesn't work in lat/long. Must be projected. Removes units from tmax. Also aggregates to a lower resolution.
  fut <- fut[,1:length(future.period),,]
 
  mean_fut <- st_apply(fut, c("x", "y"), mean)
  # mean_fut2 <- mean_fut %>% mutate(grid = as.vector(mean_grid$mean)) %>% dplyr::select(grid) #This is some fuckery that has to be done bececause Daymet grid is flipped somehow, puts it on same grid
  # flip <- st_flip(mean_fut2, "y") #flip Daymet
  
  delta <- mean_fut - mean_grid
  saveRDS(delta, file = paste(data.dir,paste(var,gcm,rcp,sep="_"),sep="/"))
  
}




# ######### EXTRA CODE USED TO FIGURE OUT FLIPPING - SAVE FOR NOW
# # Dimension columns are reversed
# st_get_dimension_values(mean_fut, 'y', max=TRUE) 
# 
# fut_y <- st_get_dimension_values(mean_fut, 'y', max=TRUE) 
# fy <- fut_y[, rev(seq_len(ncol(fut_y)))] # This correctly reverses the dimension columns
# fy1 <- lapply(seq_len(ncol(fy)), function(i) fy[,i])
# 
# mean_fut2 <- st_set_dimensions(mean_fut, 2, values = fy1 )
# 
# 
# 
# as.vector(mean_grid$mean)
# 
# 
# mean_fut2 <- mean_fut %>% mutate(grid = as.vector(mean_grid$mean),
#                                  delta = mean - grid) %>% dplyr::select(grid)
# 
# flip <- st_flip(mean_fut2, "y")
# 
# delta <- mean_fut - flip
# 
# 
# 
# ggplot() + # Resolution is course
#   geom_stars(data = delta, alpha = 0.8) + 
#   scale_y_reverse() +
#   # facet_wrap("time") +
#   # scale_fill_viridis() + 
#   #coord_equal() + 
#   theme_map() +
#   theme(legend.position = "bottom") +
#   theme(legend.key.width = unit(2, "cm"))
# 
# 
