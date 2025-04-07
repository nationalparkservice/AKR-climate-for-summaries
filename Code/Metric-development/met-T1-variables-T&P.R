for (G in 1:length(GCMs)){
  gcm = GCMs[G]
  for(R in 1:length(RCPs)){
    rcp = RCPs[R]
    path = paste(data.dir, gcm, rcp, sep = '/')
    file.list = list.files(path = path, pattern = '.nc', full.names = TRUE)
    hist_filelist = Filter(function(x) grepl(paste(historical.period, collapse = "|"), x), file.list)
    fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
    
    # model.dir <- paste0(plot.dir,"/",gcm,".",rcp)
    # dir.create(model.dir,showWarnings=FALSE)
    
    # index for df
    GR <- paste(gcm,rcp,sep=".")
    index <- match(GR, GCM.RCP)
    print(c(index, GR))
    
    Baseline_Means$GCM[index] = gcm
    Baseline_Means$RCP[index] = rcp
    Future_Means$GCM[index] = gcm
    Future_Means$RCP[index] = rcp
    Deltas$GCM[index] = gcm
    Deltas$RCP[index] = rcp
  
    ############################################################################
    ##    CREATE STARS OBJECTS FOR T1 PLOTS   ##################################
    ############################################################################
    
    # shp = whatever shapefile you want to use to crop the area of interest
    
    # HISTORICAL
    
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(hist_filelist)){
      suppressMessages(
      l[[i]] <- read_ncdf(hist_filelist[i], curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
      )
    }
    
    # Crop
    
    cropped_hist <- list() # create list for cropped stars objects
    
    for(i in 1:length(l)){ # add cropped stars objects to a new list
      nc = l[[i]]
      nc = st_transform(nc, st_crs(shp))
      nc_crop = nc[shp]
      cropped_hist[[i]] = nc_crop
    }
    
    cropped_st_hist <- list()
    
    for(i in 1:length(cropped_hist)){
      cropped_st_hist[[i]] <- st_as_stars(cropped_hist[[i]])
    }
    
    # FUTURE
    
    l <- list() # Create a list to put the stars objects into
    
    for(i in 1:length(fut_filelist)){
      suppressMessages(
      l[[i]] <- read_ncdf(fut_filelist[i], curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
      )
    }
    
    # Crop
    
    cropped_fut <- list() # create list for cropped stars objects
    
    for(i in 1:length(l)){ # add cropped stars objects to a new list
      nc = l[[i]]
      nc = st_transform(nc, st_crs(shp))
      nc_crop = nc[shp]
      cropped_fut[[i]] = nc_crop
    }
    
    cropped_st_fut <- list()
    
    for(i in 1:length(cropped_fut)){
      cropped_st_fut[[i]] <- st_as_stars(cropped_fut[[i]])
    }
    
    ############################################################################
    ##    CREATE T1 variables   ################################################
    ############################################################################
    
    #Tmean
    
    var = "Tmean (F)"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s %>% mutate(tmean = (tmax + tmin)/2) -> s 
      s = select(s, tmean)
      hist_var[[H]] = s[,,,] #all months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s %>% mutate(tmean = (tmax + tmin)/2) -> s 
      s = select(s, tmean)
      fut_var[[F]] = s[,,,] #all months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> fut_var_stars
    
    mean_hist <- st_apply(hist_var_stars, c("x", "y"), mean) # find mean
    mean_fut <- st_apply(fut_var_stars, c("x", "y"), mean)
    # delta <- mean_fut - mean_hist
    
    
    #### Add values to Means dfs
    Baseline_Means$Tmean_F[index] = mean(mean_hist$mean, na.rm=TRUE)
    Future_Means$Tmean_F[index] = mean(mean_fut$mean, na.rm=TRUE)
    # Deltas$Tmean_F[index] = mean(delta$mean, na.rm=TRUE)
    
    
    # # ggplot - delta
    # ggplot() +
    #   geom_stars(data = delta, alpha = 0.8) +
    #   geom_sf(data = shp, aes(), fill = NA) +
    #   scale_fill_viridis(direction=1, option = "H",begin = .5, end = 1,
    #                      guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + #Turbo for temp delta
    #   labs(title = paste0("Change in ", var, " -- ", gcm, ".", rcp), fill="mean (F)") +
    #   theme(legend.position = "bottom",
    #         legend.key.width = unit(2, "cm"),
    #         legend.key.height = unit(.2, "cm"),
    #         plot.title=element_text(size=12,face="bold",hjust=0.5))
    # 
    # ggsave(paste(var, gcm, rcp, ".png", sep = '_'),path = model.dir, width = 4.5, height=4)


    #Precip
    var = "Precip (in)"
    hist_var <- list()
    
    for(H in 1:length(cropped_st_hist)){
      s = cropped_st_hist[[H]]
      s = select(s, pcp)
      hist_var[[H]] = s[,,,] #set for months
    }
    
    fut_var <- list()
    
    for(F in 1:length(cropped_st_fut)){
      s = cropped_st_fut[[F]]
      s = select(s, pcp)
      fut_var[[F]] = s[,,,] #set for months
    }
    
    hist_var_stars <- Reduce(c, hist_var)
    hist_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> hist_var_stars
    
    fut_var_stars <- Reduce(c, fut_var) 
    fut_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> fut_var_stars
    
    sum_hist <- st_apply(hist_var_stars, c("x", "y"), FUN=function(x) sum(x)/length(historical.period)) # find sum
    sum_fut <- st_apply(fut_var_stars, c("x", "y"), FUN=function(x) sum(x)/length(future.period))
    # delta <- sum_fut - sum_hist
    
    
    #### Add values to Means dfs
    Baseline_Means$Precip_in[index] = mean(sum_hist$pcp_in, na.rm=TRUE)
    Future_Means$Precip_in[index] = mean(sum_fut$pcp_in, na.rm=TRUE)
    # Deltas$Precip_in[index] = mean(delta$pcp_in, na.rm=TRUE)
    
    
    # # ggplot - delta
    # ggplot() + 
    #   geom_stars(data = delta, alpha = 0.8) + 
    #   geom_sf(data = shp, aes(), fill = NA) + 
    #   scale_fill_viridis(direction=-1, option = "E",begin = .5, end = 1, 
    #                      guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + #cividis for precip delta
    #   labs(title = paste0("Change in ", var, " -- ", gcm, ".", rcp), fill="inches/year") +
    #   theme(legend.position = "bottom",
    #         legend.key.width = unit(2, "cm"),
    #         legend.key.height = unit(.2, "cm"),
    #         plot.title=element_text(size=12,face="bold",hjust=0.5))
    # 
    # ggsave(paste(var, gcm, rcp, ".png", sep = '_'),path = model.dir, width = 4.5, height=4)
    # 
    # 

  rm(hist_var, fut_var, hist_var_stars, fut_var_stars, sum_hist, sum_fut, delta,mean_hist,mean_fut)
  gc()
  }
}

###############################
##### Daymet data
print("extracting Daymet")
grid_filelist = list.files(path = "E:/NCAR_AK/met/monthly/daymet", pattern= '.nc', full.names = TRUE)
grid_filelist = Filter(function(x) grepl(paste(daymet.period, collapse = "|"), x), grid_filelist)

# DAYMET ----

l <- list() # Create a list to put the stars objects into

for(i in 1:length(grid_filelist)){
  invisible(capture.output(
    suppressWarnings(
      l[[i]] <- read_stars(grid_filelist[i], curvilinear = c("longitude", "latitude")) # need to read in as ncdf or coordinate system does not translate (not sure why)
    )))
}

# Crop

cropped_grid <- list() # create list for cropped stars objects

for(i in 1:length(l)){ # add cropped stars objects to a new list
  nc = l[[i]]
  nc = st_transform(nc, st_crs(shp))
  nc_crop = nc[shp]
  cropped_grid[[i]] = nc_crop
}

cropped_st_grid <- list()

for(i in 1:length(cropped_grid)){
  cropped_st_grid[[i]] <- st_as_stars(cropped_grid[[i]])
}
# assign(paste0("cropped_st_grid_",GCMs[G]), cropped_st_grid)
saveRDS(cropped_st_grid, file = paste(data.dir,"cropped_st_Daymet",sep="/"))

#Tmean
var = "Tmean (F)"
grid_var <- list()

for(H in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[H]]
  s %>% mutate(tmean = (tmax + tmin)/2) -> s 
  s = select(s, tmean)
  grid_var[[H]] = s[,,,] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$tmean <- units::drop_units(grid_var_stars$tmean)
grid_var_stars %>% mutate(tmean_f = tmean * 9/5 + 32) %>% select(tmean_f) -> grid_var_stars

mean_grid <- st_apply(grid_var_stars, c("x", "y"), mean)
Daymet_Means[3] = mean(mean_grid$mean, na.rm=TRUE)

# Precip
var = var = "Precip (in)"
grid_var <- list()

for(H in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[H]]
  s = select(s, pcp)
  grid_var[[H]] = s[,,,] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars$pcp <- units::drop_units(grid_var_stars$pcp)
grid_var_stars %>% mutate(pcp_in = pcp / 25.4) %>% select(pcp_in) -> grid_var_stars

sum_grid <- st_apply(grid_var_stars, c("x", "y"), FUN=function(x) sum(x)/length(daymet.period)) # find sum
Daymet_Means[4] = mean(sum_grid$pcp_in, na.rm=TRUE)

Daymet_Means[1] = "gridmet"
Daymet_Means[2] = "gridmet"

# delta <- mean_fut - mean_hist
Deltas$Tmean_F = Future_Means$Tmean_F - Daymet_Means$Tmean_F
Deltas$Precip_in = Future_Means$Precip_in - Daymet_Means$Precip_in


rm(cropped_fut, cropped_hist, cropped_st_fut, cropped_st_hist,l,nc,nc_crop,s)
gc()
  