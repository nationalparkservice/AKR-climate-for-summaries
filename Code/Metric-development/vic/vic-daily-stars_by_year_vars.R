## VIC DAILY -- shortwave, runoff, swe, deficit, sm, soil temp

print("extracting Daymet")
DF = data.frame()

grid_filelist = list.files(path = paste(vic.dir,"daily/daymet",sep='/'), pattern= '.nc', full.names = TRUE)
eb_grid_filelist <- grid_filelist[grep("eb", grid_filelist)]
wf_grid_filelist <- grid_filelist[grep("wf", grid_filelist)]
ws_grid_filelist <- grid_filelist[grep("ws", grid_filelist)]


# DAYMET ----
for(i in 1:length(wf_grid_filelist)){
  yr = as.POSIXct(sub('.*\\wf_', '', sub("\\..*", "", wf_grid_filelist[i])),format="%Y")
  print(yr)
  
  invisible(capture.output(suppressWarnings(
    (grid_star_wf = read_stars(wf_grid_filelist[i], sub=c("RUNOFF","EVAP","PRCP","SNOW_MELT") ,curvilinear = c("longitude", "latitude"))))))
  grid_star_wf = st_transform(grid_star_wf, st_crs(shp))
  grid_crop_wf = grid_star_wf[shp]
  grid_crop_wf = drop_units(grid_crop_wf)
  
  invisible(capture.output(
    suppressWarnings(
      (grid_star_ws = read_stars(ws_grid_filelist[i], sub=c("SWE","SM1") ,curvilinear = c("longitude", "latitude"))) )))
  grid_star_ws = st_transform(grid_star_ws, st_crs(shp))
  grid_crop_ws = grid_star_ws[shp]
  grid_crop_ws = drop_units(grid_crop_ws)
  
  invisible(capture.output(
    suppressWarnings(
      (grid_star_eb = read_stars(eb_grid_filelist[i], sub=c("NET_SHORT","SOIL_TEMP1") ,curvilinear = c("longitude", "latitude"))) )))
  grid_star_eb = st_transform(grid_star_eb, st_crs(shp))
  grid_crop_eb = grid_star_eb[shp]
  grid_crop_eb = drop_units(grid_crop_eb)
  
  rm(grid_star_wf, grid_star_ws, grid_star_eb)

## timeseries df
ts1 <- st_apply((grid_crop_wf), c("time"),mean,na.rm=TRUE, rename=FALSE)
df1 <-data.frame(ts1)
ts2 <- st_apply((grid_crop_ws), c("time"),mean,na.rm=TRUE, rename=FALSE)
df2 <-data.frame(ts2)
ts3 <- st_apply((grid_crop_eb), c("time"),mean,na.rm=TRUE, rename=FALSE)
df3 <-data.frame(ts3)
df <- full_join(df1,full_join(df2,df3))
df$GCM = "Daymet"
DF<-rbind(DF,df)

rm(ts1,ts2,ts3,df1,df2,df3,grid_crop_ws,grid_crop_eb,grid_crop_wf)
gc()
}

for (G in 1:length(GCMs)){
  # setting variables ----
  gcm = sub("\\..*", "", GCMs[G])
    rcp = sub('.*\\.', '', GCMs[G])
    path = paste(vic.dir, "daily/BCSD", gcm, rcp, sep = '/')
    file.list = list.files(path = path, pattern = '.nc4', full.names = TRUE)
    fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
    
    eb_fut_filelist <- fut_filelist[grep("eb", fut_filelist)]
    wf_fut_filelist <- fut_filelist[grep("wf", fut_filelist)]
    ws_fut_filelist <- fut_filelist[grep("ws", fut_filelist)]
    
    # model.dir <- paste0(data.dir,"/",gcm,".",rcp)
    # dir.create(model.dir,showWarnings=FALSE)
    
    print(paste0("extracting ", GCMs[G]))

    # Creating stars objects ####
    
      # FUTURE ----
    # fut_annual <- list() # Create a list to put the stars objects into
    for(i in 1:length(wf_fut_filelist)){
      yr = as.POSIXct(sub('.*\\wf_', '', sub("\\..*", "", wf_fut_filelist[i])),format="%Y")
      print(yr)
      
      invisible(capture.output(suppressWarnings(
        (fut_star_wf = read_stars(wf_fut_filelist[i], sub=c("RUNOFF","EVAP","PRCP","SNOW_MELT") ,curvilinear = c("longitude", "latitude"))))))
      fut_star_wf = st_transform(fut_star_wf, st_crs(shp))
      fut_crop_wf = fut_star_wf[shp]
      fut_crop_wf = drop_units(fut_crop_wf)
      
      invisible(capture.output(
        suppressWarnings(
          (fut_star_ws = read_stars(ws_fut_filelist[i], sub=c("SWE","SM1") ,curvilinear = c("longitude", "latitude"))) )))
      fut_star_ws = st_transform(fut_star_ws, st_crs(shp))
      fut_crop_ws = fut_star_ws[shp]
      fut_crop_ws = drop_units(fut_crop_ws)
      
      invisible(capture.output(
        suppressWarnings(
          (fut_star_eb = read_stars(eb_fut_filelist[i], sub=c("NET_SHORT","SOIL_TEMP1") ,curvilinear = c("longitude", "latitude"))) )))
      fut_star_eb = st_transform(fut_star_eb, st_crs(shp))
      fut_crop_eb = fut_star_eb[shp]
      fut_crop_eb = drop_units(fut_crop_eb)
      
      rm(fut_star_wf, fut_star_ws, fut_star_eb)
      
      ## timeseries df
      ts1 <- st_apply((fut_crop_wf), c("time"),mean,na.rm=TRUE, rename=FALSE)
      df1 <-data.frame(ts1)
      ts2 <- st_apply((fut_crop_ws), c("time"),mean,na.rm=TRUE, rename=FALSE)
      df2 <-data.frame(ts2)
      ts3 <- st_apply((fut_crop_eb), c("time"),mean,na.rm=TRUE, rename=FALSE)
      df3 <-data.frame(ts3)
      df <- full_join(df1,full_join(df2,df3))
      df$GCM = GCMs[G]
      DF<-rbind(DF,df)
      
      rm(ts1,ts2,ts3,df1,df2,df3,fut_crop_ws,fut_crop_eb,fut_crop_wf)
      gc()
      # )
}
}

write.csv(DF,paste0(data.dir,"/Daily_vic.csv"),row.names=FALSE)

