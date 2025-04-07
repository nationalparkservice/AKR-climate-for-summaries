## MET MONTHLY
print("extracting Daymet")
DF = data.frame()

grid_filelist = list.files(path = paste(met.dir,"daymet",sep='/'), pattern= '.nc', full.names = TRUE)

# DAYMET ----
for(i in 1:length(grid_filelist)){
# grid_annual <- list() # Create a list to put the stars objects into
yr = as.POSIXct(sub('.*\\met_', '', sub("\\..*", "", grid_filelist[i])),format="%Y")
print(yr)
invisible(capture.output(
  suppressWarnings(
    (grid_star = read_stars(grid_filelist[i], sub=c("tmax","tmin","pcp") ,curvilinear = c("longitude", "latitude"))))))
grid_star = st_transform(grid_star, st_crs(shp))
grid_crop = grid_star[shp]
grid_crop = drop_units(grid_crop)
rm(grid_star)

## timeseries df
ts <- st_apply((grid_crop %>% dplyr::select(tmax,tmin,pcp)), c("time"),mean,na.rm=TRUE, rename=FALSE)
df <-data.frame(ts)
df$GCM = "Daymet"
DF<-rbind(DF,df)

rm(ts,grid_crop)
gc()
}

for (G in 1:length(GCMs)){
  # setting variables ----
  gcm = sub("\\..*", "", GCMs[G])
    rcp = sub('.*\\.', '', GCMs[G])
    path = paste(met.dir, gcm, rcp, sep = '/')
    file.list = list.files(path = path, pattern = '.nc4', full.names = TRUE)
    fut_filelist = Filter(function(x) grepl(paste(future.period, collapse = "|"), x), file.list)
    
    # model.dir <- paste0(data.dir,"/",gcm,".",rcp)
    # dir.create(model.dir,showWarnings=FALSE)
    
    print(paste0("extracting ", GCMs[G]))

    # Creating stars objects ####
    
      # HISTORICAL ----
    # fut_annual <- list() # Create a list to put the stars objects into
    for(i in 1:length(fut_filelist)){
      yr = as.POSIXct(sub('.*\\met_', '', sub("\\..*", "", fut_filelist[i])),format="%Y")
      print(yr)
      invisible(capture.output(
        suppressWarnings(
      (fut_star = read_ncdf(fut_filelist[i], var=c("tmax","tmin","pcp"), curvilinear = c("longitude", "latitude"))))))
      fut_star = st_transform(fut_star, st_crs(shp))
      fut_crop = fut_star[shp]
      fut_crop = drop_units(fut_crop)
      rm(fut_star)
      
      ## timeseries df
      ts <- st_apply((fut_crop %>% dplyr::select(tmax,tmin,pcp)), c("time"),mean,na.rm=TRUE, rename=FALSE)
      df <-data.frame(ts)
      df$GCM = GCMs[G]
      DF<-rbind(DF,df)
      
      rm(ts,fut_crop)
      gc()
      # )
}
}

write.csv(DF,paste0(data.dir,"/Daily_met.csv"),row.names=FALSE)

