##########################
# CWD plots # 
##########################

# WB from Tercek http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/

Tercek <- c("inmcm4","NorESM1-M","MRI-CGCM3","MIROC5","MIROC-ESM-CHEM",
            "IPSL-CM5A-LR","HadGEM2-CC365","GFDL-ESM2G","CanESM2","CSIRO-Mk3-6-0","CNRM-CM5","CCSM4","BNU-ESM")
Tercek.WB <- c(paste0(Tercek,".rcp45"),paste0(Tercek,".rcp85"))

## Extract CFs from SessionInfo
SessionInfo <- read.table(paste0(CF.folders,SiteID,"/SessionInfo.txt"), sep = "^") #for downloaded centroid CFs
# SessionInfo <- read.table(paste0(CF.folders,"/SessionInfo.txt"), sep = "^") #for custom or Koppen CFs

# extract GCMs and CFs from 9th row of SessionInfo - extract between parenthesis and convert to vector
GCMs <- regmatches(SessionInfo[9,], regexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]]; GCMs <- gsub("\\(|\\)", "", GCMs)
GCMs <- strsplit(GCMs,split=", ",fixed=TRUE)[[1]]
CF.list <- tail(regmatches(SessionInfo[9,], gregexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]], 1); CF.list <- gsub("\\(|\\)", "", CF.list)
CF.list <- strsplit(CF.list,split=", ",fixed=TRUE)[[1]]

CF.GCM <- data.frame(CF=CF.list, GCM=GCMs)
CF.GCM <- subset(CF.GCM, CF %in% CFs)
CF1 <- CF.GCM |> filter(CF == CFs[1])
CF2 <- CF.GCM |> filter(CF == CFs[2])

Tercek <- (CF.GCM$GCM %in% Tercek.WB)

# If both GCMs available in Tercek models
if(FALSE %in% Tercek){
  
  #####################
  ### ClimateToolbox Deficit
  ### For each variable
  # Var
  Var = "deficit_ANN" #tasmean_ANN, pr_ANN, Deficit, deficit_ANN
  long.title = "Annual Climatic Water Deficit (in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
  units = "(in/year)" #(°F), (in/year)
  
  #Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
  # temp = temp_div , temp_seq
  # prcp = prec_div , prec_seq
  # snow = cryo_div , cryo_seq
  VarType = "prec" #temp, prec, cryo
  
  inName <- "C:/Users/brobb/OneDrive - DOI/Projects/AKR_CFs/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
  SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
  # SheetNames
  
  # first sheet is only figures of color maps
  for(i in 2:length(SheetNames)){
    # cat(str_c("i ->  ", i , "   "))
    d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
    d1$PalName <- SheetNames[i]
    d1 <- na.omit(d1)
    ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
  }
  
  # head(PalData)
  PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)
  
  div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
  seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))
  
  if(Var=="Deficit"| Var == "deficit_ANN") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
  # 
  # # test palettes
  # hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
  #   geom_point(size = 3)
  # hw_plot + scale_color_gradientn(colours = div.pal$Hex)
  
  Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
  CF1.file <- map.file(CF1$GCM) 
  CF2.file <- map.file(CF2$GCM) 
  
  Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
  CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
  CF2.rast <- rast(paste0(tifDir, CF2.file)) 
  
  # Need to flip MACA WB data
  Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
  CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
  CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it
  
  # # Clip raster to park
  # 
  # Hist.rast <- crop(Hist.rast, buff)
  # CF1.rast <- crop(CF1.rast, buff)
  # CF2.rast <- crop(CF2.rast, buff)
  
  #Tercek plots code -- clip raster to park and make delta files, convert to in
  
  Hist.rast <- crop(project(Hist.rast, crs(buff)), buff)
  CF1.rast <- crop(project(CF1.rast, crs(buff)), buff)
  CF2.rast <- crop(project(CF2.rast, crs(buff)), buff)
  
  # Historical plot
  Hist.plot <- historical.plot(Hist.rast)
  Hist.plot
  
  # Future delta plots
  scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
  scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])
  
  cf1.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])} else{
    map.plot.seq.pos(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])
  }
  cf2.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])} else{
    map.plot.seq.pos(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])
  }
  
  
  # Merge into one plot
  
  maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                     top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                    gp=gpar(fontface="bold", col="black", fontsize=18)))
  
  hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                         face = "bold", size = 18))
  
  a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
  a
  
  if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
  ggsave(paste0(SiteID,"-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)
} else {
  
  ### Tercek Deficit
  ### For each variable
  # Var
  Var = "Deficit" #tasmean_ANN, pr_ANN, Deficit
  long.title = "Annual Climatic Water Deficit (in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
  units = "(in/year)" #(°F), (in/year)
  
  #Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
  # temp = temp_div , temp_seq
  # prcp = prec_div , prec_seq
  # snow = cryo_div , cryo_seq
  VarType = "prec" #temp, prec, cryo
  
  inName <- "C:/Users/brobb/OneDrive - DOI/Projects/AKR_CFs/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
  SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
  # SheetNames
  
  # first sheet is only figures of color maps
  for(i in 2:length(SheetNames)){
    # cat(str_c("i ->  ", i , "   "))
    d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
    d1$PalName <- SheetNames[i]
    d1 <- na.omit(d1)
    ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
  }
  
  # head(PalData)
  PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)
  
  div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
  seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))
  
  if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
  # 
  # # test palettes
  # hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
  #   geom_point(size = 3)
  # hw_plot + scale_color_gradientn(colours = div.pal$Hex)
  
  Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
  CF1.file <- map.file(CF1$GCM) 
  CF2.file <- map.file(CF2$GCM) 
  
  Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
  CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
  CF2.rast <- rast(paste0(tifDir, CF2.file)) 
  
  # Need to flip MACA WB data
  # Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
  # CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
  # CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it
  
  # # Clip raster to park
  # 
  # Hist.rast <- crop(Hist.rast, buff)
  # CF1.rast <- crop(CF1.rast, buff)
  # CF2.rast <- crop(CF2.rast, buff)
  
  #Tercek plots code -- clip raster to park and make delta files, convert to in
  
  Hist.rast <- crop(project(Hist.rast, crs(buff)), buff)
  CF1.rast.temp <- crop(project(CF1.rast, crs(buff)), buff)
  CF2.rast.temp <- crop(project(CF2.rast, crs(buff)), buff)
  
  CF1.rast <- CF1.rast.temp - Hist.rast
  CF2.rast <- CF2.rast.temp - Hist.rast
  
  Hist.rast <- Hist.rast/25.4
  CF1.rast <- CF1.rast/25.4
  CF2.rast <- CF2.rast/25.4
  
  # Historical plot
  Hist.plot <- historical.plot(Hist.rast)
  Hist.plot
  
  # Future delta plots
  scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
  scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])
  
  cf1.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])} else{
    map.plot.seq.pos(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])
  }
  cf2.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])} else{
    map.plot.seq.pos(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])
  }
  
  
  # Merge into one plot
  
  maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                     top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                    gp=gpar(fontface="bold", col="black", fontsize=18)))
  
  hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                         face = "bold", size = 18))
  
  a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
  a
  
  if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
  ggsave(paste0(SiteID,"-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)
}
