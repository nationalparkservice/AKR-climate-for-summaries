# This script was made by Brecken Robb to compare Northern Climate Report data
# to our CF data for the Western Arctic parks (CAKR, KOVA, NOAT)
# Jan 2025

## IMPORTANT: 3 INSTANCES OF CODE UPDATES REQUIRED - SEARCH TERM *UPDATE* TO FIND THEM ALL ##

# *************************************************************************** #

# Initials ----

## Uncomment if running separate than from within "CF-selectionT&P.Rmd"

# rm(list = ls())
# 
# library(dplyr)
# 
# SiteID <- "NOAT" #*UPDATE*

base.dir = paste0("C:/Users/brobb/OneDrive - DOI/Projects/AKR_CFs/",SiteID) #*UPDATE*
data.dir = paste0(base.dir,"/Data")

# *************************************************************************** #

# Read in data frames ----

## CCRP CF data ----
CCRP_P <- read.csv(paste0(data.dir,"/Annual.precipIn_ANN.csv"))
CCRP_T <- read.csv(paste0(data.dir,"/Annual.tmeanF_ANN.csv"))

## NCR CF data ----

#### **UNSURE BEST WAY TO READ THESE IN IN AN AUTOMATED FASHION** ####

# *UPDATE* - Uncomment T & P data sets for your park u  nit of interest
# skip function excludes the first 15 lines of "READ_ME" information in the data sets

## Cape Krusenstern (CAKR)
# NCR_T <- read.csv("https://earthmaps.io/temperature/area/NPS4?format=csv",
#                   skip = 15)
# NCR_P <- read.csv("https://earthmaps.io/precipitation/area/NPS4?format=csv",
#                   skip = 15)

## Kobuk Valley (KOVA)
# NCR_T <- read.csv("https://earthmaps.io/temperature/area/NPS17?format=csv", 
#                   skip = 15)
# NCR_P <- read.csv("https://earthmaps.io/precipitation/area/NPS17?format=csv", 
#                   skip = 15)

## Noatak (NOAT)
NCR_T <- read.csv("https://earthmaps.io/temperature/area/NPS22?format=csv",
                  skip = 15)
NCR_P <- read.csv("https://earthmaps.io/precipitation/area/NPS22?format=csv",
                  skip = 15)

# *************************************************************************** #

# Calculate NCR means ----

## NCR historical T data ----
# CRU-TS40

## Subset NCR historical (1950-2009) T data frame (NCR_T) to historical means across binned seasons (i.e., DJF, etc.)
# Data lumped so can't subset to match ours (1980-2009)
NCR_T_his.subset <- NCR_T[1:4,6]

# Take average of 4 binned season values to produce historical T mean (1950-2009)
NCR_T_his.mean <- mean(as.matrix(NCR_T_his.subset))

## NCR historical P data ----
# CRU-TS40

## Subset NCR historical (1950-2009) P data frame (NCR_P) to historical means across binned seasons (i.e., DJF, etc.)
# Data lumped so can't subset to match ours (1980-2009)
NCR_P_his.subset <- NCR_P[1:4,6]

# Take average of 4 binned season values to produce historical P mean (2010-2099)
NCR_P_his.mean <- mean(as.matrix(NCR_P_his.subset))

# Multiply by 4 to sum binned seasons (i.e., DJF, etc.)
NCR_P_his.mean.sum <- NCR_P_his.mean*4

# Transform NCR values from mm to in
NCR_P_his.mean.in <- NCR_P_his.mean.sum/25.4

## NCR future T data ----
# MRI-CGCM3.rcp45 & CCSM4.rcp85

# Identify models & scenarios
GCM_CFs <- c("MRI-CGCM3.rcp45","CCSM4.rcp85")

# Subset data to match ours as closely as possible with NCR lumping (2030-2069)
model.scenario.t <- paste(NCR_T$model, NCR_T$scenario, sep = ".")
NCR_T$model.scenario <- model.scenario.t
NCR_T_fut.mean <- NCR_T %>%
  filter(model.scenario %in% c("MRI-CGCM3.rcp45","CCSM4.rcp85")) %>%
  filter(date_range %in% c("2030_2039", "2040_2049", "2050_2059", "2060_2069")) %>%
  summarize(mean_mean = mean(mean))

## NCR future P data ----
# MRI-CGCM3.rcp45 & CCSM4.rcp85

# Subset data to match ours as closely as possible with NCR lumping (2030-2069)
model.scenario.p <- paste(NCR_P$model, NCR_P$scenario, sep = ".")
NCR_P$model.scenario <- model.scenario.p
NCR_P_fut.mean <- NCR_P %>%
  filter(model.scenario %in% c("MRI-CGCM3.rcp45","CCSM4.rcp85")) %>%
  filter(date_range %in% c("2030_2039", "2040_2049", "2050_2059", "2060_2069")) %>%
  summarize(mean_mean = mean(mean))

# Multiply by 4 to sum binned seasons (i.e., DJF, etc.)
NCR_P_fut.mean.sum <- NCR_P_fut.mean*4

# Transform NCR values from mm to in
NCR_P_fut.mean.in <- NCR_P_fut.mean.sum/25.4

# *************************************************************************** #

# Calculate CCRP means ----

## Subset our historical & future T & P data to means

# Set new historical (1950-2009) & future periods (2010-2099) to match NCR - after changing to observational data
# we had to adjust our historical timeframe to 1980-2009 and future timeframe to 2035-2065 for our data - left NCR timeframe as is

## CCRP historical T means ----
# Daymet

# Subset
CCRP_T_his.mean.F <- CCRP_T %>%
  filter(GCM == "Daymet") %>%
  filter(between(Year, 1980, 2009)) %>%
  summarize(mean_tmeanF = mean(Annual.tmeanF, na.rm = TRUE))

## Convert our T values from *F to *C
CCRP_T_his.mean.C <- (CCRP_T_his.mean.F - 32) * 5/9

## CCRP historical P means ----
# Daymet

# Subset
CCRP_P_his.mean <- CCRP_P %>%
  filter(GCM == "Daymet") %>%
  filter(between(Year, 1980, 2009)) %>%
  summarize(mean_precipIn = mean(Annual.precipIn, na.rm = TRUE))

## CCRP future T means ----
# MRI-CGCM3.rcp45 & CCSM4.rcp85

# Subset
CCRP_T_fut.subset <- CCRP_T[CCRP_T$GCM %in% c("MRI-CGCM3.rcp45","CCSM4.rcp85"), c("Annual.tmeanF")]
CCRP_T_fut.mean.F <- mean(as.matrix(CCRP_T_fut.subset))

## Convert our T values from *F to *C
CCRP_T_fut.mean.C <- (CCRP_T_fut.mean.F - 32) * 5/9

## CCRP future P means ----
# MRI-CGCM3.rcp45 & CCSM4.rcp85

# Subset
CCRP_P_fut.subset <- CCRP_P[CCRP_P$GCM %in% c("MRI-CGCM3.rcp45","CCSM4.rcp85"), c("Annual.precipIn")]
CCRP_P_fut.mean <- mean(as.matrix(CCRP_P_fut.subset))

# *************************************************************************** #

# Compare NCR & CCRP historical and future means ----

## Write results to .csv ----

#### **NEED HELP FROM AMBER TO WRITE .CSV FILES** ####

# df<-data.frame(year=future.period,mean=NA)
# for (i in 1:length(future.period)){
#   t <-st_apply(fut1[i],1:2,mean)
#   df$mean[i] <- mean(t$mean,na.rm=TRUE)
# }

# Create empty data frame with predefined column names and types

df <- data.frame(
  Data_Source = character(),
  GCM = numeric(),
  Temp_mean_C = numeric(),
  Precip_mean_in = numeric(),
  stringsAsFactors = FALSE
)

Data_Source = 

# Populate data frame using a for loop
for (i in 1:4) {
  new_row <- data.frame(
    Data_Source = i,
    Value = runif(1),
    Category = sample(c("A", "B", "C"), 1)
  )
  
  df <- rbind(df, new_row)  # Add the new row to the data frame
}

# Write data frame to .csv file
write.csv(df,paste0(data.dir,"/","NCR_DataComparison_", SiteID, ".csv"),row.names=FALSE)

# rm()
