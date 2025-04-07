## Written by Brecken Robb - January 2025; edited by ACR Mar 2025
## Purpose is to create and export climate futures metrics table
## Data comes from CCRP CFs [NCR data removed]
## writes csv with inputs needed in-line in reports
###############################################################################

# Library ----

# library(dplyr) -- pulls from plot-creation.Rmd
library(data.table) # to read in NCR data from website

###############################################################################

# Set directories ----

# SiteID <- "KOVA" #*UPDATE* -- pulls from plot-creation.Rmd

# base.dir = paste0("C:/Users/brobb/OneDrive - DOI/Projects/AKR_CFs/",SiteID) #*UPDATE* -- pulls from plot-creation.Rmd
# data.dir = paste0(base.dir,"/Data") -- pulls from plot-creation.Rmd
# plot.dir = paste0(base.dir,"/Figures") -- pulls from plot-creation.Rmd

###############################################################################

# Read in data ----

## CCRP ----
# Read in only .csv files from data directory that end in "_ANN"
file_list <- list.files(path = data.dir, pattern = "*_ANN.csv", full.names = TRUE)

## Use if need to exclude additional .csv files that still met above naming (pattern) criteria 
# exclude_files <- c("file_name1.csv", "file_name2.csv")
# file_list <- file_list[!basename(file_list) %in% exclude_files]

# Read in .csv files that met previous 'read' criteria as one list
data_list <- lapply(file_list, function(file) {
  data <- read.csv(file)
  # data$filename <- basename(file) # Adds filename column
  return(data)
})

# Merge data into one data frame
combined <- bind_rows(data_list)

# Read in met monthly and daily data
monthly.met = read.csv(paste0(data.dir,"/Monthly_met.csv"))
daily.met = read.csv(paste0(data.dir,"/Daily_met.csv"))

###############################################################################

# Transform data ----

## CCRP ----

## "_ANN.csv" files
# reduce replicate Year and GCM rows
annual <- combined %>%
  group_by(Year, GCM) %>%
  summarize(across(everything(), ~ first(na.omit(.))), .groups = "drop")

# Average by GCM
annual.avg <- annual %>% ## WARNING because GCM field is not numeric
  group_by(GCM) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

# Drop "Year" column
annual.final <- annual.avg[, !names(annual.avg) %in% c("Year")]

## met monthly data

# drop historical data from 2 GCMs (keep Daymet)
monthly.met.filter <- monthly.met %>%
  filter(!(GCM %in% GCMs & Period == "Historical"))

# drop columns
monthly.met.final <- monthly.met.filter[, !names(monthly.met.filter) %in% c("Period", "CF")]

## met daily data

daily.met.avg <- daily.met %>%
  group_by(GCM) %>%
  summarize(across(everything(), ~ first(na.omit(.))), .groups = "drop")

# Drop "year" column
daily.met.final <- daily.met.avg[, !names(daily.met.avg) %in% c("year")]

###############################################################################

# Join all data frames and export ----

# Merge all data frames
merged <- merge(annual.final, monthly.met.final, by = c("GCM")) %>%
  merge(daily.met.final, by = "GCM") 

# Name rows according to GCMs
rownames(merged)[rownames(merged) == "1"] <- "Climate Future 2" # CCSM4.rcp85
rownames(merged)[rownames(merged) == "2"] <- "Historical" # Daymet
rownames(merged)[rownames(merged) == "3"] <- "Climate Future 1" # MRI-CGCM3.rcp45

# Drop GCM columns (can't have when calculating deltas below)
merged <- merged %>% dplyr::select(-GCM)

# Reorder rows
merged <- merged[c("Historical", "Climate Future 1", "Climate Future 2"), ]

# Transpose data frame
merged_transpose <- t(merged)
merged_transpose <- data.frame(merged_transpose, stringsAsFactors=FALSE) # converts back to data frame

### Write .csv of absolute values ----
write.csv(merged_transpose,paste0(data.dir,"/","Metrics_Table_Absolute.csv"),row.names=TRUE)

# # Add rownames back in as a column to calculate deltas
# merged_transpose$ClimateMetric <- rownames(merged_transpose)

# Calculate deltas for all future variables
delta.CF1 <- merged_transpose$Climate.Future.1 - merged_transpose$Historical
delta.CF2 <- merged_transpose$Climate.Future.2 - merged_transpose$Historical

# Combine delta results into a new data frame
merged_transpose_deltas <- data.frame(merged_transpose$Historical, delta.CF1, delta.CF2) # merged_transpose$ClimateMetric, 

# Add meaningful column names
colnames(merged_transpose_deltas)[colnames(merged_transpose_deltas) == "merged_transpose.Historical"] <- "Historical"
colnames(merged_transpose_deltas)[colnames(merged_transpose_deltas) == "delta.CF1"] <- "Climate Future 1"
colnames(merged_transpose_deltas)[colnames(merged_transpose_deltas) == "delta.CF2"] <- "Climate Future 2"

# Create list of rownames from absolute values table then add back to deltas table
rownames <- rownames(merged_transpose)
merged_transpose_deltas$RowNames <- rownames
rownames(merged_transpose_deltas) <- merged_transpose_deltas$RowNames
# Remove redundant rownames column
merged_transpose_deltas$RowNames <- NULL

### Write .csv of delta values ----
write.csv(merged_transpose_deltas,paste0(data.dir,"/","Metrics_Table_Deltas.csv"),row.names=TRUE)


### read in Deltas from all GCMs and combine to output results needed in-line
columns <- c("CF","FullRangeTmean","FullRangePr","FullRangePr%","CFTmean","CFPr","CFPr%")
report.output = data.frame(matrix(nrow = 2, ncol = length(columns))) 
colnames(report.output) = columns
report.output$CF <- CFs

all.deltas <- read.csv(paste0(data.dir,"/Deltas.csv")) # Read in deltas file for all CFs
report.output$FullRangeTmean[1] <- min(all.deltas$Tmean_F) 
report.output$FullRangeTmean[2] <- max(all.deltas$Tmean_F)
report.output$FullRangePr[1] <- min(all.deltas$Precip_in)*30
report.output$FullRangePr[2] <- max(all.deltas$Precip_in)*30
report.output$`FullRangePr%`[1] <- (min(all.deltas$Precip_in)/merged$Annual.precipIn[1])*100
report.output$`FullRangePr%`[2] <- (max(all.deltas$Precip_in)/merged$Annual.precipIn[1])*100
report.output$CFTmean <- as.vector(t(merged_transpose_deltas[2,2:3])) # #s differ from full range
report.output$CFPr <- as.vector(t(merged_transpose_deltas[1,2:3])) # #s differ from full range
report.output$`CFPr%` <- (report.output$CFPr/merged$Annual.precipIn[1])*100

### # SWE table - SWE on, SWE off, mean SWE, peak SWE
swe.data <- read.csv(paste0(data.dir,"/SWE.metrics.csv")) # Read in deltas file for all CFs
swe.aggregate <- aggregate(as.matrix(swe.data[4:8])~CF,swe.data,mean)

swe.delta <- swe.aggregate %>%
  mutate(across(-CF, ~ . - swe.aggregate[swe.aggregate$CF == "Historical", ][[cur_column()]])) |> 
  filter(CF != "Historical") 

report.output <- left_join(report.output,swe.delta,by="CF")
write.csv(report.output,paste0(data.dir,"/","CF_summary_output.csv"))





rm(annual.final, combined, daily.met, daily.met.avg, daily.met.final, data_list, 
   merged, merged_transpose, merged_transpose_deltas, monthly.met, monthly.met.filter, 
   monthly.met.final, delta.CF1, delta.CF2, file_list,  model.scenario, rownames)

