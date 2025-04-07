## Written by Brecken Robb - January 2025
## Purpose is to create and export climate futures metrics table
## Data comes from CCRP CFs and Northern Climate Reports (NCR)

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

## NCR ----
# Read in NCR park unit identifier .csv to pull data from correct SiteID
NCR_ID <- read.csv("./Data/tables/NCR_NPS_SiteIDs.csv")

# Read the .csv data from the URL
Indicators_csv <- NCR_ID %>%
  filter(ParkCode == SiteID) %>%
  pull(Indicators_csv)

if (length(Indicators_csv) == 0) {
  stop("SiteID not found in the dataset.")
} else {
  NCR.indicators <- fread(Indicators_csv) # add 'skip = 13' to exclude the first 13 rows of metadata if it doesn't do so already

  # Print the data
  print(head(NCR.indicators))
}

###############################################################################

# Transform data ----

## CCRP ----

## "_ANN.csv" files
# reduce replicate Year and GCM rows
annual <- combined %>%
  group_by(Year, GCM) %>%
  summarize(across(everything(), ~ first(na.omit(.))), .groups = "drop")

# Average by GCM
annual.avg <- annual %>%
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

## NCR data ----

# Create model + scenario column
model.scenario <- paste(NCR.indicators$model, NCR.indicators$scenario, sep = ".")
# Revert Daymet model + scenario text to "Daymet" after the column merge
model.scenario <- gsub("Daymet.historical", "Daymet", model.scenario)

# Join new column with df
NCR.indicators$model.scenario <- model.scenario
# Rename CCSM4 model
NCR.indicators$model.scenario <- gsub("NCAR-CCSM4", "CCSM4", NCR.indicators$model.scenario)
# Rename column name to match other data frames
colnames(NCR.indicators)[colnames(NCR.indicators) == "model.scenario"] <- "GCM"

# Drop "model" and "scenario" columns
NCR.indicators[, c("model", "scenario") := NULL]

# Drop unnecessary rows ("longterm" (end of century) in era, wrong scenarios)
NCR.indicators <- NCR.indicators[NCR.indicators$era != "longterm", ]
NCR.indicators <- NCR.indicators[NCR.indicators$GCM != "MRI-CGCM3.rcp85", ]
NCR.indicators <- NCR.indicators[NCR.indicators$GCM != "CCSM4.rcp45", ]

# Drop "era" column
NCR.indicators[, c("era") := NULL]

# Convert data structure from long to wide to match prior data frames
NCR.indicators.final <- NCR.indicators %>%
  pivot_wider(names_from = indicator, values_from = c(min, mean, max))

###############################################################################

# Join all data frames and export ----

# Merge all data frames
merged <- merge(annual.final, monthly.met.final, by = c("GCM")) %>%
  merge(daily.met.final, by = "GCM") %>%
  merge(NCR.indicators.final, by = "GCM")

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

rm(annual.final, combined, daily.met, daily.met.avg, daily.met.final, data_list, 
   merged, merged_transpose, merged_transpose_deltas, monthly.met, monthly.met.filter, 
   monthly.met.final, NCR_ID, NCR.indicators, NCR.indicators.final, delta.CF1, 
   delta.CF2, file_list, Indicators_csv, model.scenario, rownames)

