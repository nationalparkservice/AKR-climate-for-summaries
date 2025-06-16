# Need to update so script runs through steps from AnnualPrecipIn.R and writes out csv with full timeseries of monthly data
# Then spits out table to replace Appendix 1 and seasonal figures

rm(list = ls())

SiteID <- "SITK" #*UPDATE*

library(stars);library(dplyr);library(ggplot2);library(ggthemes);library(viridis);library(here);library(ggrepel);library(rlang);library(units)


working.dir <- paste0("C:/Users/arunyon/OneDrive - DOI/AKR-CFs/",SiteID) #*UPDATE*
# working.dir <- paste0("C:/Users/brobb/OneDrive - DOI/Projects/AKR_CFs/AKR-CFs/",SiteID) #*UPDATE*

data.dir <- paste(working.dir, "Data", sep="/")
plot.dir = paste0(working.dir,"/Figures")

historical.period <- as.character(seq(1950,1999,1))
future.period <- as.character(seq(2035,2065,1))
daymet.period <- as.character(seq(1980,2016,1))

projections <- readxl::read_excel(paste0("C:/Users/arunyon/OneDrive - DOI/AKR-CFs/", "AKCFs-Projections-used.xlsx"))
CFs <- c("Climate Future 1", "Climate Future 2")
cols <- c("#6EB2D4","#CA0020")
sid = SiteID
GCMs <- unlist(projections[which(projections$SiteID == sid),][2:3])
CF_GCM <- data.frame(CF=CFs,GCM=GCMs)




###########
## Daymet - read in cropped object and create df with monthly timeseries

cropped_st_grid <- readRDS(paste(data.dir,"cropped_st_Daymet",sep="/"))

grid_var <- list()

for(F in 1:length(cropped_st_grid)){
  s = cropped_st_grid[[F]]
  s %>% mutate(tmean = (tmax + tmin)/2) -> s 
  grid_var[[F]] = s[,,,] #all months
}

grid_var_stars <- Reduce(c, grid_var)
grid_var_stars <- drop_units(grid_var_stars)
grid_var_stars %>% mutate(pcp_in = pcp / 25.4,
                          tmean_f = tmean * 9/5 + 32,
                          tmax_f = tmax * 9/5 + 32,
                          tmin_f = tmin * 9/5 + 32) %>% 
  dplyr::select(pcp_in, tmean_f, tmax_f, tmin_f) -> grid_var_stars

time.period = st_get_dimension_values(grid_var_stars,"time")

df<-data.frame(time = time.period)
for (i in 1:length(time.period)){
  # print(paste0(i, " iteration")) # takes a long time to run so turned this on to make sure it was working. Turned off to increase speed
  t <-st_apply(grid_var_stars[,,,i],1:2,mean)
  df$pcp_in[i] <- mean(t$pcp_in,na.rm=TRUE)
  df$tmean_f[i] <- mean(t$tmean_f,na.rm=TRUE)
  df$tmin_f[i] <- mean(t$tmin_f,na.rm=TRUE)
  df$tmax_f[i] <- mean(t$tmax_f,na.rm=TRUE)
}
df$pcp_in = df$pcp_in * 30

df$month = format(df$time,"%m")

df = df %>% mutate(season = case_when(month %in% c("01", "02", "12") ~ "DJF", 
                                      month %in% c("03", "04", "05") ~ "MAM",
                                      month %in% c("06", "07" ,"08") ~ "JJA",
                                      month %in% c("09", "10", "11") ~ "SON"),
                   year = as.integer(format(df$time, "%Y")))

df$GCM = "Daymet"

# write.csv(df, paste0(data.dir, "gridmet_monthly.csv"))
DF = df



###########
## Future - read in cropped object and create df with monthly timeseries
# No modeled historical included

for (G in 1:length(GCMs)){
  gcm = sub("\\..*", "", GCMs[G])
  rcp = sub('.*\\.', '', GCMs[G])

  cropped_st_fut <- readRDS(paste(data.dir,paste0("cropped_st_fut_",gcm,"_",rcp),sep="/"))
  
  fut_var <- list()
  
  for(F in 1:length(cropped_st_fut)){
    s = cropped_st_fut[[F]]
    s %>% mutate(tmean = (tmax + tmin)/2) -> s 
    fut_var[[F]] = s[,,,] #all months
  }

  fut_var_stars <- Reduce(c, fut_var)
  fut_var_stars <- drop_units(fut_var_stars)
  fut_var_stars %>% mutate(pcp_in = pcp / 25.4,
                            tmean_f = tmean * 9/5 + 32,
                            tmax_f = tmax * 9/5 + 32,
                            tmin_f = tmin * 9/5 + 32) %>% 
    dplyr::select(pcp_in, tmean_f, tmax_f, tmin_f) -> fut_var_stars
  
  
  time.period = st_get_dimension_values(fut_var_stars,"time")
  
  df<-data.frame(time = time.period)
  for (i in 1:length(time.period)){
    # print(paste0(i, " iteration")) # takes a long time to run so turned this on to make sure it was working. Turned off to increase speed
    t <-st_apply(fut_var_stars[,,,i],1:2,mean)
    df$pcp_in[i] <- mean(t$pcp_in,na.rm=TRUE)
    df$tmean_f[i] <- mean(t$tmean_f,na.rm=TRUE)
    df$tmin_f[i] <- mean(t$tmin_f,na.rm=TRUE)
    df$tmax_f[i] <- mean(t$tmax_f,na.rm=TRUE)
  }
  df$pcp_in = df$pcp_in * 30
  
  df$month = format(df$time,"%m")
  
  df = df %>% mutate(season = case_when(month %in% c("01", "02", "12") ~ "DJF", 
                                        month %in% c("03", "04", "05") ~ "MAM",
                                        month %in% c("06", "07" ,"08") ~ "JJA",
                                        month %in% c("09", "10", "11") ~ "SON"),
                     year = as.integer(format(df$time, "%Y")))
  df$GCM = paste(gcm,rcp, sep=".")
  
DF = rbind(DF,df)
  
}

write.csv(DF, paste(data.dir, "monthly_data.csv", sep="/"))




#############
# Appendix 1 table - need to be top row historical (abs) and next 2 rows deltas, cols are ANN, DJF, MAM, JJA, SON
DF = read.csv(paste(data.dir, "monthly_data.csv", sep="/"))

DF = merge(DF, CF_GCM,by="GCM", all=TRUE)
DF$CF[which(is.na(DF$CF))] = "Historical"

col_order = c("CF", "DJF", "MAM", "JJA", "SON")
row_order = c("Historical", "Climate Future 1", "Climate Future 2")


#                 ###################   FUNCTIONS ################

# Format abs table
abs.df <- function(DF, var_name) {
  DF %>%
    aggregate(as.formula(paste(var_name, "~ CF + season")), data = ., FUN = mean) %>% 
    tidyr::pivot_wider(names_from = season, values_from = var_name) %>%
    mutate(CF = factor(CF, levels = row_order)) %>%
    arrange(CF) %>%
    dplyr::select(all_of(col_order))
}

# Delta for CFs but historical remains abs
delta.df.temp <- function(DF){
  DF %>% mutate(ANN = rowMeans(.[2:5])) %>% 
    select(CF, ANN, everything()) %>% 
                mutate(across(c(ANN, DJF, MAM, JJA, SON), 
                         ~ if_else(row_number() == 1, . , . - first(.))),
                across(c(ANN, DJF, MAM, JJA, SON), round, 1)) 
}

#                 ###################        ################


tmin_abs = abs.df(DF=DF, var_name = "tmin_f")
tmin_delta = delta.df.temp(DF = tmin_abs)

tmax_abs = abs.df(DF=DF, var_name = "tmax_f")
tmax_delta = delta.df.temp(DF = tmax_abs)

tmean_abs = abs.df(DF=DF, var_name = "tmean_f")
tmean_delta = delta.df.temp(DF = tmean_abs)

precip_season_total =  aggregate(pcp_in~CF+season+year, DF, sum)
pcp_abs = abs.df(DF = precip_season_total, var_name = "pcp_in")

pcp_delta = pcp_abs %>% mutate(ANN = rowSums(.[2:5])) %>% 
  select(CF, ANN, everything()) %>% 
  mutate(across(c(ANN, DJF, MAM, JJA, SON), 
              ~ if_else(row_number() == 1, . , . - first(.))),
       across(c(ANN, DJF, MAM, JJA, SON), round, 1)) 

# write excel with deltas as each pages for easy report copying
openxlsx::write.xlsx(list("tmean" = tmean_delta, "precip" = pcp_delta,"tmin"= tmin_delta, "tmax" = tmax_delta),
                 file = (paste0(data.dir,"/_Appendix1_data.xlsx")),colNames=TRUE)





#############
# Update seasonal bar graphs
pcp_plot_df = pcp_abs %>% tidyr::pivot_longer(!CF, names_to = "season", values_to = "value")
tmin_plot_df = tmin_abs %>% tidyr::pivot_longer(!CF, names_to = "season", values_to = "value")

barchart = function(data, long.title,yaxis){
  ggplot(data, aes(x=factor(season, levels = col_order), y=value, 
                   fill=factor(CF, levels = c('Historical', 'Climate Future 1', 'Climate Future 2')))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, linetype="solid", color = "black", size = 0.75) +
    theme(axis.text = element_text(size = 18),    # Text size for axis tick mark labels
          axis.title.x = element_text(size = 22),   # Text size and alignment for x-axis label
          axis.title.y = element_text(size = 22),   # Text size and alignment for y-axis label
          plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
          legend.title = element_text(size = 18),  # Text size of legend category labels
          legend.text = element_text(size = 17),  # Text size of legend title
          legend.position = "bottom") +
    labs(title = long.title, 
         x = "Season", 
         y = yaxis,
         fill = "") +
    scale_fill_manual(values = c("Historical" = "grey", "Climate Future 1" = "#6EB2D4", "Climate Future 2" = "#CA0020"))
}

pcp_plot = barchart(data = pcp_plot_df, long.title = "Seasonal total precipitation (in/season); 1979-2016 vs 2035-2065", yaxis = "Total precipitation (in/season)")
ggsave("NEW_seasonal_precipIn_bar_historical.png", plot = pcp_plot, width = 15, height = 9, path = plot.dir,bg="white")


tmin_plot = barchart(data = tmin_plot_df, long.title = "Seasonal minimum temperature (°F); 1979-2016 vs 2035-2065", yaxis = "Minimum temperature (°F)") +
  geom_hline(yintercept=32, linetype="dashed", color = "black", size = 0.75) # add dashed line at freezing

ggsave("NEW_seasonal_tminF_bar_historical.png", plot = tmin_plot, width = 15, height = 9, path = plot.dir,bg="white")


