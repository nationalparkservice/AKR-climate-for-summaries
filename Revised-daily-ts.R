# Revise ts plot of max temp above 32 F because metric is W but plot says annual
# Metric counts days below but plot is days above by subtracting by 90, creating leap year issue

library(dplyr);library(ggplot2);library(ggthemes);library(viridis);library(here); library(ggpubr)


rm(list = ls())

SiteID <- "YUCH" #*UPDATE*


working.dir <- paste0("C:/Users/arunyon/OneDrive - DOI/AKR-CFs/",SiteID) #*UPDATE*
# working.dir <- paste0("C:/Users/brobb/OneDrive - DOI/Projects/AKR_CFs/AKR-CFs/",SiteID) #*UPDATE*

data.dir <- paste(working.dir, "Data", sep="/")
plot.dir = paste0(working.dir,"/Figures")

projections <- readxl::read_excel(paste0("C:/Users/arunyon/OneDrive - DOI/AKR-CFs/", "AKCFs-Projections-used.xlsx"))
CFs <- c("Climate Future 1", "Climate Future 2")
cols <- c("#6EB2D4","#CA0020")
sid = SiteID
GCMs <- unlist(projections[which(projections$SiteID == sid),][2:3])
CF_GCM <- data.frame(CF=CFs,GCM=GCMs)


# Wrangle data
DF = read.csv(paste0(data.dir,"/Daily_met.csv"))

head(df)
df = merge(DF, CF_GCM,by="GCM",all=TRUE)
df$CF[which(is.na((df$CF)))] = "Historical"
# df$CF_col[which(is.na((df$CF_col)))] = "grey"
df$CF = factor(df$CF, levels=c("Historical",CFs))
df$Year = as.Date(df$year, format="%Y-%m-%d")
df = subset(df, Year!="2017-08-13")
# df$W.under32 = 90 - df$W.under32


                          ###############################
                                  # PLOT FUNCTION
ts.plot <- function(data, var, y_lab, title){
  ggplot(data=data, aes(x=Year, y=eval(parse(text=var)), group=CF, colour = CF)) +
    
    geom_line(colour = "black",size=2.5, stat = "identity") +
    geom_line(size = 2, stat = "identity") +
    geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
    theme(axis.text=element_text(size=16),
          # axis.text.x=element_blank(),
          axis.title.x=element_text(size=16,vjust=1.0),
          axis.title.y=element_text(size=16,vjust=1.0),
          plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
          legend.text=element_text(size=14), legend.title=element_text(size=14),
          legend.position = "bottom") +
    labs(title = title, # If putting all 5 plots together, use: title = ""; textGrob below adds titles - If doing plots separately, use: title = "Annual threshold exceedances (days/year)"
         x = "Year", y = y_lab) +
    scale_color_manual(name="",values = c("grey",cols)) +
    scale_fill_manual(name="",values = c("grey",cols)) +
    scale_shape_manual(name="",values = c(21,22,23,24)) 
  # coord_fixed(ratio = .5)
}

                           ###############################

W.under32 = ts.plot(data=df,var="W.under32",y_lab= "Tmin ≤ 32 (\u00B0F) in DJF", title = "Winter (DJF) days maximum temperature below 32 (°F)")

ggsave(paste0("NEW-W-under32.png"), plot = W.under32, width = 10, height = 5, path = plot.dir, bg = "white")
