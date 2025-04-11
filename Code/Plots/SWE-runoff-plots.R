# runoff$name="runoff"; runoff$X=NULL;names(runoff)[2]="var"
# SWE$name="SWE"; SWE$X=NULL;names(SWE)[2]="var"

df = merge(SWE,runoff,by=c("time","GCM"))

df = merge(df, CF_GCM,by="GCM",all=TRUE)
df$CF[which(is.na((df$CF)))] = "Historical"
df$CF_col[which(is.na((df$CF_col)))] = "grey"
df$CF = factor(df$CF, levels=c(CFs,"Historical"))
df$date = as.Date(df$time, format="%Y-%m-%d")
df$Year = as.factor(substr(df$time, 1, 4))
df$yday = yday(df$date)


#### Runoff plot
runoff.plot <- function(data, col,CF){  
ggplot() +
  geom_line(data=data,aes(x=yday,y=RUNOFF_in,group=Year),colour=col,size=.7) +
    geom_vline(xintercept=91, linetype="dashed", color = "black") +
    geom_text(aes(x=91, label="Apr 1\n", y=max(df$RUNOFF)), 
              colour="black", angle=90, text=element_text(size=11),hjust=1) +
    geom_vline(xintercept=274, linetype="dashed", color = "black") +
    geom_text(aes(x=274, label="\nOct 1", y=max(df$RUNOFF)), 
              colour="black", angle=90, text=element_text(size=11),hjust=1) +
  theme(axis.text=element_text(size=16),
        # axis.text.x=element_blank(),
        axis.title.x=element_text(size=16,vjust=1.0),
        axis.title.y=element_text(size=16,vjust=1.0),
        plot.title=element_blank(),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.position = "bottom") +
  labs(title = "", 
       x = "", y = CF) +
  ylim(0,max(df$RUNOFF_in))
  # coord_fixed(ratio = .5)
}

CF1.runoff = runoff.plot(data=subset(df, CF == CFs[1]),col=cols[1],CF=CFs[1])
CF1.runoff
CF2.runoff = runoff.plot(data=subset(df, CF == CFs[2]),col=cols[2],CF=CFs[2])
CF2.runoff
# CF3.runoff = runoff.plot(data=subset(df, CF == CFs[3]),col=cols[3],CF=CFs[3])
Hist.runoff = runoff.plot(data=subset(df, CF == "Historical"),col="grey",CF="Historical")
Hist.runoff


### SWE plot
SWE.plot <- function(data, col,CF){  
  ggplot() +
    geom_line(data=data,aes(x=yday,y=SWE_in,group=Year),colour=col,size=.7) +
    geom_vline(xintercept=91, linetype="dashed", color = "black") +
    geom_text(aes(x=91, label="Apr 1\n", y=max(df$SWE_in)), 
              colour="black", angle=90, text=element_text(size=11),hjust=1) +
    geom_vline(xintercept=274, linetype="dashed", color = "black") +
    geom_text(aes(x=274, label="\nOct 1", y=max(df$SWE_in)), 
              colour="black", angle=90, text=element_text(size=11),hjust=1) +
    theme(axis.text=element_text(size=16),
          # axis.text.x=element_blank(),
          axis.title.x=element_text(size=16,vjust=1.0),
          axis.title.y=element_text(size=16,vjust=1.0),
          plot.title=element_blank(),
          legend.text=element_text(size=14), legend.title=element_text(size=14),
          legend.position = "bottom",
          plot.margin = margin(t=10, r = 10, b = 10, l = 10),) + # for grid2 only plot
    labs(title = "", 
         x = "", y = CF) + # y = CF for grid2 only plot
    ylim(0,max(df$SWE_in))
  # coord_fixed(ratio = .5)
}

CF1.SWE = SWE.plot(data=subset(df, CF == CFs[1]),col=cols[1],CF=CFs[1])
CF2.SWE = SWE.plot(data=subset(df, CF == CFs[2]),col=cols[2],CF=CFs[2])
# CF3.SWE = SWE.plot(data=subset(df, CF == CFs[3]),col=cols[3],CF=CFs[3])
Hist.SWE = SWE.plot(data=subset(df, CF == "Historical"),col="grey",CF="Historical")


#### Runoff and SWE plots
# Runoff gridded plot
grid1 <- ggarrange(Hist.runoff, CF1.runoff, CF2.runoff, ncol = 1, nrow = 3) # CF3.runoff)

grid1 = annotate_figure(grid1, left = textGrob("Runoff (in)", rot = 90, vjust = 0.5, 
                                               gp = gpar(cex = 1.3)), # Changed for more white space around y-axis title; originally vjust = 1
                        bottom = textGrob("Julian day", gp = gpar(cex = 1.3)),
                        top = textGrob("Daily runoff for each climate future",
                                       gp=gpar(fontface="bold", col="black", fontsize=16)))
grid1
ggsave(paste0("SWE-runoff-only.png"), plot = grid1, width = 7, height = 8, path = plot.dir, bg = "white")

# SWE gridded plot
grid2 <- ggarrange(Hist.SWE, CF1.SWE, CF2.SWE, ncol = 1, nrow = 3) #CF3.SWE

grid2 =  annotate_figure(grid2, left = textGrob("SWE (in)", rot = 90, vjust = 0.5, 
                                                gp = gpar(cex = 1.3)), # Changed for more white space around y-axis title; originally vjust = 1
                        bottom = textGrob("Julian day", gp = gpar(cex = 1.3)),
                        top = textGrob("Daily SWE for each climate future",
                                       gp=gpar(fontface="bold", col="black", fontsize=16)))
grid2
ggsave(paste0("SWE-runoff-daily-only.png"), plot = grid2, width = 7, height = 8, path = plot.dir, bg = "white")

#### Final plot arrangement
grid = ggarrange(grid1,grid2,nrow=1,ncol=2) 
annotate_figure(grid, top = textGrob(SiteID,
                                      gp=gpar(fontface="bold", col="black", fontsize=20)))

ggsave(paste0("SWE-runoff.png"), plot = grid, width = 15, height = 9, path = plot.dir, bg = "white")


#### Calculate snow metrics

max.swe <- df %>%
  group_by(CF, CF_col, Year) %>%
  summarize(max_SWE_in = max(SWE_in, na.rm = TRUE),
            .groups = "drop")

mean.swe <- df %>%
  group_by(CF, CF_col, Year) %>%
  summarize(mean_SWE_in = mean(SWE_in, na.rm = TRUE),
            .groups = "drop")

snow.on <- df %>%
  filter(SWE_in == 0) %>%
  group_by(CF, CF_col, Year) %>%
  summarize(first.day = min(yday),
            .groups = "drop")

snow.off <- df %>%
  filter(SWE_in == 0) %>%
  group_by(CF, CF_col, Year) %>%
  summarize(
    last.day = max(yday),
    subsequent.day = max(yday) + 1,
    .groups = "drop")

SWE.metrics <- max.swe %>%
  left_join(mean.swe, by = c("CF", "CF_col", "Year")) %>%
  left_join(snow.on, by = c("CF", "CF_col", "Year")) %>%
  left_join(snow.off, by = c("CF", "CF_col", "Year"))

write.csv(SWE.metrics, paste0(data.dir,"/SWE.metrics.csv"), row.names=FALSE)

max.swe.mean <- max.swe %>%
  group_by(CF,CF_col) %>%
  summarize(mean_max_SWE_in = mean(max_SWE_in, na.rm = TRUE),
            se_max_SWE_in = sd(max_SWE_in, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

max.swe.mean$CF <- factor(max.swe.mean$CF, levels = c('Historical', 'Climate Future 1', 'Climate Future 2'))


#### Bar chart

barchart <- ggplot(max.swe.mean, aes(x=factor(CF, levels = c('Historical', 'Climate Future 1', 'Climate Future 2')), 
                               y=mean_max_SWE_in, 
                               fill=factor(CF, levels = c('Historical', 'Climate Future 1', 'Climate Future 2')))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype="solid", color = "black", size = 0.75) +
  theme(axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        legend.position = "none") +
  labs(title = "Average maximum annual snow depth (SWE) for 2050", 
       x = "", 
       y = "Maximum SWE (in)",
       fill = "") +
  scale_fill_manual(values = c("Historical" = "grey", "Climate Future 1" = "#6EB2D4", "Climate Future 2" = "#CA0020"))
barchart

# ggsave(paste0("max.SWE_barchart.png"),plot = barchart,width = 15,height = 9,path = plot.dir,bg="white")


#### Box plot

# There are 2 outliers in Climate Future 2 that show up twice because they are represented in
# both the geom_boxplot argument and geom_point argument - can't remember how I (BCR) resolved
# this in previous script before it was erased, but for sake of time since we aren't using
# this plot I am ignoring it
boxplot <- ggplot(max.swe, aes(x = factor(CF, levels = c('Historical', 'Climate Future 1', 'Climate Future 2')), 
                               y = max_SWE_in, 
                               fill = factor(CF, levels = c('Historical', 'Climate Future 1', 'Climate Future 2')))) +
  geom_boxplot(stat = "boxplot", position = "dodge", color = "black") +
  geom_point(stat = "identity", position = position_jitter(width = 0.2), shape = 21, size = 6) +
  theme(axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        legend.position = "none") +
  labs(title = "Maximum annual snow depth (SWE) for 2050", 
       x = "", 
       y = "Maximum SWE (in)",
       fill = "") +
  scale_colour_manual(values = c("grey" = "grey", 
                                 "#6EB2D4" = "#6EB2D4", 
                                 "#CA0020" = "#CA0020")) +
  scale_fill_manual(values = c("Historical" = "grey", 
                               "Climate Future 1" = "#6EB2D4", 
                               "Climate Future 2" = "#CA0020"))
boxplot

# ggsave(paste0("max.SWE_boxplot.png"),plot = boxplot,width = 15,height = 9,path = plot.dir,bg="white")


#### Jitter plot

jitterplot <- ggplot(max.swe, aes(x=factor(CF, levels = c('Historical', 'Climate Future 1', 'Climate Future 2')), 
                                  y=max_SWE_in, 
                                  colour=factor(CF_col, levels = c("grey","#6EB2D4","#CA0020")))) +
  geom_point(stat = "identity", position = position_jitter(width = 0.15), size = 10, shape = 21, 
             color = "black",
             aes(fill = factor(CF_col))) +
  geom_segment(data = max.swe.mean, 
               aes(x = as.numeric(CF) - 0.3, 
                   xend = as.numeric(CF) + 0.3, 
                   y = mean_max_SWE_in, 
                   yend = mean_max_SWE_in, 
                   color = "black",
                   width = 0.2), 
               size = 2) +
  theme(axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        legend.position = "none") +
  labs(title = "Maximum annual snow depth (SWE) for 2050", 
       x = "", 
       y = "Maximum SWE (in)",
       fill = "") +
  scale_fill_manual(values = c("grey" = "grey", 
                               "#6EB2D4" = "#6EB2D4", 
                               "#CA0020" = "#CA0020")) +
  
  scale_color_manual(values = c("black" = "black"))
jitterplot

ggsave(paste0("max.SWE_jitterplot.png"),plot = jitterplot,width = 15,height = 9,path = plot.dir,bg="white")

rm(max.swe,mean.swe,snow.on,snow.off,max.swe.mean,barchart,boxplot,jitterplot)
