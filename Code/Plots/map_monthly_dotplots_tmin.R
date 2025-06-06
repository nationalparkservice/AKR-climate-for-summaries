var = "tminF"
long.title = "minimum temperature (\u00B0F)" 
delta.var = "tmin"
scale="inferno"

# shp = wrst # area is wrst

# create CF file lists
rds.ls = list.files(path = dir, pattern = paste0(var,"_"), full.names = TRUE)
CF1.ls = Filter(function(x) grepl(paste(GCMs[1], collapse = "|"), x), rds.ls)
CF2.ls = Filter(function(x) grepl(paste(GCMs[2], collapse = "|"), x), rds.ls)
# CF3.ls = Filter(function(x) grepl(paste(GCMs[3], collapse = "|"), x), rds.ls)

# read in RDS for setting scale limits
# CF1
DJF.cf1 <- readRDS(Filter(function(x) grepl(paste("DJF", collapse = "|"), x), CF1.ls))
MAM.cf1 <- readRDS(Filter(function(x) grepl(paste("MAM", collapse = "|"), x), CF1.ls))
JJA.cf1 <- readRDS(Filter(function(x) grepl(paste("JJA", collapse = "|"), x), CF1.ls))
SON.cf1 <- readRDS(Filter(function(x) grepl(paste("SON", collapse = "|"), x), CF1.ls))
# CF2
DJF.cf2 <- readRDS(Filter(function(x) grepl(paste("DJF", collapse = "|"), x), CF2.ls))
MAM.cf2 <- readRDS(Filter(function(x) grepl(paste("MAM", collapse = "|"), x), CF2.ls))
JJA.cf2 <- readRDS(Filter(function(x) grepl(paste("JJA", collapse = "|"), x), CF2.ls))
SON.cf2 <- readRDS(Filter(function(x) grepl(paste("SON", collapse = "|"), x), CF2.ls))
# # CF3
# DJF.cf3 <- readRDS(Filter(function(x) grepl(paste("DJF", collapse = "|"), x), CF3.ls))
# MAM.cf3 <- readRDS(Filter(function(x) grepl(paste("MAM", collapse = "|"), x), CF3.ls))
# JJA.cf3 <- readRDS(Filter(function(x) grepl(paste("JJA", collapse = "|"), x), CF3.ls))
# SON.cf3 <- readRDS(Filter(function(x) grepl(paste("SON", collapse = "|"), x), CF3.ls))

scale.min = min(c(DJF.cf1$mean,MAM.cf1$mean,JJA.cf1$mean,SON.cf1$mean,
                  DJF.cf2$mean,MAM.cf2$mean,JJA.cf2$mean,SON.cf2$mean),na.rm=TRUE)
                  # DJF.cf3$mean,MAM.cf3$mean,JJA.cf3$mean,SON.cf2$mean),na.rm=TRUE)
scale.max = max(c(DJF.cf1$mean,MAM.cf1$mean,JJA.cf1$mean,SON.cf1$mean,
                  DJF.cf2$mean,MAM.cf2$mean,JJA.cf2$mean,SON.cf2$mean),na.rm=TRUE)
                  # DJF.cf3$mean,MAM.cf3$mean,JJA.cf3$mean,SON.cf2$mean),na.rm=TRUE)

# ggplot
map.plot <- function(data, title,xaxis,metric,col){
  ggplot() + 
    geom_raster(data = ak_df ,aes(x = x, y = y,alpha=HYP_HR_SR_W_1), show.legend=FALSE) +
    geom_stars(data = data, alpha = 0.8) + 
    geom_sf(data = shp, aes(), fill = NA) + 
    scale_fill_viridis(direction=1, option = scale, 
                       limits = c(scale.min, scale.max), oob = scales::squish) + 
    labs(title = title) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          # plot.title=element_blank(),
          plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, size=5),
          plot.margin = unit(c(.5,0,0,0), "cm")) + 
    labs(fill = metric)
}

# CF1
DJF.cf1.plot <- map.plot(data=DJF.cf1, title=CFs[1],metric=paste0("Average ",long.title),col=CF_GCM$CF_col[1])
MAM.cf1.plot <- map.plot(data=MAM.cf1, title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[1])
JJA.cf1.plot <- map.plot(data=JJA.cf1, title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[1])
SON.cf1.plot <- map.plot(data=SON.cf1, title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[1])

# CF2
DJF.cf2.plot <- map.plot(data=DJF.cf2, title=CFs[2],metric=paste0("Average ",long.title),col=CF_GCM$CF_col[2])
MAM.cf2.plot <- map.plot(data=MAM.cf2, title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[2])
JJA.cf2.plot <- map.plot(data=JJA.cf2, title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[2])
SON.cf2.plot <- map.plot(data=SON.cf2, title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[2])

# # CF3
# DJF.cf3.plot <- map.plot(data=JJA.cf3, title=CFs[3],metric=paste0("Average ",long.title),col=CF_GCM$CF_col[3])
# MAM.cf3.plot <- map.plot(data=MAM.cf3, title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[3])
# JJA.cf3.plot <- map.plot(data=JJA.cf3, title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[3])
# SON.cf3.plot <- map.plot(data=SON.cf3, title="",metric=paste0("Average ",long.title),col=CF_GCM$CF_col[3])

###########################################################
##### NONE OF THESE WORK ##################################
# Merge into one plot

maps.all <- grid_arrange_shared_legend(DJF.cf1.plot,DJF.cf2.plot, # THIS WORKS, BUT WANT TO HAVE SHARED RC TITLES
                                       MAM.cf1.plot,MAM.cf2.plot,    # FOR CF AND SEASON
                                       JJA.cf1.plot,JJA.cf2.plot,
                                       SON.cf1.plot,SON.cf2.plot,
                                       nrow = 4,ncol=2, position = "bottom") 

djf <- textGrob("DJF", gp = gpar(fontface="bold", size = 12))
mam <- textGrob("MAM", gp = gpar(fontface="bold", size = 12))
jja <- textGrob("JJA", gp = gpar(fontface="bold", size = 12))
son <- textGrob("SON", gp = gpar(fontface="bold", size = 12))

seasons <- grid.arrange(djf,mam,jja,son,ncol=1)

maps <- grid.arrange(seasons,maps.all,ncol = 2, widths = c(1,15))


################################### MONTHLY DOT PLOT ##################

dotplot <- ggplot(delta,aes(x=(eval(parse(text=delta.var))),y=season,fill=CF)) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") + 
  geom_point(stat="identity",size=8,colour="black",aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),    #Text size for axis tick mark labels
        axis.title.x=element_blank(),               #Text size and alignment for x-axis label
        plot.title=element_blank(),
        # axis.title.y=element_text(size=16, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
        # plot.title=element_text(size=20, vjust=0.5, face="bold", margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
        legend.title=element_text(size=16),                                                                    #Text size of legend category labels
        legend.text=element_text(size=14),                                                                   #Text size of legend title
        legend.position = "bottom")  +
  labs(title = paste0("Change in seasonal ",long.title), 
       x = "Change (°F)", y = "") +
  scale_fill_manual(name="",values =cols) +
  scale_shape_manual(name="",values = c(21,22,23)) +
  scale_y_discrete(limits=rev)
dotplot

g <- grid.arrange(maps, dotplot, ncol = 2, widths = c(6, 4), clip = FALSE)

ann <- annotate_figure(g, top = text_grob(paste0("Change in seasonal ", long.title), 
                                      face = "bold", size = 20))

ggsave(paste0("seasonal_",var,".png"), plot = ann, width = 15, height = 9, path = plot.dir,bg="white")


################################### MONTHLY BAR CHART ##################

# Seasons as groups

## Without historical data

barchart <- ggplot(df.fut[-c(9:12),], aes(x=season,y=(eval(parse(text=delta.var))),fill=CF)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept=32, linetype="dashed", color = "black", size = 0.75) +
  geom_hline(yintercept = 0, linetype="solid", color = "black", size = 0.75) +
  theme(axis.text=element_text(size=18),    #Text size for axis tick mark labels
        axis.title.x=element_text(size=22),   #Text size and alignment for x-axis label
        axis.title.y=element_text(size=22),   #Text size and alignment for y-axis label
        plot.title=element_blank(),
        legend.title=element_text(size=18),   #Text size of legend category labels
        legend.text=element_text(size=17),   #Text size of legend title
        legend.position = "bottom")  +
  labs(title = paste0("Seasonal ",long.title), 
       x = "Season", 
       y = "Minimum temperature (°F)",
       fill = "") +
  scale_fill_manual(name="",values = cols) #+
barchart

ann <- annotate_figure(barchart, top = text_grob(paste0("Seasonal ",long.title, "; 1979-2016 vs 2035-2065"),
                                          face = "bold", size = 22))

ggsave(paste0("seasonal_",var,"_bar.png"), plot = ann, width = 15, height = 9, path = plot.dir,bg="white")

## With historical data

# Drop duplicative df.hist rows
df.hist.2 <- df.hist[1:(nrow(df.hist) - 4), ]
# Combine df.hist and df.fut data
df.hist.fut <- rbind(df.hist.2,df.fut)

barchart <- ggplot(df.hist.fut, aes(x=factor(season, levels = c('DJF', 'MAM', 'JJA', 'SON')), y=(eval(parse(text=delta.var))), fill=factor(CF, levels = c('Historical', 'Climate Future 1', 'Climate Future 2')))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 32, linetype = "dashed", color = "black", size = 0.75) +
  geom_hline(yintercept = 0, linetype="solid", color = "black", size = 0.75) +
  theme(axis.text = element_text(size = 18),    # Text size for axis tick mark labels
        axis.title.x = element_text(size = 22),   # Text size and alignment for x-axis label
        axis.title.y = element_text(size = 22),   # Text size and alignment for y-axis label
        plot.title = element_blank(),
        legend.title = element_text(size = 18),  # Text size of legend category labels
        legend.text = element_text(size = 17),  # Text size of legend title
        legend.position = "bottom") +
  labs(title = paste0("Seasonal ", long.title), 
       x = "Season", 
       y = "Minimum temperature (°F)",
       fill = "") +
  scale_fill_manual(values = c("Historical" = "grey", "Climate Future 1" = "#6EB2D4", "Climate Future 2" = "#CA0020"))
barchart

ann <- annotate_figure(barchart, top = text_grob(paste0("Seasonal ",long.title, "; 1979-2016 vs 2035-2065"),
                                          face = "bold", size = 22))

ggsave(paste0("seasonal_",var,"_bar_historical.png"), plot = ann, width = 15, height = 9, path = plot.dir,bg="white")

rm(df.hist.2)
