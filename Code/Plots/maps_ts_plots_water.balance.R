var = "water.balance"
long.title = "total annual water balance (in/year)" 
scale="inferno" # scale="viridis" originally, but changed to align dry / wet values more closely will colors (e.g., cool colors = wet, warm colors = dry)
# ratio= 0.9 #aspect ratio for ts - was originally 0.3 but BCR changed to make ts only graphs look nicer

# shp = wrst # area is wrst

# create CF file lists
rds.ls = list.files(path = dir, pattern = paste0(var,"_"), full.names = TRUE)
CF1.ls = Filter(function(x) grepl(paste(GCMs[1], collapse = "|"), x), rds.ls)
CF2.ls = Filter(function(x) grepl(paste(GCMs[2], collapse = "|"), x), rds.ls)
# CF3.ls = Filter(function(x) grepl(paste(GCMs[3], collapse = "|"), x), rds.ls)

# Generate sample data for ts plot
df = read.csv(Filter(function(x) grepl(paste(".csv", collapse = "|"), x), rds.ls))
df = merge(df, CF_GCM,by="GCM",all=TRUE)
df$CF[which(is.na((df$CF)))] = "Historical"
df$CF_col[which(is.na((df$CF_col)))] = "grey"
df$CF = factor(df$CF, levels=c("Historical",CFs))
df = subset(df, Year != 1980)

means <- df %>% group_by(CF) %>%
  summarize(var = mean(eval(parse(text=var)))) 

# read in RDS for setting scale limits
cf1 <- readRDS(CF1.ls)
cf2 <- readRDS(CF2.ls)
# cf3 <- readRDS(CF3.ls)

scale.min = min(c(cf1$mean,cf2$mean),na.rm=TRUE)
scale.max = max(c(cf1$mean,cf2$mean),na.rm=TRUE)


#### Maps plot
map.plot <- function(data,title,xaxis,metric,col){
  ggplot() + 
    geom_raster(data = ak_df ,aes(x = x, y = y,alpha=HYP_HR_SR_W_1), show.legend=FALSE) +
    geom_stars(data = data, alpha = 0.8) + 
    geom_sf(data = shp, aes(), fill = NA) + 
    scale_fill_viridis(direction=1, option = scale,
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5),
                       limits = c(scale.min, scale.max), oob = scales::squish) + #mako for WB delta
    labs(title = title) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          plot.title=element_text(size=14,face="bold",hjust=0.5,margin=margin(t=10)), # added margin argument to lower CF1/2 text above maps
          plot.background = element_rect(colour = col, fill=NA, size=5),
          legend.title=element_text(size=11),
          legend.text=element_text(size=10)) +
    labs(fill = paste0("Change in total annual water balance (in/year)"))
}

cf1.plot <- map.plot(data=readRDS(CF1.ls),title=CFs[1],metric=paste0("Change in total annual water balance (in/year)"),col=cols[1])
cf2.plot <- map.plot(data=readRDS(CF2.ls),title=CFs[2],metric=paste0("Change in total annual water balance (in/year)"),col=cols[2])
# cf3.plot <- map.plot(data=readRDS(CF3.ls),title=CFs[3],metric=long.title,col=cols[3])

maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,  ncol = 2, nrow = 1, position = "bottom",
                                   top = textGrob(paste0("Change in total annual water balance (in/year)"),
                                                  gp=gpar(fontface="bold", col="black", fontsize=16)))

ggsave(paste0(var,"_ANN_maps.png"), plot = maps, width = 15, height = 9, scale = 0.65, path = plot.dir,bg="white")


#### Time series plot
ts <- ggplot(df, aes(x=Year, y=(eval(parse(text=var))), group=CF, colour = CF)) +
  
  geom_line(colour = "black",size=2.5, stat = "identity") +
  geom_line(size = 2, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=20),
        # axis.text.x=element_blank(),
        axis.title.x=element_text(size=20,vjust=1.0),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_blank(),
        legend.text=element_text(size=18), legend.title=element_text(size=14),
        legend.position = "bottom") +
  labs(title = paste0("Change in annual ",long.title), 
       x = "Year", y = "Total annual water balance (in/year)") +
  scale_color_manual(name="",values = c("grey",cols)) +
  scale_fill_manual(name="",values = c("grey",cols)) +
  scale_shape_manual(name="",values = c(21,22,23, 24))
  # coord_fixed(ratio = ratio) 
ts

ggsave(paste0(var,"_ANN_ts.png"), plot = ts, width = 15, height = 7, scale = 1, path = plot.dir,bg="white")


#### Maps and ts plot
g <- ggarrange(maps, ts, nrow=2)
g

# ggsave(paste0(var,"_ANN_maps_ts.png"), plot = g, width = 15, height = 5.25, scale = 1, path = plot.dir,bg="white")


#### Maps, ts, and table plot
delta.var <- means
delta.var$var[1:2] <- delta.var$var[1:2] - delta.var$var[3]
delta.var$var <- signif(delta.var$var, digits = 1)

table <- tableGrob(delta.var, rows = NULL, cols=NULL)
table <- annotate_figure(table,
                top = text_grob("Historical = absolute value \n CFs = change values", color = "black",
                                 face = "italic", size = 12))
# ggsave(paste0(var,"_ANN_ts_table.png"), plot = table, width = 15, height = 5, scale = 1, path = plot.dir,bg="white")

tsplots <- grid.arrange(ts, table, ncol = 2, widths = c(4,1), clip = FALSE)
# ggsave(paste0(var,"_ANN_ts_plots.png"), plot = tsplots, width = 15, height = 5, scale = 1, path = plot.dir,bg="white")

g <- ggarrange(maps, tsplots, nrow=2)
g

ggsave(paste0(var,"_ANN.png"), width = 15, height = 9, path = plot.dir,bg="white")

