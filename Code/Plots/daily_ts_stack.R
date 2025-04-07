long.names = c("Freeze thaw cycles", "Tmin < 32 (\u00B0F)", "Tmax > 68 (\u00B0F)",
               "Tmin > 32 (\u00B0F) in DJF", "Precipitation > 0.5 inches","Cumulative growing degrees (\u00B0F)")
ratios = c(1,1,1,1,1)

head(df)
df = merge(DF, CF_GCM,by="GCM",all=TRUE)
df$CF[which(is.na((df$CF)))] = "Historical"
df$CF_col[which(is.na((df$CF_col)))] = "grey"
df$CF = factor(df$CF, levels=c("Historical",CFs))
df$Year = as.Date(df$year, format="%Y-%m-%d")
df = subset(df, Year!="2017-08-13")
df$W.under32 = 90 - df$W.under32

means = df %>% group_by(CF) %>%
  summarize(mfreeze.thaw = mean(freeze.thaw), 
            munder32 = mean(under32),
            mover20 = mean(over20),
            mW.under32 = mean(W.under32),
            mpcp.over.5 = mean(pcp.over.5))

#### Time series plots
# Function
ts.plot <- function(data, var, title){
ggplot(data=data, aes(x=Year, y=eval(parse(text=var)), group=CF, colour = CF)) +
  
  geom_line(colour = "black",size=2.5, stat = "identity") +
  geom_line(size = 2, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  theme(axis.text=element_text(size=16),
        # axis.text.x=element_blank(),
        axis.title.x=element_text(size=16,vjust=1.0),
        axis.title.y=element_text(size=16,vjust=1.0),
        plot.title = element_blank(), # If putting all 5 plots together, use: "plot.title = element_blank()" - If doing plots separately, use: plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.position = "bottom") +
  labs(title = "", # If putting all 5 plots together, use: title = ""; textGrob below adds titles - If doing plots separately, use: title = "Annual threshold exceedances (days/year)"
       x = "Year", y = title) +
  scale_color_manual(name="",values = c("grey",cols)) +
  scale_fill_manual(name="",values = c("grey",cols)) +
  scale_shape_manual(name="",values = c(21,22,23,24)) 
  # coord_fixed(ratio = .5)
}

# Individual variable ts plots
freeze.thaw = ts.plot(data=df,var="freeze.thaw",title=long.names[1])
ggsave(paste0("ts-stack-freeze-thaw.png"), plot = freeze.thaw, width = 10, height = 5, path = plot.dir, bg = "white")

under32 = ts.plot(data=df,var="under32",title=long.names[2])
ggsave(paste0("ts-stack-under32.png"), plot = under32, width = 10, height = 5, path = plot.dir, bg = "white")

over20 = ts.plot(data=df,var="over20",title=long.names[3])
ggsave(paste0("ts-stack-over20.png"), plot = over20, width = 10, height = 5, path = plot.dir, bg = "white")

W.under32 = ts.plot(data=df,var="W.under32",title=long.names[4])
ggsave(paste0("ts-stack-W-under32.png"), plot = W.under32, width = 10, height = 5, path = plot.dir, bg = "white")

pcp.over.5 = ts.plot(data=df,var="pcp.over.5",title=long.names[5])
ggsave(paste0("ts-stack-pcp-over-5.png"), plot = pcp.over.5, width = 10, height = 5, path = plot.dir, bg = "white")

gdd = ts.plot(data=df,var="GDD",title=long.names[6])
ggsave(paste0("ts-stack-gdd.png"), plot = gdd, width = 10, height = 5, path = plot.dir, bg = "white")


#### Create grids for plot arrangement
grid1 <- grid_arrange_shared_legend(freeze.thaw,under32,over20, ncol = 1, nrow = 3, position = "bottom", 
                                   top = textGrob(paste0("Annual threshold exceedances for ",SiteID, " (days/year)"),
                                                  gp=gpar(fontface="bold", col="black", fontsize=16)))

grid2 <- grid_arrange_shared_legend(W.under32,pcp.over.5, ncol = 1, nrow = 2, position = "bottom", 
                                    top = textGrob(paste0("Annual threshold exceedances for ",SiteID, " (days/year)"),
                                                   gp=gpar(fontface="bold", col="black", fontsize=16)))


#### Delta data frames
delta.var <- data.frame(means)
names(delta.var) = c("CF","freeze-thaw", "tmin<32", "tmax>68","DJF>32","prcp>0.5")
for (i in 1:3){
  delta.var[i,2:6] = delta.var[i,2:6] - delta.var[4,2:6]
}
delta.var[,2:6] <- signif(delta.var[,2:6], digits = 1)


#### Final plot arrangement
g <- ggarrange(grid1, grid2, ncol=2)
g

ggsave(paste0("ts-stack.png"), plot = g, width = 15, height = 9, path = plot.dir, bg = "white")


