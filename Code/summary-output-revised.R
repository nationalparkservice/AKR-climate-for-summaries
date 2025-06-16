# Script to output data needed to revise thresholds table

library(dplyr)

rm(list=ls())

SiteID <- "SITK"

data <- read.csv(paste0("C:/Users/arunyon/OneDrive - DOI/AKR-CFs/", SiteID, "/Data/Metrics_Table_Deltas.csv"))


d <- subset(data, X %in% c("freeze.thaw", "under32", 
                           "over20", "W.under32", "pcp.over.5", "soil.temp"))

d[,2:4] <- round(d[,2:4], digits=1)
d <- d[match(c("freeze.thaw", "under32", 
               "over20", "W.under32", "pcp.over.5", "soil.temp"), d$X),]

write.csv(d, paste0("C:/Users/arunyon/OneDrive - DOI/AKR-CFs/", SiteID, "/Data/_revised_thresholds_output.csv"))

swe = read.csv(paste0("C:/Users/arunyon/OneDrive - DOI/AKR-CFs/", SiteID, "/Data/SWE.metrics.csv"))
sw = aggregate(max_SWE_in~CF, swe, mean)
sw
sw$max_SWE_in[1]-sw$max_SWE_in[3]
sw$max_SWE_in[2]-sw$max_SWE_in[3]
