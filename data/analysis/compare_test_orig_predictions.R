library(CAST)
library(sf)
library(mapview)
library(lubridate)
library(ggplot2)
library(caret)
library(latticeExtra)
library(raster)
library(rgdal)
library(reshape2)
library(rowr)
library(hrbrthemes)

# # Plot dif from all files
# setwd("E:/Masterarbeit/Data/snow_testyears/snow_testyears_wo_glaciers")
# orig <- list.files(path= "E:/Masterarbeit/Data/snow_testyears/snow_testyears_wo_glaciers", pattern="*.tif$")
# orig_stack <- as.data.frame(stack(orig))
# orig_melted <- melt(orig_stack)
# names(orig_melted)[names(orig_melted) == "value"] <- "orig"
# 
# 
# setwd("E:/Masterarbeit/Data/validation_stacks/pred_snow_testyears/pred_snow_testyears_wo_glaciers")
# pred <- list.files(path= "E:/Masterarbeit/Data/validation_stacks/pred_snow_testyears/pred_snow_testyears_wo_glaciers", pattern="*.tif$")
# pred_stack <- as.data.frame(stack(pred))
# pred_melted <- melt(pred_stack)
# names(pred_melted)[names(pred_melted) == "value"] <- "pred"
# 
# df_combined <- cbind(orig_melted, pred_melted$pred)
# 
# df_combined <- df_combined[complete.cases(df_combined), ]
# write.csv(df_combined,"E:/Masterarbeit/Data/validation_stacks/orig_pred_full.csv", row.names = FALSE)
# 
# RMSE = function(f, o){
#   sqrt(mean((f - o)^2))
# }
# 
# RMSE(df_combined$`pred_melted$pred`, df_combined$orig)
# 
# plot(df_combined$orig, df_combined$`pred_melted$pred`)
# 

setwd("E:/Masterarbeit/Data/validation_stacks/")

###Load CSV
df_combined <- read.csv("orig_pred_full.csv")
df_combined$variable <- NULL
o4 <- ggplot(df_combined, aes(orig, pred_melted.pred)) +
  stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE) +       
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  geom_point(shape = '.', col = 'white')
o4

o1 <- ggplot(df_combined, aes(orig, pred_melted.pred)) +
  geom_point(alpha = 0.05)
o1

o6 <- ggplot(df_combined, aes(orig, pred_melted.pred)) +
  geom_point(alpha = 0.1) +
  geom_rug(alpha = 0.01)

o6
