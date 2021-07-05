library(raster)
library(rgdal)
library(foreach)
library(doParallel)
library(reshape)
library(dplyr)

dir_aqua <- "E:/Masterarbeit/Data/validation_stacks/pred_snow_aqua/"
dir_terra <- "E:/Masterarbeit/Data/validation_stacks/pred_snow_terra/"
dir_pred <- "E:/Masterarbeit/Data/validation_stacks/pred_snow/"
dir_orig <- "E:/Masterarbeit/Data/snow/"
dir_difs <- "E:/Masterarbeit/Data/validation_stacks/snow_difs_pred_orig/"
dir_snow_testyears <- "E:/Masterarbeit/Data/snow_testyears/"   
dir_snow_testyears_wo_glaciers<- paste0(dir_snow_testyears, "snow_testyears_wo_glaciers/")
dir_pred_snow_testyears <- "E:/Masterarbeit/Data/validation_stacks/pred_snow_testyears/"
dir_pred_snow_testyears_wo_glaciers<- paste0(dir_pred_snow_testyears, "pred_snow_testyears_wo_glaciers/")

setwd(dir_aqua)
aqua_files <- list.files(dir_aqua, pattern ="*.tif")
file.rename(aqua_files, paste0(substr(aqua_files, 1, 13), "aqua.tif"))

setwd(dir_terra)
terra_files <- list.files(dir_terra, pattern ="*.tif")
file.rename(terra_files, paste0(substr(terra_files, 1, 13), "terr.tif"))
# Copy file from dir_aqua and dir_terra to dir_pred

setwd(dir_pred)
pred_files <- list.files(dir_pred, pattern ="*.tif", full.names=TRUE)
setwd(dir_orig)
orig_files <- list.files(dir_orig, pattern ="*.tif", full.names=TRUE)

x = substr(pred_files, 55, 63)
y = substr(orig_files, 32, 40)

df_pred <- as.data.frame(cbind(pred_files, x))
df_orig <- as.data.frame(cbind(orig_files, y))


df <- merge(df_pred, df_orig, by.x="x", by.y="y", all.x = TRUE, all.y = FALSE)




# for(year in years){
#   lst_pred_aqua <- list.files("validation_stacks/pred_lst_aqua/", pattern = paste0("lst_", year, ".*\\.tif$"), full.names = TRUE, ignore.case = TRUE)
#   lst_orig_aqua <- list.files("lst_new/", pattern = paste0("^",year, ".*aqua\\.tif$"), full.names = TRUE, ignore.case = TRUE)
#   
#   x = substr(lst_pred_aqua, 37, 44)
#   y = substr(lst_orig_aqua, 9, 16)
#   
#   df_pred <- as.data.frame(cbind(lst_pred_aqua, x))
#   df_orig <- as.data.frame(cbind(lst_orig_aqua, y))
#   df <- merge(df_pred, df_orig, by.x = "x", 
#               by.y = "y", all.x = TRUE, all.y = FALSE)
#### Calculate difference in new raster
  for (row in 1:nrow(df)) {
    if (!is.na(df[row, "orig_files"])){
      ras_pred <- raster(df[row, "pred_files"])
      ras_orig <- raster(df[row, "orig_files"])
      newras <- ras_pred - ras_orig
      writeRaster(newras, filename=paste0(dir_difs, "dif_", substr(df[row, "orig_files"], 32, 40)),
                  format="GTiff", overwrite=TRUE)
    } else {
      }
  }


#copy originalfiles and predicted files of dates that have prediction and observed value to dir_snow_testyears and pred_snow_testyears
for (row in 1:nrow(df)) {
  if (!is.na(df[row, "orig_files"])){
    file.copy(df[row, "orig_files"], paste0(dir_snow_testyears))
    file.copy(df[row, "pred_files"], paste0(dir_pred_snow_testyears))
  } else {
  }
}


# Mask glaciers
glaciers <- readOGR(dsn = "E:/Masterarbeit/Data/kazbegi/glaciers", layer = "glaciers")

setwd(dir_snow_testyears)
test_orig <- list.files(dir_snow_testyears, pattern="*.tif")

numCores <-3
#numCores <- detectCores()
registerDoParallel(numCores)
foreach (x=1:length(test_orig), .packages='raster')%dopar% {
  ras <- raster(test_orig[[x]])
  kaz_wo_glac <- mask(ras, glaciers, inverse = TRUE)
  writeRaster(kaz_wo_glac, filename=paste0(dir_snow_testyears_wo_glaciers, "woglac_", test_orig[[x]]),
              format="GTiff", overwrite=TRUE)
}

setwd(dir_pred_snow_testyears)
test_pred <- list.files(dir_pred_snow_testyears, pattern="*.tif")
numCores <-3
#numCores <- detectCores()
registerDoParallel(numCores)
foreach (x=1:length(test_pred), .packages='raster')%dopar% {
#for (x in test_pred) {
  ras <- raster(test_pred[[x]])
  kaz_wo_glac <- mask(ras, glaciers, inverse = TRUE)
  writeRaster(kaz_wo_glac, filename=paste0(dir_pred_snow_testyears_wo_glaciers, "woglac_", test_pred[[x]]),
              format="GTiff", overwrite=TRUE)
}

setwd(dir_pred_snow_testyears_wo_glaciers)
pred_stack <- stack(list.files(dir_pred_snow_testyears_wo_glaciers, pattern= "*.tif"))
pred_stack_df <- as.data.frame(pred_stack)
m <- melt(pred_stack_df)
colnames(m)<- c("variable", "pred")

setwd(dir_snow_testyears_wo_glaciers)
orig_stack <- stack(list.files(dir_snow_testyears_wo_glaciers, pattern= "*.tif"))
orig_stack_df <- as.data.frame(orig_stack)
m_o <- melt(orig_stack_df)
colnames(m_o)<- c("variable", "orig")

m_all <- cbind(m, m_o$orig)
colnames(m_all)[3] <- "orig"

library(scales)
#plot(orig~pred, data=m_all, col = alpha("blue", 0.4))

library(ggplot2)
#g <- ggplot(m_all, aes(x=pred, y=orig))+ geom_point(alpha = 0.01)
g <- ggplot(m_all, aes(x=orig, y=pred)) +
  stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE) +       
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  geom_point(shape = '.', col = 'white')

g

saveRDS(g, file = "C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/validation_stacks/compare_predictions_ggplot.rds")
