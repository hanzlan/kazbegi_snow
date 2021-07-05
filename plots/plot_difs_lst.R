library(raster)
library(RColorBrewer)
library(rasterVis)
library(hrbrthemes)

dif_dir <- "E:/Masterarbeit/Data/validation_stacks/lst_difs_pred_orig_woglaciers/"
setwd(dif_dir)

colr <- colorRampPalette(brewer.pal(11, 'RdYlBu')) # Define colors
colrmse <- colorRampPalette(brewer.pal(11, 'YlOrRd')) # Define colors

# Plot mean rmse from all files
setwd(paste0(dif_dir, "all_rmse/"))
all_rmse <- list.files(paste0(dif_dir, "all_rmse/"), pattern="*.tif")
all_s <- raster(all_rmse)
levelplot(all_s,  col.regions=colrmse, at=seq(8, 23, len=16))

# Plot mean from all files
setwd(paste0(dif_dir, "all_means/"))
all_difs <- list.files(paste0(dif_dir, "all_means/"), pattern="*.tif")
all_s <- raster(all_difs)
levelplot(all_s,  col.regions=colr, at=seq(-19, 19, len=39))
