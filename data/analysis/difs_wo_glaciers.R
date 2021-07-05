library(raster)
library(rgdal)

dir_difs <- "E:/Masterarbeit/Data/validation_stacks/snow_orig_mean/"

dif_files <- list.files(dir_difs, pattern = "\\.tif$", full.names = TRUE, ignore.case = TRUE)
glaciers <- readOGR(dsn = "E:/Masterarbeit/Data/kazbegi/glaciers", layer = "glaciers")

#dir_results <- "E:/Masterarbeit/Data/validation_stacks/snow_difs_pred_orig_woglaciers/"

for (x in dif_files) {
  ras <- raster(x)
  kaz_wo_glac <- mask(ras, glaciers, inverse = TRUE)
  writeRaster(kaz_wo_glac, filename=paste0(dir_difs, "difwoglac_", substr(x, 64,65)),
                  format="GTiff", overwrite=TRUE)
}
