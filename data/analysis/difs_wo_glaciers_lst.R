library(raster)
library(rgdal)

dir_difs <- "E:/Masterarbeit/Data/validation_stacks/lst_difs_pred_orig/"

dif_files <- list.files(dir_difs, pattern = "\\.tif$", full.names = TRUE, ignore.case = TRUE)
glaciers <- readOGR(dsn = "E:/Masterarbeit/Data/kazbegi/glaciers", layer = "glaciers")

dir_results <- "E:/Masterarbeit/Data/validation_stacks/lst_difs_pred_orig_woglaciers/"

for (x in dif_files) {
  ras <- raster(x)
  kaz_wo_glac <- mask(ras, glaciers, inverse = TRUE)
  writeRaster(kaz_wo_glac, filename=paste0(dir_results, "difwoglac_", substr(x, 63,76)),
                  format="GTiff", overwrite=TRUE)
}
