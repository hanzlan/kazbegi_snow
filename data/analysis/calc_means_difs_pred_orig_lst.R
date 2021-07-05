library(raster)


dif_dir <- "E:/Masterarbeit/Data/validation_stacks/lst_difs_pred_orig_woglaciers/"
setwd(dif_dir)
dif_files <- list.files(dif_dir, pattern="*.tif")

# Calculate mean over all files
stack_filt <- stack(dif_files)
calc(stack_filt, fun = mean, na.rm = TRUE, filename = paste0(dif_dir, "all_means/all_means.tif"))

# Calculate RMSE over all files
calc(stack_filt, fun = function(x){sqrt(mean(x^2, na.rm=TRUE))}, filename = paste0(dif_dir, "all_means/all_rmse.tif"))
