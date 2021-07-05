library(raster)


dir_orig <- "E:/Masterarbeit/Data/snow_testyears/snow_testyears_wo_glaciers/"
setwd(dir_orig)
orig_files <- list.files(dir_orig, pattern="*.tif")
mean_dir <- "E:/Masterarbeit/Data/validation_stacks/snow_orig_mean/"

# Calculate mean over all files
stack_filt <- stack(pred_files)
calc(stack_filt, fun = mean, na.rm=TRUE, filename = paste0(mean_dir, "all_pred_mean.tif"))


# Calculate means for the years 2005, 2008, 2011, 2014 , 2017 and 2020 
years <- c(2005, 2008, 2011, 2014, 2017, 2020)

for(y in years){
  filtered <- list.files(dir_orig, pattern = paste0("_", y))
  stack_filt <- stack(filtered)
  calc(stack_filt, fun = mean, na.rm = TRUE, filename = paste0(mean_dir, "yearly_means/",
                                                               y, ".tif"))
}


# Calculate monthly means
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
for(m in months){
  filtered <- list.files(dir_orig, pattern = paste0(".*", m, ".{6}\\.tif$"))
  stack_filt <- stack(filtered)
  calc(stack_filt, fun = mean, na.rm = TRUE, filename = paste0(mean_dir, "monthly_means/",
                                                               m, ".tif"))
}
