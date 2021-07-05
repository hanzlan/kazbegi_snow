library(raster)


dif_dir <- "E:/Masterarbeit/Data/validation_stacks/snow_difs_pred_orig_woglaciers/"
setwd(dif_dir)
dif_files <- list.files(dif_dir, pattern="*.tif")
rmse_dir <- "E:/Masterarbeit/Data/validation_stacks/snow_rmse/"

# Calculate mean over all files
stack_filt <- stack(dif_files)
calc(stack_filt, fun = function(x){sqrt(mean(x^2, na.rm=TRUE))}, filename = paste0(rmse_dir, "all_rmse.tif"))


# Calculate means for the years 2005, 2008, 2011, 2014 , 2017 and 2020 
years <- c(2005, 2008, 2011, 2014 , 2017, 2020 )

for(y in years){
  filtered <- list.files(dif_dir, pattern = paste0("_", y))
  stack_filt <- stack(filtered)
  calc(stack_filt, fun = mean, na.rm = TRUE, filename = paste0(rmse_dir, "yearly_means/",
                                                               y, ".tif"))
}


# Calculate monthly means
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
for(m in months){
  filtered <- list.files(dif_dir, pattern = paste0(".*", m, ".{3}\\.tif$"))
  stack_filt <- stack(filtered)
  calc(stack_filt, fun = mean, na.rm = TRUE, filename = paste0(rmse_dir, "monthly_means/",
                                                               m, ".tif"))
}


vect <- c(1,2,3,4,NA)
blablub <- function(x) sqrt(mean(x^2, na.rm=TRUE))
bla <- blablub(vect)
