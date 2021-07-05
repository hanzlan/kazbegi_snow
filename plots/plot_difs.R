library(raster)
library(RColorBrewer)
library(rasterVis)
library(hrbrthemes)

orig_dir <- "E:/Masterarbeit/Data/validation_stacks/snow_orig_mean/"
pred_dir <- "E:/Masterarbeit/Data/validation_stacks/snow_pred_mean/"
plot_dir <- "E:/Masterarbeit/Data/validation_stacks/levelplot_snow_mean/"
dif_dir <- "E:/Masterarbeit/Data/validation_stacks/snow_difs_pred_orig_woglaciers/"
month_plot_dir <- "E:/Masterarbeit/Data/validation_stacks/levelplot_snow_mean/month/"
setwd(plot_dir)

colr <- colorRampPalette(brewer.pal(11, 'RdYlBu')) # Define colors
colsnow <- colorRampPalette(brewer.pal(9, 'Blues')) # Define colors
colrmse <- colorRampPalette(brewer.pal(9, 'YlOrRd')) # Define colors

# Plot mean rmse from all files
setwd(paste0(dif_dir, "all_rmse/"))
all_rmse <- list.files(paste0(dif_dir, "all_rmse/"), pattern="*.tif")
all_s <- raster(all_rmse)
levelplot(all_s,  col.regions=colrmse, at=seq(10, 28, len=19))

# Plot dif from all files
setwd(dif_dir)
all_difs <- list.files(dif_dir, pattern="*.tif$")
all_s <- stack(all_difs)
levelplot(all_s,  col.regions=colr, at=seq(-30, 30, len=61),
          names.attr= c("All years", "2005", "2008","2011", "2014", "2017", "2020"))


# Plot yearly means
setwd(paste0(dif_dir, "yearly_means/"))
yearly_difs <- list.files(paste0(dif_dir, "yearly_means/"), pattern="*.tif")
yearly_s <- stack(yearly_difs)
levelplot(yearly_s,  col.regions=colr, at=seq(-30, 30, len=61), names.attr= c("2005", "2008", "2011", "2014" , "2017", "2020"))

# Plot all years (original, pred)
setwd(plot_dir)
rasters <- list.files(plot_dir, pattern="*.tif$")
raster_s <- stack(rasters)
levelplot(raster_s,  col.regions=colsnow, at=seq(0, 100, len=101), layout=c(2,7),
          names.attr= c("All years", "All years", "2005", "2005" , "2008", "2008",
                        "2011", "2011", "2014", "2014", "2017", "2017",
                        "2020", "2020"))

# Plot all months (original, pred)
setwd(month_plot_dir)
rasters <- list.files(month_plot_dir, pattern="*.tif$")
raster_s <- stack(rasters)
levelplot(raster_s,  col.regions=colsnow, at=seq(0, 100, len=101), layout=c(2,12),
          names.attr= c("Jan", "Jan" , "Feb", "Feb", "Mar", "Mar", "Apr", "Apr",
                        "May", "May", "Jun", "Jun", "Jul", "Jul", "Aug", "Aug",
                        "Sep", "Sep", "Oct", "Oct", "Nov", "Nov", "Dec", "Dec"))


# Plot monthly means
setwd(paste0(dif_dir, "monthly_means/"))
monthly_difs <- list.files(paste0(dif_dir, "monthly_means/"))
monthly_s <- stack(monthly_difs)
levelplot(monthly_s,  col.regions=colr, at=seq(-37, 37, len=75), layout=c(1,12),
          names.attr= c("Jan", "Feb", "Mar", "Apr" , "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Compare elevation and the differences between predicted and observed values
dem <- raster("E:/Masterarbeit/Data/dem/dem_kazbegi_clip.tif")
dem_df <- as.data.frame(dem)
all_s_df <- as.data.frame(all_s)

df_dem_all <- cbind(dem_df, all_s_df)
plot(y=df_dem_all$dem_kazbegi_clip, x=df_dem_all$all_means, 
     xlab= "Differences between predicted and observed values", ylab="Elevation in m",
     abline(lm(dem_kazbegi_clip ~ all_means, data = df_dem_all), col = "blue"))

# Plot DEM
hist(dem, xlab="Elevation in m", main="")#, breaks=100)
axis(1, labels=c(0, 1000, 2000, 3000, 4000, 5000), at=seq(from=0, to=5000, by=1000))

# Plot Slope
slope <- raster("E:/Masterarbeit/Data/dem/slope_kazbegi_clip.tif")
hist(slope, xlab="Slope in °", main="", breaks=25)
axis(1, labels=c(0, 10, 20, 30, 40, 50, 60), at=seq(from=0, to=60, by=10))
axis(2, labels=c(0, 100, 200, 300, 400, 500, 600, 700), at=seq(from=0, to=700, by=100))

# Plot Aspect
asp <- raster("E:/Masterarbeit/Data/dem/asp_kazbegi_clip.tif")
asp_df <- as.data.frame(asp)
hist(asp, xlab="Aspect in °", main="", breaks=24, axes=F)
axis(1, labels=c(0, 45, 90, 135, 180, 225, 270, 315, 360), at=seq(from=0, to=360, by=45))
axis(2, labels=c(0, 100, 200, 300, 400, 500, 600, 700), at=seq(from=0, to=700, by=100))


