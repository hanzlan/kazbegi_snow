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
library(hrbrthemes)

# Plot dif from all files
setwd("E:/Masterarbeit/Data/snow_testyears/snow_testyears_wo_glaciers/")
snow_orig <- list.files(path= "E:/Masterarbeit/Data/snow_testyears/snow_testyears_wo_glaciers/", pattern="*.tif$")

df <- as.data.frame(snow_orig)
df$notna <- NA
df$mean <- NA
for (row in 1:nrow(df)) {
  rasta <- raster(df$snow_orig[row])
  test <- as.data.frame(rasta)
  df$notna[row] <- sum(!is.na(test))
  df$mean[row] <- cellStats(rasta, "mean")
  
}

df$date <- substr(df$snow_orig, 13, 20)
df$datum <- as.Date(df$date, tryFormats = "%Y%m%d")
df$doy <- format(df$datum, "%j")

plot(df$doy, df$notna)
plot(df$doy, df$mean)
