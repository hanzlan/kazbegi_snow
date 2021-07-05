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

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

df_train <- read.csv("dataframes/df_train_small30.csv")

# Plot dif from all files
setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/lst_merged/")
lstmerged <- list.files(path= "C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/lst_merged/", pattern="*.tif$")
lst_stack <- as.data.frame(stack(lstmerged))
lst_merg_melted <- melt(lst_stack)
lst_merg_melted <- lst_merg_melted[complete.cases(lst_merg_melted), ]
lst_merg_melted$variable <- "M"

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/lst_new/")
lstorig <- list.files(path= "C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/lst_new/", pattern="*.tif$")
lst_orig_stack <- as.data.frame(stack(lstorig))
lst_orig_melted <- melt(lst_orig_stack)
lst_orig_melted <- lst_orig_melted[complete.cases(lst_orig_melted), ]
lst_orig_melted$variable <- "O"

lst_df_melted <- melt(lst_stack)
train_lst <- df_train$lst
na_list <- rep(NA, 36793777)
lst_list <- c(train_lst, na_list)
lst_df_melted$train <- lst_list
lst_df_melted$variable <- NULL
colnames(lst_df_melted) <- c("Test", "Train")
lst_df_melted2 <- melt(lst_df_melted)
lst_df_melted2 <- lst_df_melted2[complete.cases(lst_df_melted2), ]

lst_df_melted3 <- rbind(lst_orig_melted, lst_df_melted2)

p <- ggplot(lst_df_melted3, aes(x=value, color=variable)) + 
  geom_density() +
  theme_ipsum() +
  theme(
    axis.title.x = element_text(size=12, hjust = 0.5),
    axis.title.y = element_text(size=12, hjust = 0.5),
    strip.text.x = element_text(size=12, hjust = 0.5),
    strip.text.y = element_text(size=12, hjust = 0.5)
  )
p
