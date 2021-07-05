###Calculate DF with elevation

library(raster)
library(reshape2)

setwd("E:/Masterarbeit/Data/snow_testyears/snow_testyears_wo_glaciers/")

orig <- list.files(pattern=".tif$")
orig_df <- as.data.frame(stack(orig))

orig_melted <- melt(orig_df)

setwd("E:/Masterarbeit/Data/dem")
dem_df <- as.data.frame(raster("dem_kazbegi_clip.tif"))
dem_long <- rep(dem_df$dem_kazbegi_clip, 3521)

orig_dem <- cbind(orig_melted, dem_long)

dem_df_complete <- orig_dem[complete.cases(orig_dem), ]

write.csv(dem_df_complete,"E:/Masterarbeit/Data/validation_stacks/orig_dem_full.csv", row.names = FALSE)

###Load df with elevation

library(dplyr)
library(ggplot2)
library(viridis)
library(season)
library(gridExtra)

setwd("E:/Masterarbeit/Data/validation_stacks/")

###Load CSV
df_combined <- read.csv("orig_pred_full.csv")
df_combined$month <- substr(df_combined$variable, 17, 18)
df_combined$date <- substr(df_combined$variable, 13, 20)
df_combined$variable <- NULL

df_ele <- read.csv("orig_dem_full.csv")
df_combined <- cbind(df_combined, df_ele$dem_long)
colnames(df_combined)[5] <- "ele"
df_combined$bins <- cut(df_combined$ele, breaks=seq(1200,4500,100),
                        labels=as.character(seq(1200,4400,100)))

RMSE = function(p, o){
  sqrt(mean((p - o)^2))
}

rmse <- RMSE(df_combined$pred_melted.pred, df_combined$orig)

# rmse_df <- df_combined %>%
#   group_by(month, bins) %>%
#   summarise(z = RMSE(p=pred_melted.pred, o=orig))

rmse_total <- df_combined %>%
  group_by(bins) %>%
  summarise(z = RMSE(p=pred_melted.pred, o=orig))

DIFF = function(p, o){
  mean(p - o)
}

# diff_df <- df_combined %>%
#   group_by(month, bins) %>%
#   summarise(z = DIFF(p=pred_melted.pred, o=orig))

diff_total <- df_combined %>%
  group_by(bins) %>%
  summarise(z = DIFF(p=pred_melted.pred, o=orig))

# orig_df <- df_combined %>%
#   group_by(month, bins) %>%
#   summarise(z = mean(orig))

orig_total <- df_combined %>%
  group_by(bins) %>%
  summarise(z = mean(orig))

# pred_df <- df_combined %>%
#   group_by(month, bins) %>%
#   summarise(z = mean(pred_melted.pred))

pred_total <- df_combined %>%
  group_by(bins) %>%
  summarise(z = mean(pred_melted.pred))

rmse_df$var <- "rmse"
diff_df$var <- "diff"
orig_df$var <- "orig"
pred_df$var <- "pred"
rmse_total$var <- "rmse"
diff_total$var <- "diff"
orig_total$var <- "orig"
pred_total$var <- "pred"

df_total <- rbind(orig_total, pred_total, diff_total, rmse_total)
df_total$month <- "13"

df <- rbind(orig_df, pred_df, diff_df, rmse_df)
df <- rbind(df, df_total)
df$bins <- as.numeric(levels(df$bins))[df$bins]

# New facet label names for supp variable
month.labs <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                "Aug", "Sep", "Oct", "Nov", "Dec", "Mean of\nall months")
names(month.labs) <- c("01", "02", "03", "04", "05", "06",
                       "07", "08", "09", "10", "11", "12", "13")

p <- ggplot(df, aes(x = bins, y = z, colour = var, group = var))+geom_line(size=1)+
  scale_x_continuous(breaks=seq(1000, 5000, 1000))+
  scale_y_continuous(breaks=seq(-10,80,10))+
  # scale_x_discrete(labels=month.abb)+
  # theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))+
  labs(x="Elevation in m",
       y="NDSI")+
  scale_colour_discrete(labels=c("Difference\n(Predicted-Observed)", 
                                 "Observed values", 
                                 "Predicted values", 
                                 "RMSE"))+
  facet_grid(.~month,
             labeller = labeller(. = NULL, month = month.labs)) + 
  theme_grey()+
  theme(legend.title=element_blank(), legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

p

####Calculate scatterplot
df_combined$dif <- df_combined$pred_melted.pred-df_combined$orig

p <- plot(y=df_combined$ele, x=df_combined$dif, 
     xlab= "Differences between predicted and observed values", ylab="Elevation in m",
     abline(lm(ele ~ dif, data = df_combined), col = "blue"))

linear <- lm(ele ~ dif, data = df_combined)

setwd("E:/Masterarbeit/Data/validation_stacks/")
saveRDS(linear, file = "ele_dif_linear.rds")

p_alpha <- plot(x=df_combined$dif, y=df_combined$ele, 
                xlab= "Differences between predicted and observed values", ylab="Elevation in m",
                col = alpha("black", 0.05),
                abline(lm(ele ~ dif, data = df_combined), col = "blue"))
summary(lm(ele ~ dif, data = df_combined))
p_alpha


####Calculate normal scatterplot
colfunc <- colorRampPalette(c('red', 'blue'))
df_combined$col <- colfunc(10)[as.numeric(cut(df_combined$ele, breaks = 10))]

plot(x=df_combined$orig, y=df_combined$pred_melted.pred, 
                xlab= "Observed values [NDSI]", ylab="Predicted values [NDSI]",
                col = alpha(df_combined$col, 0.01))
abline(0, 1, col = "black", lwd=3)
abline(lm(pred_melted.pred ~ orig, data = df_combined), col="green", lwd=3)

summary(lm(pred_melted.pred ~ orig, data = df_combined))
rsq <- function (x, y) cor(x, y) ^ 2
rsquared1 <- rsq(df_combined$pred_melted.pred, df_combined$orig)
rsquared2 <- rsq(df_combined$orig, df_combined$pred_melted.pred)

p_orig_pred


cat("Inputs are:\n") 
d = df_combined$pred_melted.pred
p = df_combined$orig
cat("d is: ", toString(d), "\n") 
cat("p is: ", toString(p), "\n") 

rmse = function(predictions, targets){ 
  cat("===RMSE readout of intermediate steps:===\n") 
  cat("the errors: (predictions - targets) is: ", 
      toString(predictions - targets), '\n') 
  cat("the squares: (predictions - targets) ** 2 is: ", 
      toString((predictions - targets) ** 2), '\n') 
  cat("the means: (mean((predictions - targets) ** 2)) is: ", 
      toString(mean((predictions - targets) ** 2)), '\n') 
  cat("the square root: (sqrt(mean((predictions - targets) ** 2))) is: ", 
      toString(sqrt(mean((predictions - targets) ** 2))), '\n') 
  return(sqrt(mean((predictions - targets) ** 2))) 
} 
cat("final answer rmse: ", rmse(d, p), "\n") 
