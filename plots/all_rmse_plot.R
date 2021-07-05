library(dplyr)
library(ggplot2)
library(viridis)
library(season)
library(gridExtra)

setwd("E:/Masterarbeit/Data/validation_stacks/")

###Load CSV
df_combined <- read.csv("orig_pred_full.csv")
df_combined$month <- substr(df_combined$variable, 13, 18)
df_combined$date <- substr(df_combined$variable, 13, 20)
df_combined$variable <- NULL

RMSE = function(p, o){
   sqrt(mean((p - o)^2))
}

rmse_df <- df_combined %>%
  group_by(month) %>%
  summarise(result = RMSE(p=pred_melted.pred, o=orig))

rmse_df$year <- as.numeric(substr(rmse_df$month, 1, 4))
rmse_df$m <- as.numeric(substr(rmse_df$month, 5, 6))
colnames(rmse_df)[2] <- "RMSE\n[NDSI Snow Cover]"


pd<-ggplot(rmse_df, aes(year, m, fill = `RMSE\n[NDSI Snow Cover]`)) + 
  geom_tile(colour="gray20", size=1.5, stat="identity") + 
  scale_fill_viridis(option="D") +
  scale_y_continuous(trans = "reverse", breaks=1:12, labels=month.abb[1:12]) +
  scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2020)) +
  xlab("") + 
  ylab("") +
  theme(
    plot.title = element_text(color="gray20",hjust=0,vjust=1, size=rel(2)),
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text = element_text(color="gray20", size=rel(1.5)),
    axis.text.y  = element_text(hjust=1),
    axis.text.x  = element_text(angle=90),
    legend.text = element_text(color="gray20", size=rel(1.3)),
    legend.background = element_rect(fill="white"),
    legend.position = "bottom",
    legend.title=element_text(color="gray20", size=rel(1.3))
  )
pd

DIFF = function(p, o){
  mean(p - o)
}

diff_df <- df_combined %>%
  group_by(month) %>%
  summarise(result = DIFF(p=pred_melted.pred, o=orig))

diff_df$year <- as.numeric(substr(diff_df$month, 1, 4))
diff_df$m <- as.numeric(substr(diff_df$month, 5, 6))
colnames(diff_df)[2] <- "Difference between Predicted and\nObserved values [NDSI Snow Cover]"


pa<-ggplot(diff_df, aes(year, m, fill = `Difference between Predicted and\nObserved values [NDSI Snow Cover]`)) + 
  geom_tile(colour="gray20", size=1.5, stat="identity") + 
  scale_fill_viridis(option="A") +
  scale_y_continuous(trans = "reverse", breaks=1:12, labels=month.abb[1:12]) +
  scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2020)) +
  xlab("") + 
  ylab("") +
  theme(
    plot.title = element_text(color="gray20",hjust=0,vjust=1, size=rel(2)),
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text = element_text(color="gray20", size=rel(1.5)),
    axis.text.y  = element_text(hjust=1),
    axis.text.x  = element_text(angle=90),
    legend.text = element_text(color="gray20", size=rel(1.3)),
    legend.background = element_rect(fill="white"),
    legend.position = "bottom",
    legend.title=element_text(color="gray20", size=rel(1.3))
  )
pa
