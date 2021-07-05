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
df_combined$mon <- substr(df_combined$month, 5, 6)

RMSE = function(p, o){
  sqrt(mean((p - o)^2))
}

rmse_df <- df_combined %>%
  group_by(month) %>%
  summarise(z = RMSE(p=pred_melted.pred, o=orig))

rmse_total <- df_combined %>%
  group_by(mon) %>%
  summarise(z = RMSE(p=pred_melted.pred, o=orig))

DIFF = function(p, o){
  mean(p - o)
}

diff_df <- df_combined %>%
  group_by(month) %>%
  summarise(z = DIFF(p=pred_melted.pred, o=orig))

diff_total<- df_combined %>%
  group_by(mon) %>%
  summarise(z = DIFF(p=pred_melted.pred, o=orig))

orig_df <- df_combined %>%
  group_by(month) %>%
  summarise(z = mean(orig))

orig_total <- df_combined %>%
  group_by(mon) %>%
  summarise(z = mean(orig))

pred_df <- df_combined %>%
  group_by(month) %>%
  summarise(z = mean(pred_melted.pred))

pred_total <- df_combined %>%
  group_by(mon) %>%
  summarise(z = mean(pred_melted.pred))

rmse_df$var <- "rmse"
rmse_total$var <- "rmse"
diff_df$var <- "diff"
diff_total$var <- "diff"
orig_df$var <- "orig"
orig_total$var <- "orig"
pred_df$var <- "pred"
pred_total$var <- "pred"

df_total <- rbind(orig_total, pred_total, diff_total, rmse_total) 
df_total$year <- "Mean of the test years"

df <- rbind(orig_df, pred_df, diff_df, rmse_df)
df$year <- substr(df$month, 1, 4)
df$mon <- substr(df$month, 5, 6)
df$month <- NULL
df <- rbind(df, df_total)

p <- ggplot(df, aes(x = mon, y = z, colour = var, group = var))+geom_line(size=1)+
  scale_y_continuous(breaks=seq(-10,80,10))+
  scale_x_discrete(labels=c("J", "F", "M","A", "M","J", "J", "A","S", "O","N","D"))+
 # scale_x_discrete(labels=month.abb)+
 # theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))+
  labs(x="Month",
       y="NDSI")+
  scale_colour_discrete(labels=c("Difference\n(Predicted-Observed)", 
                                 "Observed values", 
                                 "Predicted values", 
                                 "RMSE"))+
  facet_grid(.~year) + 
  theme_grey()+
  theme(legend.title=element_blank(), legend.position = "bottom")

p
