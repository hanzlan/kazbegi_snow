library(ggplot2)
library(dplyr)
library(zoo)
library(reshape)
library(hrbrthemes)
library(lubridate)
library()
library(scales)

setwd("E:/Masterarbeit/Data/dataframes/")

# df_orig <- read.csv("df_with_doycos_aspcos.csv")
# df_orig <- df_orig[complete.cases(df_orig), ]
# df <- df_orig
# df <- df[c("days", "doy","snow", "month", "year")]
# a <- aggregate(.~days, data=df, mean, na.rm=TRUE) # Calculate daily means over all the trainingpoints
# standev <- aggregate(.~days, data=df, sd, na.rm=TRUE)
# a$meanminsd <- a$snow-standev$snow
# a$meanplusd <- a$snow+standev$snow
# a$yearmonth <- zoo::as.yearmon(a$days)
# a$snow <- as.numeric(a$snow)
# a$year <- as.numeric(a$year)
# a$month <- as.numeric(a$month)
# a$doy <- as.numeric(a$doy)
# a_doy <- aggregate(. ~doy, data=a[,2:3], mean, na.rm=TRUE) # Calculate means for doy 
# sd_doy <- aggregate(. ~doy, data=a[,2:3], sd, na.rm=TRUE)
# a_doy$meanminsd <- a$snow-standev$snow
# a_doy$meanplusd <- a$snow+standev$snow
# a_monthly <- aggregate(. ~yearmonth, data=a[,2:6], mean, na.rm=TRUE) # Calculate monthly means 
# a_yearly <- aggregate(. ~year, data=a_monthly[,2:5], mean, na.rm=TRUE) # Calculate yearly means 
# a_yearly_n <- as.data.frame(cbind(a_yearly$year, a_yearly$snow))
# colnames(a_yearly_n) <- c("year", "snow_mean_y")
# a_new <- dplyr::left_join(a_monthly,a_yearly_n, by = "year")
# a_new_resh <- melt(a_new, id.vars=c("yearmonth", "month", "year", "doy"))

### calculate mean snow per doy
df_orig <- read.csv("df_with_doycos_aspcos.csv")
df_orig <- df_orig[complete.cases(df_orig), ]
df <- df_orig
df <- df[c("days", "doy","snow", "month", "year")]
a <- aggregate(snow~doy, data=df, mean, na.rm=TRUE) # Calculate daily means over all the trainingpoints
standev <- aggregate(snow~doy, data=df, sd, na.rm=TRUE)
a$meanminsd <- a$snow-standev$snow
a$meanplusd <- a$snow+standev$snow
a$snow <- as.numeric(a$snow)
a$doy <- as.numeric(a$doy)
a$meanminsd[a$meanminsd < 0] <- 0
#colnames(a_yearly_n) <- c("year", "snow_mean_y")
#a_new <- dplyr::left_join(a_monthly,a_yearly_n, by = "year")
#a_new_resh <- melt(a_new, id.vars=c("yearmonth", "month", "year", "doy"))

# plot means for doy
a %>%
  ggplot(aes(x=doy)) +
  geom_ribbon(aes(ymin=meanminsd, ymax=meanplusd, fill = "Standard deviation (\u00b1 1\u03c3)"), alpha=0.7) +
  geom_line(aes(y=snow, colour = "Mean"), size = 1) +
  scale_color_manual(name = "Snow Cover [NDSI]",
                     values = c("Mean" = "black")) +
  scale_fill_manual("",values=c("grey")) +
  theme_ipsum() +
  # scale_colour_manual(labels = c("Monthly mean", "Yearly mean"), values = c("black", "red")) +
  scale_x_continuous(labels = seq(from=0, to=366, by=30), breaks=seq(from=0, to=366, by=30))+
  theme_ipsum()+
  theme(
    legend.position="bottom",
    legend.title = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    axis.title.x = element_text(size=14, hjust = 0.5),
    axis.title.y = element_text(size=14, hjust = 0.5),
    strip.text.x = element_text(size=14, hjust = 0.5),
    strip.text.y = element_text(size=14, hjust = 0.5)) +   
  xlab("Day of the year") + ylab("NDSI snow cover")


'''
a_new_resh %>%
  ggplot(aes(x=as.Date(yearmonth), y=value, group=variable, colour=variable)) +
  geom_line() + 
  scale_colour_manual(labels = c("Monthly mean", "Yearly Mean"), values = c("black", "red")) +
 # scale_x_date(date_breaks="2 years") +
 # scale_x_datetime("Year", date_labels = "%Y",
  #                 sec.axis = sec_axis(~./12, name = "month",
   #                                    labels = scales::time_format("%m")))+
  facet_grid(. ~ year, scale = "free_x") +
  scale_x_date(labels = date_format("%b"), expand = c(0, 0), date_breaks="6 months") +
 theme(panel.spacing.x = unit(0, "lines"),
      legend.position="bottom",
    legend.title = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    axis.title.x = element_text(size=14, hjust = 0.5),
    axis.title.y = element_text(size=14, hjust = 0.5),
    strip.text.x = element_text(size=14, hjust = 0.5),
    strip.text.y = element_text(size=14, hjust = 0.5)) +   
    xlab("Date") + ylab("NDSI") +
  ggtitle("Means of NDSI snow cover across all training points")
'''

#calculate yearly and monthly means
df$yearmonth <- zoo::as.yearmon(df$days)
yearmonth <- aggregate(snow~yearmonth, data=df, mean, na.rm=TRUE) # Calculate daily means over all the trainingpoints
yearnum <- aggregate(year~yearmonth, data=df, mean, na.rm=TRUE)
yearmonth_sd <- aggregate(snow~yearmonth, data=df, sd, na.rm=TRUE)
yearmonth$minsdym <- yearmonth$snow-yearmonth_sd$snow
yearmonth$plusdym <- yearmonth$snow+yearmonth_sd$snow
yearmonth$minsdym[yearmonth$minsdym < 0] <- 0

yearmonth <- cbind(yearmonth, yearnum$year)
colnames(yearmonth)[5] <- "year"
colnames(yearmonth)[2] <- "snowym"

year <- aggregate(snow~year, data=df, mean, na.rm=TRUE) # Calculate daily means over all the trainingpoints
year_sd <- aggregate(snow~year, data=df, sd, na.rm=TRUE)
year$minsdyear <- year$snow-year_sd$snow
year$plusdyear <- year$snow+year_sd$snow
year$minsdyear[year$minsdyear < 0] <- 0

a_new <- dplyr::left_join(yearmonth,year, by = "year")

loadfonts(device="win")
a_new %>%
  ggplot(aes(x=as.Date(yearmonth))) +
  geom_ribbon(aes(ymin=minsdym, ymax=plusdym, fill = "Monthly Standard deviation (\u00b1 1\u03c3)"), alpha=0.7) +
  geom_ribbon(aes(ymin=minsdyear, ymax=plusdyear, fill = "Yearly Standard deviation (\u00b1 1\u03c3)"), alpha=0.1) +
  geom_line(aes(y=snowym, colour = "Monthly mean"), size = 1) +
  geom_line(aes(y=snow, colour = "Yearly mean"), size = 1) +
  scale_color_manual(name = "Snow Cover [NDSI]",
                     values = c("Monthly mean" = "black", "Yearly mean" = "red")) +
  scale_fill_manual("",values=c("grey", "red")) +
  theme_ipsum() +
  # scale_colour_manual(labels = c("Monthly mean", "Yearly mean"), values = c("black", "red")) +
  scale_x_date(date_breaks="1 year", labels = date_format("%Y"))+
  theme_ipsum()+
  theme(
    legend.position="bottom",
    legend.title = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    axis.title.x = element_text(size=14, hjust = 0.5),
    axis.title.y = element_text(size=14, hjust = 0.5),
    strip.text.x = element_text(size=14, hjust = 0.5),
    strip.text.y = element_text(size=14, hjust = 0.5)) +   
  xlab("Date") + ylab("NDSI snow cover")


a_new_resh <- melt(a_new, id.vars=c("yearmonth", "year"))

# Plot yearly and monthly means
a_new_resh %>%
  ggplot(aes(x=as.Date(yearmonth), y=value, group=variable, colour=variable)) +
  geom_line() + 
  scale_colour_manual(labels = c("Monthly mean", "Yearly mean"), values = c("black", "red")) +
   scale_x_date(date_breaks="2 years", labels = date_format("%Y")) + theme_ipsum()+
  theme(
        legend.position="bottom",
        legend.title = element_blank(),
        plot.title = element_text(size=14, hjust = 0.5),
        axis.title.x = element_text(size=14, hjust = 0.5),
        axis.title.y = element_text(size=14, hjust = 0.5),
        strip.text.x = element_text(size=14, hjust = 0.5),
        strip.text.y = element_text(size=14, hjust = 0.5)) +   
  xlab("Date") + ylab("NDSI snow cover") +
  ggtitle("Means of NDSI snow cover across all training points")

## aggregate by day, compare points
df2 <- df_orig
df2 <- df2[c("variable", "snow", "month")]
df2$snow <- as.numeric(df2$snow)
df2$month <- as.numeric(df2$month)
b <- aggregate(. ~variable+month, data=df2, mean, na.rm=TRUE) # Calculate monthly means for every trainingpoint


b_new %>%
  ggplot(aes(x=month, y=snow, group=month)) +
  geom_boxplot() + theme_ipsum() +
  # scale_colour_manual(labels = c("Monthly mean", "Yearly mean"), values = c("black", "red")) +
  scale_x_continuous(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                   "Oct", "Nov", "Dec"), breaks=c(1, 2, 3, 4, 5, 6, 7 , 8, 9, 10, 11, 12)) + theme_ipsum()+
 # scale_x_date(date_labels = "%m")+
  theme(
    legend.position="bottom",
    legend.title = element_blank(),
    plot.title = element_text(size=14, hjust = 0.5),
    axis.title.x = element_text(size=14, hjust = 0.5),
    axis.title.y = element_text(size=14, hjust = 0.5),
    strip.text.x = element_text(size=14, hjust = 0.5),
    strip.text.y = element_text(size=14, hjust = 0.5)) +   
  xlab("Month") + ylab("NDSI snow cover") +
  ggtitle("Monthly means of NDSI snow cover at the training points")
