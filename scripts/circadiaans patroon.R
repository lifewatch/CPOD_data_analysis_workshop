#### Day and night ####
rm(list=ls(all=TRUE))
#packages necessary
library(dplyr)
library(lubridate)
library(ggplot2)
getwd() #
#import metadata
sunrise_set <- data.frame(read.table("Data day 1/Metadata/sunrise_set.txt", sep="", header =TRUE))
sunrise_set <- select(sunrise_set, -X)
colnames(sunrise_set) <- c("Date", "Rise", "Set")
sunrise_set$Rise <- parse_date_time(sunrise_set$Rise, orders = "ymd HMS")
sunrise_set$Set <- parse_date_time(sunrise_set$Set, orders = "ymd HMS")
sunrise_set$Date <- as.POSIXct(sunrise_set$Date,format="%Y-%m-%d", tz="UTC")

#import the cpod data per day
poddata <- read.table("Data day 1/hi_mod_1hr.tab", "\t", header = T)
poddata$Quality <- "hi+mod"
poddata$Date <- as.POSIXct(paste(year(poddata$Time), month(poddata$Time), day(poddata$Time), sep = "-"),format="%Y-%m-%d", tz="UTC")

#merge these two dataframes together
poddata <- left_join(poddata, sunrise_set)
poddata$Time <- parse_date_time(poddata$Time, orders = "ymd HMS")
poddata$Day <- ifelse(poddata$Rise < poddata$Time & poddata$Time <= poddata$Set, "Day", "Night")
poddata <- select(poddata, -Date, -Rise, -Set)

##plot poddata over 24hours
#Extra column with hour data
poddata$uur<-format(as.POSIXlt(poddata$Time),format = '%H')
names(poddata)
#calculate click intensity and frequency, these are relative measures
poddata$Click_frequency <- poddata$Dpm/poddata$Recorded
poddata$Click_intensity <- poddata$Number_clicks_filtered/poddata$Click_frequency

library(ggplot2)
ggplot(poddata, aes(x = factor(uur), y = Click_frequency, group= Day, colour=Day)) +
  stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(y = "Click frequency per hour")

#If you plot all data points, the circadiurnaliteit is maksed because it is a zero-inflated dataset
#plot will also show the seasonality , looking at the overlap points day/night

#samen plot for click intensity
ggplot(poddata, aes(x = factor(uur), y = Click_intensity, group= Day, colour=Day)) +
  stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(y = "Click intensity per hour")
#no clear diurnal pattern in number clicks produced by porpoise

#####same plot per month
poddata$Month<- as.factor(month(poddata$Time))
ggplot(poddata, aes(x = factor(uur), y = Click_frequency, group= Day, fill=Day)) +facet_wrap(~Month)+
  stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  theme(axis.text=element_text(angle=-90, vjust=0.5, size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(y = "Click frequency per hour")+
  labs(x= "hours of the day")+

#again, seasonality plays a role with smaller differences in case of lower detection rates (densities)
#january is good month for the porpoises in the bpns (should be visible also in the seasonal analysis by the other group)

#same plot for click intensity
ggplot(poddata, aes(x = factor(uur), y = Click_intensity, group= Day, fill=Day)) +facet_wrap(~Month)+
  stat_summary(fun.y = mean, geom = "bar") + 
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(y = "Click intensity per hour")+
  labs(x= "hours of the day")
#no clear influence of hour of the day or month of the year on click intensity, keep in mind this is a relative measure.


#### waiting time ####
rm(list=ls(all=TRUE))
#Data per min is needed to calculate waiting time
library(dplyr)
library(lubridate)
library(ggplot2)
getwd() #
sunrise_set <- data.frame(read.table("Data day 1/Metadata/sunrise_set.txt", " ", header =T))
sunrise_set <- select(sunrise_set, -X)
colnames(sunrise_set) <- c("Date", "Rise", "Set")
sunrise_set$Rise <- parse_date_time(sunrise_set$Rise, orders = "ymd HMS")
sunrise_set$Set <- parse_date_time(sunrise_set$Set, orders = "ymd HMS")
sunrise_set$Date <-parse_date_time(sunrise_set$Date, orders= "Ymd")

df <- data.frame(read.table("Data per minute/min_Hi.tab", header = TRUE, sep = "\t", fill = TRUE))
#df <- read.table("min_Mod.tab", header = TRUE, sep = "\t", fill = TRUE)
#df <- read.table("min_Mod.tab", header = TRUE, sep = "\t", fill = TRUE)

#adjust columns to their correct format
df$Receiver <- as.factor(df$Receiver)
df$Station <- as.factor(df$Station)
df$Mooring_type <- as.factor(df$Mooring_type)
df$Species <- as.factor(df$Species)
df$Time<-as.character(df$Time)
df$Recorded<-as.factor(df$Recorded)

library(lubridate)
library(tidyr)
library(dplyr)

df$Time <- parse_date_time(df$Time, orders = "ymd HMS")
levels(df$Station)
df$zone<- ifelse(df$Station=="bpns-Oostendebank Oost", "Oostende",
                 ifelse(df$Station=="bpns-Reefballs-cpower", "bpns-Reefballs-cpower",
                        ifelse(df$Station== "bpns-Wenduinebankw", "Oostende",
                               ifelse(df$Station=="bpns-WK16", "Oostende",
                                      ifelse(df$Station=="bpns-Belwind C05", "bpns-Belwind C05",
                                             ifelse(df$Station== "bpns-D1", "Middelkerke",
                                                    ifelse(df$Station=="bpns-WK9", "WK9en12",
                                                           ifelse(df$Station=="bpns-Gootebank", "Gootebank",
                                                                  ifelse(df$Station=="bpns-VG2" , "Gootebank",
                                                                         ifelse(df$Station== "bpns-WK12", "WK9en12",
                                                                                ifelse(df$Station== "bpns-Reefballs Belwind", "bpns-Reefballs Belwind",
                                                                                       ifelse(df$Station==  "bpns-Oostdyck West",  "bpns-Oostdyck West", 
                                                                                              ifelse(df$Station== "bpns-LST420", "Middelkerke",
                                                                                                     ifelse(df$Station== "bpns-Middelkerke South", "Middelkerke",
                                                                                                            ifelse(df$Station== "bpns-Lottobuoy", "bpns-Lottobuoy",
                                                                                                                   ifelse(df$Station== "bpns-Birkenfels", "bpns-Birkenfels", NA))))))))))))))))
df$zone <-as.factor(df$zone)
levels(df$zone)

#write function 
df$zone<-as.character(df$zone)
WT<-lapply(unique(df$zone), function(x){
 df$Timenr<-as.numeric(as.POSIXct(df$Time, format = "%Y%m%d %H:%M" , tz="UTC"))
 o<-filter(df, df$zone == x)
 o<-select(o, .data$Timenr, .data$Dpm, .data$zone)
 aux <- rle(o[,2])
 waitingo<-cbind(UTC =o[cumsum(aux$lengths)[which(aux$values == 0)] - aux$lengths[aux$values == 0] +1, 1],
                  Result = rle((o$Dpm == 1))$lengths[!rle((o$Dpm == 1))$values], Zone = unique(o$zone))
 waitingo<-data.frame(waitingo)
 })
WT<-do.call("rbind", lapply(WT, data.frame))
colnames(WT)[2] <- "WaitingTime"
#change UTC back to a date
WT$UTC<-as.numeric(levels(WT$UTC))[WT$UTC]
WT$UTC<-as_datetime(as.POSIXct(WT$UTC, origin="1970-01-01 00:00:00"))
#waiting time should be at least 10 min
library(dplyr)
WT$WaitingTime<-as.numeric(WT$WaitingTime)
WT<- filter(WT,WT$WaitingTime > 10)
##merge the two databases
#extra parameter Date in order to have a mutual column with the sunrise_set database, so these can be merged
str(WT)
WT$Date <- as.POSIXct(paste(year(WT$UTC), month(WT$UTC), day(WT$UTC), sep = "-"),format="%Y-%m-%d", tz="UTC")
str(WT)
str(sunrise_set)
sunrise_set$Date<-parse_date_time(sunrise_set$Date, orders="ymd")

WT<- left_join(WT, sunrise_set)
str(WT)
#define 4 categories: 
#sunset = 30min before and after sunset
#sunrise = 30min before and after sunrise
#day = between sunrise+30min and sunrise-30min
#night = between sunset+30min until sunrise-30min the day after
WT$Day<-with(WT, ifelse(WT$Rise-(30*60) < WT$UTC & WT$UTC <= WT$Rise +(30*60), "Sunrise", ifelse(WT$Set-(30*60) < WT$UTC & WT$UTC <= (WT$Set +(30*60)), "Sunset", ifelse(WT$Rise +(30*60)< WT$UTC & WT$UTC <= (WT$Set-(30*60)), "Daytime", "Night"))))
#if you want to define Night with ifelse, you need to define it as between sunset day 1 and Sunrise Day 2.
WT_mean<-aggregate(cbind(meanWT=WT$WaitingTime)~Zone+Day, data=WT, FUN=mean)
WT_sd<-aggregate(cbind(sdWT=WT$WaitingTime)~Zone+Day, data=WT, FUN=sd)
WT_mean<-cbind(WT_mean, WT_sd$sdWT)
colnames(WT_mean)[4] <- "sdWT"
#barplot per zone for each level of Day
ggplot(WT_mean, aes(x = factor(Day), y = meanWT)) + geom_bar(stat = "identity",position = "dodge", fill = "lightblue") + geom_errorbar(aes(ymin = meanWT -sdWT, ymax = meanWT + sdWT), width = 0.3, color = "darkblue")+ facet_wrap(~Zone)
