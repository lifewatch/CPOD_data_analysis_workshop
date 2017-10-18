rm(list=ls())
getwd() 
#import data per minute
df<- data.frame(read.table("Data per minute/min_Hi.tab", header = TRUE, sep = "\t", fill = TRUE))
dfM <- data.frame(read.table("Data per minute/min_Mod.tab", header = TRUE, sep = "\t", fill = TRUE))

#merge the two qualities together

str(df)
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
#Define extra column of min and seconds
df$minute<-format(as.POSIXlt(df$Time),format = '%M:%S')
#Extra column with hour data
df$uur<-format(as.POSIXlt(df$Time),format = '%H')
#intermittent recording instead of continuous recording
#according to the settings of C-POD
#1min on, 1 min off; 1 on 2 off, 1 on 4 off, 1 on, 9 off.
duty1<-subset(df, strftime(df$Time, '%M') %in% c(seq(0,60,2)))
duty2<-subset(df, strftime(df$Time, '%M') %in% c(seq(0,60,3)))
duty3<-subset(df, strftime(df$Time, '%M') %in% c(seq(0,60,5)))
duty4<-subset(df, strftime(df$Time, '%M') %in% c(seq(0,60,10)))
#according to the settings of the cpod it only saves half the battery independent of the number of mins chosen 

#if we would work with another acoustic device: check for other intermittent options
#first 10min of a hour
duty10<- subset(df , df$minute < "11:00" )
#first 20min of a hour
duty20<- subset(df , df$minute < "21:00" )
#first 30min of a hour
duty30<- subset(df , df$minute < "31:00" )
#last 10min of an hour
dutymin50<-subset(df , df$minute > "50:00" )
#30 random minutes uit je uur selecteren en subset van maken
library(base)
dutyspread<-subset(df, strftime(df$Time, '%M') %in% c(sample(1:60, 30, replace=FALSE)))
names(df)
#function to summarize the data
make_subset <- function(poddf, interval) {
  poddf$Time_10min <- parse_date_time(as.character(as.POSIXlt(floor(as.numeric(poddf$Time)/(10*60))*(10*60),origin=(as.POSIXlt('1970-01-01 00:00')), tz="UTC")), orders = "ymd HMS")
  poddf$Time_hour <- parse_date_time(paste(date(poddf$Time), hour(poddf$Time)), orders = "ymd H")
  
  d10m <- group_by(poddf, zone, Time_hour, Time_10min)
  dsum10m <- summarise(d10m,
                       sum_msecs = sum(Milliseconds),
                       sum_nfiltered = sum(Number_clicks_filtered),
                       sum_nall = sum(Number_clicks_total),
                       sum_lost = sum(Lost_minutes),
                       sum_recorded = sum(as.integer(as.character(Recorded))),
                       sum_dpm = sum(Dpm))
  dsum10m$dp10m <- if_else(dsum10m$sum_dpm == 0, 0, 1)
  dsum10m <- data.frame(dsum10m)
  dsum10m$Time <- dsum10m$Time_10min
  dsum10m$click_frequency <- dsum10m$sum_dpm/dsum10m$sum_recorded
  dsum10m$click_intensity <- dsum10m$sum_nfiltered/dsum10m$click_frequency
  
  dh <- group_by(dsum10m, zone,Time_hour)
  dsumh <- summarise(dh,
                     sum_msecs = sum(sum_msecs),
                     sum_nfiltered = sum(sum_nfiltered),
                     sum_nall = sum(sum_nall),
                     sum_lost = sum(sum_lost),
                     sum_recorded = sum(sum_recorded),
                     sum_dpm = sum(sum_dpm),
                     sum_dp10m = sum(dp10m))
  dsumh$dph <- if_else(dsumh$sum_dpm == 0, 0, 1)
  dsumh <- data.frame(dsumh)
  dsumh$Time <- dsumh$Time_hour
  dsumh$click_frequency <- dsumh$sum_dpm/dsumh$sum_recorded
  dsumh$click_intensity <- dsumh$sum_nfiltered/dsumh$click_frequency
  
  dd <- group_by(dsumh, zone, date(Time_hour))
  dsumd <- summarise(dd,
                     sum_msecs = sum(sum_msecs),
                     sum_nfiltered = sum(sum_nfiltered),
                     sum_nall = sum(sum_nall),
                     sum_lost = sum(sum_lost),
                     sum_recorded = sum(sum_recorded),
                     sum_dpm = sum(sum_dpm),
                     sum_dp10m = sum(sum_dp10m),
                     sum_dph = sum(dph))
  dsumd$dph <- if_else(dsumd$sum_dpm == 0, 0, 1)
  dsumd <- data.frame(dsumd)
  dsumd$Time <- dsumd$date.Time_hour.
  dsumd$click_frequency <- dsumd$sum_dpm/dsumd$sum_recorded
  dsumd$click_intensity <- dsumd$sum_nfiltered/dsumd$click_frequency
  
  dsumpoddf <- if(interval == "10min") {
    dsumpoddf <- dsum10m
  } else{
    if(interval == "hour") {
      dsumpoddf <- dsumh
    } else{
      if(interval == "day") {
        dsumpoddf <- dsumd
      }
    }
  }
  
  return(dsumpoddf)
}

#summarize according to the chosen duty cycle per hour
df_hour <- make_subset(df, "hour")
podduty1_hour <-make_subset(duty1, "hour")
podduty2_hour <-make_subset(duty2, "hour")
podduty3_hour <-make_subset(duty3, "hour")
podduty4_hour <-make_subset(duty4, "hour")
podduty10_hour <- make_subset(duty10, "hour")
podduty20_hour <- make_subset(duty20, "hour")
podduty30_hour <- make_subset(duty30, "hour")
podduty50_hour <- make_subset(dutymin50, "hour")
poddutyspread_hour <-make_subset(dutyspread, "hour")
#summarize per day
df_day <- make_subset(df, "day")
podduty1_day <-make_subset(duty1, "day")
podduty2_day <-make_subset(duty2, "day")
podduty3_day <-make_subset(duty3, "day")
podduty4_day <-make_subset(duty4, "day")
podduty10_day <- make_subset(duty10, "day")
podduty20_day <- make_subset(duty20, "day")
podduty30_day <- make_subset(duty30, "day")
podduty50_day <- make_subset(dutymin50, "day")
poddutyspread_day <-make_subset(dutyspread, "day")
#plot dpm per day vs hours of the day
#datasets onder elkaar zetten en benoemen van welke duty cycle ze zijn (extra kolom maken)
df_hour$DUTY<-'cont'
podduty10_hour$DUTY <- 'duty10'
podduty20_hour$DUTY <- 'duty20'
podduty30_hour$DUTY <- 'duty30'
podduty50_hour$DUTY <- 'duty50'
poddutyspread_hour$DUTY <- 'dutyspread'
podduty1_hour$DUTY <- 'duty1'
podduty2_hour$DUTY <- 'duty2'
podduty3_hour$DUTY <- 'duty5'
podduty4_hour$DUTY <- 'duty10'
#same for the day-dataset
df_day$DUTY<-'cont'
podduty10_day$DUTY <- 'duty10'
podduty20_day$DUTY <- 'duty20'
podduty30_day$DUTY <- 'duty30'
podduty50_day$DUTY <- 'duty50'
poddutyspread_day$DUTY <- 'dutyspread'
podduty1_day$DUTY <- 'duty1'
podduty2_day$DUTY <- 'duty2'
podduty3_day$DUTY <- 'duty5'
podduty4_day$DUTY <- 'duty10'
#merge
library(dplyr)
podtime<-bind_rows(df_hour, podduty10_hour, podduty20_hour, podduty30_hour, podduty50_hour, poddutyspread_hour, 
                   podduty1_hour, podduty2_hour, podduty3_hour, podduty4_hour)
podtime_day<-bind_rows(df_day, podduty10_day, podduty20_day, podduty30_day, podduty50_day, poddutyspread_day, 
                       podduty1_day, podduty2_day, podduty3_day, podduty4_day)
#aggregate the data over the each hour of the day and visualize
names(podtime)
podtime$uur<-format(as.POSIXlt(podtime$Time_hour),format = '%H')
podtime_mean=aggregate(cbind(meanrecorded=podtime$sum_recorded, meannall=podtime$sum_nall,meandp10m=podtime$sum_dp10m,
                             meanDPM=podtime$sum_dpm, meannfiltered=podtime$sum_nfiltered, 
                             meanclickfreq=podtime$click_frequency)~ uur+DUTY, data=podtime, FUN=mean)
library(ggplot2)
ggplot(data=podtime_mean, aes(x=uur, y=meanclickfreq, group= DUTY, colour=DUTY))+geom_point()+geom_line()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(y = "Click frequency per hour")
names(podtime_day)
#visualize per zone
podtime_zone=aggregate(cbind(meanrecorded=podtime$sum_recorded, meannall=podtime$sum_nall,meandp10m=podtime$sum_dp10m,
                             meanDPM=podtime$sum_dpm, meannfiltered=podtime$sum_nfiltered, 
                             meanclickfreq=podtime$click_frequency)~ uur+DUTY+zone, data=podtime, FUN=mean)
ggplot(data=podtime_zone, aes(x=uur, y=meanclickfreq, group= DUTY, colour=DUTY))+geom_point()+
  geom_line()+facet_wrap(~zone)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(y = "Click frequency per hour")+
  labs(x= "Hours of the day")+
  scale_x_discrete(breaks = c("00","05","09", "13","17","21"))
#visualize the dataset per day
ggplot(data=podtime_day, aes(x=Time, y=click_frequency, group= DUTY, colour=DUTY))+geom_point()+geom_line()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(y = "Click frequency per day")



