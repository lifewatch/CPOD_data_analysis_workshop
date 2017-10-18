rm(list=ls())
df <- read.table("Minute_Hi.tab", header = TRUE, sep = "\t", fill = TRUE)
#data contains all min data of all stations of quality Hi
head(df)
df <- as.data.frame(df)

df$Receiver <- as.factor(df$Receiver)
df$Station <- as.factor(df$Station)
df$Mooring_type <- as.factor(df$Mooring_type)
df$Species <- as.factor(df$Species)
df$Time<-as.character(df$Time)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
levels(df$Station)
df$datetime <- parse_date_time(df$Time, orders = "ymd HMS")
df$day<-parse_date_time(date(df$datetime), orders = "ymd")
roundUp <- function(x,to=100)
{
  to*(x%/%to + as.logical(x%%to))
}
df$Nallbin<-roundUp(df$Number_clicks_total)
#make a subset with only pos Dpm
#dfpos<-filter(df, df$Dpm == "1")
#names(dfpos)
df_agg<-aggregate(cbind(sum_Dpm=df$Dpm,sum_Nfil=df$Number_clicks_filtered,
                         sum_minson=df$Recorded)~ Nallbin+day, data=df, FUN=sum)
df_agg$PPM<-df_agg$sum_Dpm/df_agg$sum_minson
df_agg$CPMM<-df_agg$sum_Nfil/df_agg$sum_Dpm
df_agg$Nallbin<-as.factor(df_agg$Nallbin)
names(df_agg)
#df_agg<-filter(df_agg, df_agg$Nallbin >= "100")
df1<-aggregate(cbind(meanPPM=df_agg$PPM)~ Nallbin, data=df_agg, FUN=mean)
df2<-aggregate(cbind(meanCPPM=df_agg$CPMM)~ Nallbin, data=df_agg, FUN=mean)

#, meanCPPM=df_agg$CPMM)
ggplot(df1, aes(Nallbin, meanPPM))+ geom_point()+ scale_x_discrete(breaks = seq(500, 4500, 500))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
ggplot(df2, aes(Nallbin, meanCPPM))+ geom_point()+scale_x_discrete(breaks = seq(500, 4500, 500))+
  theme(axis.text=element_text(size=12),
       axis.title=element_text(size=14,face="bold"))
