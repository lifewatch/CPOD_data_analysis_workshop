#import of script: quality classes are already defined in the rshiny data explorer

#remove everything before start of the script
rm(list=ls())
getwd() # Check your current working directory.
#use data from 2016 for this analysis. 
#quality classes of this data Hi and Mod
df <- read.table("Data/Hour2016.tab", header = TRUE, sep = "\t", fill = TRUE)
head(df)
df <- as.data.frame(df)

df$Receiver <- as.factor(df$Receiver)
df$Station <- as.factor(df$Station)
df$Mooring_type <- as.factor(df$Mooring_type)
df$Species <- as.factor(df$Species)
df$Time<-as.character(df$Time)
df$Time <- parse_date_time(df$Time, orders = "ymd HMS")

library(lubridate)
library(tidyr)
library(dplyr)
#check for NA values
table(is.na(df))
levels(df$Station)
#combine cpod stations in the same zone
#new column zone, hiermee werken ipv station!
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

df$zone<-as.factor(df$zone)
levels(df$zone)
levels(df$Station)
#in case you work with more zones, make sure you have enough data per zone
#some stations have very limited data, leave these cpods out of the data analysis
#birkenfels has only a couple of days of data (2017)
#belwind C05 is limited to 2014

##the analysis will be performed on the zones sampled in 2016
#bpns_Reefballs_cpower  
#bpns_Lottobuoy         
#bpns_Oostdyck_West     
#bpns_Reefballs_Belwind 
#Gootebank              
#Middelkerke            
#Oostende               
#WK9en12                

#toBeRemoved1<-which(df$zone=="Birkenfels")
#toBeRemoved2<-which(df$zone=="bpns-Belwind C05")
#df<-df[-toBeRemoved1,]
#df<-df[-toBeRemoved2,] 

#Make a dataframe with the useful parameters
head(df)
df1 <- df[,c("Time","Species","Dp10m","Dp10m","zone")]
str(df1)
# # We have 8 zones

#check availability of data over year 2016 per zone
#make plot based on date
df1$ymd<-parse_date_time(date(df1$Time), orders = "ymd")

library(ggplot2)
ggplot(df1, aes(ymd, zone))+geom_point()


# Transpose the dataframe
df1_transp <- NULL
#unique returns  a vector, data frame or array like x but with duplicate elements/rows removed.
df1_transp <- unique(df1$Time)
df1_transp <- as.data.frame (df1_transp)
colnames(df1_transp) <- c("Unique_Time")
#add for each zone a column, default NA values
df1_transp$bpns_Reefballs_cpower  <- NA
df1_transp$bpns_Lottobuoy         <- NA
df1_transp$bpns_Oostdyck_West     <- NA
df1_transp$bpns_Reefballs_Belwind <- NA
df1_transp$Gootebank              <- NA
df1_transp$Middelkerke            <- NA
df1_transp$Oostende               <- NA
df1_transp$WK9en12                <- NA
head(df1_transp)
str(df1)
#now, the data per zone can be added to the time rows
#Voor "bpns-Reefballs cpower" zoek op de unieke tijdscode en vul in met het respectivelijke Dp10m.
dataset<-filter(df1, df1$zone== "bpns-Reefballs-cpower")
i<- NULL

for (i in 1: length(df1_transp$Unique_Time))
{
  test <- NULL
  test <- which(dataset$Time == df1_transp$Unique_Time[i])
  #value <- NULL
  
  if (length(test) > 0)
  {
    value <- dataset$Dp10m[test]
    df1_transp$bpns_Reefballs_cpower[i] <- value
  } else {
    df1_transp$bpns_Reefballs_cpower[i] <-NA
  }
}
print(df1_transp$bpns_Reefballs_cpower)
names(df1_transp)
#Voor "bpns-Lottobuoy" zoek op de unieke tijdscode en vul in met het respectivelijke Dp10m.
# Als er geen 
dataset<-filter(df1, df1$zone==  "bpns-Lottobuoy")

i<- NULL

for (i in 1: length(df1_transp$Unique_Time))
{
  test <- NULL
  test <- which(dataset$Time == df1_transp$Unique_Time[i])
  value <- NULL
  
  if (length(test) > 0)
  {
    value <- dataset$Dp10m[test]
    df1_transp$bpns_Lottobuoy[i] <- value
  } else {
    df1_transp$bpns_Lottobuoy[i] <- NA
  }
}

sum(df1_transp$bpns_Lottobuoy)
#Voor "bpns-Oostdyck West" zoek op de unieke tijdscode en vul in met het respectivelijke Dp10m.
# Als er geen 
dataset<-filter(df1, df1$zone==  "bpns-Oostdyck West")

i<- NULL

for (i in 1: length(df1_transp$Unique_Time))
{
  test <- NULL
  test <- which(dataset$Time == df1_transp$Unique_Time[i])
  #value <- NULL
  
  if (length(test) > 0)
  {
    value <- dataset$Dp10m[test]
    df1_transp$bpns_Oostdyck_West[i] <- value
  } else {
    df1_transp$bpns_Oostdyck_West[i] <- NA
  }
}


#Voor "bpns-Reefballs Belwind" zoek op de unieke tijdscode en vul in met het respectivelijke Dp10m.
# Als er geen 
dataset<-filter(df1, df1$zone==  "bpns-Reefballs Belwind")

i<- NULL

for (i in 1: length(df1_transp$Unique_Time))
{
  test <- NULL
  test <- which(dataset$Time == df1_transp$Unique_Time[i])
  #value <- NULL
  
  if (length(test) > 0)
  {
    value <- dataset$Dp10m[test]
    df1_transp$bpns_Reefballs_Belwind[i] <- value
  } else {
    df1_transp$bpns_Reefballs_Belwind[i] <- NA
  }
}


#Voor "Gootebank" zoek op de unieke tijdscode en vul in met het respectivelijke Dp10m.
# Als er geen 
dataset<-filter(df1, df1$zone==  "Gootebank")

i<- NULL

for (i in 1: length(df1_transp$Unique_Time))
{
  test <- NULL
  test <- which(dataset$Time == df1_transp$Unique_Time[i])
  #value <- NULL
  
  if (length(test) > 0)
  {
    value <- dataset$Dp10m[test]
    df1_transp$Gootebank[i] <- value
  } else {
    df1_transp$Gootebank[i] <- NA
  }
}


#Voor "Middelkerke" zoek op de unieke tijdscode en vul in met het respectivelijke Dp10m.
# Als er geen 
dataset<-filter(df1, df1$zone==  "Middelkerke")
i<- NULL

for (i in 1: length(df1_transp$Unique_Time))
{
  test <- NULL
  test <- which(dataset$Time == df1_transp$Unique_Time[i])
  #value <- NULL
  
  if (length(test) > 0)
  {
    value <- dataset$Dp10m[test]
    df1_transp$Middelkerke[i] <- value
  } else {
    df1_transp$Middelkerke[i] <- NA
  }
}


#Voor "Oostende" zoek op de unieke tijdscode en vul in met het respectivelijke Dp10m.
# Als er geen 
dataset<-filter(df1, df1$zone==  "Oostende")
i<- NULL

for (i in 1: length(df1_transp$Unique_Time))
{
  test <- NULL
  test <- which(dataset$Time == df1_transp$Unique_Time[i])
  #value <- NULL
  
  if (length(test) > 0)
  {
    value <- dataset$Dp10m[test]
    df1_transp$Oostende[i] <- value
  } else {
    df1_transp$Oostende[i] <- NA
  }
}


#Voor "WK9en12" zoek op de unieke tijdscode en vul in met het respectivelijke Dp10m.
# Als er geen 
dataset<-filter(df1, df1$zone==  "WK9en12")


i<- NULL

for (i in 1: length(df1_transp$Unique_Time))
{
  test <- NULL
  test <- which(dataset$Time == df1_transp$Unique_Time[i])
  #value <- NULL
  
  if (length(test) > 0)
  {
    value <- dataset$Dp10m[test]
    df1_transp$WK9en12[i] <- value
  } else {
    df1_transp$WK9en12[i] <- NA
  }
}

#Dit is de finaal getransponeerde matrix met daarin voor elke gerapporteerde tijdsindicatie en voor elke Cpod locatie de Dp10m
head(df1_transp)
#check hoeveel rijen er zijn waarvoor er data is in elke zone
overlap <-  df1_transp[complete.cases(df1_transp), ] 

#write this dataset to csv format on your pc
#write.csv(df1_transp, "te.csv")

# For the analysis:
## several options: 
### working with the times where there is data for each zone
### working with all the data 
#### using the 2016 data, we will work with all the data because there are only 14 hours with overlap in all the zones
overlap <-  df1_transp[complete.cases(overlap), ] 

##working with all the data
Kolomnummertjes <- c(2:9)

# mean of everything per time
average_all                   <- combn(Kolomnummertjes,8)
overlap$average_all <- NA
average_all                   <- as.vector(average_all) 
overlap$average_all <- rowMeans(overlap[,average_all], na.rm = TRUE)

# mean of all the zones - 1 locationvper time
average_all_minus_1_location <- combn(Kolomnummertjes,7)
ncol(average_all_minus_1_location)
overlap$average_all_minus_1_location_1 <- NA
overlap$average_all_minus_1_location_2 <- NA
overlap$average_all_minus_1_location_3 <- NA
overlap$average_all_minus_1_location_4 <- NA
overlap$average_all_minus_1_location_5 <- NA
overlap$average_all_minus_1_location_6 <- NA
overlap$average_all_minus_1_location_7 <- NA
overlap$average_all_minus_1_location_8 <- NA

kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_1_location[,1]
overlap$average_all_minus_1_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_2         <- average_all_minus_1_location[,2]
overlap$average_all_minus_1_location_2 <- rowMeans(overlap[,kolomnummercombinatie_2], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_3         <- average_all_minus_1_location[,3]
overlap$average_all_minus_1_location_3 <- rowMeans(overlap[,kolomnummercombinatie_3], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_4         <- average_all_minus_1_location[,4]
overlap$average_all_minus_1_location_4 <- rowMeans(overlap[,kolomnummercombinatie_4], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_5        <- average_all_minus_1_location[,5]
overlap$average_all_minus_1_location_5 <- rowMeans(overlap[,kolomnummercombinatie_5], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_6        <- average_all_minus_1_location[,6]
overlap$average_all_minus_1_location_6 <- rowMeans(overlap[,kolomnummercombinatie_6], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_7         <- average_all_minus_1_location[,7]
overlap$average_all_minus_1_location_7 <- rowMeans(overlap[,kolomnummercombinatie_7], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_8         <- average_all_minus_1_location[,8]
overlap$average_all_minus_1_location_8 <- rowMeans(overlap[,kolomnummercombinatie_8], na.rm = TRUE)

# gemiddelde van alles - 2 locaties  
average_all_minus_2_location <- combn(Kolomnummertjes,6)
ncol(average_all_minus_2_location)
overlap$average_all_minus_2_location_1 <- NA
overlap$average_all_minus_2_location_2 <- NA
overlap$average_all_minus_2_location_3 <- NA
overlap$average_all_minus_2_location_4 <- NA
overlap$average_all_minus_2_location_5 <- NA
overlap$average_all_minus_2_location_6 <- NA
overlap$average_all_minus_2_location_7 <- NA
overlap$average_all_minus_2_location_8 <- NA
overlap$average_all_minus_2_location_9 <- NA
overlap$average_all_minus_2_location_10<- NA
overlap$average_all_minus_2_location_11<- NA
overlap$average_all_minus_2_location_12 <- NA
overlap$average_all_minus_2_location_13 <- NA
overlap$average_all_minus_2_location_14 <- NA
overlap$average_all_minus_2_location_15 <- NA
overlap$average_all_minus_2_location_16 <- NA
overlap$average_all_minus_2_location_17 <- NA
overlap$average_all_minus_2_location_18 <- NA
overlap$average_all_minus_2_location_19 <- NA
overlap$average_all_minus_2_location_20 <- NA
overlap$average_all_minus_2_location_21 <- NA
overlap$average_all_minus_2_location_22 <- NA
overlap$average_all_minus_2_location_23 <- NA
overlap$average_all_minus_2_location_24 <- NA
overlap$average_all_minus_2_location_25 <- NA
overlap$average_all_minus_2_location_26 <- NA
overlap$average_all_minus_2_location_27 <- NA
overlap$average_all_minus_2_location_28 <- NA

kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_2_location[,1]
overlap$average_all_minus_2_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_2         <- average_all_minus_2_location[,2]
overlap$average_all_minus_2_location_2 <- rowMeans(overlap[,kolomnummercombinatie_2], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_3         <- average_all_minus_2_location[,3]
overlap$average_all_minus_2_location_3 <- rowMeans(overlap[,kolomnummercombinatie_3], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_2_location[,1]
overlap$average_all_minus_2_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_4         <- average_all_minus_2_location[,4]
overlap$average_all_minus_2_location_4 <- rowMeans(overlap[,kolomnummercombinatie_4], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_5         <- average_all_minus_2_location[,5]
overlap$average_all_minus_2_location_5 <- rowMeans(overlap[,kolomnummercombinatie_5], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_6         <- average_all_minus_2_location[,6]
overlap$average_all_minus_2_location_6 <- rowMeans(overlap[,kolomnummercombinatie_6], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_7         <- average_all_minus_2_location[,7]
overlap$average_all_minus_2_location_7 <- rowMeans(overlap[,kolomnummercombinatie_7], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_8         <- average_all_minus_2_location[,8]
overlap$average_all_minus_2_location_8 <- rowMeans(overlap[,kolomnummercombinatie_8], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_9         <- average_all_minus_2_location[,9]
overlap$average_all_minus_2_location_9 <- rowMeans(overlap[,kolomnummercombinatie_9], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_10         <- average_all_minus_2_location[,10]
overlap$average_all_minus_2_location_10 <- rowMeans(overlap[,kolomnummercombinatie_10], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_11         <- average_all_minus_2_location[,11]
overlap$average_all_minus_2_location_11 <- rowMeans(overlap[,kolomnummercombinatie_11], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_12         <- average_all_minus_2_location[,12]
overlap$average_all_minus_2_location_12 <- rowMeans(overlap[,kolomnummercombinatie_12], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_13         <- average_all_minus_2_location[,13]
overlap$average_all_minus_2_location_13 <- rowMeans(overlap[,kolomnummercombinatie_13], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_14         <- average_all_minus_2_location[,14]
overlap$average_all_minus_2_location_14 <- rowMeans(overlap[,kolomnummercombinatie_14], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_15         <- average_all_minus_2_location[,15]
overlap$average_all_minus_2_location_15 <- rowMeans(overlap[,kolomnummercombinatie_15], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_16         <- average_all_minus_2_location[,16]
overlap$average_all_minus_2_location_16 <- rowMeans(overlap[,kolomnummercombinatie_16], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_17         <- average_all_minus_2_location[,17]
overlap$average_all_minus_2_location_17 <- rowMeans(overlap[,kolomnummercombinatie_17], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_18         <- average_all_minus_2_location[,18]
overlap$average_all_minus_2_location_18 <- rowMeans(overlap[,kolomnummercombinatie_18], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_19         <- average_all_minus_2_location[,19]
overlap$average_all_minus_2_location_19 <- rowMeans(overlap[,kolomnummercombinatie_19], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_20         <- average_all_minus_2_location[,20]
overlap$average_all_minus_2_location_20 <- rowMeans(overlap[,kolomnummercombinatie_20], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_21         <- average_all_minus_2_location[,21]
overlap$average_all_minus_2_location_21 <- rowMeans(overlap[,kolomnummercombinatie_21], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_22         <- average_all_minus_2_location[,22]
overlap$average_all_minus_2_location_22 <- rowMeans(overlap[,kolomnummercombinatie_22], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_23         <- average_all_minus_2_location[,23]
overlap$average_all_minus_2_location_23 <- rowMeans(overlap[,kolomnummercombinatie_23], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_24         <- average_all_minus_2_location[,24]
overlap$average_all_minus_2_location_24 <- rowMeans(overlap[,kolomnummercombinatie_24], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_25         <- average_all_minus_2_location[,25]
overlap$average_all_minus_2_location_25 <- rowMeans(overlap[,kolomnummercombinatie_25], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_26         <- average_all_minus_2_location[,26]
overlap$average_all_minus_2_location_26 <- rowMeans(overlap[,kolomnummercombinatie_26], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_27         <- average_all_minus_2_location[,27]
overlap$average_all_minus_2_location_27 <- rowMeans(overlap[,kolomnummercombinatie_27], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_28         <- average_all_minus_2_location[,28]
overlap$average_all_minus_2_location_28 <- rowMeans(overlap[,kolomnummercombinatie_28], na.rm = TRUE)


# gemiddelde van alles - 3 locaties  
average_all_minus_3_location <- combn(Kolomnummertjes,5)
ncol(average_all_minus_3_location)
overlap$average_all_minus_3_location_1 <- NA
overlap$average_all_minus_3_location_2 <- NA
overlap$average_all_minus_3_location_3 <- NA
overlap$average_all_minus_3_location_4 <- NA
overlap$average_all_minus_3_location_5 <- NA
overlap$average_all_minus_3_location_6 <- NA
overlap$average_all_minus_3_location_7 <- NA
overlap$average_all_minus_3_location_8 <- NA
overlap$average_all_minus_3_location_9 <- NA
overlap$average_all_minus_3_location_10<- NA
overlap$average_all_minus_3_location_11<- NA
overlap$average_all_minus_3_location_12 <- NA
overlap$average_all_minus_3_location_13 <- NA
overlap$average_all_minus_3_location_14 <- NA
overlap$average_all_minus_3_location_15 <- NA
overlap$average_all_minus_3_location_16 <- NA
overlap$average_all_minus_3_location_17 <- NA
overlap$average_all_minus_3_location_18 <- NA
overlap$average_all_minus_3_location_19 <- NA
overlap$average_all_minus_3_location_20 <- NA
overlap$average_all_minus_3_location_21 <- NA
overlap$average_all_minus_3_location_22 <- NA
overlap$average_all_minus_3_location_23 <- NA
overlap$average_all_minus_3_location_24 <- NA
overlap$average_all_minus_3_location_25 <- NA
overlap$average_all_minus_3_location_26 <- NA
overlap$average_all_minus_3_location_27 <- NA
overlap$average_all_minus_3_location_28 <- NA
overlap$average_all_minus_3_location_29 <- NA
overlap$average_all_minus_3_location_30 <- NA
overlap$average_all_minus_3_location_31 <- NA
overlap$average_all_minus_3_location_32 <- NA
overlap$average_all_minus_3_location_33 <- NA
overlap$average_all_minus_3_location_34 <- NA
overlap$average_all_minus_3_location_35 <- NA
overlap$average_all_minus_3_location_36 <- NA
overlap$average_all_minus_3_location_37 <- NA
overlap$average_all_minus_3_location_38 <- NA
overlap$average_all_minus_3_location_39 <- NA
overlap$average_all_minus_3_location_40 <- NA
overlap$average_all_minus_3_location_41 <- NA
overlap$average_all_minus_3_location_42 <- NA
overlap$average_all_minus_3_location_43 <- NA
overlap$average_all_minus_3_location_44 <- NA
overlap$average_all_minus_3_location_45 <- NA
overlap$average_all_minus_3_location_46 <- NA
overlap$average_all_minus_3_location_47 <- NA
overlap$average_all_minus_3_location_48 <- NA
overlap$average_all_minus_3_location_49 <- NA
overlap$average_all_minus_3_location_50 <- NA
overlap$average_all_minus_3_location_51 <- NA
overlap$average_all_minus_3_location_52 <- NA
overlap$average_all_minus_3_location_53 <- NA
overlap$average_all_minus_3_location_54 <- NA
overlap$average_all_minus_3_location_55 <- NA
overlap$average_all_minus_3_location_56 <- NA
kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_3_location[,1]
overlap$average_all_minus_3_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_2         <- average_all_minus_3_location[,2]
overlap$average_all_minus_3_location_2 <- rowMeans(overlap[,kolomnummercombinatie_2], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_3         <- average_all_minus_3_location[,3]
overlap$average_all_minus_3_location_3 <- rowMeans(overlap[,kolomnummercombinatie_3], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_3_location[,1]
overlap$average_all_minus_3_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_4         <- average_all_minus_3_location[,4]
overlap$average_all_minus_3_location_4 <- rowMeans(overlap[,kolomnummercombinatie_4], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_5         <- average_all_minus_3_location[,5]
overlap$average_all_minus_3_location_5 <- rowMeans(overlap[,kolomnummercombinatie_5], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_6         <- average_all_minus_3_location[,6]
overlap$average_all_minus_3_location_6 <- rowMeans(overlap[,kolomnummercombinatie_6], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_7         <- average_all_minus_3_location[,7]
overlap$average_all_minus_3_location_7 <- rowMeans(overlap[,kolomnummercombinatie_7], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_8         <- average_all_minus_3_location[,8]
overlap$average_all_minus_3_location_8 <- rowMeans(overlap[,kolomnummercombinatie_8], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_9         <- average_all_minus_3_location[,9]
overlap$average_all_minus_3_location_9 <- rowMeans(overlap[,kolomnummercombinatie_9], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_10         <- average_all_minus_3_location[,10]
overlap$average_all_minus_3_location_10 <- rowMeans(overlap[,kolomnummercombinatie_10], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_11         <- average_all_minus_3_location[,11]
overlap$average_all_minus_3_location_11 <- rowMeans(overlap[,kolomnummercombinatie_11], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_12         <- average_all_minus_3_location[,12]
overlap$average_all_minus_3_location_12 <- rowMeans(overlap[,kolomnummercombinatie_12], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_13         <- average_all_minus_3_location[,13]
overlap$average_all_minus_3_location_13 <- rowMeans(overlap[,kolomnummercombinatie_13], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_14         <- average_all_minus_3_location[,14]
overlap$average_all_minus_3_location_14 <- rowMeans(overlap[,kolomnummercombinatie_14], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_15         <- average_all_minus_3_location[,15]
overlap$average_all_minus_3_location_15 <- rowMeans(overlap[,kolomnummercombinatie_15], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_16         <- average_all_minus_3_location[,16]
overlap$average_all_minus_3_location_16 <- rowMeans(overlap[,kolomnummercombinatie_16], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_17         <- average_all_minus_3_location[,17]
overlap$average_all_minus_3_location_17 <- rowMeans(overlap[,kolomnummercombinatie_17], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_18         <- average_all_minus_3_location[,18]
overlap$average_all_minus_3_location_18 <- rowMeans(overlap[,kolomnummercombinatie_18], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_19         <- average_all_minus_3_location[,19]
overlap$average_all_minus_3_location_19 <- rowMeans(overlap[,kolomnummercombinatie_19], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_20         <- average_all_minus_3_location[,20]
overlap$average_all_minus_3_location_20 <- rowMeans(overlap[,kolomnummercombinatie_20], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_21         <- average_all_minus_3_location[,21]
overlap$average_all_minus_3_location_21 <- rowMeans(overlap[,kolomnummercombinatie_21], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_22         <- average_all_minus_3_location[,22]
overlap$average_all_minus_3_location_22 <- rowMeans(overlap[,kolomnummercombinatie_22], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_23         <- average_all_minus_3_location[,23]
overlap$average_all_minus_3_location_23 <- rowMeans(overlap[,kolomnummercombinatie_23], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_24         <- average_all_minus_3_location[,24]
overlap$average_all_minus_3_location_24 <- rowMeans(overlap[,kolomnummercombinatie_24], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_25         <- average_all_minus_3_location[,25]
overlap$average_all_minus_3_location_25 <- rowMeans(overlap[,kolomnummercombinatie_25], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_26         <- average_all_minus_3_location[,26]
overlap$average_all_minus_3_location_26 <- rowMeans(overlap[,kolomnummercombinatie_26], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_27         <- average_all_minus_3_location[,27]
overlap$average_all_minus_3_location_27 <- rowMeans(overlap[,kolomnummercombinatie_27], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_28         <- average_all_minus_3_location[,28]
overlap$average_all_minus_3_location_28 <- rowMeans(overlap[,kolomnummercombinatie_28], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_29         <- average_all_minus_3_location[,29]
overlap$average_all_minus_3_location_29 <- rowMeans(overlap[,kolomnummercombinatie_29], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_30         <- average_all_minus_3_location[,30]
overlap$average_all_minus_3_location_30 <- rowMeans(overlap[,kolomnummercombinatie_30], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_31         <- average_all_minus_3_location[,31]
overlap$average_all_minus_3_location_31 <- rowMeans(overlap[,kolomnummercombinatie_31], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_32         <- average_all_minus_3_location[,32]
overlap$average_all_minus_3_location_32 <- rowMeans(overlap[,kolomnummercombinatie_32], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_33         <- average_all_minus_3_location[,33]
overlap$average_all_minus_3_location_33 <- rowMeans(overlap[,kolomnummercombinatie_33], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_34         <- average_all_minus_3_location[,34]
overlap$average_all_minus_3_location_34 <- rowMeans(overlap[,kolomnummercombinatie_34], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_35         <- average_all_minus_3_location[,35]
overlap$average_all_minus_3_location_35 <- rowMeans(overlap[,kolomnummercombinatie_35], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_36         <- average_all_minus_3_location[,36]
overlap$average_all_minus_3_location_36 <- rowMeans(overlap[,kolomnummercombinatie_36], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_37         <- average_all_minus_3_location[,37]
overlap$average_all_minus_3_location_37 <- rowMeans(overlap[,kolomnummercombinatie_37], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_38         <- average_all_minus_3_location[,38]
overlap$average_all_minus_3_location_38 <- rowMeans(overlap[,kolomnummercombinatie_38], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_39         <- average_all_minus_3_location[,39]
overlap$average_all_minus_3_location_39 <- rowMeans(overlap[,kolomnummercombinatie_39], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_40         <- average_all_minus_3_location[,40]
overlap$average_all_minus_3_location_40 <- rowMeans(overlap[,kolomnummercombinatie_40], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_41         <- average_all_minus_3_location[,41]
overlap$average_all_minus_3_location_41 <- rowMeans(overlap[,kolomnummercombinatie_41], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_42         <- average_all_minus_3_location[,42]
overlap$average_all_minus_3_location_42 <- rowMeans(overlap[,kolomnummercombinatie_42], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_43         <- average_all_minus_3_location[,43]
overlap$average_all_minus_3_location_43 <- rowMeans(overlap[,kolomnummercombinatie_43], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_44         <- average_all_minus_3_location[,44]
overlap$average_all_minus_3_location_44 <- rowMeans(overlap[,kolomnummercombinatie_44], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_45         <- average_all_minus_3_location[,45]
overlap$average_all_minus_3_location_45 <- rowMeans(overlap[,kolomnummercombinatie_45], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_46         <- average_all_minus_3_location[,46]
overlap$average_all_minus_3_location_46 <- rowMeans(overlap[,kolomnummercombinatie_46], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_47         <- average_all_minus_3_location[,47]
overlap$average_all_minus_3_location_47 <- rowMeans(overlap[,kolomnummercombinatie_47], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_48         <- average_all_minus_3_location[,48]
overlap$average_all_minus_3_location_48 <- rowMeans(overlap[,kolomnummercombinatie_48], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_49         <- average_all_minus_3_location[,49]
overlap$average_all_minus_3_location_49 <- rowMeans(overlap[,kolomnummercombinatie_49], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_50         <- average_all_minus_3_location[,50]
overlap$average_all_minus_3_location_50 <- rowMeans(overlap[,kolomnummercombinatie_50], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_51         <- average_all_minus_3_location[,51]
overlap$average_all_minus_3_location_51 <- rowMeans(overlap[,kolomnummercombinatie_51], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_52         <- average_all_minus_3_location[,52]
overlap$average_all_minus_3_location_52 <- rowMeans(overlap[,kolomnummercombinatie_52], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_53         <- average_all_minus_3_location[,53]
overlap$average_all_minus_3_location_53 <- rowMeans(overlap[,kolomnummercombinatie_53], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_54         <- average_all_minus_3_location[,54]
overlap$average_all_minus_3_location_54 <- rowMeans(overlap[,kolomnummercombinatie_54], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_55         <- average_all_minus_3_location[,55]
overlap$average_all_minus_3_location_55 <- rowMeans(overlap[,kolomnummercombinatie_55], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_56         <- average_all_minus_3_location[,56]
overlap$average_all_minus_3_location_56 <- rowMeans(overlap[,kolomnummercombinatie_56], na.rm = TRUE)


# gemiddelde van alles - 4 locaties  
average_all_minus_4_location <- combn(Kolomnummertjes,4)
ncol(average_all_minus_4_location)
overlap$average_all_minus_4_location_1 <- NA
overlap$average_all_minus_4_location_2 <- NA
overlap$average_all_minus_4_location_3 <- NA
overlap$average_all_minus_4_location_4 <- NA
overlap$average_all_minus_4_location_5 <- NA
overlap$average_all_minus_4_location_6 <- NA
overlap$average_all_minus_4_location_7 <- NA
overlap$average_all_minus_4_location_8 <- NA
overlap$average_all_minus_4_location_9 <- NA
overlap$average_all_minus_4_location_10<- NA
overlap$average_all_minus_4_location_11<- NA
overlap$average_all_minus_4_location_12 <- NA
overlap$average_all_minus_4_location_13 <- NA
overlap$average_all_minus_4_location_14 <- NA
overlap$average_all_minus_4_location_15 <- NA
overlap$average_all_minus_4_location_16 <- NA
overlap$average_all_minus_4_location_17 <- NA
overlap$average_all_minus_4_location_18 <- NA
overlap$average_all_minus_4_location_19 <- NA
overlap$average_all_minus_4_location_20 <- NA
overlap$average_all_minus_4_location_21 <- NA
overlap$average_all_minus_4_location_22 <- NA
overlap$average_all_minus_4_location_23 <- NA
overlap$average_all_minus_4_location_24 <- NA
overlap$average_all_minus_4_location_25 <- NA
overlap$average_all_minus_4_location_26 <- NA
overlap$average_all_minus_4_location_27 <- NA
overlap$average_all_minus_4_location_28 <- NA
overlap$average_all_minus_4_location_29 <- NA
overlap$average_all_minus_4_location_30 <- NA
overlap$average_all_minus_4_location_31 <- NA
overlap$average_all_minus_4_location_32 <- NA
overlap$average_all_minus_4_location_33 <- NA
overlap$average_all_minus_4_location_34 <- NA
overlap$average_all_minus_4_location_35 <- NA
overlap$average_all_minus_4_location_36 <- NA
overlap$average_all_minus_4_location_37 <- NA
overlap$average_all_minus_4_location_38 <- NA
overlap$average_all_minus_4_location_39 <- NA
overlap$average_all_minus_4_location_40 <- NA
overlap$average_all_minus_4_location_41 <- NA
overlap$average_all_minus_4_location_42 <- NA
overlap$average_all_minus_4_location_43 <- NA
overlap$average_all_minus_4_location_44 <- NA
overlap$average_all_minus_4_location_45 <- NA
overlap$average_all_minus_4_location_46 <- NA
overlap$average_all_minus_4_location_47 <- NA
overlap$average_all_minus_4_location_48 <- NA
overlap$average_all_minus_4_location_49 <- NA
overlap$average_all_minus_4_location_50 <- NA
overlap$average_all_minus_4_location_51 <- NA
overlap$average_all_minus_4_location_52 <- NA
overlap$average_all_minus_4_location_53 <- NA
overlap$average_all_minus_4_location_54 <- NA
overlap$average_all_minus_4_location_55 <- NA
overlap$average_all_minus_4_location_56 <- NA
overlap$average_all_minus_4_location_57 <- NA
overlap$average_all_minus_4_location_58 <- NA
overlap$average_all_minus_4_location_59 <- NA
overlap$average_all_minus_4_location_60 <- NA
overlap$average_all_minus_4_location_61 <- NA
overlap$average_all_minus_4_location_62 <- NA
overlap$average_all_minus_4_location_63 <- NA
overlap$average_all_minus_4_location_64 <- NA
overlap$average_all_minus_4_location_65 <- NA
overlap$average_all_minus_4_location_66 <- NA
overlap$average_all_minus_4_location_67 <- NA
overlap$average_all_minus_4_location_68 <- NA
overlap$average_all_minus_4_location_69 <- NA
overlap$average_all_minus_4_location_70 <- NA


kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_4_location[,1]
overlap$average_all_minus_4_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_2         <- average_all_minus_4_location[,2]
overlap$average_all_minus_4_location_2 <- rowMeans(overlap[,kolomnummercombinatie_2], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_3         <- average_all_minus_4_location[,3]
overlap$average_all_minus_4_location_3 <- rowMeans(overlap[,kolomnummercombinatie_3], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_4_location[,1]
overlap$average_all_minus_4_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_4         <- average_all_minus_4_location[,4]
overlap$average_all_minus_4_location_4 <- rowMeans(overlap[,kolomnummercombinatie_4], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_5         <- average_all_minus_4_location[,5]
overlap$average_all_minus_4_location_5 <- rowMeans(overlap[,kolomnummercombinatie_5], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_6         <- average_all_minus_4_location[,6]
overlap$average_all_minus_4_location_6 <- rowMeans(overlap[,kolomnummercombinatie_6], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_7         <- average_all_minus_4_location[,7]
overlap$average_all_minus_4_location_7 <- rowMeans(overlap[,kolomnummercombinatie_7], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_8         <- average_all_minus_4_location[,8]
overlap$average_all_minus_4_location_8 <- rowMeans(overlap[,kolomnummercombinatie_8], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_9         <- average_all_minus_4_location[,9]
overlap$average_all_minus_4_location_9 <- rowMeans(overlap[,kolomnummercombinatie_9], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_10         <- average_all_minus_4_location[,10]
overlap$average_all_minus_4_location_10 <- rowMeans(overlap[,kolomnummercombinatie_10], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_11         <- average_all_minus_4_location[,11]
overlap$average_all_minus_4_location_11 <- rowMeans(overlap[,kolomnummercombinatie_11], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_12         <- average_all_minus_4_location[,12]
overlap$average_all_minus_4_location_12 <- rowMeans(overlap[,kolomnummercombinatie_12], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_13         <- average_all_minus_4_location[,13]
overlap$average_all_minus_4_location_13 <- rowMeans(overlap[,kolomnummercombinatie_13], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_14         <- average_all_minus_4_location[,14]
overlap$average_all_minus_4_location_14 <- rowMeans(overlap[,kolomnummercombinatie_14], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_15         <- average_all_minus_4_location[,15]
overlap$average_all_minus_4_location_15 <- rowMeans(overlap[,kolomnummercombinatie_15], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_16         <- average_all_minus_4_location[,16]
overlap$average_all_minus_4_location_16 <- rowMeans(overlap[,kolomnummercombinatie_16], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_17         <- average_all_minus_4_location[,17]
overlap$average_all_minus_4_location_17 <- rowMeans(overlap[,kolomnummercombinatie_17], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_18         <- average_all_minus_4_location[,18]
overlap$average_all_minus_4_location_18 <- rowMeans(overlap[,kolomnummercombinatie_18], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_19         <- average_all_minus_4_location[,19]
overlap$average_all_minus_4_location_19 <- rowMeans(overlap[,kolomnummercombinatie_19], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_20         <- average_all_minus_4_location[,20]
overlap$average_all_minus_4_location_20 <- rowMeans(overlap[,kolomnummercombinatie_20], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_21         <- average_all_minus_4_location[,21]
overlap$average_all_minus_4_location_21 <- rowMeans(overlap[,kolomnummercombinatie_21], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_22         <- average_all_minus_4_location[,22]
overlap$average_all_minus_4_location_22 <- rowMeans(overlap[,kolomnummercombinatie_22], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_23         <- average_all_minus_4_location[,23]
overlap$average_all_minus_4_location_23 <- rowMeans(overlap[,kolomnummercombinatie_23], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_24         <- average_all_minus_4_location[,24]
overlap$average_all_minus_4_location_24 <- rowMeans(overlap[,kolomnummercombinatie_24], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_25         <- average_all_minus_4_location[,25]
overlap$average_all_minus_4_location_25 <- rowMeans(overlap[,kolomnummercombinatie_25], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_26         <- average_all_minus_4_location[,26]
overlap$average_all_minus_4_location_26 <- rowMeans(overlap[,kolomnummercombinatie_26], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_27         <- average_all_minus_4_location[,27]
overlap$average_all_minus_4_location_27 <- rowMeans(overlap[,kolomnummercombinatie_27], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_28         <- average_all_minus_4_location[,28]
overlap$average_all_minus_4_location_28 <- rowMeans(overlap[,kolomnummercombinatie_28], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_29         <- average_all_minus_4_location[,29]
overlap$average_all_minus_4_location_29 <- rowMeans(overlap[,kolomnummercombinatie_29], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_30         <- average_all_minus_4_location[,30]
overlap$average_all_minus_4_location_30 <- rowMeans(overlap[,kolomnummercombinatie_30], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_31         <- average_all_minus_4_location[,31]
overlap$average_all_minus_4_location_31 <- rowMeans(overlap[,kolomnummercombinatie_31], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_32         <- average_all_minus_4_location[,32]
overlap$average_all_minus_4_location_32 <- rowMeans(overlap[,kolomnummercombinatie_32], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_33         <- average_all_minus_4_location[,33]
overlap$average_all_minus_4_location_33 <- rowMeans(overlap[,kolomnummercombinatie_33], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_34         <- average_all_minus_4_location[,34]
overlap$average_all_minus_4_location_34 <- rowMeans(overlap[,kolomnummercombinatie_34], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_35         <- average_all_minus_4_location[,35]
overlap$average_all_minus_4_location_35 <- rowMeans(overlap[,kolomnummercombinatie_35], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_36         <- average_all_minus_4_location[,36]
overlap$average_all_minus_4_location_36 <- rowMeans(overlap[,kolomnummercombinatie_36], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_37         <- average_all_minus_4_location[,37]
overlap$average_all_minus_4_location_37 <- rowMeans(overlap[,kolomnummercombinatie_37], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_38         <- average_all_minus_4_location[,38]
overlap$average_all_minus_4_location_38 <- rowMeans(overlap[,kolomnummercombinatie_38], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_39         <- average_all_minus_4_location[,39]
overlap$average_all_minus_4_location_39 <- rowMeans(overlap[,kolomnummercombinatie_39], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_40         <- average_all_minus_4_location[,40]
overlap$average_all_minus_4_location_40 <- rowMeans(overlap[,kolomnummercombinatie_40], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_41         <- average_all_minus_4_location[,41]
overlap$average_all_minus_4_location_41 <- rowMeans(overlap[,kolomnummercombinatie_41], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_42         <- average_all_minus_4_location[,42]
overlap$average_all_minus_4_location_42 <- rowMeans(overlap[,kolomnummercombinatie_42], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_43         <- average_all_minus_4_location[,43]
overlap$average_all_minus_4_location_43 <- rowMeans(overlap[,kolomnummercombinatie_43], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_44         <- average_all_minus_4_location[,44]
overlap$average_all_minus_4_location_44 <- rowMeans(overlap[,kolomnummercombinatie_44], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_45         <- average_all_minus_4_location[,45]
overlap$average_all_minus_4_location_45 <- rowMeans(overlap[,kolomnummercombinatie_45], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_46         <- average_all_minus_4_location[,46]
overlap$average_all_minus_4_location_46 <- rowMeans(overlap[,kolomnummercombinatie_46], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_47         <- average_all_minus_4_location[,47]
overlap$average_all_minus_4_location_47 <- rowMeans(overlap[,kolomnummercombinatie_47], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_48         <- average_all_minus_4_location[,48]
overlap$average_all_minus_4_location_48 <- rowMeans(overlap[,kolomnummercombinatie_48], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_49         <- average_all_minus_4_location[,49]
overlap$average_all_minus_4_location_49 <- rowMeans(overlap[,kolomnummercombinatie_49], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_50         <- average_all_minus_4_location[,50]
overlap$average_all_minus_4_location_50 <- rowMeans(overlap[,kolomnummercombinatie_50], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_51         <- average_all_minus_4_location[,51]
overlap$average_all_minus_4_location_51 <- rowMeans(overlap[,kolomnummercombinatie_51], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_52         <- average_all_minus_4_location[,52]
overlap$average_all_minus_4_location_52 <- rowMeans(overlap[,kolomnummercombinatie_52], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_53         <- average_all_minus_4_location[,53]
overlap$average_all_minus_4_location_53 <- rowMeans(overlap[,kolomnummercombinatie_53], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_54         <- average_all_minus_4_location[,54]
overlap$average_all_minus_4_location_54 <- rowMeans(overlap[,kolomnummercombinatie_54], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_55         <- average_all_minus_4_location[,55]
overlap$average_all_minus_4_location_55 <- rowMeans(overlap[,kolomnummercombinatie_55], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_56         <- average_all_minus_4_location[,56]
overlap$average_all_minus_4_location_56 <- rowMeans(overlap[,kolomnummercombinatie_56], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_57        <- average_all_minus_4_location[,57]
overlap$average_all_minus_4_location_57 <- rowMeans(overlap[,kolomnummercombinatie_57], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_58         <- average_all_minus_4_location[,58]
overlap$average_all_minus_4_location_58 <- rowMeans(overlap[,kolomnummercombinatie_58], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_59         <- average_all_minus_4_location[,59]
overlap$average_all_minus_4_location_59 <- rowMeans(overlap[,kolomnummercombinatie_59], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_60         <- average_all_minus_4_location[,60]
overlap$average_all_minus_4_location_60 <- rowMeans(overlap[,kolomnummercombinatie_60], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_61         <- average_all_minus_4_location[,61]
overlap$average_all_minus_4_location_61 <- rowMeans(overlap[,kolomnummercombinatie_61], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_62         <- average_all_minus_4_location[,62]
overlap$average_all_minus_4_location_62 <- rowMeans(overlap[,kolomnummercombinatie_62], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_63         <- average_all_minus_4_location[,63]
overlap$average_all_minus_4_location_63 <- rowMeans(overlap[,kolomnummercombinatie_63], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_64         <- average_all_minus_4_location[,64]
overlap$average_all_minus_4_location_64 <- rowMeans(overlap[,kolomnummercombinatie_64], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_65         <- average_all_minus_4_location[,65]
overlap$average_all_minus_4_location_65 <- rowMeans(overlap[,kolomnummercombinatie_65], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_66         <- average_all_minus_4_location[,66]
overlap$average_all_minus_4_location_66 <- rowMeans(overlap[,kolomnummercombinatie_66], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_67         <- average_all_minus_4_location[,67]
overlap$average_all_minus_4_location_67 <- rowMeans(overlap[,kolomnummercombinatie_67], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_68         <- average_all_minus_4_location[,68]
overlap$average_all_minus_4_location_68 <- rowMeans(overlap[,kolomnummercombinatie_68], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_69        <- average_all_minus_4_location[,69]
overlap$average_all_minus_4_location_69 <- rowMeans(overlap[,kolomnummercombinatie_69], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_70        <- average_all_minus_4_location[,70]
overlap$average_all_minus_4_location_70 <- rowMeans(overlap[,kolomnummercombinatie_70], na.rm = TRUE)



# gemiddelde van alles - 5 locaties  
average_all_minus_5_location <- combn(Kolomnummertjes,3)
ncol(average_all_minus_5_location)
overlap$average_all_minus_5_location_1 <- NA
overlap$average_all_minus_5_location_2 <- NA
overlap$average_all_minus_5_location_3 <- NA
overlap$average_all_minus_5_location_4 <- NA
overlap$average_all_minus_5_location_5 <- NA
overlap$average_all_minus_5_location_6 <- NA
overlap$average_all_minus_5_location_7 <- NA
overlap$average_all_minus_5_location_8 <- NA
overlap$average_all_minus_5_location_9 <- NA
overlap$average_all_minus_5_location_10<- NA
overlap$average_all_minus_5_location_11<- NA
overlap$average_all_minus_5_location_12 <- NA
overlap$average_all_minus_5_location_13 <- NA
overlap$average_all_minus_5_location_14 <- NA
overlap$average_all_minus_5_location_15 <- NA
overlap$average_all_minus_5_location_16 <- NA
overlap$average_all_minus_5_location_17 <- NA
overlap$average_all_minus_5_location_18 <- NA
overlap$average_all_minus_5_location_19 <- NA
overlap$average_all_minus_5_location_20 <- NA
overlap$average_all_minus_5_location_21 <- NA
overlap$average_all_minus_5_location_22 <- NA
overlap$average_all_minus_5_location_23 <- NA
overlap$average_all_minus_5_location_24 <- NA
overlap$average_all_minus_5_location_25 <- NA
overlap$average_all_minus_5_location_26 <- NA
overlap$average_all_minus_5_location_27 <- NA
overlap$average_all_minus_5_location_28 <- NA
overlap$average_all_minus_5_location_29 <- NA
overlap$average_all_minus_5_location_30 <- NA
overlap$average_all_minus_5_location_31 <- NA
overlap$average_all_minus_5_location_32 <- NA
overlap$average_all_minus_5_location_33 <- NA
overlap$average_all_minus_5_location_34 <- NA
overlap$average_all_minus_5_location_35 <- NA
overlap$average_all_minus_5_location_36 <- NA
overlap$average_all_minus_5_location_37 <- NA
overlap$average_all_minus_5_location_38 <- NA
overlap$average_all_minus_5_location_39 <- NA
overlap$average_all_minus_5_location_40 <- NA
overlap$average_all_minus_5_location_41 <- NA
overlap$average_all_minus_5_location_42 <- NA
overlap$average_all_minus_5_location_43 <- NA
overlap$average_all_minus_5_location_44 <- NA
overlap$average_all_minus_5_location_45 <- NA
overlap$average_all_minus_5_location_46 <- NA
overlap$average_all_minus_5_location_47 <- NA
overlap$average_all_minus_5_location_48 <- NA
overlap$average_all_minus_5_location_49 <- NA
overlap$average_all_minus_5_location_50 <- NA
overlap$average_all_minus_5_location_51 <- NA
overlap$average_all_minus_5_location_52 <- NA
overlap$average_all_minus_5_location_53 <- NA
overlap$average_all_minus_5_location_54 <- NA
overlap$average_all_minus_5_location_55 <- NA
overlap$average_all_minus_5_location_56 <- NA
kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_5_location[,1]
overlap$average_all_minus_5_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_2         <- average_all_minus_5_location[,2]
overlap$average_all_minus_5_location_2 <- rowMeans(overlap[,kolomnummercombinatie_2], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_3         <- average_all_minus_5_location[,3]
overlap$average_all_minus_5_location_3 <- rowMeans(overlap[,kolomnummercombinatie_3], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_5_location[,1]
overlap$average_all_minus_5_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_4         <- average_all_minus_5_location[,4]
overlap$average_all_minus_5_location_4 <- rowMeans(overlap[,kolomnummercombinatie_4], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_5         <- average_all_minus_5_location[,5]
overlap$average_all_minus_5_location_5 <- rowMeans(overlap[,kolomnummercombinatie_5], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_6         <- average_all_minus_5_location[,6]
overlap$average_all_minus_5_location_6 <- rowMeans(overlap[,kolomnummercombinatie_6], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_7         <- average_all_minus_5_location[,7]
overlap$average_all_minus_5_location_7 <- rowMeans(overlap[,kolomnummercombinatie_7], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_8         <- average_all_minus_5_location[,8]
overlap$average_all_minus_5_location_8 <- rowMeans(overlap[,kolomnummercombinatie_8], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_9         <- average_all_minus_5_location[,9]
overlap$average_all_minus_5_location_9 <- rowMeans(overlap[,kolomnummercombinatie_9], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_10         <- average_all_minus_5_location[,10]
overlap$average_all_minus_5_location_10 <- rowMeans(overlap[,kolomnummercombinatie_10], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_11         <- average_all_minus_5_location[,11]
overlap$average_all_minus_5_location_11 <- rowMeans(overlap[,kolomnummercombinatie_11], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_12         <- average_all_minus_5_location[,12]
overlap$average_all_minus_5_location_12 <- rowMeans(overlap[,kolomnummercombinatie_12], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_13         <- average_all_minus_5_location[,13]
overlap$average_all_minus_5_location_13 <- rowMeans(overlap[,kolomnummercombinatie_13], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_14         <- average_all_minus_5_location[,14]
overlap$average_all_minus_5_location_14 <- rowMeans(overlap[,kolomnummercombinatie_14], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_15         <- average_all_minus_5_location[,15]
overlap$average_all_minus_5_location_15 <- rowMeans(overlap[,kolomnummercombinatie_15], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_16         <- average_all_minus_5_location[,16]
overlap$average_all_minus_5_location_16 <- rowMeans(overlap[,kolomnummercombinatie_16], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_17         <- average_all_minus_5_location[,17]
overlap$average_all_minus_5_location_17 <- rowMeans(overlap[,kolomnummercombinatie_17], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_18         <- average_all_minus_5_location[,18]
overlap$average_all_minus_5_location_18 <- rowMeans(overlap[,kolomnummercombinatie_18], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_19         <- average_all_minus_5_location[,19]
overlap$average_all_minus_5_location_19 <- rowMeans(overlap[,kolomnummercombinatie_19], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_20         <- average_all_minus_5_location[,20]
overlap$average_all_minus_5_location_20 <- rowMeans(overlap[,kolomnummercombinatie_20], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_21         <- average_all_minus_5_location[,21]
overlap$average_all_minus_5_location_21 <- rowMeans(overlap[,kolomnummercombinatie_21], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_22         <- average_all_minus_5_location[,22]
overlap$average_all_minus_5_location_22 <- rowMeans(overlap[,kolomnummercombinatie_22], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_23         <- average_all_minus_5_location[,23]
overlap$average_all_minus_5_location_23 <- rowMeans(overlap[,kolomnummercombinatie_23], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_24         <- average_all_minus_5_location[,24]
overlap$average_all_minus_5_location_24 <- rowMeans(overlap[,kolomnummercombinatie_24], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_25         <- average_all_minus_5_location[,25]
overlap$average_all_minus_5_location_25 <- rowMeans(overlap[,kolomnummercombinatie_25], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_26         <- average_all_minus_5_location[,26]
overlap$average_all_minus_5_location_26 <- rowMeans(overlap[,kolomnummercombinatie_26], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_27         <- average_all_minus_5_location[,27]
overlap$average_all_minus_5_location_27 <- rowMeans(overlap[,kolomnummercombinatie_27], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_28         <- average_all_minus_5_location[,28]
overlap$average_all_minus_5_location_28 <- rowMeans(overlap[,kolomnummercombinatie_28], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_29         <- average_all_minus_5_location[,29]
overlap$average_all_minus_5_location_29 <- rowMeans(overlap[,kolomnummercombinatie_29], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_30         <- average_all_minus_5_location[,30]
overlap$average_all_minus_5_location_30 <- rowMeans(overlap[,kolomnummercombinatie_30], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_31         <- average_all_minus_5_location[,31]
overlap$average_all_minus_5_location_31 <- rowMeans(overlap[,kolomnummercombinatie_31], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_32         <- average_all_minus_5_location[,32]
overlap$average_all_minus_5_location_32 <- rowMeans(overlap[,kolomnummercombinatie_32], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_33         <- average_all_minus_5_location[,33]
overlap$average_all_minus_5_location_33 <- rowMeans(overlap[,kolomnummercombinatie_33], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_34         <- average_all_minus_5_location[,34]
overlap$average_all_minus_5_location_34 <- rowMeans(overlap[,kolomnummercombinatie_34], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_35         <- average_all_minus_5_location[,35]
overlap$average_all_minus_5_location_35 <- rowMeans(overlap[,kolomnummercombinatie_35], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_36         <- average_all_minus_5_location[,36]
overlap$average_all_minus_5_location_36 <- rowMeans(overlap[,kolomnummercombinatie_36], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_37         <- average_all_minus_5_location[,37]
overlap$average_all_minus_5_location_37 <- rowMeans(overlap[,kolomnummercombinatie_37], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_38         <- average_all_minus_5_location[,38]
overlap$average_all_minus_5_location_38 <- rowMeans(overlap[,kolomnummercombinatie_38], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_39         <- average_all_minus_5_location[,39]
overlap$average_all_minus_5_location_39 <- rowMeans(overlap[,kolomnummercombinatie_39], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_40         <- average_all_minus_5_location[,40]
overlap$average_all_minus_5_location_40 <- rowMeans(overlap[,kolomnummercombinatie_40], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_41         <- average_all_minus_5_location[,41]
overlap$average_all_minus_5_location_41 <- rowMeans(overlap[,kolomnummercombinatie_41], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_42         <- average_all_minus_5_location[,42]
overlap$average_all_minus_5_location_42 <- rowMeans(overlap[,kolomnummercombinatie_42], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_43         <- average_all_minus_5_location[,43]
overlap$average_all_minus_5_location_43 <- rowMeans(overlap[,kolomnummercombinatie_43], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_44         <- average_all_minus_5_location[,44]
overlap$average_all_minus_5_location_44 <- rowMeans(overlap[,kolomnummercombinatie_44], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_45         <- average_all_minus_5_location[,45]
overlap$average_all_minus_5_location_45 <- rowMeans(overlap[,kolomnummercombinatie_45], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_46         <- average_all_minus_5_location[,46]
overlap$average_all_minus_5_location_46 <- rowMeans(overlap[,kolomnummercombinatie_46], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_47         <- average_all_minus_5_location[,47]
overlap$average_all_minus_5_location_47 <- rowMeans(overlap[,kolomnummercombinatie_47], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_48         <- average_all_minus_5_location[,48]
overlap$average_all_minus_5_location_48 <- rowMeans(overlap[,kolomnummercombinatie_48], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_49         <- average_all_minus_5_location[,49]
overlap$average_all_minus_5_location_49 <- rowMeans(overlap[,kolomnummercombinatie_49], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_50         <- average_all_minus_5_location[,50]
overlap$average_all_minus_5_location_50 <- rowMeans(overlap[,kolomnummercombinatie_50], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_51         <- average_all_minus_5_location[,51]
overlap$average_all_minus_5_location_51 <- rowMeans(overlap[,kolomnummercombinatie_51], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_52         <- average_all_minus_5_location[,52]
overlap$average_all_minus_5_location_52 <- rowMeans(overlap[,kolomnummercombinatie_52], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_53         <- average_all_minus_5_location[,53]
overlap$average_all_minus_5_location_53 <- rowMeans(overlap[,kolomnummercombinatie_53], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_54         <- average_all_minus_5_location[,54]
overlap$average_all_minus_5_location_54 <- rowMeans(overlap[,kolomnummercombinatie_54], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_55         <- average_all_minus_5_location[,55]
overlap$average_all_minus_5_location_55 <- rowMeans(overlap[,kolomnummercombinatie_55], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_56         <- average_all_minus_5_location[,56]
overlap$average_all_minus_5_location_56 <- rowMeans(overlap[,kolomnummercombinatie_56], na.rm = TRUE)




# gemiddelde van alles - 6 locaties  
average_all_minus_6_location <- combn(Kolomnummertjes,2)
ncol(average_all_minus_6_location)
overlap$average_all_minus_6_location_1 <- NA
overlap$average_all_minus_6_location_2 <- NA
overlap$average_all_minus_6_location_3 <- NA
overlap$average_all_minus_6_location_4 <- NA
overlap$average_all_minus_6_location_5 <- NA
overlap$average_all_minus_6_location_6 <- NA
overlap$average_all_minus_6_location_7 <- NA
overlap$average_all_minus_6_location_8 <- NA
overlap$average_all_minus_6_location_9 <- NA
overlap$average_all_minus_6_location_10<- NA
overlap$average_all_minus_6_location_11<- NA
overlap$average_all_minus_6_location_12 <- NA
overlap$average_all_minus_6_location_13 <- NA
overlap$average_all_minus_6_location_14 <- NA
overlap$average_all_minus_6_location_15 <- NA
overlap$average_all_minus_6_location_16 <- NA
overlap$average_all_minus_6_location_17 <- NA
overlap$average_all_minus_6_location_18 <- NA
overlap$average_all_minus_6_location_19 <- NA
overlap$average_all_minus_6_location_20 <- NA
overlap$average_all_minus_6_location_21 <- NA
overlap$average_all_minus_6_location_22 <- NA
overlap$average_all_minus_6_location_23 <- NA
overlap$average_all_minus_6_location_24 <- NA
overlap$average_all_minus_6_location_25 <- NA
overlap$average_all_minus_6_location_26 <- NA
overlap$average_all_minus_6_location_27 <- NA
overlap$average_all_minus_6_location_28 <- NA

kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_6_location[,1]
overlap$average_all_minus_6_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_2         <- average_all_minus_6_location[,2]
overlap$average_all_minus_6_location_2 <- rowMeans(overlap[,kolomnummercombinatie_2], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_3         <- average_all_minus_6_location[,3]
overlap$average_all_minus_6_location_3 <- rowMeans(overlap[,kolomnummercombinatie_3], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_6_location[,1]
overlap$average_all_minus_6_location_1 <- rowMeans(overlap[,kolomnummercombinatie_1], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_4         <- average_all_minus_6_location[,4]
overlap$average_all_minus_6_location_4 <- rowMeans(overlap[,kolomnummercombinatie_4], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_5         <- average_all_minus_6_location[,5]
overlap$average_all_minus_6_location_5 <- rowMeans(overlap[,kolomnummercombinatie_5], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_6         <- average_all_minus_6_location[,6]
overlap$average_all_minus_6_location_6 <- rowMeans(overlap[,kolomnummercombinatie_6], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_7         <- average_all_minus_6_location[,7]
overlap$average_all_minus_6_location_7 <- rowMeans(overlap[,kolomnummercombinatie_7], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_8         <- average_all_minus_6_location[,8]
overlap$average_all_minus_6_location_8 <- rowMeans(overlap[,kolomnummercombinatie_8], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_9         <- average_all_minus_6_location[,9]
overlap$average_all_minus_6_location_9 <- rowMeans(overlap[,kolomnummercombinatie_9], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_10         <- average_all_minus_6_location[,10]
overlap$average_all_minus_6_location_10 <- rowMeans(overlap[,kolomnummercombinatie_10], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_11         <- average_all_minus_6_location[,11]
overlap$average_all_minus_6_location_11 <- rowMeans(overlap[,kolomnummercombinatie_11], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_12         <- average_all_minus_6_location[,12]
overlap$average_all_minus_6_location_12 <- rowMeans(overlap[,kolomnummercombinatie_12], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_13         <- average_all_minus_6_location[,13]
overlap$average_all_minus_6_location_13 <- rowMeans(overlap[,kolomnummercombinatie_13], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_14         <- average_all_minus_6_location[,14]
overlap$average_all_minus_6_location_14 <- rowMeans(overlap[,kolomnummercombinatie_14], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_15         <- average_all_minus_6_location[,15]
overlap$average_all_minus_6_location_15 <- rowMeans(overlap[,kolomnummercombinatie_15], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_16         <- average_all_minus_6_location[,16]
overlap$average_all_minus_6_location_16 <- rowMeans(overlap[,kolomnummercombinatie_16], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_17         <- average_all_minus_6_location[,17]
overlap$average_all_minus_6_location_17 <- rowMeans(overlap[,kolomnummercombinatie_17], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_18         <- average_all_minus_6_location[,18]
overlap$average_all_minus_6_location_18 <- rowMeans(overlap[,kolomnummercombinatie_18], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_19         <- average_all_minus_6_location[,19]
overlap$average_all_minus_6_location_19 <- rowMeans(overlap[,kolomnummercombinatie_19], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_20         <- average_all_minus_6_location[,20]
overlap$average_all_minus_6_location_20 <- rowMeans(overlap[,kolomnummercombinatie_20], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_21         <- average_all_minus_6_location[,21]
overlap$average_all_minus_6_location_21 <- rowMeans(overlap[,kolomnummercombinatie_21], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_22         <- average_all_minus_6_location[,22]
overlap$average_all_minus_6_location_22 <- rowMeans(overlap[,kolomnummercombinatie_22], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_23         <- average_all_minus_6_location[,23]
overlap$average_all_minus_6_location_23 <- rowMeans(overlap[,kolomnummercombinatie_23], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_24         <- average_all_minus_6_location[,24]
overlap$average_all_minus_6_location_24 <- rowMeans(overlap[,kolomnummercombinatie_24], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_25         <- average_all_minus_6_location[,25]
overlap$average_all_minus_6_location_25 <- rowMeans(overlap[,kolomnummercombinatie_25], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_26         <- average_all_minus_6_location[,26]
overlap$average_all_minus_6_location_26 <- rowMeans(overlap[,kolomnummercombinatie_26], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_27         <- average_all_minus_6_location[,27]
overlap$average_all_minus_6_location_27 <- rowMeans(overlap[,kolomnummercombinatie_27], na.rm = TRUE)

kolomnummercombinatie           <- NULL
kolomnummercombinatie_28         <- average_all_minus_6_location[,28]
overlap$average_all_minus_6_location_28 <- rowMeans(overlap[,kolomnummercombinatie_28], na.rm = TRUE)


# gemiddelde van alles - 7 locaties 
average_all_minus_7_location <- combn(Kolomnummertjes,1)
ncol(average_all_minus_7_location)
overlap$average_all_minus_7_location_1 <- NA
overlap$average_all_minus_7_location_2 <- NA
overlap$average_all_minus_7_location_3 <- NA
overlap$average_all_minus_7_location_4 <- NA
overlap$average_all_minus_7_location_5 <- NA
overlap$average_all_minus_7_location_6 <- NA
overlap$average_all_minus_7_location_7 <- NA
overlap$average_all_minus_7_location_8 <- NA

kolomnummercombinatie           <- NULL
kolomnummercombinatie_1         <- average_all_minus_7_location[,1]
overlap$average_all_minus_7_location_1 <- overlap[,kolomnummercombinatie_1]

kolomnummercombinatie           <- NULL
kolomnummercombinatie_2         <- average_all_minus_7_location[,2]
overlap$average_all_minus_7_location_2 <- overlap[,kolomnummercombinatie_2]

kolomnummercombinatie           <- NULL
kolomnummercombinatie_3         <- average_all_minus_7_location[,3]
overlap$average_all_minus_7_location_3 <- overlap[,kolomnummercombinatie_3]

kolomnummercombinatie           <- NULL
kolomnummercombinatie_4         <- average_all_minus_7_location[,4]
overlap$average_all_minus_7_location_4 <- overlap[,kolomnummercombinatie_4]

kolomnummercombinatie           <- NULL
kolomnummercombinatie_5        <- average_all_minus_7_location[,5]
overlap$average_all_minus_7_location_5 <- overlap[,kolomnummercombinatie_5]

kolomnummercombinatie           <- NULL
kolomnummercombinatie_6        <- average_all_minus_7_location[,6]
overlap$average_all_minus_7_location_6 <- overlap[,kolomnummercombinatie_6]

kolomnummercombinatie           <- NULL
kolomnummercombinatie_7         <- average_all_minus_7_location[,7]
overlap$average_all_minus_7_location_7 <- overlap[,kolomnummercombinatie_7]

kolomnummercombinatie           <- NULL
kolomnummercombinatie_8         <- average_all_minus_7_location[,8]
overlap$average_all_minus_7_location_8 <- overlap[,kolomnummercombinatie_8]

#Even wegschrijven
write.csv(overlap, "data_sensoranalysis.csv")
head(overlap)

#SSE berekening: sum of squared errors of prediction
SSE_minus_1 <- c(sum((overlap$average_all - overlap$average_all_minus_1_location_1)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_1_location_2)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_1_location_3)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_1_location_4)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_1_location_5)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_1_location_6)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_1_location_7)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_1_location_8)^2,na.rm = TRUE))


SSE_minus_2 <- c(sum((overlap$average_all - overlap$average_all_minus_2_location_1)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_2)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_3)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_4)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_5)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_6)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_7)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_8)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_9)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_10)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_11)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_12)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_13)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_14)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_15)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_16)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_17)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_18)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_19)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_20)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_21)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_22)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_23)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_24)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_25)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_26)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_27)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_2_location_28)^2,na.rm = TRUE))


SSE_minus_3 <- c(sum((overlap$average_all - overlap$average_all_minus_3_location_1)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_2)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_3)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_4)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_5)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_6)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_7)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_8)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_9)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_10)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_11)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_12)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_13)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_14)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_15)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_16)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_17)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_18)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_19)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_20)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_21)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_22)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_23)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_24)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_25)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_26)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_27)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_28)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_29)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_30)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_31)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_32)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_33)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_34)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_35)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_36)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_37)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_38)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_39)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_40)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_41)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_42)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_43)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_44)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_45)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_46)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_47)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_48)^2,na.rm = TRUE),            
                 sum((overlap$average_all - overlap$average_all_minus_3_location_49)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_50)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_51)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_52)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_53)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_54)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_55)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_3_location_56)^2,na.rm = TRUE))



SSE_minus_4 <- c(sum((overlap$average_all - overlap$average_all_minus_4_location_1)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_2)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_3)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_4)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_5)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_6)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_7)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_8)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_9)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_10)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_11)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_12)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_13)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_14)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_15)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_16)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_17)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_18)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_19)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_20)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_21)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_22)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_23)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_24)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_25)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_26)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_27)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_28)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_29)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_30)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_31)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_32)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_33)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_34)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_35)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_36)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_37)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_38)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_39)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_40)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_41)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_42)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_43)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_44)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_45)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_46)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_47)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_48)^2,na.rm = TRUE),            
                 sum((overlap$average_all - overlap$average_all_minus_4_location_49)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_50)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_51)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_52)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_53)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_54)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_55)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_56)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_57)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_58)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_59)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_60)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_61)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_62)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_63)^2,na.rm = TRUE),            
                 sum((overlap$average_all - overlap$average_all_minus_4_location_64)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_65)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_66)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_67)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_68)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_69)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_4_location_70)^2,na.rm = TRUE))


SSE_minus_5 <- c(sum((overlap$average_all - overlap$average_all_minus_5_location_1)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_2)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_3)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_4)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_5)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_6)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_7)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_8)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_9)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_10)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_11)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_12)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_13)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_14)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_15)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_16)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_17)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_18)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_19)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_20)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_21)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_22)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_23)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_24)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_25)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_26)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_27)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_28)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_29)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_30)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_31)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_32)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_33)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_34)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_35)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_36)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_37)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_38)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_39)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_40)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_41)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_42)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_43)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_44)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_45)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_46)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_47)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_48)^2,na.rm = TRUE),            
                 sum((overlap$average_all - overlap$average_all_minus_5_location_49)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_50)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_51)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_52)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_53)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_54)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_55)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_5_location_56)^2,na.rm = TRUE))



SSE_minus_6 <- c(sum((overlap$average_all - overlap$average_all_minus_6_location_1)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_2)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_3)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_4)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_5)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_6)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_7)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_8)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_9)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_10)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_11)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_12)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_13)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_14)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_15)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_16)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_17)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_18)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_19)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_20)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_21)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_22)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_23)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_24)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_25)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_26)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_27)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_6_location_28)^2,na.rm = TRUE))


SSE_minus_7 <- c(sum((overlap$average_all - overlap$average_all_minus_7_location_1)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_7_location_2)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_7_location_3)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_7_location_4)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_7_location_5)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_7_location_6)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_7_location_7)^2,na.rm = TRUE),
                 sum((overlap$average_all - overlap$average_all_minus_7_location_8)^2,na.rm = TRUE))

SSE_minus_1 <- as.data.frame(SSE_minus_1)
SSE_minus_1$Stations <- "All-1"
colnames(SSE_minus_1) <- c("SSE", "Stations")

SSE_minus_2 <- as.data.frame(SSE_minus_2)
SSE_minus_2$Stations <- "All-2"
colnames(SSE_minus_2) <- c("SSE", "Stations")

SSE_minus_3 <- as.data.frame(SSE_minus_3)
SSE_minus_3$Stations <- "All-3"
colnames(SSE_minus_3) <- c("SSE", "Stations")

SSE_minus_4 <- as.data.frame(SSE_minus_4)
SSE_minus_4$Stations <- "All-4"
colnames(SSE_minus_4) <- c("SSE", "Stations")

SSE_minus_5 <- as.data.frame(SSE_minus_5)
SSE_minus_5$Stations <- "All-5"
colnames(SSE_minus_5) <- c("SSE", "Stations")

SSE_minus_6 <- as.data.frame(SSE_minus_6)
SSE_minus_6$Stations <- "All-6"
colnames(SSE_minus_6) <- c("SSE", "Stations")

SSE_minus_7 <- as.data.frame(SSE_minus_7)
SSE_minus_7$Stations <- "All-7"
colnames(SSE_minus_7) <- c("SSE", "Stations")

SSE_data <- rbind(SSE_minus_1,SSE_minus_2,SSE_minus_3,SSE_minus_4,SSE_minus_5,SSE_minus_6,SSE_minus_7)
SSE_data$SSE <- as.numeric(as.character(SSE_data$SSE))
SSE_data$Stations <- as.factor(SSE_data$Stations)
head(SSE_data)
summary(SSE_data)

plot(SSE_data$Stations,SSE_data$SSE)
##plot the SSE values vs the number of stations combined
ggplot(SSE_data, aes(Stations, SSE))+ geom_boxplot()+
  xlab("nr of zones combined")+
  ggtitle("HiMod data of 2016")
#test if number of stations is significant different 
kruskal.test(SSE ~ Stations, data = SSE_data)
pairwise.wilcox.test(SSE_data$SSE, SSE_data$Stations, p.adjust.method="none")


