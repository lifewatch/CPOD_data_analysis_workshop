#### 0. Load data ####
# Defining a working directory facilitates the access to folders and documents. This can be done by creating an R project or by code setwd().
getwd() # Check your current working directory.
list.files() # See the files in your working directory

# In the folder 'Data', you can find several data tables, downloaded from the RShiny application. The names indicate the quality of the detected porpoise clicks. 
list.files("Data day 1")

# The file 'hi_mod.tab' contains the detections of quality hi and mod pooled together for each hour. We will use this file for today's session.

# Importing the data is possible by code or by the 'Import Dataset' button in the Environment window on the right.
poddata <- read.table("Data day 1/hi_mod_1hr.tab", "\t", header = T)

#### 1. Data preparation ####
str(poddata) # Investigate the structure of the object poddata.

# Deployment_fk = An identifying number assigned to each deployment
# Time = Date and time
# Milliseconds = The sum of milliseconds in which a porpoise was detected
# Number_clicks_filtered = The amount of clicks, identified as porpoise clicks
# Number_clicks_total = The total amount of clicks, from different sources
# Lost_minutes = The number of minutes for which the C-POD reached a detection threshold and recorded the minute only partially.
# Recorded = The number of minutes that the C-POD was on.
# Dpm = The sum of Detection Positive Minutes.
# Dp10m = The sum of Detection Positive 10 Minutes.
# Station = The name of the station.
# Latitude
# Longitude
# Mooring_type = Indicates whether the C-POD was deployed on a surface-buoy or a bottom mooring.
# Receiver = The name of the receiver.

#### 1.1 Organizing the data and adding relevant variables ####
# Adding the variable quality might be useful for future reference.
poddata$Quality <- "hi+mod"

# Detection positive hour will indicate whether there was a detection within the hour (1) or not (0).
poddata$Dph <- ifelse(poddata$Dpm >0, 1, 0)


poddata$month <- month(poddata$Time)
poddata$year <- year(poddata$Time)


monthly = poddata %>%
  group_by(month)%>%
  select(month, Dph)


df <- aggregate(monthly$Dph, list(monthly$month), sum)
colnames(df)[1] <- "Month"
colnames(df)[2] <- "Dph"
#par(mar=c(6,4.1,4.1,2.1))
barplot(df$x, names.arg=df$Group.1, cex.names=0.8, las=2)
title(ylab="Number of eels", line = 3, cex.lab=1)
title(xlab="Location ID", line = 4, cex.lab=1)

rec = poddata %>%
  group_by(month)%>%
  select(month, Recorded)
df2 <- aggregate(rec$Recorded, list(rec$month), sum)
colnames(df2)[1] <- "Month"
colnames(df2)[2] <- "rec"


df <- merge(df,df2, by="Month")


plot(df$Dph, col="red", xlab = "Month", ylab="Dph", type = "b")
par(new = T)
with(df,plot(df$rec, col = "blue",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))
axis(4)
with(df,mtext("Active recordings",side=4, line=2.5))


df$Dph_adj <- df$Dph / df$rec 
barplot(df$Dph_adj, names.arg=df$Month, cex.names=0.8, las=2)

dev.off()
par(mfrow=c(2,2))
#### 2014 ####
df2014 <- filter(poddata, year == "2014")

monthly = df2014 %>%
  group_by(month)%>%
  select(month, Dph)


df <- aggregate(monthly$Dph, list(monthly$month), sum)
colnames(df)[1] <- "Month"
colnames(df)[2] <- "Dph"


rec = poddata %>%
  group_by(month)%>%
  select(month, Recorded)
df2 <- aggregate(rec$Recorded, list(rec$month), sum)
colnames(df2)[1] <- "Month"
colnames(df2)[2] <- "rec"


df <- merge(df,df2, by="Month")

#plot(df$Dph, col="red", xlab = "Month", ylab="Dph", type = "b")
#par(new = T)
#with(df,plot(df$rec, col = "blue",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))
#axis(4)
#with(df,mtext("Active recordings",side=4, line=2.5))

df$Dph_adj <- df$Dph / df$rec 
#barplot(df$Dph_adj, names.arg=df$Month, main = "2014",cex.names=0.8, las=2)

#### LifeWatch Temperature and Chlorophyll data ####
lw_reg <- read.table("Data day 1/Metadata/LW_reg.tab")
lw_reg$Month <- parse_date_time(as.character(lw_reg$Month), orders = "ymd")
round(unique(poddata$Latitude), digits = 1) %in% round(unique(lw_reg$Latitude), digits = 1)

lw_reg$x <- round(lw_reg$Longitude, digits = 1)
lw_reg$y <- round(lw_reg$Latitude, digits = 1)

lw_reg$month <- month(lw_reg$Month)
lw_reg$month <- factor(lw_reg$month)

lw_reg$year <- year(lw_reg$Month)
lw_reg$year <- factor(lw_reg$year)

lw_reg <- filter(lw_reg, year == "2014")

# Select temp
temp <- lw_reg %>%
  group_by(month)%>%
  select(month, Temp)

temp <- na.omit(temp)

temp <- aggregate(temp$Temp, list(temp$month), mean)
colnames(temp)[1] <- "Month"
colnames(temp)[2] <- "Temp"

df$Month <- factor(df$Month)
temp$Month <- factor(temp$Month)


#df_2014 <- merge(df,temp, by="Month")
df_2014 <- left_join(df, temp)
df_2014[is.na(df_2014)] <- 0

# Select chlorophyl
chl <- lw_reg %>%
  group_by(month)%>%
  select(month, Chla)

chl <- na.omit(chl)

chl <- aggregate(chl$Chla, list(chl$month), mean)
colnames(chl)[1] <- "Month"
colnames(chl)[2] <- "Chla"

chl$Month <- factor(chl$Month)

#df_2014 <- merge(df_2014,chl, by="Month")
df_2014 <- left_join(df_2014, chl)
df_2014[is.na(df_2014)] <- 0

# Create final plot
barplot(df_2014$Dph_adj, names.arg=df_2014$Month, main = "2014",ylab = "Dph_adj",cex.names=0.8, las=2)
par(new = T)
with(df_2014,plot(df_2014$Temp, col = "blue",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))
axis(4)
with(df,mtext("Temperature (°C)",side=4, line=2.5))
par(new = T)
with(df_2014,plot(df_2014$Chla, col = "red",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))


#### 2015 ####
df2015 <- filter(poddata, year == "2015")

monthly = df2015 %>%
  group_by(month)%>%
  select(month, Dph)


df <- aggregate(monthly$Dph, list(monthly$month), sum)
colnames(df)[1] <- "Month"
colnames(df)[2] <- "Dph"


rec = poddata %>%
  group_by(month)%>%
  select(month, Recorded)
df2 <- aggregate(rec$Recorded, list(rec$month), sum)
colnames(df2)[1] <- "Month"
colnames(df2)[2] <- "rec"


df <- merge(df,df2, by="Month")

#plot(df$Dph, col="red", xlab = "Month", ylab="Dph", type = "b")
#par(new = T)
#with(df,plot(df$rec, col = "blue",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))
#axis(4)
#with(df,mtext("Active recordings",side=4, line=2.5))

df$Dph_adj <- df$Dph / df$rec 
#barplot(df$Dph_adj, names.arg=df$Month, main = "2015",cex.names=0.8, las=2)

#### LifeWatch Temperature and Chlorophyll data ####
lw_reg <- read.table("Data day 1/Metadata/LW_reg.tab")
lw_reg$Month <- parse_date_time(as.character(lw_reg$Month), orders = "ymd")
round(unique(poddata$Latitude), digits = 1) %in% round(unique(lw_reg$Latitude), digits = 1)

lw_reg$x <- round(lw_reg$Longitude, digits = 1)
lw_reg$y <- round(lw_reg$Latitude, digits = 1)

lw_reg$month <- month(lw_reg$Month)
lw_reg$month <- factor(lw_reg$month)

lw_reg$year <- year(lw_reg$Month)
lw_reg$year <- factor(lw_reg$year)

lw_reg <- filter(lw_reg, year == "2015")

# Select temp
temp <- lw_reg %>%
  group_by(month)%>%
  select(month, Temp)

temp <- na.omit(temp)

temp <- aggregate(temp$Temp, list(temp$month), mean)
colnames(temp)[1] <- "Month"
colnames(temp)[2] <- "Temp"

df$Month <- factor(df$Month)
temp$Month <- factor(temp$Month)

#df_2015 <- merge(df,temp, by="Month")
df_2015 <- left_join(df, temp)
df_2015[is.na(df_2015)] <- 0



# Select chlorophyl
chl <- lw_reg %>%
  group_by(month)%>%
  select(month, Chla)

chl <- na.omit(chl)

chl <- aggregate(chl$Chla, list(chl$month), mean)
colnames(chl)[1] <- "Month"
colnames(chl)[2] <- "Chla"



#df_2015 <- merge(df,chl, by="Month")
df_2015 <- left_join(df_2015, chl)

# Create final plot
barplot(df_2015$Dph_adj, names.arg=df_2015$Month, main = "2015",cex.names=0.8, las=2)
par(new = T)
with(df_2015,plot(df_2015$Temp, col = "blue",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))
axis(4)
with(df,mtext("Temperature (°C)",side=4, line=2.5))
par(new = T)
with(df_2015,plot(df_2015$Chla, col = "red",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))


#### 2016 ####
df2016 <- filter(poddata, year == "2016")

monthly = df2016 %>%
  group_by(month)%>%
  select(month, Dph)


df <- aggregate(monthly$Dph, list(monthly$month), sum)
colnames(df)[1] <- "Month"
colnames(df)[2] <- "Dph"


rec = poddata %>%
  group_by(month)%>%
  select(month, Recorded)
df2 <- aggregate(rec$Recorded, list(rec$month), sum)
colnames(df2)[1] <- "Month"
colnames(df2)[2] <- "rec"


df <- merge(df,df2, by="Month")

#plot(df$Dph, col="red", xlab = "Month", ylab="Dph", type = "b")
#par(new = T)
#with(df,plot(df$rec, col = "blue",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))
#axis(4)
#with(df,mtext("Active recordings",side=4, line=2.5))

df$Dph_adj <- df$Dph / df$rec 
#barplot(df$Dph_adj, names.arg=df$Month, main = "2016",cex.names=0.8, las=2)

#### LifeWatch Temperature and Chlorophyll data ####
lw_reg <- read.table("Data day 1/Metadata/LW_reg.tab")
lw_reg$Month <- parse_date_time(as.character(lw_reg$Month), orders = "ymd")
round(unique(poddata$Latitude), digits = 1) %in% round(unique(lw_reg$Latitude), digits = 1)

lw_reg$x <- round(lw_reg$Longitude, digits = 1)
lw_reg$y <- round(lw_reg$Latitude, digits = 1)

lw_reg$month <- month(lw_reg$Month)
lw_reg$month <- factor(lw_reg$month)

lw_reg$year <- year(lw_reg$Month)
lw_reg$year <- factor(lw_reg$year)

lw_reg <- filter(lw_reg, year == "2016")

# Temp
temp <- lw_reg %>%
  group_by(month)%>%
  select(month, Temp)

temp <- na.omit(temp)

temp <- aggregate(temp$Temp, list(temp$month), mean)
colnames(temp)[1] <- "Month"
colnames(temp)[2] <- "Temp"


df$Month <- factor(df$Month)
temp$Month <- factor(temp$Month)


#df_2016 <- merge(df,temp, by="Month")
df_2016 <- left_join(df, temp)
df_2016[is.na(df_2016)] <- 0


# Select chlorophyl
chl <- lw_reg %>%
  group_by(month)%>%
  select(month, Chla)

chl <- na.omit(chl)

chl <- aggregate(chl$Chla, list(chl$month), mean)
colnames(chl)[1] <- "Month"
colnames(chl)[2] <- "Chla"

chl$Month <- factor(chl$Month)


#df_2016 <- merge(df_2016,chl, by="Month")
df_2016 <- left_join(df_2016, chl)
df_2016[is.na(df_2016)] <- 0


# Create final plot
barplot(df_2016$Dph_adj, names.arg=df_2016$Month, main = "2016",cex.names=0.8, las=2)
par(new = T)
with(df_2016,plot(df_2016$Temp, col = "blue",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))
axis(4)
with(df,mtext("Temperature (°C)",side=4, line=2.5))
par(new = T)
with(df_2016,plot(df_2016$Chla, col = "red",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))



#### 2017 ####
df2017 <- filter(poddata, year == "2017")

monthly = df2017 %>%
  group_by(month)%>%
  select(month, Dph)


df <- aggregate(monthly$Dph, list(monthly$month), sum)
colnames(df)[1] <- "Month"
colnames(df)[2] <- "Dph"


rec = poddata %>%
  group_by(month)%>%
  select(month, Recorded)
df2 <- aggregate(rec$Recorded, list(rec$month), sum)
colnames(df2)[1] <- "Month"
colnames(df2)[2] <- "rec"


df <- merge(df,df2, by="Month")

#plot(df$Dph, col="red", xlab = "Month", ylab="Dph", type = "b")
#par(new = T)
#with(df,plot(df$rec, col = "blue",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))
#axis(4)
#with(df,mtext("Active recordings",side=4, line=2.5))

df$Dph_adj <- df$Dph / df$rec 
#barplot(df$Dph_adj, names.arg=df$Month, main = "2017",cex.names=0.8, las=2)



# Create final plot
df_2017 <- df
barplot(df_2017$Dph_adj, names.arg=df$Month, main = "2017",cex.names=0.8, las=2)
#par(new = T)
#with(df_2017,plot(df_2017$Temp, col = "blue",type = "b", xaxt="n", yaxt="n",xlab="",ylab=""))
#axis(4)
#with(df,mtext("Temperature (°C)",side=4, line=2.5))




