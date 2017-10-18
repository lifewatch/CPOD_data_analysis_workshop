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

# The exact locations for some C-POD deployments changed slightly over the years. Adding a variable 'Zone' will be useful to link the data of adjacent stations.
poddata$Zone<- ifelse(poddata$Station=="bpns-Oostendebank Oost", "Oostende",
              ifelse(poddata$Station=="bpns-Reefballs-cpower", "bnps-Reefballs-cpower",
              ifelse(poddata$Station== "bpns-Wenduinebankw", "Oostende",
              ifelse(poddata$Station=="bpns-WK16", "Oostende",
              ifelse(poddata$Station=="bpns-Belwind C05", "bpns-Belwind C05",
              ifelse(poddata$Station== "bpns-D1", "Middelkerke",
              ifelse(poddata$Station=="bpns-WK9", "WK9en12",
              ifelse(poddata$Station=="bpns-Gootebank", "Gootebank",
              ifelse(poddata$Station=="bpns-VG2" , "Gootebank",
              ifelse(poddata$Station== "bpns-WK12", "WK9en12",
              ifelse(poddata$Station== "bpns-Reefballs Belwind", "bpns-Reefballs Belwind",
              ifelse(poddata$Station==  "bpns-Oostdyck West",  "bpns-Oostdyck West", 
              ifelse(poddata$Station== "bpns-LST420", "Middelkerke",
              ifelse(poddata$Station== "bpns-Middelkerke South", "Middelkerke",
              ifelse(poddata$Station== "bpns-Lottobuoy", "bpns-Lottobuoy",
              ifelse(poddata$Station== "bpns-Birkenfels", "bpns-Birkenfels", NA))))))))))))))))
poddata$Zone<-as.factor(poddata$Zone)
levels(poddata$Zone)
# Click frequency (also called PPM, Porpoise Positive Minutes) and Click intensity are common metrics in acoustic porpoise research.
poddata$Click_frequency <- poddata$Dpm/poddata$Recorded
poddata$Click_intensity <- poddata$Number_clicks_filtered/poddata$Click_frequency

# Understanding the content of a variable, we can assign an appropriate data type.
poddata$Receiver <- as.factor(poddata$Receiver)
poddata$Station <- as.factor(poddata$Station)
poddata$Mooring_type <- as.factor(poddata$Mooring_type)
poddata$Quality <- as.factor(poddata$Quality)
poddata$Species <- as.factor(poddata$Species)
poddata$Zone <- as.factor(poddata$Zone)

# The lubridate package provides some useful tools to handle dates and times.
library(lubridate)

# To install a package use: install.packages("lubridate")

# Some explanation on lubridate:
today()
now()
moonlanding <- "24-07-1969 16:50:35"
moonlanding <- parse_date_time(moonlanding, orders = "dmy HMS")
day(moonlanding)
date(moonlanding)

rm(moonlanding)

# We can now transform our own variable Time.
poddata$Time <- parse_date_time(poddata$Time, orders = "ymd HMS")

# The summary function gives a quick peek at our data.
summary(poddata)

#### 1.2 Data subsetting ####
# Subset the data with generic R code: some examples
poddata[1,]
poddata[1:100,]
poddata[,1]
poddata[,c(2,4)]

unique(poddata$Station) 
poddata[poddata$Station == "bpns-Lottobuoy",]

# The R package dplyr provides useful functions for data organization.
library(dplyr)

# The filter function allows for subsetting of rows.
filter(poddata, Station == "bpns-Lottobuoy")
filter(poddata, Station == "bpns-Lottobuoy" | Station == "bpns-WK12")
filter(poddata, Station != "bpns-Lottobuoy")
filter(poddata, Dpm > 0)
filter(poddata, Dpm > 0 & Station == "bpns-Lottobuoy")

# The select function allows for subsetting of columns.
select(poddata, Receiver)
select(poddata, -Receiver)
select(poddata, Station, Latitude, Longitude)

# Side note: some function names of dplyr are identical to function names of other packages. This can cause an error when using the function. In this case, write the package name and :: before the function.
dplyr::select(poddata, Receiver)

# Side note: this can be combined with the lubridate functions.
filter(poddata, year(Time) == 2017)

# A value of zero for the variable Recorded indicates the C-POD was not 'on' during the entire hour. Hence, we can filter out these rows.
poddata <- filter(poddata, Recorded > 0)

# A value of 60 for the variable Recorded indicates the C-POD was not 'on' for some minutes during the hour. We can choose whether to filter out these rows.
poddata <- filter(poddata, Recorded == 60)

#### 1.3 Summarizing data ####
# Combining the functions group_by and summarise allows for simple organization: an example.
poddata_group <- group_by(poddata, Zone)
groups(poddata_group)
poddata_sum <- summarise(poddata_group,
                         Dpm_median = median(Dpm),
                         Dpm_mean = mean(Dpm),
                         Dpm_max = max(Dpm))
poddata_sum

rm(poddata_group, poddata_sum) # Remove the example data frames

# We will now apply these functions to get a new data frame that summarizes the detections per day.
poddata_day <- poddata # Copy the data in a new data frame.

# Three ways to take the date out of our time variable. Using the lubridate::date() function is very elegant, HOWEVER: lubridate has some bugs when combined with the packages dplyr/plyr.
poddata_day$Time <- date(poddata_day$Time)
poddata_day$Time <- parse_date_time(paste(year(poddata$Time), month(poddata$Time), day(poddata$Time), sep = "-"), orders = "ymd")
poddata_day$Time <- as.POSIXct(paste(year(poddata$Time), month(poddata$Time), day(poddata$Time), sep = "-"),format="%Y-%m-%d", tz="UTC")

# Now we can group by Time.
poddata_group <- dplyr::group_by(poddata_day, Deployment_fk, Receiver, Station, Zone, Latitude, Longitude, Mooring_type, Quality, Time)
# :: will load the package but will not attach it to the search path, you call the external variable group_by from the package dplyr
poddata_day <- dplyr::summarise(poddata_group,
                         Milliseconds = sum(Milliseconds),
                         Number_clicks_filtered = sum(Number_clicks_filtered),
                         Number_clicks_total = sum(Number_clicks_total),
                         Lost_minutes = sum(Lost_minutes),
                         Dpm = sum(Dpm),
                         Dp10m = sum(Dp10m),
                         Dph = sum(Dph),
                         Recorded = sum(Recorded))
poddata_day$Click_frequency <- poddata_day$Dpm/poddata_day$Recorded
poddata_day$Click_intensity <- poddata_day$Number_clicks_filtered/poddata_day$Click_frequency
rm(poddata_group)

# Again, we can choose to only use full days.
poddata_day <- filter(poddata_day, Recorded == 24*60)
#hoofdstukken maken via vier # dan titel en afsluiten met 4#
#### 2. Data exploration ####
# The ggplot2 packages enables the construction of plots. We will first use this to make simple plots for exploration and later apply more advanced plotting techniques.
library(ggplot2)

#### 2.1 A first look at the data availability ####
# Making a graph in ggplot: an example to test the data availability.
ggplot(data = poddata_day, aes(x= Time, y = Quality)) # This makes an empty plot

# Three ways to code the same plot.
ggplot(data = poddata_day, aes(x= Time, y = Quality)) + geom_point()
ggplot() + geom_point(data = poddata_day, aes(x= Time, y = Quality))
ggplot(data = poddata_day) + geom_point(aes(x= Time, y = Quality))

# Now we can investigate the data availability per station.
ggplot(data = poddata_day) + geom_point(aes(x= Time, y = Station))

# Same for zones. By defining the theme argument, we can change the graphics of our plot.
ggplot(data = poddata_day) + geom_point(aes(x= Time, y = Zone)) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 16),
        axis.title = element_blank())


#### 2.2 In line with Zuur's protocol for data exploration ####
# In the next steps, we will visually explore the data in line with the first four steps of Zuur's protocol.

#### 2.2.1 Step 1: Are there outliers in Y and X? ####
# Construct a basic boxplot to visualize the spread of the data and check for outliers.
ggplot(data = poddata_day) + 
  geom_boxplot(aes(x = factor(0), y = Dpm), fill="#008EAA") + 
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.title.x = element_blank()) +
  labs(y = "DPM per day")

ggplot(data = poddata_day) + 
  geom_boxplot(aes(x = factor(0), y = Click_frequency), fill="#008EAA") + 
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.title.x = element_blank()) +
  labs(y = "Click frequency per day")

# We can do the same for Dp10m and Dph.
ggplot(data = poddata_day) + 
  geom_boxplot(aes(x = factor(0), y = Dp10m), fill="#008EAA") + 
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.title.x = element_blank()) +
  labs(y = "DP10M per day")

#input van Jan VAB:
#meer info uit dotchart halen ook volgens zuur of er outliers zijn of niet
dotchart(poddata_day$Dpm)
#

ggplot(data = poddata_day) + 
  geom_boxplot(aes(x = factor(0), y = Dph), fill="#008EAA") + 
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(0,24), breaks = c(0, 4, 8, 12, 16, 20, 24)) + 
  labs(y = "DPH per day")

# Cleveland dotplots allow for a further inspection of outliers.
#ongeveer hetzelfde als die dotchart van Jan VAB
ggplot(data = poddata_day) + 
  geom_point(aes(x= Dpm, y = Time)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_blank()) +
  labs(x = "DPM per day")

ggplot(data = poddata_day) + 
  geom_point(aes(x= Dpm, y = Station)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_blank()) +
  labs(x = "DPM per day")

ggplot(data = poddata_day) + 
  geom_point(aes(x= Dpm, y = Zone)) +
  theme_bw() + 
  theme(axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_blank()) +
  labs(x = "DPM per day")

#geen outliers gevonden

##### 2.2.2 Step 2: Do we have homogeneity of variance? #####
# To check for homogeinity of variance, we can make conditional boxplots.
ggplot(data = poddata_day) + 
  geom_boxplot(aes(x = as.factor(month(Time)), y = Dpm), fill="#008EAA") + 
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.title.x = element_blank()) +
  labs(y = "DPM per day")

ggplot(data = poddata_day) + 
  geom_boxplot(aes(x = Zone, y = Dpm), fill="#008EAA") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.title.x = element_blank()) +
  labs(y = "DPM per day")
###bij dpm zelf niet homogeen verdeeld
ggplot(data = poddata_day) + 
  geom_boxplot(aes(x = as.factor(month(Time)), y = Dpm), fill="#008EAA") + 
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "DPM per day") + 
  facet_wrap(~Zone)
##pj: vraagstelling Seizoenaliteit; dan kijken of homogeniteit tss stations hetzelfde is

#gaten in de data: welk station mag je dan effectief meenemen in je analyse
#daarvoor kijken naar je plotje waar er data is per zone over de time
#beslist om birkenfels eruit te gooien
#belwind C05 bij reefballs Belwind als 1 zone

#Bob: wat is de invloed van de pod zelf want elk cpod heeft een eigen detectie vermogen
#pod nr  dus meenemen als random factor in je analyse

##### 2.2.3 Step 3: Are the data normally distributed? #####
# Although we can already suspect the data are not normally distributed, a histogram can shed light on the distribution of a variable.
ggplot(data = poddata_day) + 
  geom_histogram(aes(x = Dpm), fill="#008EAA") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_blank()) +
  labs(x = "DPM per day")

ggplot(data = poddata_day) + 
  geom_histogram(aes(x = Dpm), fill="#008EAA",binwidth = 10) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_blank()) +
  labs(x = "DPM per day")

ggplot(data = poddata_day) + 
  geom_histogram(aes(x = Dpm), fill="#008EAA",binwidth = 10) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_blank()) +
  labs(x = "DPM per day")

##### 2.2.4 Step 4: Are there lots of zeros in the data? #####
# Next, Zuur advises to make a frequency plot to check for the amount of zeros in the data. The geom_rug argument facilitates the 
ggplot(data = poddata_day) + 
  geom_histogram(aes(x = Dpm), fill="#008EAA", binwidth = 1, position = "dodge", size = 0.5) +
  geom_rug(aes(x = Dpm))+
  theme_bw() + 
  theme(axis.title = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        legend.title = element_blank()) +
  labs(x = "DPM per day") 

#### Further exploration: depends on research question! Step 5-7 of Zuur's protocol might be useful when using metadata!
