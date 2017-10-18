### Higher occurrence in wind mill area? ###
### Inge van der Knaap, Klaas Deneudt, Tom Augustijns, Jolien Goossens

library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)

##### plotting surface and bottom mooring for working group ####

unique (poddata_day$Zone) 

# bpns-Lottobuoy
# bnps-Reefballs-cpower

# DPM vs DP10M

## compairing all Dpm values to mooring type
## geom point plot 
poddata_day %>% 
  filter(Time>"2015-09-01 UTC") %>% 
  ggplot() +
  geom_point(aes(Time, Dpm, colour=factor(Mooring_type))) 

## geom boxplot 
poddata_day %>% 
  filter(Time>"2015-09-01 UTC") %>% 
  ggplot() +
  geom_boxplot(aes(Time, Dpm, group=Mooring_type, colour=factor(Mooring_type)))

## comparing 
# bpns-Lottobuoy
# bnps-Reefballs-cpower
poddata_day %>% 
  filter(Time>"2015-09-01 UTC") %>% 
  filter(Zone == "bnps-Reefballs-cpower" | Zone == "bpns-Lottobuoy") %>% 
  ggplot() +
  geom_point(aes(Time, Dpm, colour=factor(Mooring_type)))  

## boxplot 
poddata_day %>% 
  filter(Time>"2015-09-01 UTC") %>% 
  filter(Zone == "bnps-Reefballs-cpower" | Zone == "bpns-Lottobuoy") %>% 
  ggplot() +  
  geom_boxplot(aes(Time, Dpm, group=Mooring_type, colour=factor(Mooring_type)))

##################### Dp10m
## compairing all Dpm values to mooring type
## geom point plot 
poddata_day %>% 
  filter(Time>"2015-09-01 UTC") %>% 
  ggplot() +
  geom_point(aes(Time, Dp10m, colour=factor(Mooring_type)))

## geom boxplot 
poddata_day %>% 
  filter(Time>"2015-09-01 UTC") %>% 
  ggplot() +
  geom_boxplot(aes(Time, Dp10m, group=Mooring_type, colour=factor(Mooring_type)))

## comparing 
# bpns-Lottobuoy
# bnps-Reefballs-cpower
poddata_day %>% 
  filter(Time>"2015-09-01 UTC") %>% 
  filter(Zone == "bnps-Reefballs-cpower" | Zone == "bpns-Lottobuoy") %>% 
  ggplot() +
  geom_point(aes(Time, Dp10m, colour=factor(Mooring_type)))  


poddata_day %>% 
  filter(Time>"2015-09-01 UTC") %>% 
  filter(Zone == "bnps-Reefballs-cpower" | Zone == "bpns-Lottobuoy") %>% 
  ggplot() +
  geom_boxplot(aes(Time, Dp10m, group=Mooring_type, colour=factor(Mooring_type))) 

## boxplot 
poddata_day %>% 
  filter(Time>"2015-09-01 UTC") %>% 
  filter(Zone == "bnps-Reefballs-cpower" | Zone == "bpns-Lottobuoy" | Zone=="bpns-Reefballs Belwind") %>% 
  ggplot() +  
  geom_boxplot(aes(Time, Dp10m, group=Zone, colour=factor(Zone)))+
  facet_wrap(~Mooring_type)


### Compare between different distances from the wind farms

# Get an error out of the data
poddata_day[poddata_day$Station == "bpns-Reefballs Belwind" & poddata_day$Mooring_type == "surface-buoy",]$Mooring_type <- "bottom-mooring"

# Add a variable to have different categories of distance to wind farms
poddata_day$distwind <- ifelse(poddata_day$Zone == "bpns-Reefballs Belwind" | 
                                 poddata_day$Zone == "bnps-Reefballs-cpower" |
                                 poddata_day$Zone == "bpns-Belwind C05" |
                                 poddata_day$Zone == "bpns-Lottobuoy", "inside",
                               ifelse(poddata_day$Zone == "WK9en12" |
                                        poddata_day$Zone == "Gootebank", "close",
                                      ifelse(poddata_day$Zone == "Middelkerke" |
                                               poddata_day$Zone == "Oostende" |
                                               poddata_day$Zone == "bpns-Oostdyck West", "far", NA))) 

# distwind is a factor with ordered levels (important for plotting)
poddata_day$distwind <- factor(poddata_day$distwind, levels =c("far", "close", "inside"),ordered = TRUE)
# Add a variable for Detection Positive Day
poddata_day$Dpd <- ifelse(poddata_day$Dpm > 0, 1, 0)

# Boxplots
ggplot(data = poddata_day) + geom_boxplot(aes(x = distwind, y = Dpm))
ggplot(data = poddata_day) + geom_boxplot(aes(x = distwind, y = Dp10m))
ggplot(data = poddata_day) + geom_boxplot(aes(x = distwind, y = Dp10m)) + facet_wrap(~Mooring_type)
ggplot(data = poddata_day[month(poddata_day$Time) == 9 & year(poddata_day$Time) == 2016,]) + geom_boxplot(aes(x = distwind, y = Dp10m)) + facet_wrap(~Mooring_type)

ggplot(data = poddata_day) + geom_boxplot(aes(x = distwind, y = Dph))
ggplot(data = poddata_day) + geom_boxplot(aes(x = distwind, y = Dph)) + facet_wrap(~Mooring_type)
ggplot(data = poddata_day[month(poddata_day$Time) == 9 & year(poddata_day$Time) == 2016,]) + geom_boxplot(aes(x = distwind, y = Dph)) + facet_wrap(~Mooring_type)

podsum <- summarise(group_by(poddata_day, distwind, month(Time)),
                    Dpd = sum(Dpd),
                    Dpdmonth = sum(Dpd)/sum(Recorded))
ggplot(data = podsum) + geom_boxplot(aes(x = distwind, y = Dpd))
ggplot(data = podsum) + geom_boxplot(aes(x = distwind, y = Dpdmonth))

poddata_day$Month <- month(poddata_day$Time)
ggplot(data = poddata_day) + geom_boxplot(aes(x = distwind, y = Dp10m)) + facet_wrap(~Month)

# Make a map of stations and distwind
poddist <- summarise(group_by(poddata_day, Zone, distwind),
                       Longitude = mean(Longitude),
                       Latitude = mean(Latitude))
plot_map() + geom_point(data =poddist, mapping =aes(Longitude, Latitude, shape = distwind), size = 2, colour = "red")

# Make a summary table to allow better interpretation of the boxplots
test <- summarise(group_by(poddata_day, Month, distwind),
                  n = length(unique(Zone)))
library(reshape2)
dcast(test,  Month~ distwind)
