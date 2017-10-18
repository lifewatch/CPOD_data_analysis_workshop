library(ggplot2)
library(dplyr)
library(rgdal)
library(lubridate)

#### Bathymetry ####
bathy <- read.table("Data day 1/Metadata/bathy.txt")
poddata$x <- round(poddata$Longitude, digits = 2)
poddata$y <- round(poddata$Latitude, digits = 2)
poddata <- left_join(poddata, bathy)
poddata <- select(poddata, -x, -y)

ggplot() + geom_raster(aes(x=x, y=y, fill = -z), data = bathy)
ggplot() + 
  coord_equal(xlim = c(2.25,3.25), ylim = c(51,52)) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0093b4"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +
  geom_raster(aes(x=x, y=y, fill = -z), data = bathy, interpolate = T)

ggplot() + 
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0093b4"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +
  geom_tile(aes(x=x, y=y, fill = -z), data = bathy)

#### Day and night ####
sunrise_set <- data.frame(read.table("Data day 1/Metadata/sunrise_set.txt", " ", header =T))

sunrise_set <- select(sunrise_set, -X)
colnames(sunrise_set) <- c("Date", "Rise", "Set")

sunrise_set$Rise <- parse_date_time(sunrise_set$Rise, orders = "ymd HMS")
sunrise_set$Set <- parse_date_time(sunrise_set$Set, orders = "ymd HMS")

sunrise_set$Date <- as.POSIXct(sunrise_set$Date,format="%Y-%m-%d", tz="UTC")
poddata$Date <- as.POSIXct(paste(year(poddata$Time), month(poddata$Time), day(poddata$Time), sep = "-"),format="%Y-%m-%d", tz="UTC")


poddata <- left_join(poddata, sunrise_set)
poddata$Day <- ifelse(poddata$Rise < poddata$Time & poddata$Time <= poddata$Set, "Day", "Night")
poddata <- select(poddata, -Date, -Rise, -Set)

#### Habitat ####
habitat_shp <- readOGR("Data day 1/Metadata", layer= "habitat")

habitat_shp@data$ET_ID <- rownames(habitat_shp@data)
habitat<- fortify(habitat_shp, region = "ET_ID")
habitat$ET_ID <- habitat$id
habitat <- left_join(habitat, habitat_shp@data, by="ET_ID")
legend <- read.csv("Data day 1/Metadata/EUNIS_HAB_Legend.csv", stringsAsFactors = F)
colnames(legend) <- c("HAB_TYP", "Legend_HAB_TYP")
habitat<- left_join(habitat, legend)

ggplot() + 
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) + 
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +  
  geom_polygon(aes(x= long, y = lat, group = group, fill = HAB_TYP), data = habitat)

ggplot() + 
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) + 
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +  
  geom_polygon(aes(x= long, y = lat, group = group, fill = Legend_HAB_TYP), data = habitat)

ggplot() + 
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) + 
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +  
  geom_polygon(aes(x= long, y = lat, group = group, fill = MSFD_BH), data = habitat)

ggplot() + 
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) + 
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +  
  geom_polygon(aes(x= long, y = lat, group = group, fill = Substrt), data = habitat)

ggplot() + 
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) + 
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +  
  geom_polygon(aes(x= long, y = lat, group = group, fill = Biozone), data = habitat)

ggplot() + 
  coord_map(xlim = c(2,3.5), ylim = c(51,52)) + 
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16)) +  
  geom_polygon(aes(x= long, y = lat, group = group, fill = Energy), data = habitat)

round(unique(poddata$Latitude), digits = 2) %in% round(unique(habitat$lat), digits = 2)

habitat$x <- round(habitat$long, digits = 2)
habitat$y <- round(habitat$lat, digits = 2)

habitatsum <- summarise(group_by(habitat, x, y),
                        HAB_TYP = names(which.max(table(HAB_TYP))),
                        MSFD_BH = names(which.max(table(MSFD_BH))),
                        Substrt = names(which.max(table(Substrt))),
                        Biozone = names(which.max(table(Biozone))),
                        Energy = names(which.max(table(Energy))))

poddata$x <- round(poddata$Longitude, digits = 2)
poddata$y <- round(poddata$Latitude, digits = 2)

poddata <- left_join(poddata, habitatsum)
poddata <- left_join(poddata, legend)
poddata <- select(poddata, -x, -y)


#### LifeWatch Temperature and Chlorophyll data ####
lw_reg <- read.table("Data day 1/Metadata/LW_reg.tab")
lw_reg$Month <- parse_date_time(as.character(lw_reg$Month), orders = "ymd")
round(unique(poddata$Latitude), digits = 1) %in% round(unique(lw_reg$Latitude), digits = 1)

lw_reg$x <- round(lw_reg$Longitude, digits = 1)
lw_reg$y <- round(lw_reg$Latitude, digits = 1)
lw_regsum <- summarise(group_by(lw_reg, Month, x,y),
                       reg_Temp = mean(reg_Temp, na.rm=T),
                       reg_Chla = mean(reg_Chla, na.rm=T))

poddata$Month <- paste(year(poddata$Time), month(poddata$Time), sep = "-")
poddata$Month <- parse_date_time(poddata$Month, orders = "ym")
poddata$x <- round(poddata$Longitude, digits = 1)
poddata$y <- round(poddata$Latitude, digits = 1)

poddata <- left_join(poddata, lw_regsum)
