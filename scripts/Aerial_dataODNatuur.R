library(ggplot2)
library(lubridate)
library(dplyr)
library(rgdal)
library(maptools)
#data file can be found in folder R scripts
aer <- read.csv("Data/Aerial.csv", stringsAsFactors = F)

ggplot() + geom_point(data = aer, aes(x=Longitude, y = Latitude,  size = Count, colour= as.factor(year(Time))))

load_shapes_plot_map <- function() {
  banks <- readOGR("Data/Mapping/banks/banks.shp")
  eez <- readOGR("Data/Mapping/eez/eez.shp")
  bight <- readOGR("Data/Mapping/world_bay_gulf/world_bay_gulf.shp")
  bankszeeland <- readOGR("Data/Mapping/Zeeland banks/banks.shp")
  bankscoastal <- readOGR("Data/Mapping/Coastal banks/banks.shp")
  bankshinder <- readOGR("Data/Mapping/Hinder banks/banks.shp")
  netherlands_coast <- readOGR("Data/Mapping/netherlands_coast/world_countries_coasts.shp")
  
  # Fortify shapefiles to use them in ggplot2
  eezfort <- fortify(eez)
  banksfort <- fortify(banks)
  bightfort <- fortify(bight)
  bankszeelandfort <- fortify(bankszeeland)
  bankscoastalfort <- fortify(bankscoastal)
  bankshinderfort <- fortify(bankshinder)
  netherlands_coastfort <- fortify(netherlands_coast)
  netherlands_coastfort <- filter(netherlands_coastfort, lat >51.36)
  
  # Make base map plot with receivers
  ggplot() + 
    coord_map(xlim = c(2,3.5), ylim = c(51,52)) + #important! this makes that the polygons are ok
    theme_bw() +
    theme(panel.background = element_rect(fill = "#0093b4"),
          panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title = element_blank(),
          axis.text = element_text(size = 16)) +
    geom_polygon(aes(x=long, y=lat, group=group), data = bightfort, fill = "white") +
    geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "#0093b4") +  
    geom_path(aes(x=long, y=lat, group=group), data = bightfort) +
    geom_polygon(aes(x=long, y=lat, group=group), data = banksfort, fill = "#016483") +
    geom_polygon(aes(x=long, y=lat, group=group), data = bankszeelandfort, fill = "#016483") +
    geom_polygon(aes(x=long, y=lat, group=group), data = bankscoastalfort, fill = "#016483") +
    geom_polygon(aes(x=long, y=lat, group=group), data = bankshinderfort, fill = "#016483") +
    geom_path(aes(x=long, y=lat), data = eezfort, colour = "black")
}

load_shapes_plot_map() + geom_point(data = aer, aes(x=Longitude, y = Latitude,  size = Count, colour= as.factor(year(Time))))

