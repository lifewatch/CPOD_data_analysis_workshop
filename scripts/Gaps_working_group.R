### Script Gaps ###
### Contributors: Gert Everaert, Jan vanaverbeke & Jan Reubens ###

#### Libraries needed ####
library(ggplot2)  # for visualisation
library (dplyr)   # for data clean-up
library(mgcv)     # for GAM models

#### 1. prepare the data ####
summary(poddata_day)

# add year and month to dataframe (needed as variables in the model)
poddata$year <- year(poddata$Time)
poddata$month <- month(poddata$Time)

# add date since 1970 to the dataframe 
poddata_day$daysince1970 <- floor(unclass(poddata_day$Time)/86400) 

#### 2. Data visualisation to define questions at stake ####
# Are there gaps in the data?
ggplot(data = poddata_day) + geom_point(aes(x= Time, y = Station))

# --> everybody on the same line? Jan V & Gert had a different meaning on gaps than Jan R.
# ask the correct questions! You can't answer everything with the data we have (e. g. difference in seasonality between stations?)


# Can you use all data? e.g. Bottom vs surface
ggplot(data = poddata_day, aes(x= Mooring_type, y = Lost_minutes)) + geom_boxplot()

# --> obvious difference in lost minutes, thus data between these two types can't be compared
# split data in two datasets related to mooring type...
bottom <-filter(poddata_day, Mooring_type == "bottom-mooring")
surface <-filter(poddata_day, Mooring_type == "surface-buoy")

#### 3. Model development ####

#autocorrelatie factor toevoegen
vf <- varIdent(form=~1|daysince1970)

M <- gam(log10(Dp10m+1) ~ s(day.of.year, k = 5) + factor(Mooring_type), data = poddata_day)
gam.check(M)
summary(M)
plot(M, page  = 1)
anova(M)
AIC(M)
plot(poddata_day$Station, resid(M))
plot(poddata_day$Month, resid(M))
plot(poddata_day$Mooring_type, resid(M))


Zoals Jan V. zei, kan je ook de distributie aanpassen naar een Poisson. Dan schrijf je family=poisson() ipv family=gaussian()


Voor opvullen van gaten kan je werken met âpredictâ.


#### Presentation ####


#autocorrelatie factor toevoegen
vf <- varIdent(form=~1|daysince1970)

# Model
M <- gamm(Dp10m ~ s(daysince1970, k = 4, by=year), random =list(Station =~1), data = poddata_day, weights=vf)
gam.check(M$gam)
summary(M$gam)
plot(M$gam, page  = 1)

# --> dataset te groot