### Script Gaps ###
### Contributors: Gert Everaert, Jan vanaverbeke & Jan Reubens ###

#### Libraries needed ####
library(ggplot2)   # for visualisation
library (dplyr)    # for data clean-up
library(mgcv)      # for GAM models
library(lubridate) # adjust timing

#### 1. prepare the data ####
summary(poddata_day)

# add year and month to dataframe (needed as variables in the model)
poddata$year <- year(poddata$Time)
poddata$month <- month(poddata$Time)

# add date since 1970 to the dataframe 
poddata_day$daysince1970 <- floor(unclass(poddata_day$Time)/86400) 

# add date of year to dataframe
poddata_day$day.of.year <- yday(poddata_day$Time)

poddata_day <-as.data.frame(poddata_day)

#### 2. Data visualisation to define questions at stake ####
# Are there gaps in the data?
ggplot(data = poddata_day) + geom_point(aes(x= Time, y = Station))

# --> everybody on the same line? Jan V & Gert had a different meaning on gaps than Jan R.
# ask the correct questions! You can't answer everything with the data we have (e. g. difference in seasonality between stations?)


# Can you use all data? e.g. Bottom vs surface...
ggplot(data = poddata_day, aes(x= Mooring_type, y = Lost_minutes)) + geom_boxplot()

# --> obvious difference in lost minutes, thus data between these two types can't be compared
# split data in two datasets related to mooring type...
bottom <-filter(poddata_day, Mooring_type == "bottom-mooring")
surface <-filter(poddata_day, Mooring_type == "surface-buoy")

#### 3. Model development ####

# Model 1
M1 <- gamm(Dp10m ~ s(daysince1970, k = 4, by=year) + factor(Mooring_type), random =list(Station =~1), data = poddata_day)
gam.check(M1$gam)
summary(M1$gam)
plot(M1$gam, page  = 1)


# check the residuals: not yet a good fit.. We decided to add a autocorrelation factor, work with both morring types separetely and group the years

# autocorrelatie factor toevoegen
vf <- varIdent(form=~1|day.of.year)

# Perform log transformation
M2 <- gamm(log10(Dp10m+1) ~ s(day.of.year, k = 5) , data = bottom, weights=vf)
gam.check(M2)
summary(M2)
plot(M2, page  = 1)
anova(M2)
AIC(M2)
plot(poddata_day$Station, resid(M2))
plot(poddata_day$Month, resid(M2))
plot(poddata_day$Mooring_type, resid(M2))

# during plenary discussion Jan V. mentioned that we can also change distribution to 'Poisson'. 
# --> family=poisson() ipv family=gaussian()
# Voor opvullen van gaten kan je werken met "predict$"

