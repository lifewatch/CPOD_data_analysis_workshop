#### 1. Dealing with time variables in R ####
# As discussed previously, a variable is recognized as time in the POSIxct data type.
lubridate::parse_date_time()
library(ggplot2)
# A first step is to plot the value of interest against time.
ggplot(data = poddata_day, aes(x = Time, y = Dpm)) + 
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title.x = element_blank()) +
  geom_point() + geom_line()

# More relevant is plotting the time series for each zone seperately. This can be done with facet_wrap or by making a list of plots.
ggplot(data = poddata_day, aes(x = Time, y = Dpm)) + 
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title.x = element_blank()) +
  geom_point() + geom_line() + facet_wrap(~Zone, nrow = length(unique(poddata$Zone))) 

lapply(unique(poddata_day$Zone), function(x){
  ggplot(data = poddata_day[poddata_day$Zone == x,], aes(x = Time, y = Dpm, group = Deployment_fk)) + 
    theme_bw() +
    theme(axis.text = element_text(size = 16),
          axis.title.x = element_blank()) +
    geom_point() + geom_line()+ggtitle(x)
})

# Discussion: how do we cope with periods without data? Is it better to consider deployments in temporal analysis?

lapply(unique(poddata_day$Deployment_fk), function(x){
  ggplot(data = poddata_day[poddata_day$Deployment_fk == x,], aes(x = Time, y = Dpm)) + 
    theme_bw() +
    theme(axis.text = element_text(size = 16),
          axis.title.x = element_blank()) +
    geom_point() + geom_line() + ggtitle(x)
})

#### 2. Autocorrelation ####
# R provides a very easy function to investigate the autocorrelation in your data: acf.
poddata_day <- arrange(poddata_day, Zone, Time)
acf(poddata_day$Dpm)

# Again, we are more interested in the autocorrelation of each Zone seperately.
par(mfrow=c(2,2))
lapply(unique(poddata_day$Deployment_fk), function(x) {
  data <- acf(poddata_day[poddata_day$Deployment_fk == x,]$Dpm, plot = F)
  plot(data, main = x)
})
dev.off()

# We can also take these values and combine them in one plot.
lacf <- lapply(unique(poddata_day$Zone), function(x){
  o <- filter(poddata_day, Zone == x)
  oa <- acf(o$Dpm)
  dfo <- data.frame(acf = oa$acf[,,1], lag = oa$lag[,,1], Zone = x)
})

library(plyr) # Now we use the package plyr to make a dataframe out of our list
dacf <- ldply(lacf, rbind)
ggplot(dacf) + geom_path(aes(x=lag, y=acf, group=Zone))


#### 3. Smoothing ####
# The goal of smoothing is generally to aid visual interpretation of a time series. Let's try on two subsets of our data.
library(dplyr)
test1 <- filter(poddata_day, Deployment_fk == 2578)
test2 <- filter(poddata_day, Deployment_fk == 2585)

# Applying geom_smooth() plots a loess smoother (weighted regression) on the series.
ggplot(data = test1, aes(x = Time, y = Dpm)) + 
  geom_point() + geom_line() + geom_smooth() + ggtitle(2578)

ggplot(data = test2, aes(x = Time, y = Dpm)) + 
  geom_point() + geom_line() + geom_smooth() + ggtitle(2585)

# Another way to smooth data, is to calculate a moving average. 
acf(test1$Dpm) # First, we check the autocorrelation to choose our window size.

library(pastecs)
movavg <- decaverage(test1$Dpm, order = 3) 
#Met order bepaal je grootte van window: als order 3 is, neem je 3 waardes links en rechts (dus een window van 7 punten)
#Times is hoeveel je een moving average berekent: 2 keer wil zeggen dat je een moving average van je eerste moving average berekent. (dus moving average nemen over je moving average, dus hoe meer je dit)
#Filtered en residual: de plot() illustreert dit: filtered is de lijn van je moving average, #residuals zijn de afstanden van je rauwe punten tot je moving average
movavg # movavg is a list
plot(movavg7)

test1$decavg <- data.frame(movavg$series)$filtered

#same can be done by
#test1$decavg <- data.frame(movavg[[2]][,1])

ggplot(test1, aes(x = Time, y = Dpm)) + geom_point() + geom_line() + 
  geom_path(aes(y = decavg), size= 1.2, colour = "red")

movavg <- decaverage(test1$Dpm, order = 3, times = 5)
test1$decavg <- data.frame(movavg$series)$filtered
ggplot(test1, aes(x = Time, y = Dpm)) + geom_point() + geom_line() + 
  geom_path(aes(y = decavg), size= 1.2, colour = "red")

# Now, we can do the same for the other subset!
acf(test2$Dpm)
movavg <- decaverage(test2$Dpm, order = 1, times = 5)
test2$decavg <- data.frame(movavg$series)$filtered
ggplot(test2, aes(x = Time, y = Dpm)) + geom_point() + geom_line() + 
  geom_path(aes(y = decavg), size= 1.2, colour = "red")

# Discussion: which smoothing technique is preferred?
