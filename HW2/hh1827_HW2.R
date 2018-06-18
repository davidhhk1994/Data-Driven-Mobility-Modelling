#HW2
#problem 1
#a
busdata = read.csv("E:/NYU/2018 spring/Mobility/asst 2/busDataHW.csv") #reading in bus information data
head(busdata)

install.packages("dplyr")
library(dplyr)

df <- dplyr::filter(busdata, grepl('B54', publishlinename)) #filtering with bus name
df <- dplyr::filter(df, grepl('0', directionref)) #filtering with direction
filtered <- dplyr::filter(df, grepl('T17:|T18:00', date)) #filtering with time
head(filtered)

#b&c
install.packages("ggplot2")
install.packages("parsedate")
install.packages("reshape2")

library("ggplot2")
library("parsedate")
library("reshape2")

filtered <- filtered[with(filtered, order(filtered$date)),]#ordering the dataset by date
filtered$date <- substr(filtered$date, 1, 16)
filtered$date <- strptime(filtered$date, "%Y-%m-%dT%H:%M")
filtered
# plotting time space diagram
bus_number = substr(filtered$datedvehiclejourneyref, 40, 42)
p <- ggplot(filtered,aes(x=date, y=CallDistanceAlongRoute,colour= bus_number)) + 
  geom_line()+
  ggtitle("Time Space Diagram of RouTe B54 during 17:00-18:00") +
  labs(y="Call Distance Along Route",x="Time") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5))
p+annotate("rect", xmin =filtered$date[125] , xmax =filtered$date[150],
           ymin =900, ymax =1300, alpha = .3) #Highlighting the bus bunching with a transparent rectangle

#Problem2
#a
bus_no <- unique(filtered$datedvehiclejourneyref) #getting the unique bus numbers from problem 1 dataset 
sel_bus <- busdata[busdata$directionref == '0' & busdata$datedvehiclejourneyref %in% bus_no,] #selecting all the records of these buses from original data 
sel_bus <- sel_bus[with(sel_bus, order(sel_bus$date)),]#ordering the dataset by date
sel_bus$date <- substr(sel_bus$date, 1, 16)
sel_bus$date <- strptime(sel_bus$date, "%Y-%m-%dT%H:%M") #formatting the date column

ggplot(sel_bus,aes(x=date, y=CallDistanceAlongRoute,colour=datedvehiclejourneyref)) + 
  geom_line()+
  theme(legend.position="none") #ploting the space time diagram of these buses 

# getting the average speed and time span over the journey of 0-6000 of each individual bus
speed <- list()
time_span <- list()
for (i in unique(bus_number)){
  single_bus <- sel_bus[substr(sel_bus$datedvehiclejourneyref, 40, 42) == i,]
  single_bus$date <- as.numeric(single_bus$date) #converting the date to numeric values with second as unit
  linearMod <- lm(date~CallDistanceAlongRoute, data=single_bus) #estimating the time at 0 and 6000 by linear regression
  new <- data.frame(CallDistanceAlongRoute=c(0, 6000))
  result <- predict(linearMod, new)
  avg_speed <- 6000/(as.numeric(result[2])-as.numeric(result[1]))
  time <- as.numeric(result[2])-as.numeric(result[1])
  speed <- append(speed, avg_speed)
  time_span <- append(time_span, time)
}


# calculating time mean speed
time_mean_speed <- mean(as.numeric(speed)) # treating the average speed of each vehicle as spot speed
time_mean_speed

# calculating space mean speed
L <- 6000
space_mean_speed <- 6000/mean(as.numeric(time_span))
space_mean_speed
