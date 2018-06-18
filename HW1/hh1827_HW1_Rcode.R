#1. import HW1_Dataset.csv, check the dimension and column names
mydata = read.csv("E:/NYU/2018 spring/Mobility/week2/HW1_Dataset.csv")
mydata
dim(mydata)

#2. Calculate actual travel time and historical travel time then find the max values
mydata <- transform(mydata, Historical_Time = Segment_Length.mile./ Historical_Speed.mph.)
mydata
mydata <- transform(mydata, Actual_Time = Segment_Length.mile./ Actual_Speed.mph.)
mydata
max(mydata$Historical_Time, na.rm = TRUE)
max(mydata$Actual_Time, na.rm = TRUE)

#3. find the actual travel time variance for each hourly interval
#travel time variance for the measurement in 7:00-8:00
hour_1 <- mydata[c(1:12),] 
hour_1
var(hour_1$Actual_Time) 
#travel time variance for the measurement in 8:00-9:00
hour_2 <- mydata[c(13:24),] 
hour_2
var(hour_2$Actual_Time)
#travel time variance for the measurement in 16:00-17:00
hour_3 <- mydata[c(25:36),] 
hour_3
var(hour_3$Actual_Time) 
#travel time variance for the measurement in 17:00-18:00
hour_4 <- mydata[c(37:48),] 
hour_4
var(hour_4$Actual_Time) 
#travel time variance for the measurement in 18:00-19:00
hour_5 <- mydata[c(49:60),] 
hour_5
var(hour_5$Actual_Time) 
#travel time variance for the measurement in 19:00-20:00
hour_6 <- mydata[c(61:72),] 
hour_6
var(hour_6$Actual_Time) 
#travel time variance for the measurement in 20:00-21:00
hour_7 <- mydata[c(73:84),] 
hour_7
var(hour_7$Actual_Time)
#travel time variance for the measurement in 21:00-22:00
hour_8 <- mydata[c(85:93),] 
hour_8
var(hour_8$Actual_Time)

#4. find time differences and its max value between actual and historical time 
mydata$Time_Diff <- abs(mydata$Actual_Time - mydata$Historical_Time)
mydata
max(mydata$Time_Diff, na.rm = TRUE)
subset(mydata, Time_Diff == max(Time_Diff), select=c(Local_Time))
#simple plot to show the difference in each five-minute interval
plot(mydata$Local_Time,mydata$Time_Diff,typ="l",xlab="Time Stamp",ylab="Time Difference",main="Figure 1: Time 
     difference in each five-minute interval")

#5. regress actual time on historical time
linearMod <- lm(Actual_Time ~ Historical_Time, data=mydata)  # build linear regression model 
print(linearMod)
summary(linearMod) #Diagmostics of the linear regression model
scatter.smooth(x=mydata$Historical_Time, y=mydata$Actual_Time, main="Actual time vs. Historical time")  # scatterplot
