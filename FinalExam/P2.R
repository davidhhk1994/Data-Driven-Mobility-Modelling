# Problem 2

#(a)
setwd("E:/NYU/2018_spring/Mobility/EXAM/data")

#### Define GM Car Following Model ####
GMCarFollowing <- function(period)
{
  
  for(i in 2:period)
  {
    Ant[i] <- LeadingCar[i,9] 
    Vnt[i] <- LeadingCar[i,14] 
    Xnt[i] <- LeadingCar[i,16] 
    
    Vn1t[i] <- Result[i-1,5] + Result[i-1,4]*dt
    Xn1t[i] <- Result[i-1,6] + Result[i-1,5]  * dt + 0.5*Result[i-1,4] *(dt)^2
    
    if(abs((Result[max(i-DeltT/dt,1),2] - Result[max(i-DeltT/dt,1),5])) > 0)
    {
      An1t[i] <- (Alm* (Vn1t[i])^m /(Result[max(i-DeltT/dt,1),3]-Result[max(i-DeltT/dt,1),6])^L) * (Result[max(i-DeltT/dt,1),2]  - Result[max(i-DeltT/dt,1),5])
    } else {
      An1t[i] <- 0
    }
    Time[i] <- FollowingCar[i,15]
    Result <- rbind(Result,cbind(Ant[i],Vnt[i],Xnt[i],An1t[i],Vn1t[i],Xn1t[i],Time[i]))
  }
  
  return(Result)
  
}
#reading in dataset
P2<-read.csv("Problem2.csv")

#transform mph tp fps
P2$speed.fps = P2$speed.mph*1.466668

#reference Start Time and Locations
RefX = -7008.71
RefY = 1638.73
minTime = 22130

#select the leading veh
LeadingCar <- P2[which(P2['ID']==379272),]
Time <- (LeadingCar$time-minTime)
TravelDistance <- sqrt((LeadingCar$x-RefX)^2+(LeadingCar$y-RefY)^2)
LeadingCar <- cbind(LeadingCar,Time,TravelDistance)

#select the following veh
FollowingCar <- df[which(df['ID']==378649),]
Time <- (FollowingCar$time-minTime)
TravelDistance <- sqrt((FollowingCar$x-RefX)^2+(FollowingCar$y-RefY)^2)
FollowingCar <- cbind(FollowingCar,Time,TravelDistance)

LeadingCar <- LeadingCar[which(LeadingCar$Time<=200),]
FollowingCar <- FollowingCar[which(FollowingCar$Time<=200),]

#### Parameter Setting ####
# Assumed Parameters
dt <- 0.1     # Time interval
DeltT <- 1.5  # Reaction time
m <- 0.1     # from -2 to 2
L <- 1        # from -1 to 4
Alm <- 10     # sensitivity coefficient
# View(LeadingCar)

## Initial Values
# Leading vehicle
Xnt <- LeadingCar$TravelDistance[1] # Initial position
Vnt <- LeadingCar$speed.fps[1] # Initial speed
Ant <- LeadingCar$acceleration.fpss[1] # Initial acceleration

#Following vehicle
Xn1t <- FollowingCar$TravelDistance[1] 
Vn1t <- FollowingCar$speed.fps[1]
An1t <- FollowingCar$acceleration.fpss[1] 
Time <- FollowingCar$Time[1]
Result <- cbind(Ant,Vnt,Xnt, An1t,Vn1t,Xn1t,Time)

#############################################

#### Running the car following model ####
Result<-GMCarFollowing(length(FollowingCar[,15]))
#############################################

#b)
### Visualize the modeling results vs. observed ones ####
dev.off()
dev.new(width=8.4, height=5.0)
par(mfrow=c(1,1))
par(mar=0.1+c(1.7,1.8,1.5,2.0), mgp=c(0.9,0.1,0.0)) # Bottom, Left, Top, Right.
plot(NA,NA,xlim=c(0,210),ylim=c(0,11000),xlab="Time (Interval=0.1 s)",ylab="Location(ft)", main="Time Space Diagram",cex.main=0.7,tck=0.01,cex.lab=0.8,xaxt='n',yaxt='n',xaxs="i",yaxs="i")
axis(1, at=seq(0,210,10), labels=seq(0,210,10),lty=1, col=1, tck=-0.01, las=1)
axis(2, at=seq(0,11000,500),labels=seq(0,11000,500),lty=1, col=1, tck=-0.01,cex.axis=0.7)

lines(Result[,7],Result[,3],lty=1,lwd=2,col=1) ##Observed results of leading vehicle
lines(Result[,7],FollowingCar[,16],lty=1,lwd=2,col=5)  ##Observed results of following vehicle
lines(Result[,7],Result[,6],lty=1,lwd=2,col=2) ##Modeled resultsof following vehicle
legend("topleft",c("Leading Vehicle","Following Vehicle Observed","Following Vehicle Modeled"),col=c(1,5,2),lty=1,lwd=2,cex=0.8)
