# Problem 3

#(a)

setwd('E:/NYU/2018_spring/Mobility/EXAM/data')

Filename1 <- "Detector3.csv"
Filename2 <- "Detector4.csv"

D3 <- read.csv(file=Filename1, header=TRUE, sep=",")
D4 <- read.csv(file=Filename2, header=TRUE, sep=",")
D3 <- na.omit(D3)

# Temporal distribution of flow, average speed, and average occupancy for Detector 3
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(1,1))

TimeInterval <- 5
LowTime <- 0
UpTime <- length(Filename1)*24*60/TimeInterval

plot(D3[,2],D3[,3],xlim=c(0,3000),ylim=c(0,80),xlab="Flow (veh/5min)",ylab="Average Speed (mph)",main="Flow vs. Average Speed",col="blue",pch=19,cex=0.8)
plot(D3[,2],D3[,4]*100,xlim=c(0,3000),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Occupancy (%)",main="Flow vs. Average Occupancy",col="blue",pch=19,cex=0.8)
plot(D3[,3],D3[,4]*100,xlim=c(0,80),ylim=c(0,100),xlab="Average Speed (mph)",ylab="Average Occupancy (%)",main="Average Occupancy vs. Average Speed",col="blue",pch=19,cex=0.8)

# Temporal distribution of flow, average speed, and average occupancy for station 400001
dev.off()
dev.new(width=8.4, height=7.0)
par(mfrow=c(1,1))

TimeInterval <- 5
LowTime <- 0
UpTime <- length(Filename)*24*60/TimeInterval

plot(D4[,2],D4[,3],xlim=c(0,3000),ylim=c(0,80),xlab="Flow (veh/5min)",ylab="Average Speed (mph)",main="Flow vs. Average Speed",col="blue",pch=19,cex=0.8)
plot(D4[,2],D4[,4]*100,xlim=c(0,3000),ylim=c(0,100),xlab="Flow (veh/5min)",ylab="Average Occupancy (%)",main="Flow vs. Average Occupancy",col="blue",pch=19,cex=0.8)
plot(D4[,3],D4[,4]*100,xlim=c(0,80),ylim=c(0,100),xlab="Average Speed (mph)",ylab="Average Occupancy (%)",main="Average Occupancy vs. Average Speed",col="blue",pch=19,cex=0.8)

#(b)
V_len <- 23
D_len <- 5.2
D3["Density"] <- 5280*D3["Occupancy"]/(V_len + D_len)

#speed vs density for detector 3
plot(D3[,3],D3[,5],xlim=c(0,80),ylim=c(0,250),xlab="Average Speed (mph)",ylab="Average density (vpm)",main="Average Density vs. Average Speed",col="blue",pch=19,cex=0.8)
# Density vs Flow for detector 3
plot(D3[,5],D3[,2],xlim=c(0,250),ylim=c(0,3000),xlab="Average density (vpm)",ylab="Flow (veh/5min)",main="Density vs. Flow",col="blue",pch=19,cex=0.8)


D4["Density"] <- 5280*D4["Occupancy"]/(V_len + D_len)
#speed vs density for detector 4
plot(D4[,3],D4[,5],xlim=c(0,80),ylim=c(0,250),xlab="Average Speed (mph)",ylab="Average density (vpm)",main="Average Density vs. Average Speed",col="blue",pch=19,cex=0.8)
#Density vs Flow for detector 4
plot(D4[,5],D4[,2],xlim=c(0,250),ylim=c(0,3000),xlab="Average density (vpm)",ylab="Flow (veh/5min)",main="Density vs. Flow",col="blue",pch=19,cex=0.8)


#(c) 
#Detector 3
vf = 70
D3["kj"] = (vf*D3["Density"]) / (vf - D3["Speed"])
ND3 = subset(D3, kj<= 150)
ND3
Kj = ND3[,6]
quantile(Kj, c(.90))
kj90 = 144.6858 

D3["estimate_v"] = vf - (vf/kj90)*D3["Density"]
D3["estimate_q"] = D3["estimate_v"]*D3["Density"]

plot(D3[,8],D3[,7],xlim=c(0,3000),ylim=c(0,80),xlab="Estimated Flow (veh/5min)",ylab="Estimated Speed (mph)",main="Flow vs. Speed",col="blue",pch=19,cex=0.8)
plot(D3[,5],D3[,8],xlim=c(0,250),ylim=c(0,3000),xlab="Estimated density (vpm)",ylab="Estimated Flow (veh/5min)",main="Density vs. Flow",col="blue",pch=19,cex=0.8)
plot(D3[,7],D3[,5],xlim=c(0,80),ylim=c(0,250),xlab="Estimated Speed (mph)",ylab="Estimated density (vpm)",main="Average Density vs. Average Speed",col="blue",pch=19,cex=0.8)

