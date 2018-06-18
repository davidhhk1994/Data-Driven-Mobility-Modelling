# Problem 1
#(a)
D1_PATH <- "E:/NYU/2018_spring/Mobility/EXAM/data/Detector1"
setwd(D1_PATH)

# READING IN DETECTOR 1 DATA
D1_files <- lapply(list.files(path = PATH), read.csv)
D1 <- do.call(rbind, D1_files)

# converting the time format
D1$Time <- as.POSIXct(D1$Time,format = "%H:%M:%S")
D1$"5min_interval" <- cut(D1$Time, breaks="5 min")

#computing flow
qD1 <- data.frame(summary(D1$"5min_interval",maxsum = 10000))
colnames(qD1) <- c("flow at D1")

D2_PATH <- "E:/NYU/2018_spring/Mobility/EXAM/data/Detector2"
setwd(D2_PATH)

# READING IN DETECTOR 2 DATA
D2_files <- lapply(list.files(path = D2_PATH), read.csv)
D2 <- do.call(rbind, D2_files)

#converting time format
D2$Time <- as.POSIXct(D2$Time,format = "%H:%M:%S")
D2$"5min_interval" <- cut(D2$Time, breaks="5 min")

# computing flow
qD2 <- data.frame(summary(D2$"5min_interval", maxsum = 10000))
colnames(qD2) <- c("flow at D2")


cum_q1 <- cumsum(qD1[,1])
cum_q2 <- cumsum(qD2[,1])
time_step1 <- as.POSIXct(row.names(qD1))
time_step2 <- as.POSIXct(row.names(qD2))
dev.off()
plot(time_step1,cum_q1, xaxt = "n",type="l",xlab="Timestep (5min)",ylab="Cumulative Flow (veh/5min)",main="Cumulative Diagram",col="blue")
lines(time_step2,cum_q2, type="l", lty=2, col="red")
axis.POSIXct(1, at=time_step1, labels=format(time_step1, "%H:%M:%S"))
legend("bottomright", c("Arrival Curve","Departure Curve"), lty=1:3, bty="n", col=c("blue","red"));

#(c)
D1$per_min <- cut(D1$Time, breaks="1 min")
D2$per_min <- cut(D2$Time, breaks="1 min")

qD1_1min <- cumsum(data.frame(summary(D1$per_min,maxsum = 10000)))
colnames(qD1_1min) <- c("qD1")

qD2_1min <- cumsum(data.frame(summary(D2$per_min,maxsum = 10000)))
colnames(qD2_1min) <- c("qD2")

dif <- merge(qD1_1min,qD2_1min, by=0, all=FALSE)
dif$diff <- dif$qD1 - dif$qD2
dif[dif$diff>900,]$Row.names[1]

#(d)
#ploting arrival curve
# randomly generating samples
set.seed(999)
new_qD2 <- array(0, dim = c(100,100,100))

for(i in 1:100){
  new_qD2[, , i] <- rnorm(10000, 500, 750)
}

est <- sample(new_qD2,length(qD2[,1]))
est_qD2 <- cumsum(qD1[,1]) - est

dev.off()
plot(time_step1,cum_q1, xaxt = "n",type="l",xlab="Timestep (5min)",ylab="Cumulative Flow (veh/5min)",main="Cumulative Diagram",col="blue")
lines(time_step1,est_qD2, type="l", lty=2, col="red")
axis.POSIXct(1, at=time_step1, labels=format(time_step1, "%H:%M:%S"))
legend("bottomright", c("Arrival Curve","Estimated Departure Curve"), lty=1:3, bty="n", col=c("blue","red"))
