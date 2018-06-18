# Problem 4

#(a)
#install.packages("simmer", dependencies = T)
library(simmer)
library(simmer.plot)

env <- simmer()

# global variables
a_rate <- 0.355 # car/sec
S_left <- 1.0
S_right= 4.0
time_step <- 1000

simulation <- function(time) {
  car <- trajectory() %>%
    log_("Arrival Time") %>%
    seize("tollbooth", 1) %>%
    set_attribute("start", function() now(env)) %>%
    timeout(function() runif(1,min=S_left,max=S_right)) %>%
    release("tollbooth", 1) %>%
    log_("Departure Time")
  
  env %>%
    add_resource("tollbooth", 1) %>%
    add_generator("first_car", car, at(rep(0, 1))) %>%
    add_generator("car", car, function() rexp(1,a_rate)) %>%
    
    run(until=time)
}

simulation(time_step)

#(b)
resources <- get_mon_resources(env)

arrivals <- get_mon_arrivals(env)
arrivals <- arrivals[,c("name","start_time","end_time","activity_time")]
names(arrivals) <- c("Customer #","B1: Arrival Time","D1: Departure Time","C1: Service Time")

arrivals$"Customer #" <- (1:length(arrivals$"Customer #"))
arrivals$"A1: Arrival Headway" <- c(0.00,arrivals$"B1: Arrival Time"[2:length(arrivals$"Customer #")] - arrivals$"B1: Arrival Time"[1:length(arrivals$"Customer #")-1])
arrivals$"E1: Delay Time" <- arrivals$"D1: Departure Time"-arrivals$"C1: Service Time"- arrivals$"B1: Arrival Time"

average_delay <- mean(arrivals$"E1: Delay Time")
average_delay 

stdev <- sd(arrivals$"E1: Delay Time")
stdev

#(c)
dev.off()
plot(arrivals$"B1: Arrival Time",arrivals$"Customer #",type="l",xlab="Timestep (s)",ylab="Cumulative Flow (veh/s)",main="Cumulative Diagram",col="blue")
lines(arrivals$"D1: Departure Time",arrivals$"Customer #", type="l", lty=2, lwd=2, col="red")
legend("bottomright", c("arrival", "departure"), col = c("blue", "red"), lty=1,lwd=2)


