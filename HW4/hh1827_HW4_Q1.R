## Q1

f <- function(x) (2.9*sqrt(x)-x^2)

n1 <- 100
n2 <- 1000
n3 <- 10000
a = 0
b1 = 1
b2 = 2


#question a.
#True integral value when a=0, b=1
integrate(f,a,b1)
#True integral value when a=0, b=2
integrate(f,a,b2)


#question b.
# With a random seed
set.seed(3155)
#(1)n=100
#when a=0, b=1
x1 <- runif(n1,min=a,max=b1)
I1 = sum(f(x1))/n1*(b1-a)
I1
#when a=0, b=2
x1 <- runif(n1,min=a,max=b2)
I1 = sum(f(x1))/n1*(b2-a)
I1

#(2)n=1000
#when a=0, b=1
x2 <- runif(n2,min=a,max=b1)
I2 = sum(f(x2))/n2*(b1-a)
I2
#when a=0, b=2
x2 <- runif(n2,min=a,max=b2)
I2 = sum(f(x2))/n2*(b2-a)
I2

#(2)n=10000
#when a=0, b=1
x3 <- runif(n3,min=a,max=b1)
I3 = sum(f(x3))/n3*(b1-a)
I3

#when a=0, b=2
x3 <- runif(n3,min=a,max=b2)
I3 = sum(f(x3))/n3*(b2-a)
I3


# Without a random seed
#(1)n=100
#when a=0, b=1
x1 <- runif(n1,min=a,max=b1)
I1 = sum(f(x1))/n1*(b1-a)
I1
#when a=0, b=2
x1 <- runif(n1,min=a,max=b2)
I1 = sum(f(x1))/n1*(b2-a)
I1

#(2)n=1000
#when a=0, b=1
x2 <- runif(n2,min=a,max=b1)
I2 = sum(f(x2))/n2*(b1-a)
I2
#when a=0, b=2
x2 <- runif(n2,min=a,max=b2)
I2 = sum(f(x2))/n2*(b2-a)
I2

#(2)n=10000
#when a=0, b=1
x3 <- runif(n3,min=a,max=b1)
I3 = sum(f(x3))/n3*(b1-a)
I3

#when a=0, b=2
x3 <- runif(n3,min=a,max=b2)
I3 = sum(f(x3))/n3*(b2-a)
I3
