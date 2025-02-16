# Lab 4 Math 314

attach(class_data314)
View(class_data314)

#Part 1: Explore normal probability plots
# generate data using the following command

x1<-rnorm(1000,1,.577)

#create a normal probability plot and histogram

hist(x1, breaks = 15,main = "Observations from a Random Normal Distribution 1",
     xlab = "Observations",ylab = "Frequency")

qqnorm(x1, main = "Normal Probability Plot For Random Observations",
       xlab = "Theoretical Quantiles", ylab = "Observations Quantiles")

qqline(x1,col="red")

summary(x1)
sd(x1)

#create another normal probability plot and histogram for a uniform
#data set

x2<-runif(1000,0,2)

hist(x2, breaks=20, main= "Observations from a Uniform  Distribution ",
     xlab="Theoretical Quantiles", ylab="Observations Quantiles")

qqnorm(x2, main = "Normal Probability Plot For Random Observations",
       xlab = "Theoretical Quantiles", ylab = "Observations Quantiles")

qqline(x2,col="red")

summary(x2)

sd(x2)
#Comment on previous graphs: The normal distribution data generated has a
# noticeable bell shape distribution, and it is also unimodal. Furthermore,
# the QQ plot also displays that points from both the observations and
# theoretical quantiles are roughly the same which implies a normal distribution
# On the other hand, the uniform did not have an apparent bell shape
# distribution , but it does hold a prominent unimodal peak. Also, the QQ plot
# also displays that points from both the observations and theoretical quantiles
# generate a linear graph, but at the ends, the graph starts to curve.


# if I was only given the histogram and QQ plot from both cases and was 
#told what the QQ plot compares, I would have a better idea which set  
#figures correspond to the normally distributed observations. 

# However, If was only given the generated graphs then I maybe would not be 
# able to decide if the graphs display normal distribution because I don't know 
# why we are comparing the observational and theoretical quantiles.

#Part 2: choose two numerical variables from class data 314 

# Shoe data

hist(Shoe,breaks = 10,main= "Math 314 Responses on Shoe Size ",
     xlab="Shoe Size", ylab=" Frequency")

qqnorm(Shoe,main = "Normal Probability Plot For Shoe Size Data",
       xlab = "Theoretical Quantiles", ylab = "Observations Quantiles")

qqline(Shoe, col="red")

summary(Shoe)

sd(Shoe,na.rm=TRUE)


hist(Height, breaks=10,main= " Math 314 Responses on Height ",
     xlab="Height (inches)", ylab="Frequency")

qqnorm(Height,main = "Normal Probability Plot For Height Data",
       xlab = "Theoretical Quantiles", ylab = "Observations Quantiles")
qqline(Height,col="red")

summary(Height)

sd(Height,na.rm = TRUE)
