# Lab 3 Math 314

dpois(10,100)
ppois(10,100)

#Part 1: Lab 1 data is required

#Apply attach command to the data set
attach(class_data314)

#Load the package in loading memory
require(ggplot2)
require(ggthemes)
# make a histogram using ggplot command


ggplot(class_data314)+geom_histogram(aes(x=Height),binwidth = 1,col="Black",fill="Grey")+
  labs(title="Student Height Responses",x="Height (inches)",y="Count (frequency)")+theme_economist()



# make a box plot using ggplot command

ggplot(class_data314,aes(x=Gender,y=Height))+geom_boxplot(col="Black",fill="Light Yellow") +
  labs(title = "Students Height According to Gender Specification",x="Gender Category",y="Height (inches)")+
  theme_economist()


#Part 2: Explore the function binomial distribution command

#Changing parameter p to .25,.50,.75, and .90 sequentially

# apply rbinom command
# this will create N observation of a binomial random variable
# with parameters n and p
# and plot the results
# Also calculate the mean and standard deviation for each
# binomial distribution calculation

N=1000
n=50
p1=.25
p2=.50
p3=.75
p4=.9
x1<-rbinom(N,n,p1)
x2<-rbinom(N,n,p2)
x3<-rbinom(N,n,p3)
x4<-rbinom(N,n,p4)

hist(x1,breaks=20, main="Observations From Binomial Distribution ",
     xlab = "Observation Values",ylab = "Frequency", col= "Light Yellow", fill="Black")


mu1x=n*p1
sigmax1=sqrt(n*p1*(1-p1))


hist(x2,breaks=20, main="Observations From Binomial Distribution ",
     xlab = "Observation Values",ylab = "Frequency", col= "Light Yellow", fill="Black")


mu2x=n*p2
sigmax2=sqrt(n*p2*(1-p2))

hist(x3,breaks=20, main="Observations From Binomial Distribution ",
     xlab = "Observation Values",ylab = "Frequency", col= "Light Yellow", fill="Black")
mu3x=n*p3
sigmax3=sqrt(n*p3*(1-p3))

hist(x4,breaks=15, main="Observations From Binomial Distribution ",
     xlab = "Observation Values",ylab = "Frequency", col= "Light Yellow", fill="Black")

mu4x=n*p4
sigmax4=sqrt(n*p4*(1-p4))


# Changing parameter n and to utilize p2
n1=25
n2=50
n3=75
n4=10

y1<-rbinom(N,n1,p2)
y2<-rbinom(N,n2,p2)
y3<-rbinom(N,n3,p2)
y4<-rbinom(N,n4,p2)


hist(y1,breaks=15, main="Observations From Binomial Distribution ",
     xlab = "Observation Values",ylab = "Frequency", col= "Light Blue", fill="Black")

mu1y=n1*p2
sigmay1=sqrt(n1*p2*(1-p2))

hist(y2,breaks=15, main="Observations From Binomial Distribution ",
     xlab = "Observation Values",ylab = "Frequency", col= "Light Blue", fill="Black")

mu2y=n2*p2
sigmay2=sqrt(n2*p2*(1-p2))

hist(y3,breaks=15, main="Observations From Binomial Distribution ",
     xlab = "Observation Values",ylab = "Frequency", col= "Light Blue", fill="Black")

mu3y=n3*p2
sigmay3=sqrt(n3*p2*(1-p2))

hist(y4,breaks=10, main="Observations From Binomial Distribution ",
     xlab = "Observation Values",ylab = "Frequency", col= "Light Blue", fill="Black")

mu4y=n4*p2
sigmay4=sqrt(n4*p2*(1-p2))
