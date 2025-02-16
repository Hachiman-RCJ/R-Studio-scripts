#Working on Lab 3 confidence intervals and assessing normality
#In today’s lab you will use visuals and statistical tests to determine 
#if different sample data distributions conform to normal distributions.
#The majority of points for this lab will be based on your Rscript file
#so make sure to keep it fully commented and only use commands that do not produce errors.
#
#
#Calculate the Confidence interval value for high and low
#However, we need to calculate the standard error by using command "describe"
#Part 2
SMean<-describe(stickleback$size)
SMean
SE=.44
# The SE is equal to .44

#Calculate Confidence intervals for high and low

Cl_higher<-mean(stickleback$size)+(2*SE)
Cl_lower<-mean(stickleback$size)-(2*SE)

#Check values for CL_higher and lower
Cl_higher
Cl_lower

#2.	Another method is to use the t-test feature 
#to quickly provide the confidence interval
sd<-t.test(stickleback$size)

#However we can repeat this for each subset of 
#your data to get each set of confidence intervals.
#but we need to use lake command first
lake<-filter(stickleback,source=="Lake")
stream<-filter(stickleback,source=="Stream")
lake
stream
#now we will use the t-test
t.test(lake$size)
t.test(stream$size)

#part 3
#1.	Plot a histogram of the stickleback length data using 
#the ggplot2 package using only the simple default settings
#ggplot(fish_data,aes(x=TL))+geom_histogram(color="Black",fill="Light Green",binwidth = 10)+ theme_classic()+ labs(x="Total Length (mm)", y= "Frequency" )

ggplot(stickleback,aes(x=size))+geom_histogram(color="Black",fill="Light Green",binwidth = 1)+theme_classic()+labs(x="Source", y="Frequency")

#add on stat function to plot a density function but add to historgram command aes(y=..density..)
ggplot(stickleback,aes(x=size))+geom_histogram(aes(y=..density..),color="Black",fill="Light Green",binwidth = 1)+theme_classic()+labs(x="Source")+stat_function(fun=dnorm,args=list(mean=mean(stickleback$size),sd=sd(stickleback$size)), col="Blue")
#
#
#
mygraph<-ggplot(stickleback,aes(x=size))+geom_histogram(aes(y=..density..),color="Black",fill="Light Green",binwidth = 1)+theme_classic()+labs(x="Source")+stat_function(fun=dnorm,args=list(mean=mean(stickleback$size),sd=sd(stickleback$size)), col="Blue")
mygraph

#. It might be easier to see these groups if you divide them out according to source

mygraph+facet_wrap(~stickleback$size)

#7.	With ggplot()’s, you can layer on several different curves at once. 
#Use the stat_function() from 3 above to add another curve to the graph object 
#you created in #4, based on the only the lake data. 
mygraph + stat_function(fun=dnorm, args=list(mean=mean(lake$size), sd=sd(lake$size)), col="Red") + stat_function(fun=dnorm, args=list(mean=mean(stream$size), sd=sd(stream$size)), col="Black")

#9.	Another way of looking at normality is to compare the 
#distribution of stickleback to normal distributions, but using a QQ plot

ggplot(stickleback,aes(sample=size))+geom_qq()+geom_qq_line()

#improving QQ plot
ggplot(stickleback,aes(sample=size,col=source))+geom_qq()+geom_qq_line()

#Part 4
#You can use your eyes to visually determine if your data are normally
#distributed, but it can be hard to tell for sure. Fortunately, statisticians
#have come up with a few tests for normality - the Anderson-Darling test and
#the Shapiro-Wilk test. 

#p values are often how people think that something is, or isn’t significant,
#and these p values range from 0 to 1. Both of these tests (Anderson Darling 
#and Shapiro Wilk) are tests that determine if something is not a 
#normal distribution - meaning that a significant p value (p < 0.05)
#indicates something is NOT a normal distribution.

#start of with Anderson Darling test
ad.test (stickleback$size)
shapiro.test(stickleback$size)

# it looks neither is significant because the p-value is less than 5 percent (0.05)
#
#Now we will check individually for both lake and stream data using 
#the previous commands
#
ad.test(lake$size)
ad.test(stream$size)

shapiro.test(lake$size)
shapiro.test(stream$size)

#P-values are still high, which implies they are not significant
#and does not follow a normal distribution
