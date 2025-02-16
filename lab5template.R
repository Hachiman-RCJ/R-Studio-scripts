require(knitr)
require(plotrix)

# This is creating a random binomial distribution and storing it
# in variable 'samples'
samples<-rbinom(100,150,.6)

# dividing each sample by 150.It looks like its calculating the point estimate
phat<-samples/150


# We are calculating 95% confidence interval
# Left hand side
leftconflimits<-phat -1.96*sqrt(phat*(1-phat)/150)

# 90% confidence interval left hand side
leftconflimits2<-phat-1.65*sqrt(phat*(1-phat)/150)


# We are calculating 95% confidence interval
# Right hand side
rightconflimits<-phat + 1.96*sqrt(phat*(1-phat)/150)

# 90% confidence interval right hand side
rightconflimits2<- phat+1.65*sqrt(phat*(1-phat)/150)


# The head() function in R is used to display the 
# first n rows present in the input data frame
# In this case it is only displaying the first row of data from leftconflimits
head(leftconflimits)

# making a 100x2 table with the data from leftconflimits and rightconflimits
conflimits<-cbind(leftconflimits,rightconflimits)
conflimits2<-cbind(leftconflimits2,rightconflimits2)
# Displaying the first row of data from conflimits
head(conflimits)
head(conflimits2)
# ifelse returns a value with the same shape as 'test' which is filled with
# elements selected from either yes or no depneidng on whether the element
# of test is true or false

# In our case, we are checking if the expression ".6<leftconflimits|.6>rightconflimits"
#is true or false. So whenever, the or statment is true then we get a red plot
# and whenever it is false then we get a black plot
# We are using the .6 because that is the population proportion 

mycolors<-ifelse(.6<leftconflimits|.6>rightconflimits,"Red","Black")
mycolors2<-ifelse(.6<leftconflimits2|.6>rightconflimits2,"Red","Black")

# Plotting the confidence interval values with error bars for each observation generated 
# by the binomial distribution
# x and y are the coordinate for the center of error 
#li and ui are lower end and upper end error bars
#col is plotted according the resutls from mycolors
# ylim is the vertical range
# abline adds one or more straight lines in a graph
plotCI(x=1:100,y=phat,li=conflimits[,1],ui=conflimits[,2],col=mycolors,ylim=c(.4,.8),
main="95% Confidence Interval And Error Bars",
xlab="Random Observations", ylab=" Point estimate ")
abline(h=.6)

#this plotCI is for 90% confidence interval
plotCI(x=1:100,y=phat,li=conflimits2[,1],ui=conflimits2[,2],col=mycolors2,ylim=c(.4,.8),
       main="90% confidence interval And Error Bars",xlab="Random Observations",
       ylab="Point Estimate")


#Part 2: Use atheist data
View(atheism)

# Figure out how to create a subset that only looks at US responses only
us12<- subset(atheism,nationality=="United States" & year=="2012")
us12

USTable<-droplevels(subset(atheism,nationality=="United States"& year=="2012"))
table(USTable$nationality,USTable$response)
#Part3
# create a 95% condifence interval on the proportion of atheist in the US
prop.test(table(us12$response),correct=FALSE)
# Results:We are 95% confident that approximately between 4 to 7 US individuals 
# consider themselves atheists.

# Part 4 & 5: Same as step 2 & 3 but choose two different countries
# and create a graph
table(atheism$nationality,atheism$response)


#creating subsets for Afghanistan and Australia respectively
Fc12<- subset(atheism,nationality=="France" & year=="2012")
Fc12
JP12<- subset(atheism,nationality=="Japan" & year=="2012")
JP12

# create 95% confidence interval on the proportion of atheist 
#in Afghanistan and Australia respectively
prop.test(table(Fc12$response),correct=FALSE)
prop.test(table(JP12$response),correct = FALSE)

# create a graph now with the categorical variables
# First, we will create a table that only contains data for France and Japan
FcJP12<-droplevels(subset(atheism,(nationality=="France"|nationality=="Japan")& year=="2012"))
table(FcJP12$nationality,FcJP12$response)

# create barplot
barplot(table(Fc12$nationality,Fc12$response),ylab = "Frequency",
        main="France Responses On Being Atheist or Not")
barplot(table(JP12$nationality,JP12$response),ylab = "Frequency",
        main="Japan Responses On Being Atheist or Not")
