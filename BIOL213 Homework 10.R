#Homework 10
#import data
#Question 2 - Tree data
#we will conduct a one sample t-test
t.test(TreeData$diameter)
#
#Question 3- fish data
#import data
#ANOVA Test
Salmon.aov<- aov(Salmondata$SalmonWeight ~ Salmondata$Site )
Salmon.aov
#Calculate the residuals for each point in the dataset
Salmon.resid <- residuals(Salmon.aov)
Salmon.resid
#plot the residual into a histogram to check for normality
hist(Salmon.resid, main = "  Salmon Residual Weight",
     xlab = "Weight",ylab = "Frequency",col = "darkmagenta")
#check for normality
ad.test(Salmon.resid)
#We fail to reject the null hypothesis and the assumptions are met
#Summary results will be computed using the summary.aov command using the 
#aov. object
summary.aov(Salmon.aov)
# since we need to reject the null hypothesis, we need to assume that their
#is difference between the salmon weight in each river
# so we will use the tukey test to see where this is occuring
TukeyHSD(Salmon.aov)
#plot results
plot(TukeyHSD(Salmon.aov), las=1) +
