#Lab 6 ANOVA
#import data
#apply Bartlett test in order to see if all three groups
#are equal in variance
bartlett.test(Lab6data$time~Lab6data$treatment)
#null hypotheisis has not been rejected and assumptions are met
#apply the one factor ANOVA test
antiviral.aov <- aov(Lab6data$time  ~ Lab6data$treatment)
antiviral.aov
#caluclate the residuals for each point in the dataset
antiviral.resid <- residuals(antiviral.aov)
antiviral.resid
#
#plot the residual computations into a histogram and check for normality
#if it does show a normal distribution then it does fit the ANOVA assumptions
# for normality

hist(antiviral.resid, main = "Antiviral Residual Data",
     xlab = "Antiviral Residual Values",ylab = "Frequency",col = "darkmagenta")
#pass normality test
#apply the anderson darling test in order to 
#test normality with the residual values
ad.test(antiviral.resid)
#We fail to reject the null hypothesis and the assumptions are met
#Summary results will be computed using the summary.aov command using the 
#aov. object
summary.aov(antiviral.aov)
#create plot of means and SE bars from the antiviral data
ggerrorplot(Lab6data,x="treatment",y="time",
            desc_stat ="mean_se", error.plot = "errorbar", add = "mean",
            xlab = "Method Treatments", ylab = "Mean Time",color = "darkmagenta")
#example 2- chernobyl
#
#apply the Bartlett test in order to check on equality of variance amoung data group
bartlett.test(nucleardata$Sr.level~nucleardata$source)
#apply a one way ANOVA
nucleardata.aov<-aov(nucleardata$Sr.level  ~ nucleardata$source)
nucleardata.aov
#obtain the residuals from the nucleardata.aov
nucleardata.res<-residuals(nucleardata.aov)
nucleardata.res
#apply the Anderson Darling test in order to test normality
ad.test(nucleardata.res)
#we fail to reject the null hypothesis that the mean values are different
#apply the summary.aov since the assumptions have been met
summary.aov(nucleardata.aov)
#apply the turkey test since the F value is significant
TukeyHSD(nucleardata.aov)
#plot results
plot(TukeyHSD(nucleardata.aov), las=1)
#
#
#example 3 Rhinos
#apply Bartlett test in order to see if all  groups
#are equal in variance
bartlett.test(rhinodata$Distance.Travelled~rhinodata$Treatment)
#null hypotheisis has not been rejected and assumptions are met
#apply the one factor ANOVA test
DistanceTraveled.aov <- aov(rhinodata$Distance.Travelled  ~ rhinodata$Treatment)
DistanceTraveled.aov
#caluclate the residuals for each point in the dataset
DistanceTraveled.resid <- residuals(DistanceTraveled.aov)
DistanceTraveled.resid
#
#plot the residual computations into a histogram and check for normality
#if it does show a normal distribution then it does fit the ANOVA assumptions
# for normality

hist(DistanceTraveled.resid, main = "Antiviral Residual Data",
     xlab = "Antiviral Residual Values",ylab = "Frequency",col = "darkmagenta")
#pass normality test
#apply the anderson darling test in order to 
#test normality with the residual values
ad.test(DistanceTraveled.resid)
#We  reject the null hypothesis and the assumptions are not met
# we will apply a non parametric statistic test
#kruskal.test
kruskal.test(rhinodata$Distance.Travelled ~ rhinodata$Treatment)
#create a box plot for the rhino data

ggplot(rhinodata,aes(x=Treatment,y=Distance.Travelled))
+geom_boxplot(color="black",fill="Orange", binwidth=10) + theme_classic()
+ labs(x="Treatment", y= "DistanceTraveled")
