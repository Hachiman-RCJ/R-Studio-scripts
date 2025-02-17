#Homework9
#import data
#question 1 - salamander length
#Organize data into two columns where we have
#data pertaining to sites and salamnder length
UpdatedSalData<-stack(salamanderdata)
UpdatedSalData
names(UpdatedSalData) <- c("sites", "SalLength")
UpdatedSalData
#works 
#We will assume  that the data fit a normal distribution 
#and the variances for each of the groups are equal
#test the null hypothesis using the ANOVA technique
SalLength.aov <- aov(UpdatedSalData$sites  ~ UpdatedSalData$SalLength)
SalLength.aov
summary.aov(SalLength.aov)
#results: F-value = 5.215 and Pr(>F)=0.00242
#now we will do the turkey test, in order to determine any differences in mean
TukeyHSD(SalLength.aov)
#plot results
plot(TukeyHSD(SalLength.aov), las=1)
#
#
#Question 2 telomere length
#import data
#test for normality, use Anderson-Darling test
ad.test(telomeresdata$years)
ad.test(telomeresdata$telomere.length)
#fail to reject normality hypothesis
#test correlation test
cor.test(telomeresdata$telomere.length,telomeresdata$years)
#results: r=-0.4306534 and p-value=0.006205
#create a scatter plot
ggplot(telomeresdata,aes(x=years,y=telomere.length))+geom_point(color="purple", size=3)+theme_classic()+labs(x="Years", y="Telomere length")


       