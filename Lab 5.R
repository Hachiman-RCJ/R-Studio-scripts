#Lab 5 Two Sample Test

#Iris Flower Example
#load ggplot2, ggpubr, and nortest packages before commencing code
#import data
#Make a table for each plant species
#use Anderson Darling Test to the sepal width of each species for normality
ad.test(Iris$sepal.width[Iris$species=="setosa"])
ad.test(Iris$sepal.width[Iris$species == "virginica"])
#We failed to reject the null hypothesis since the p-values for both the 
#setosa and virginica are too big
#
#Make a t test
t.test(Iris$sepal.width[Iris$species == "setosa"])
t.test(Iris$sepal.width[Iris$species == "virginica"])
#Make a graph for the means including standard error bars
#
ggerrorplot(Iris,x="species",y="sepal.width",
            desc_stat ="mean_se", error.plot = "errorbar", add = "mean",
              xlab = "Iris Flower", ylab = "Mean sepal.width",color = "Orange")

#applying two sample t.test on Iris data
t.test(Iris$sepal.width ~ Iris$species, alternative ="two.sided", paired = F)
#
#
#
#We will run a similar test on egg size from Rockhopper Penguins
#importing data
#Use Anderson Darling Test to the egg size for normality for AMASS and BMASS
ad.test(Rockhopper$AMASS)
ad.test(Rockhopper$BMASS)
#We fail to reject the normality for both data sets
#plot ggplot histogram
#in order to visualize the ditribution
ggplot(Rockhopper) + 
  geom_histogram(aes(x=AMASS), col= "Red", fill="Grey") + 
  geom_histogram(aes(x=BMASS), col= "Blue", fill="Black", alpha=0.5) + 
  xlab("Penguin ID") + ylab ("Mass") + theme_classic()

#apply paired t.test
#
t.test(Rockhopper$AMASS,Rockhopper$BMASS,paired=T,alternative = "greater",data=Rockhopper)
#
#
#We will run a third example related to frog species number of egg layed
#in a pond
#import frog data
#create a histogram using ggplot
frogs
RPIPData<-filter(frogs,species=="RPIP")
RCATData<-filter(frogs,species=="RCAT")
RPIPgraph<-ggplot(RPIPData,aes(x=eggs))+geom_histogram(color="Black",fill="Light Green",binwidth = 3)+theme_classic()+labs(x="Number of Eggs", y="Frequency")
RCATgraph<-ggplot(RCATData,aes(x=eggs))+geom_histogram(color="Black",fill="Light Green",binwidth = 3)+theme_classic()+labs(x="Number of Eggs", y="Frequency")
RPIPgraph
RCATgraph
#we are now going to test for normality
ad.test(frogs$eggs[frogs$species=="RPIP"])
ad.test(frogs$eggs[frogs$species=="RCAT"])
#both do not pass the normality test using the anderson darling test
#so we will apply the wilcox test instead, a non parametric test in order to
#test the null hypothesis
wilcox.test(frogs$eggs ~ frogs$species, paired=F)

