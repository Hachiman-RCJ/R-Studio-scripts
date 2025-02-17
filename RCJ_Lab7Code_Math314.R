# Lab 7 & and last one!
# Thank you for a great quarter Dr. Temple!

#Note: Recess 1 happens before lunch. Recess 2 happens after lunch.

library(lattice)
View(nutri)
attach(nutri)
# First objective: Does the placement of recess affects the number of calories consumed?

# create boxplot and a histogram for the number of calories consumed by
# each group of students according to their respective recess.

boxplot(Calories~Recess,xlab = "Recess Groups", ylab = "Calories",
        main="Calories Consumed By Each Recess Group" )
histogram(~Calories|Recess,xlab = "Calories Consumed",
          ylab = "Percent of Students",main="Calories Consumed By Each Recess Group")

# count how many students there is in each group
table(Recess)

# calculate the mean and standard deviation of the number
# of calories consumed for each recess position
tapply(Calories, Recess, summary)
tapply(Calories, Recess, sd)





# Second objective: hypothesis test: to check if there is a significant
# difference on the number of calories consumed by recess. 
#significant level 0.05
# apply a two sample proportion test. 

alpha=0.05

t.test(Calories~Recess,paired=FALSE,conf.level=.95,alternative="two.sided",var.equal=FALSE)



# third objective: look at the relantionship between protein intake and  calories consumed

plot(Calories,`Pro (g)`,xlab = "Calories",ylab="Protein Intake (g)",
     main="Protein Intake Vs. Calories Consumed ")


#fourth objective: calculate correlation R

cor(Calories,`Pro (g)`)


#fifth objective: get the regression line

Model<-lm(`Pro (g)`~Calories)
summary(Model)


#include the regression line on the graph
plot(Calories,`Pro (g)`,xlab = "Calories",ylab="Protein Intake (g)",
     main="Protein Intake Vs. Calories Consumed ")
abline(Model)


# residual plot and histogram

resid(Model)

plot(resid(Model)~Calories,xlab="Calories", ylab="Residual")
abline(h=0)

hist(resid(Model),xlab = "Residuals",ylab = "Frequency",main = "")
