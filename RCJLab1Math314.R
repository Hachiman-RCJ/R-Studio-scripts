#Math 314 Lab 1

#uploading data
View(class_data314)

#creating a histogram for a numerical variable
hist(class_data314$Sleep)

#create another histogram with breaks
hist(class_data314$Sleep,breaks=4)
hist(class_data314$Sleep,breaks=20)


#create a box plot this time
boxplot(class_data314$Sleep)


#update both the histogram and boxplot by adding title and labels
hist(class_data314$Sleep,breaks =4,main="Hours Math 314 Students Sleep"
     ,xlab="Sleeping Hours (hr)",ylab="Number of Students(Frequency)")

boxplot(class_data314$Sleep,ylab="Sleeping hours (hr)",
        main="Hours Math 314 students sleep")

#run the command summary for the numerical data in number of sleeping hours
summary(class_data314$Sleep)

#calculate the standard deviation value for sleeping hour data
#we added na.rm=TRUE, so it can calculate the standard deviation
#this tells Rstudio to ignore/remove the NA values
sd(class_data314$Sleep,na.rm=TRUE)


#Look at the relationship between two numerical variables
#We will choose the numerical variables height and driving
#add the adequate labels
plot(class_data314$Driving~class_data314$Height,pch=16,
     xlab="Student height (inches)",ylab="Amount of time Driving (min)"
     ,main="Driving Time Vs. Student Height")
