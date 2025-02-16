#Lab 2 Math 314 

#view class data collected
View(class_data314)

#try attach command
#attach command will assume you are referencing the class data
#so we will only need to type the variable of interest
attach(class_data314)


#we will use the following categorical variables: 
# Gender and Longest Toe

#create a cross tabulate for the two categorical variables
#(row,column)

x=table(Toe,Gender)

#create a row and column proportion table respectively

RowProp=proportions(table(Toe,Gender),1)
ColProp=proportions(table(Toe,Gender),2)

#create a bar plot for tabulated data x

barplot(x,main = "The Longest Toe From Each Gender",xlab = "Genders",ylab = "Frequency",
        beside = TRUE,legend.text =c("Big Toe","Index Toe","Middle Toe"),
        names.arg = c("Female","Male","Non-Binary"),col = c("Grey","Light Blue","Light Green"))

#create a second barplot that uses column proportions
barplot(ColProp,main = "The Longest Toe From Each Gender (In Proportions)",xlab = "Genders",ylab = "Frequency",
        beside = TRUE,legend.text =c("Big Toe","Index Toe","Middle Toe"),
        names.arg = c("Female","Male","Non-Binary"),col = c("Grey","Light Blue","Light Green"))

#create a mosaic plot using both count and proportion tables

mosaicplot(x,main = "The Longest Toe From Each Gender",xlab = "Toe",
           ylab = "Gender", col=c("Grey","Light Blue","Light Green"))

mosaicplot(ColProp,main ="The Longest Toe From Each Gender (In Proportions)",
           xlab = "Genders",ylab = "Frequency",
           col=c("Grey","Light Blue","Light Green") )

#analyze the relationship between a categorical and numerical variable
# we will use the following variables: Longest Toe  and height

# we will create a box plot for each combination between the two variables

boxplot(Height~Toe,ylab = "Student Height (Inches)",xlab = "Toe", 
        main="Student's Longest Toe and Height Responses      ")


#Now we will calculate the summary statistics
#using tapply command
# This will give us the summary for each scenario shown in the box plot

tapply(Height, Toe, summary)

#Next we will calculate the standard deviation for each combination
#using the tapply command

tapply(Height,Toe,sd)


