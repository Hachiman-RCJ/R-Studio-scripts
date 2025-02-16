#Tutoring data project
library(readxl)
attach(B_FinalProjectData_Math314)
require(knitr)
require(plotrix)
require(dplyr)
require(tidyr)
require(lattice)
library(ggpubr)


#rewriting the time in, time out, and dates data

B_FinalProjectData_Math314$TI<-format(as.POSIXct(B_FinalProjectData_Math314$TI,format='%I:%M %p'), format = "%H:%M:%S")
B_FinalProjectData_Math314$TO<-format(as.POSIXct(B_FinalProjectData_Math314$TO,format='%I:%M %p'), format = "%H:%M:%S")

q<-strsplit(B_FinalProjectData_Math314$TI,split = ":")
v<-strsplit(B_FinalProjectData_Math314$TO,split = ":")
w<-strsplit(B_FinalProjectData_Math314$Date,split = "/")

B_FinalProjectData_Math314['Year']<-"0"
B_FinalProjectData_Math314['TIUpdate']<-"0"
B_FinalProjectData_Math314['TOUpdate']<-"0"

#assigning each TI and TO a corresponding value based on time recorded
for (i in 1:length(B_FinalProjectData_Math314$TI)) {
  B_FinalProjectData_Math314$TIUpdate[i]=round(round(as.integer(q[[i]][1])+(as.integer(q[[i]][2])/60),digits = 0))
  B_FinalProjectData_Math314$TOUpdate[i]=round(round(as.integer(v[[i]][1])+(as.integer(v[[i]][2])/60),digits = 0))
}

B_FinalProjectData_Math314$TIUpdate=as.integer(B_FinalProjectData_Math314$TIUpdate)
B_FinalProjectData_Math314$TOUpdate=as.integer(B_FinalProjectData_Math314$TOUpdate)



#assigning corresponding year to each data point
for (i in 1:length(B_FinalProjectData_Math314$Date)) {
  if(w[[i]][3]==2021){
    B_FinalProjectData_Math314$Year[i]=2021
    
  }else 
    B_FinalProjectData_Math314$Year[i]=2022
}



# Rewriting the dates into a shorter version
for (i in 1:length(B_FinalProjectData_Math314$Date)) {
  for (j in 9:12) {
    for (k in sprintf("%02d",1:31)) {
      
      if(as.integer(w[[i]][1])==9 & (w[[i]][2]==k)){
        B_FinalProjectData_Math314$Date[i]= paste0("S-", as.integer(w[[i]][2]))
        
      }else if(as.integer(w[[i]][1])==10 & as.integer(w[[i]][2]==k)){
        B_FinalProjectData_Math314$Date[i]= paste0("O-", as.integer(w[[i]][2]))
        
      } else if(as.integer(w[[i]][1])==11 & as.integer(w[[i]][2]==k)){
        B_FinalProjectData_Math314$Date[i]= paste0("N-", as.integer(w[[i]][2]))
        
      }else if(as.integer(w[[i]][1])==12 & as.integer(w[[i]][2]==k)){
        B_FinalProjectData_Math314$Date[i]= paste0("D-", as.integer(w[[i]][2]))
        
      }
    }
  }
}





# make a new column based in a more non specific date such as Monday-Friday or Sunday
Mo=c("S-27","O-4","O-11","O-18","O-25","N-1","N-8","N-15","N-22","N-29","N-6")
Tu=c("S-28","O-5","O-12","O-19","O-26","N-2","N-9","N-16","N-23","N-30","D-7")
We=c("S-29","O-6","O-13","O-20","O-27","N-3","N-10","N-17","N-24","D-1","D-8")
Th=c("S-30","O-7","O-14","O-21","O-28","N-4","N-11","N-18","N-25","D-2","D-9")
Fr=c("O-1","O-8","O-15","O-22","O-29","N-5","N-12","N-19","N-26","D-3","D-10")
Su=c("O-3","O-10","O-17","O-24","O-31","N-7","N-14","N-21","N-28","D-5","D-12")


Mo2=c("S-26","O-3","O-11","O-10","O-17","O-24","O-31","N-7","N-14","N-21","N-28")
Tu2=c("S-27","O-4","O-12","O-11","O-18","O-25","N-1","N-8","N-15","N-22","N-29")
We2=c("S-21","S-28","O-5","O-12","O-19","O-26","N-2","N-9","N-16","N-23","N-30")
Th2=c("S-22","S-29","O-6","O-13","O-20","O-27","N-3","N-10","N-17","N-24","D-1")
Fr2=c("S-23","S-30","O-7","O-14","O-21","O-28","N-4","N-11","N-18","N-25","D-2")
Su2=c("S-25","O-2","O-9","O-16","O-23","O-30","N-6","N-13","N-20","N-27","D-4")


B_FinalProjectData_Math314['UpdatedDates']<-"0"

for (i in 1:length(B_FinalProjectData_Math314$Date)) {
  for (j in 9:12) {
    for (k in 1:11) {
      if(as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==Mo[[k]][1] &
         B_FinalProjectData_Math314$Year[i]==2021){
        B_FinalProjectData_Math314$UpdatedDates[i]="M"
      } else if (as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==Tu[[k]][1] &
                 B_FinalProjectData_Math314$Year[i]==2021){
        B_FinalProjectData_Math314$UpdatedDates[i]="T"
      }else if (as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==We[[k]][1] &
                B_FinalProjectData_Math314$Year[i]==2021){
        B_FinalProjectData_Math314$UpdatedDates[i]="W"
      } else if (as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==Th[[k]][1] &
                 B_FinalProjectData_Math314$Year[i]==2021){
        B_FinalProjectData_Math314$UpdatedDates[i]="Th"
      }else if (as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==Fr[[k]][1] & 
                B_FinalProjectData_Math314$Year[i]==2021){
        B_FinalProjectData_Math314$UpdatedDates[i]="F"
      }else if (as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==Su[[k]][1] & 
                B_FinalProjectData_Math314$Year[i]==2021){
        B_FinalProjectData_Math314$UpdatedDates[i]="Su"
      }
    }
  }
}

for (i in 1:length(B_FinalProjectData_Math314$Date)) {
  for (j in 9:12) {
    for (k in 1:11) {
      if(as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==Mo2[[k]][1] &
         B_FinalProjectData_Math314$Year[i]==2022){
        B_FinalProjectData_Math314$UpdatedDates[i]="M"
      } else if (as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==Tu2[[k]][1] &
                 B_FinalProjectData_Math314$Year[i]==2022){
        B_FinalProjectData_Math314$UpdatedDates[i]="T"
      }else if (as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==We2[[k]][1] &
                B_FinalProjectData_Math314$Year[i]==2022){
        B_FinalProjectData_Math314$UpdatedDates[i]="W"
      } else if (as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==Th2[[k]][1] &
                 B_FinalProjectData_Math314$Year[i]==2022){
        B_FinalProjectData_Math314$UpdatedDates[i]="Th"
      }else if (as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==Fr2[[k]][1] & 
                B_FinalProjectData_Math314$Year[i]==2022){
        B_FinalProjectData_Math314$UpdatedDates[i]="F"
      }else if (as.integer(w[[i]][1])==j & B_FinalProjectData_Math314$Date[i]==Su2[[k]][1] & 
                B_FinalProjectData_Math314$Year[i]==2022){
        B_FinalProjectData_Math314$UpdatedDates[i]="Su"
      }
    }
  }
}


# Filter data into 2021 ane 2022 data respectively
Data2021<-(filter(B_FinalProjectData_Math314,Year==2021))
Data2022<-(filter(B_FinalProjectData_Math314,Year==2022))

# make a qqplot for Time spent at the Learning Commons (LC) in hours for each year respectively
#ggqqplot(Data2021$TTIH)
#ggqqplot(Data2022$TTIH)


# histogram total time spent in LC in hours from each year (details on each figure)
#need to implement a condition that plots only information relevant to 2021 and 2022 respectively
histogram(~B_FinalProjectData_Math314$TTIH|B_FinalProjectData_Math314$Year,
          xlab = "Total Time Spent In Hours",ylab = "Frequency as a Percentage",
          main="Amount Of Time Students Are Present At The Tutoring Center")

#histogram total time spent at the LC in hours for each day in respect to the year
# also boxplots
histogram(~Data2021$TTIH|Data2021$UpdatedDates,xlab = "Total Time Spent In Hours",
          ylab = "Frequency as a Percentage",main="Amount Of Time Students Are Present At The Tutoring Center Fall 2021")
histogram(~Data2022$TTIH|Data2022$UpdatedDates,xlab = "Total Time Spent In Hours",
          ylab = "Frequency as a Percentage", main="Amount Of Time Students Are Present At The Tutoring Center Fall 2022")

boxplot(Data2021$TTIH~Data2021$UpdatedDates,xlab = "",
        ylab = "Total Time Spent In Hours",main="Amount Of Time Students Are Present At The Tutoring Center Fall 2021")
boxplot(Data2022$TTIH~Data2022$UpdatedDates,xlab = "",
        ylab = "Total Time Spent In Hours",main="Amount Of Time Students Are Present At The Tutoring Center Fall 2022")


#histogram that illustrates the time in and out from student at the LC by the year
histogram(~B_FinalProjectData_Math314$TIUpdate|B_FinalProjectData_Math314$Year,xlab = "Time Checked In ",ylab = "Frequency as a Percentage",
          main="Check in Times From Students in Fall 2021 and 2022")
histogram(~B_FinalProjectData_Math314$TOUpdate|B_FinalProjectData_Math314$Year,xlab = "Time Checked Out ",ylab = "Frequency as a Percentage",
          main="Check out Times From Students in Fall 2021 and 2022")



# t.test for if there is a difference between 2021 and 2022 student activity at the LC
table(Year)
tapply(TTIH, Year, summary)
tapply(TTIH, Year, sd)

alpha=0.05
t.test(B_FinalProjectData_Math314$TTIH~B_FinalProjectData_Math314$Year,paired=FALSE,conf.level=.95,alternative="two.sided",var.equal=FALSE)


# t.test if there is a difference between 2021 and 2022 on what time do student start arriving and leaving
#contains corresponding histograms as well (this needs fixing)

tapply(B_FinalProjectData_Math314$TIUpdate, Year, summary)
tapply(B_FinalProjectData_Math314$TIUpdate, Year, sd)

tapply(B_FinalProjectData_Math314$TOUpdate, Year, summary)
tapply(B_FinalProjectData_Math314$TOUpdate, Year, sd)




t.test(B_FinalProjectData_Math314$TIUpdate~Year,alternative="two.sided",var.equal=FALSE)
t.test(B_FinalProjectData_Math314$TOUpdate~Year,alternative="two.sided",var.equal=FALSE)





# create plot that tells us the frequency of students showing up for each day in fall 2021 and fall 2022.

ggplot(Data2021,aes(x=UpdatedDates))+geom_bar()+scale_x_discrete(limits=c("M","T","W","Th","F","Su"))+
  theme_classic()+labs(x="Days", y="Frequency",title = "Fall 2021 Student Attendance At The Tutoring Center")

ggplot(Data2022,aes(x=UpdatedDates))+geom_bar()+scale_x_discrete(limits=c("M","T","W","Th","F","Su"))+
  theme_classic()+labs(x="Days", y="Frequency",title = "Fall 2022 Student Attendance At The Tutoring Center")


# create a table to see which day was the busiest throughout the quarter
table(Data2021$Year,Data2021$UpdatedDates)
table(Data2022$Year,Data2022$UpdatedDates)



# are the student attendance frequency  variables independent/dependent by the year variables.
#chi square test will be conducted.

categoricalData<-table(B_FinalProjectData_Math314$UpdatedDates,B_FinalProjectData_Math314$Year)
Rowprop=proportions(categoricalData,1)
chisq.test(B_FinalProjectData_Math314$UpdatedDates,B_FinalProjectData_Math314$Year,correct = FALSE)






# Check the correlation between the amount of time spent at the LC with 
# the data that tells us what time student showing up
# for both 2021 and 2022 respectively


plot(Data2021$TIUpdate,Data2021$TTIH,xlab = "Time In",ylab = "Time Spent in Hours",
     main = "TTIH Vs. TIUpdate")
cor(Data2021$TIUpdate,Data2021$TTIH)
Model2021<-lm(Data2021$TTIH~Data2021$TIUpdate)
abline(Model2021)
plot(Data2022$TIUpdate,Data2022$TTIH,,xlab="Time In",ylab = "Time Spent in Hours",
     main = "TTIH Vs. TIUpdate")
cor(Data2022$TIUpdate,Data2022$TTIH)
Model2022<-lm(Data2022$TTIH~Data2022$TIUpdate)
abline(Model2022)



summary(Model2021)
summary(Model2022)

resid(Model2021)
resid(Model2022)

plot(resid(Model2021)~Data2021$TIUpdate,xlab="Time in",ylab = "Residual Values",
     main="Residual Values From TTIH Vs. TIUpdate")
abline(h=0)

plot(resid(Model2022)~Data2022$TIUpdate,xlab="Time in",ylab = "Residual Values",
     main="Residual Values From TTIH Vs. TIUpdate")
abline(h=0)







