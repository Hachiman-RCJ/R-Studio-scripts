#Fall quarter 2022

attach(FinalFall2021TutorData2)

#code on working on refining what course appears more often

ggplot(Fall2022Tutoring3,aes(x=`Course Subject`))+geom_bar()+theme_classic()

  
barplot(FinalFall2021TutorData2$`Course Subject`,FinalFall2021TutorData2$UpdatedDates)

histogram(~UpdatedDates|`Course Subject`)


hist(`Course Subject`~UpdatedDates)

#rewriting the date data

y<-strsplit(Fall2022Tutoring3$Date,split = "/")

# Rewriting the dates into a shorter version
for (i in 1:length(Fall2022Tutoring3$Date)) {
  for (j in 9:11) {
    for (k in sprintf("%02d",1:31)) {
      
      if(as.integer(y[[i]][1])==9 & (y[[i]][2]==k)){
        Fall2022Tutoring3$Date[i]= paste0("S-", as.integer(y[[i]][2]))
        
      }else if(as.integer(y[[i]][1])==10 & as.integer(y[[i]][2]==k)){
        Fall2022Tutoring3$Date[i]= paste0("O-", as.integer(y[[i]][2]))
        
      } else if(as.integer(y[[i]][1])==11 & as.integer(y[[i]][2]==k)){
        Fall2022Tutoring3$Date[i]= paste0("N-", as.integer(y[[i]][2]))
      }
      
    }
  }
}

#ploting date data
PlotByDate<-ggplot(Fall2022Tutoring3,aes(x=Date))+geom_bar()+
  theme_classic()+labs(title = "Fall 2021 Student Visit at Learning Commons",
                       x="Date",y="Frequency")
PlotByDate<-PlotByDate+scale_x_discrete(limits=c("S-27","S-28","S-29","S-30","O-1","O-4",
                                                 "O-5","O-6","O-7","O-8","O-11","O-12","O-13","O-14","O-15","O-17","O-18",
                                                 "O-19","O-20","O-21","O-22","O-24","O-25","O-26","O-27","O-28",
                                                 "O-29","O-31","N-1","N-2","N-3","N-4","N-5","N-7"))

PlotByDate


# Rewriting the dates into a shorter version
# rewriting the dates into Monday,Tuesday,...,or Sunday
# create another column for dataframe FinalFall2021TutorData2
#Monday:27,4,11,18,25,1,8,15,22,29,6
#Tuesday:28,5,12,19,26,2,9,16,23,30,7
#Wednesday:29,6,13,20,27,3,10,17,24,1,8
#Thursday:30,7,14,21,28,4,11,18,25,2,9
#Friday:1,8,15,22,29,5,12,19,26,3,10
#Sunday: 3,10,17,24,31,7,14,21,28,5,12


Mo=c("S-27","O-4","O-11","O-18","O-25","N-1","N-8","N-15","N-22","N-29","N-6")
Tu=c("S-28","O-5","O-12","O-19","O-26","N-2","N-9","N-16","N-23","N-30","D-7")
We=c("S-29","O-6","O-13","O-20","O-27","N-3","N-10","N-17","N-24","D-1","D-8")
Th=c("S-30","O-7","O-14","O-21","O-28","N-4","N-11","N-18","N-25","D-2","D-9")
Fr=c("O-1","O-8","O-15","O-22","O-29","N-5","N-12","N-19","N-26","D-3","D-10")
Su=c("O-3","O-10","O-17","O-24","O-31","N-7","N-14","N-21","N-28","D-5","D-12")

Fall2022Tutoring3['UpdatedDates']<-"0"


for (i in 1:) {
  for (j in 9:11) {
    for (k in 1:11) {
      if(as.integer(y[[i]][1])==j & Fall2022Tutoring3$Date[i]==Mo[[k]][1]){
        Fall2022Tutoring3$UpdatedDates[i]="M"
      } else if (as.integer(y[[i]][1])==j & Fall2022Tutoring3$Date[i]==Tu[[k]][1]){
        Fall2022Tutoring3$UpdatedDates[i]="T"
      }else if (as.integer(y[[i]][1])==j & Fall2022Tutoring3$Date[i]==We[[k]][1]){
        Fall2022Tutoring3$UpdatedDates[i]="W"
      } else if (as.integer(y[[i]][1])==j & Fall2022Tutoring3$Date[i]==Th[[k]][1]){
        Fall2022Tutoring3$UpdatedDates[i]="Th"
      }else if (as.integer(y[[i]][1])==j & Fall2022Tutoring3$Date[i]==Fr[[k]][1]){
        Fall2022Tutoring3$UpdatedDates[i]="F"
      }else if (as.integer(y[[i]][1])==j & Fall2022Tutoring3$Date[i]==Su[[k]][1]){
        Fall2022Tutoring3$UpdatedDates[i]="Su"
      }
    }
  }
}


# create a new plot that tells us the number of students that attend each 
#day during fall quarter

ggplot(FinalFall2021TutorData2,aes(x=UpdatedDates))+
  geom_bar()+scale_x_discrete(limits=c("M","T","W","Th","F","Su"))

table(FinalFall2021TutorData2$UpdatedDates,FinalFall2021TutorData2$`Course Subject`)

barplot(table(`Total Time In Minutes`,UpdatedDates))







