# Lab 6 Math 314

View(Football)
attach(Football)
#part 1
#create a variable for wins and losses

Football$Wins<-as.factor(ifelse(Football$Fav_pts>Football$Un_pts,"F","U"))
Football$Wins

# check results by looking at the first line
head(Football$Wins)

# hypothesis test on whether the favored team is winning more frequently than
# underdog team. alpha=0.05

#Part 2
# create a table of the wins from favored and unfavored team
#total matches = 451+221=672
# compute the point estimate (phat), standard error, z-score and p-value
table(Football$Wins)
n=672
phat=451/n
SEF=sqrt((.5*(1-.5))/n)
Zscore=(phat-.5)/SEF
pvalue=2*(1-pnorm(Zscore))
alpha=0.05
pvalue<alpha

prop.test(table(Football$Wins),p=.5,correct = FALSE)
# Thus, we reject the null hypothesis.

#part 3,4,5

table(Football$Home,Football$Wins)
prop.test(table(Football$Home,Football$Wins),correct = FALSE)



# part 6
xf<-table(Football$Home,Football$Wins)


barplot(xf,main="Home and Away Winning Games From Favored and Unfavored Teams ", xlab = "Teams",ylab = "Winning Frequency",
        beside = TRUE,legend.text = c("Favored Team","Unfavored Team"),
        names.arg = c("Home Games","Away Games"),col = c("Grey","Light Blue"))



