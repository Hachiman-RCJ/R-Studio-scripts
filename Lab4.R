#Lab 4: Goodness of fit and Contingency
#Correctly write a null and an alternative hypothesis
#Conduct chi squared for goodness of fit and contingency 
#Correctly intepret P-values 
#
#
#Calculate the mean number of antlion using the summary (x) command
#such that x is your data file name
summary(antlion)
#We will create table to input the data found from summary command
#This will help summarize the antlion pit count
#
antTable<-table(antlion$pits)
#
#Checking if information is saved in antTable
antTable
#
#Create and name a vector object for the obseerved frequencies 
#
observed_object<-as.vector(antTable)
#Checking data_object
observed_object
#
#New vector will represent the probabilities for each number of antlion
#pits per quadrat, assuming the null hypothesis is true
#
probability_object<-dpois(0:8,mean(antlion$pits))
#checking probability_object
probability_object

#now we will compute another vector object
#this vector execution will be on expected frequecies
expected_object <- probability_object*length(antlion$pits) 
#check expected_object
expected_object
#now we will combine the expected and observed data into a single data table.
#The length() command will give the sample size (n) of the data.
combined_object<-rbind(observed_object,expected_object)
row.names(combined_object) <- c("Observed", "Expected")
#checking combined_object
combined_object
#We will be creating a plot of the observed and expected values for each number
#of antlion  pits per quadrant 
#we will be using the barplot this time because our data is in a box form
barplot(combined_object, beside=T, xlab="Length"
        , ylab="Frequency", col=c("Light Green", "Orange")) 
#Improving barplot
barplot(combined_object, beside=T,xlab="Length"
        , ylab="Frequency",col=c("Light Green", "Orange"),ylim=c(0,25))
#We will add the x-axis categories using the pits command
pits<-c(0,1,2,3,4,5,6,7,8)
pits
#we will add a key figure legend and the x-axis values by formating our barplot
barplot(combined_object, beside=T,xlab="Length"
        , ylab="Frequency",col=c("Light Green", "Orange"),
        ylim=c(0,25),names.arg = c(pits),legend=rownames(combined_object))
#compute a chi square goodness of fit test using the observed and expected data
chisq.test(combined_object,p=probability_object,rescale.p = T)
# Now we will look at Tests of Association (Contingency Analysis)
#use titanic data
data("Titanic")
Titanic
#create a tables representing sex and survival possibilities
table_name <- apply(Titanic, c(2, 4), sum)
table_name
#improve tablename
TitanicXaxis<-c("No","No","Yes","Yes")

#create a barplot for titanic data
barplot(table_name, beside=T,xlab="Survival"
        , ylab="Frequency",col=c("Light Green", "Orange"),
        names.arg = c(TitanicXaxis),legend=rownames(table_name))
#creating mosaic plot using titanic data
mosaicplot(table_name, col = c("Black", "Grey"),main="")
#compute chi square value for titanic data
chisq.test(table_name)
