#Set working directory
setwd("C:/Users/Tom Zhang/Documents/R QTM100")

#Import data set
fruitFly <- read.csv("fruitfly.csv")

#A box plot may be produced to compare the different distributions
#of lifespan in different groups.
boxplot(fruitFly$lifespan ~ fruitFly$type,
        xlab = "Type",
        ylab = "Life Span")

#The group for the shortest average life span is group 5.

#We would want to get the mean and standard deviation of
#the lifespan of that group.

#We would want to import the tidyverse package
library("tidyverse")

#We would like to filter for group 5 data and calculate for
#the values
group5 <- fruitFly %>% filter(type == 5)
mean(group5$lifespan)
sd(group5$lifespan)
#The mean life span is 38.72 and the standard deviation is 12.10

#For future operations, we would want to have a summary of the data
#set with the values of means and standard deviation of each
#group's life span
fruitFly %>% 
  group_by(type) %>% 
  summarise(mean = mean(lifespan),
            sd = sd(lifespan))

#Using the data that we got, we can calculate some probabilities
#using the normal distribution of specific days that the flies
#may survive
pnorm(30, 38.72, 12.10)
pnorm(50, 38.72, 12.10) - pnorm(30, 38.72, 12.10)
pnorm(70, 38.72, 12.10) - pnorm(50, 38.72, 12.10)
1 - pnorm(70, 38.72, 12.10)
#The probabilities of surviving less than 30 days, between 30
#and 50 days, between 50 and 70 days, and more than 70 days 
#are 0.236, 0.589, 0.171, 0.005 respectively

#The flies that escaped are probably from the "supplied with 8 
#newly pregnant females" group, because they have a higher
#life span which is shown in the pregnant females group
#but not the virgin females group

fruitflysubset<-subset(fruitFly,type==5) 

#Find the 10th, 25th, 50th, 75th and the 90th percentile
#for group 5 in a theoretical normal distribution
qnorm(0.1, mean = 38.72, sd = 12.10)
qnorm(0.25, mean = 38.72, sd = 12.10)
qnorm(0.5, mean = 38.72, sd = 12.10)
qnorm(0.75, mean = 38.72, sd = 12.10)
qnorm(0.9, mean = 38.72, sd = 12.10)

#Find the actual percentiles
quantile(fruitflysubset$lifespan, 
         probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

#Therefore we can fill out the table:
#Supplied with 8 virgin females: N(38.72, 12.10)
#       Theoretical Observed
#10th   23.21       21.8
#25th   30.56       32.0
#50th   38.72       40.0
#75th   46.88       47.0
#90th   54.23       54.0

#Find proportion of fruit flies living at least 50 days
mean(fruitflysubset$lifespan>50)
#The value is 0.20

#The table can be filled in with the following values:
#Supplied with 10 virgin females, Bin(10, 0.20)
#The following, from 0 to 10:
dbinom(0,10,0.2)
dbinom(1,10,0.2)
dbinom(2,10,0.2)
dbinom(3,10,0.2)
dbinom(4,10,0.2)
dbinom(5,10,0.2)
dbinom(6,10,0.2)
dbinom(7,10,0.2)
dbinom(8,10,0.2)
dbinom(9,10,0.2)
dbinom(10,10,0.2)

#The probability of exactly 6 flies survive when supplied
#with 10 virgin females
dbinom(6,10,0.2)

#The value is 0.0055, which is less than 0.13
#Therefore the group provided with pregnant females is more
#likely to have an exact survival of 6 out of 10

#For the group provided with pregnant females: 8
#For the group provided with virgin females: 2
#The group provided with 8 pregnant females is expected to
#have a higher survival rate

#The group provided with 8 pregnant females is more likely to
#have more than 5 survivals. The probability for the pregnant
#group and the virgin group are respectively:
#pregnant group
1-pbinom(4,10,0.76)
#virgin group
1-pbinom(4,10,0.2)