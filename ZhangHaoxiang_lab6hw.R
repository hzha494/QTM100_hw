#Read data set
gardasil <- read.table("gardasil.txt", header = TRUE)

#Find proportion of patients going to an urban clinic
proportion_urban <- mean(gardasil$LocationType == "urban")
proportion_urban

#Find proportion of patients going to a suburban clinic
proportion_suburban <- 1 - proportion_urban
proportion_suburban

#It can be found that the proportion going to suburban clinics is
#0.682 and the proportion going to urban clinics is 0.318

#We can use a one-sample proportion hypothesis test to determine
#whether if the number of suburban patients has a significant
#difference than the general population

#Null Hypothesis: The proportion of suburban patients in the sample 
#is equal to the known proportion of suburban patients in the 
#general population.
#The proportion of suburban patients in the sample is different than 
#the known proportion of suburban patients in the general population

#We can run the test using the prop.test() function
prop.test(x = 1413 * proportion_suburban,
          n = 1413,
          p = 0.7)
#The z score is -1.52, the confidence interval is between 0.656 and 0.706, 
#the p-value is 0.1372
#The null hypothesis is failed to reject. It means that this sample
#fails to prove the hypothesis that the sample is not a representation
#of the general population in terms of location type

#We can use the age group variable
proportion_age <- mean(gardasil$AgeGroup == "11-17")
proportion_age
#The proportion of the sample with women less than 18 years old is 0.496

#Null Hypothesis: The proportion of women under age of 18 in the sample 
#is equal to the known proportion of women under age of 18 in the 
#general population.
#The proportion of women under age of 18 in the sample is different than 
#the known proportion of women with age under 18 in the general population

#Now we can run the hypothesis test
prop.test(x = 1413 * proportion_age,
          n = 1413,
          p = 0.53)
#The p-value is 0.011, the confidence interval is between 0.470 and 0.523
#The z score is -2.55
#We reject the null hypothesis. This means that practically this sample is
#not representable for the general population in terms of women under the
#age of 18 taking the HPV vaccine

#calculate p-value
p_value <- 2 * pnorm(-2.55)
p_value
#The p value is 0.011
#This p-value represents the probability of observing a sample proportion 
#as extreme or more extreme than the one observed, assuming the null 
#hypothesis is true.

#The Johns Hopkins team should be slightly worried that their data is
#inappropriate, since the data is partially not representable of the
#entire population