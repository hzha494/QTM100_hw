#Import the data set
yrbss <- read.csv("yrbss2013.csv")

# Calculate frequency distribution and proportion for the gender variable
table(yrbss$gender)
prop.table(table(yrbss$gender))

#The proportion of females is 0.504
#The proportion of males is 0.496

# Calculate frequency distribution and proportion for the carried_weapon variable
table(yrbss$carried_weapon)
prop.table(table(yrbss$carried_weapon))

#The proportion of no is 0.844
#The proportion of yes is 0.156

#Create a for loop that gets 100 samples of size 10 from the population

#Generate variable to store all values
proportions <- vector(length = 100)

#Start for loop
for(i in 1:100) {
  #take 10 samples out of the entire population
  sample <- sample(yrbss$gender, size = 10, replace = TRUE)
  
  #Find out the proportion of needed responses, in this case female
  proportion <- sum(sample == "female")/10
  
  #Store the value into the vector
  proportions[i] <- proportion
}

#Calculate for the needed value
mean(proportions)

#The value is 0.499

#Using the same way, form a for loop for 5000 samples of size 10,
#200 samples of size 20 and 200 samples of size 5000
proportions <- vector(length = 5000)
for(i in 1:5000) {
  sample <- sample(yrbss$gender, size = 10, replace = TRUE)
  proportion <- sum(sample == "female")/10
  proportions[i] <- proportion
}
mean(proportions)

proportions <- vector(length = 200)
for(i in 1:200) {
  sample <- sample(yrbss$gender, size = 20, replace = TRUE)
  proportion <- sum(sample == "female")/20
  proportions[i] <- proportion
}
mean(proportions)

proportions <- vector(length = 200)
for(i in 1:200) {
  sample <- sample(yrbss$gender, size = 5000, replace = TRUE)
  proportion <- sum(sample == "female")/5000
  proportions[i] <- proportion
}
mean(proportions)

#The values are 0.500, 0.496, and 0.504 respectively for females
#The values are 0.500, 0.504, and 0.496 respectively for males

#Using the same method we can form same for loops for the carried_weapon variable
proportions <- vector(length = 100)
for(i in 1:100) {
  sample <- sample(yrbss$carried_weapon, size = 10, replace = TRUE)
  proportion <- sum(sample == "no")/10
  proportions[i] <- proportion
}
mean(proportions)

proportions <- vector(length = 5000)
for(i in 1:5000) {
  sample <- sample(yrbss$carried_weapon, size = 10, replace = TRUE)
  proportion <- sum(sample == "no")/10
  proportions[i] <- proportion
}
mean(proportions)

proportions <- vector(length = 200)
for(i in 1:200) {
  sample <- sample(yrbss$carried_weapon, size = 20, replace = TRUE)
  proportion <- sum(sample == "no")/20
  proportions[i] <- proportion
}
mean(proportions)

proportions <- vector(length = 200)
for(i in 1:200) {
  sample <- sample(yrbss$carried_weapon, size = 5000, replace = TRUE)
  proportion <- sum(sample == "no")/5000
  proportions[i] <- proportion
}
mean(proportions)

#The values for no is 0.837, 0.845, 0.847, 0.844 respectively
#The values for yes is 0.163, 0.155, 0.153, 0.156 respectively

#Increasing sample size seem to have a larger impact on the sampling distribution
#Increasing sample size makes the measurement more reliable.
#When the sample size is increased, the sampling distribution becomes narrower 
#and more peaked, with a smaller variance and a more accurate estimate of the 
#population proportion. This is because a larger sample size provides more information 
#and reduces the effect of random sampling variation.