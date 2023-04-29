#import dataset
abductees <- read.csv("abductees.csv")

#Genderate variable age
abductees$age <- 90 - abductees$yearbir

#Modift edulevel into three levels: less than college, college and more than college
abductees$educat <- cut(abductees$educat, 
                        breaks = c(-Inf, 12, 16, Inf),
                        labels = c("Less than or equal to High School",
                                   "College Education",
                                   "More than College"))

#We would also want to modify the marstat variable into married and others
abductees$marstat <- ifelse(abductees$marstat == "Married", 
                            "Married", 
                            "Other")

#We would like to present a side-by-side box plot for the ages grouped by
#different abduction experiences
boxplot(age ~ abdfeel, data = abductees, 
        main = "Distribution of age by abduction experience", 
        xlab = "Abduction experience", 
        ylab = "Age at time of survey")

#There is not a clear trend in this case

#We would want to visualize the data more clearly by ordering the different
#levels of abduction experiences
abductees$abdfeel <- factor(abductees$abdfeel, 
                            levels = c("Entirely negative", 
                                       "Mostly negative", 
                                       "About equally positive and negative", 
                                       "Mostly positive", 
                                       "Entirely positive"))
#We would then produce the box plot again
boxplot(age ~ abdfeel, data = abductees, 
        main = "Distribution of age by abduction experience", 
        xlab = "Abduction experience", 
        ylab = "Age at time of survey")

#Now we can clearly visualize that as the ages go higher, it seems like on
#average the abduction experience is better

#We would like to find out several data from the data set

#Firstly we want to find out the average age of male respondents
mean(abductees$age[abductees$sex == "male"], na.rm = TRUE)
#The value is approximately 47.5

#Secondly we want to find out the average number of abduction times among 
#individuals classified as “other” for marital status?
mean(abductees$abdtimes[abductees$marstat == "Other"], na.rm = TRUE)
#The value is approximately 3.3

#Lastly we want to find out how many individuals have a college level 
#education, but no more than a college education
sum(abductees$educat == "College Education", na.rm = TRUE)
#There are 29 individuals