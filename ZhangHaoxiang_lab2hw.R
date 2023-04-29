#Set working directory
setwd("C:/Users/Tom Zhang/Documents/R QTM100")

#import data set
ADNI <- read.table("ADNI.txt", header = T)

#There are 276 observations

ADNI_structure <- str(ADNI)

#Data type in reality vs. Data type in R

#First, we can generate a data frame of data types in different columns of ADNI
ADNI_class <- data.frame(sapply(ADNI, class))

#Then we can modify the name of the column
colnames(ADNI_class) <- "In_R"

#Then we can add the information of whether a variable is numerical or categorical
ADNI_class$In_reality <- c("numerical", #Age
                           "categorical", #Gender
                           "categorical", #APOE4
                           "numerical", #MMSE
                           "numerical", #Wholebrain
                           "numerical", #adas
                           "categorical" #DX
                           )
ADNI_class

#Recode APOE4 variable in ADNI data frame to a factor variable
ADNI$APOE4 <- factor(ADNI$APOE4, 
                     labels = c(0,1,2), 
                     levels = c(0,1,2))

#Show summary of APOE4 variable after modification
summary(ADNI$APOE4)
#The variant of two copies of ApoE2 allele is the least common

#A boxplot may be used to visualize the distribution of age
boxplot(ADNI$AGE)

#The distribution is symmetrical
#It is more clustered towards the center
#The range is about 35, the median is about 74

#A side by side box plot may be used to visualize the distribution
#of MMSE and adas scores in different DX status
boxplot(ADNI$MMSE ~ ADNI$DX)
boxplot(ADNI$adas ~ ADNI$DX)

#For the MMSE test, the people diagnosed with Alzheimer's disease
#showed the lowest testing scores, having a median of 23 and locating
#mostly between 21 and 25. 
#Mild cognitive impairment patients follows, with a median of 28 and 
#the majority locating between 26 and 29. 
#The normal people scored the highest, with the majority of data located between 28 and 30.
#There is a tendency showing that for people having lower scores, they might
#be more likely to be diagnozed with Alzheimer's disease.

#For the adas test, Alzheimer's disease patients scored on average the highest
#with a median of 30 and the majority locating around 30.
#Mild cognitive impairment patients follow, with a median approximately
#around 18 and major data ranging from 10 to over 20.
#Normal people scored the lowest, with a median around 10 and the majority of
#data being located around 10.
#There is also a tendency in the test, which is that the higher the score
#the more likely the patient is diagnozed with Alzheimer's disease.

#The adas test showed more outliers, with 2 outliers in the Alzheimer's disease
#group and 1 outlier in the normal group.

#Generate a new variable, WholeBrain_Modify, which is Wholebrain divided by 100000
ADNI$WholeBrain_Modify <- ADNI$WholeBrain / 100000

#Find summary for DX
ADNI$DX <- factor(ADNI$DX)
summary(ADNI$DX)

#Find mean and standard deviation for age and brain volume
#with respect to DX using the favstats function from mosaic package

#load mosaic
library(mosaic)

#Find data using favstats function
favstats(ADNI$AGE)
favstats(ADNI$AGE~ADNI$DX)
favstats(ADNI$WholeBrain_Modify)
favstats(ADNI$WholeBrain_Modify~ADNI$DX)

#Find data for APOE4 using dplyr functions
library(tidyverse)
APOE4_summarise <- ADNI %>% 
  group_by(APOE4, DX) %>% 
  summarise(n = n())

temp <- APOE4_summarise %>% 
  group_by(DX) %>% 
  summarise(DX_num = sum(n))

APOE4_summarise <- merge(APOE4_summarise, temp, by = "DX")

APOE4_summarise$percentage <- APOE4_summarise$n * 100 / APOE4_summarise$DX_num

APOE4_whole <- ADNI %>% 
  group_by(APOE4) %>% 
  summarise(n = n(),
            percentage = n() / 2.76)

#Find data for males using a similar way
gender_summarise <- ADNI %>% 
  group_by(GENDER, DX) %>% 
  summarise(n = n()) %>% 
  filter(GENDER == "Male") %>% 
  merge(temp, by = "DX")

gender_summarise$percentage <- gender_summarise$n / gender_summarise$DX_num

#Therefore the table can be filled out

#Overall		Diagnosis Group	
#AD	MCI	Normal
#n = 276	n = 54	n =128	n = 94
#Age	73.6 +/- 7.0	73.9 +/- 8.0	72.9 +/- 7.3	74.3 +/- 5.8
#Gender (male)	153(55.4%)	30(55.6%)	75(58.6%)	48(51.1%)
#Brain volume x105 mm3	10.2 +/- 1.1	9.7 +/- 1.2	10.3 +/- 1.1	10.4 +/- 1.0
#APOE4				
#No copies	137(49.6%)	18(33.3%)	57(44.5%)	62 (66.0%)
#One copy	109(39.5%)	25(46.3%)	56(43.8%)	28 (29.8%) 
#Two copies	30(10.9%)	11(20.4%)	15(11.7%)	4 (4.3%)

#I would include a photo copy of the table in my submission

#The ADNI study has 276 participants. The average age is 73.6 years and 
#55.4% are male. The Alzheimer’s group has a lower average brain volume than 
#the Normal group (9.7 +/- 1.2 vs 10.4 +/- 1.0 mm3).  Patients with Alzheimer’s 
#diagnosis have a higher prevalence of two copies of the APOE4 allele 
#compared to normal diagnosis patients ( 20.4% vs 4.3%).
