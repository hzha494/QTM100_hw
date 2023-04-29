#Part 1

#import dataset
yrbss <- read.csv("yrbss2013.csv")

# Create new variable height_cm
yrbss$height_cm <- yrbss$height_m * 100

# Summary statistics for height_cm
summary(yrbss$height_cm)

# Mean and standard deviation of height_cm
mean(yrbss$height_cm)
sd(yrbss$height_cm)

# The mean is 168.729, the standard deviation is 10.298

# We can observe the distribution through a histogram
hist(yrbss$height_cm, main = "Distribution of Height (cm)")

# Create a for loop that gets 100 samples of size 10 from the population.
means <- c()
for (i in 1:100) {
  sample <- sample(yrbss$height_cm, size = 10)
  means <- c(means, mean(sample))
}

# Report mean of sample means and standard deviation of sample means
mean(means)
sd(means)

#The mean is 169.45 and the standard deviation is 3.19

hist(means)

# The distribution is skewed to the left, and does not show specific patterns
# other than that.

# Create a for loop that gets 5000 samples of size 10 from the population.
means <- c()
for (i in 1:5000) {
  sample <- sample(yrbss$height_cm, size = 10)
  means <- c(means, mean(sample))
}

# Report mean of sample means and standard deviation of sample means
mean(means)
sd(means)

#The mean is 168.801 and the standard deviation is 3.251

hist(means)

# The distribution is slightly skewed to the left, but is more symmetrical.
# It resembles more to a normal distribution.

# Create a for loop that gets 200 samples of size 20 from the population.
means <- c()
for (i in 1:200) {
  sample <- sample(yrbss$height_cm, size = 20)
  means <- c(means, mean(sample))
}

# Report mean of sample means and standard deviation of sample means
mean(means)
sd(means)

#The mean is 168.711 and the standard deviation is 2.371

hist(means)

# The distribution slightly skewed to the right, but better than the first
# case. It is also more similar to a normal distribution.

# Create a for loop that gets 200 samples of size 5000 from the population.
means <- c()
for (i in 1:200) {
  sample <- sample(yrbss$height_cm, size = 5000)
  means <- c(means, mean(sample))
}

# Report mean of sample means and standard deviation of sample means
mean(means)
sd(means)

#The mean is 168.726 and the standard deviation is 0.092

hist(means)

# The distribution is the most symmetrical in all cases, and is the most
# similar to a normal distribution.

# Increasing the sample size seems to have a larger impact on the sampling
# distribution. This is an application of the Central Limit Theorem, which
# states that  when an infinite number of successive random samples are 
# taken from a population, the distribution of sample means calculated for 
# each sample will become approximately normally distributed with mean µ 
# and standard deviation s/ÖN ( ~N(µ,s/ÖN)) as the sample size (N) becomes 
# larger, irrespective of the shape of the population distribution.
# (referenced from the Emory University Psychology Department Material)



#Part 2

# Import the Data Set
lead <- read.csv("lead.csv")

# Find mean and standard deviation of IQ
lead_mean <- mean(lead$Iqf)
lead_sd <- sd(lead$Iqf)
lead_mean
lead_sd

# The mean is 91.081, the standard deviation is 14.404.

hist(lead$Iqf)

# The distribution is not totally normal, which is slightly skewed to the right.

# We should run a two-sided hypothesis test for sample mean.

# The null hypothesis is the sample mean is equal to the population mean. Which 
# is that the full scale IQ is equal to 85
# The alternative hypothesis is the sample mean is not equal to the population
# mean, which is the full scale IQ not equal to 85.

z = (lead_mean - 85) / (lead_sd / sqrt( nrow(lead) ) )
z
# The z-score is 4.701

p = 2 * (1 - pnorm(z))
p
# The p-value is 2.590e^-6, which is very small.

# Find confidence intervals
confint(lm(Iqf ~ 1, lead), level = 0.95)
#The confidence interval is between 88.520 and 93.641

# Both small p value that is less than the critical value 0.05 and not having 85 
# included in the confidence interval suggests that we reject the null hypothesis.

# We can use a similar test, a two-sided hypothesis test for sample mean.

# The null hypothesis is the sample mean is equal to the population mean. Which 
# is that the blood lead level in 1972 is equal to 36.
# The alternative hypothesis is the sample mean is not equal to the population
# mean, which is the the blood lead level in 1972 is not equal to 36.

z = (mean(lead$Ld72, na.rm = TRUE) - 36) / (sd(lead$Ld72, na.rm = TRUE) / sqrt( nrow(lead) ) )
z
# The z-score is -1.164

p = 2 * (1 - pnorm(-z))
p
# The p-value is 0.244

# Find confidence intervals
confint(lm(Ld72 ~ 1, lead), level = 0.95)
#The confidence interval is between 32.199 and 37.008

# Both a large p value that is greater than the critical value 0.05 and having 36 
# included in the confidence interval suggests that we fail to reject the null 
# hypothesis.