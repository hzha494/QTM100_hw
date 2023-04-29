# Set working directory
setwd("C:/Users/Tom Zhang/Documents/R QTM100")

# Load Data Set
data <- read.csv("SurveySp13.csv")

# Inspect the distribution of days_drink using a table
table(data$days_drink)

# The Distribution is as shown in the output, with corresponding values:
# 0  1  2  3   4  5  6 
# 87 40 44 27  8  1  1 

# Recode days_drink into a factor variable with three categories
data$drink_category <- cut(data$days_drink, 
                           breaks = c(-Inf, 0, 2, Inf), 
                           labels = c("0 days", 
                                      "1-2 days", 
                                      "3 or more days"))

# Then we would want to see the trend of GPA in different drinking categories
# Calculate the mean GPA for each category of drink_category
aggregate(GPA ~ drink_category, data = data, mean)
# We can observe that for 0 days, mean GPA is 3.489, 1-2 days is 3.412, 
# 3 or more days is 3.479.
# There is no clear linear trend

# Create a histogram of GPA for each category of drink_category
par(mfrow = c(1, 3))
hist(data$GPA[data$drink_category == "0 days"], main = "0 days")
hist(data$GPA[data$drink_category == "1-2 days"], main = "1-2 days")
hist(data$GPA[data$drink_category == "3 or more days"], main = "3 or more days")
# We can see that the distribution of GPA in each drinking group are not
# close to normal distribution, but all skewed to the left instead of being
# symmetrical.

# An ANOVA can be appropriate because drinking categories is a categorical
# data while GPA is continuous. However, we should also be cautious since
# the distribution of GPA in each group are not normal but rather skewed.

# Estimate an ANOVA for drink_category and GPA
model <- aov(GPA ~ drink_category, data = data)
summary(model)
# We fail to reject our null hypothesis that the means are different
# across different categories. That means that it is not plausible for
# us to provide an estimate for the relationship between days of drinking
# and GPA for QTM100 students in 2013.

# Perform a Tukey test
pacman::p_load(multcomp)
tukey <- glht(model, linfct=mcp(drink_category="Tukey"))
summary(tukey)
# The mean GPA for students who drink 3 or more days a week is estimated 
# to be 0.06672 higher than that of students who drink 1-2 days a week.
# The p-value associated is 0.670.

# None of the comparisons had a p-value less than 0.05 so all of them
# are not significant.

# The ANOVA test did not show a significant difference, so doing a post-hoc
# test may not be necessary. However, it may not be a fully waste of time
# since it is a good practice.

# Step 1: State the null hypothesis
# H0: There is no difference in the average GPA between male and female students

# Step 2: State the alternative hypothesis
# HA: There is a difference in the average GPA between male and female students

# Step 3: Set the significance level
alpha <- 0.05

# Step 4: Collect the data
# We have the data already

# Step 5: Calculate test statistics
# Step 6: Perform the hypothesis testing
# Both steps can be done in a line of r function
t.test(GPA ~ gender, data = data, var.equal = FALSE)

# Step 7: Communicate the results
# We have a p value of 0.488, which is larger than our alpha which is 0.05. This
# means that we fail to reject the null hypothesis which there is no difference 
# in the average GPA between male and female students. This means that the GPA
# is mostly the same


a <- read.csv("mariokart.csv")

data_subset <- subset(a, ship_sp %in% c("standard", "firstClass", "priority"), select = c(total_pr, ship_sp))

group_var <- ifelse(data_subset$ship_sp == "standard", "standard", "firstClass/priority")

t_test_results <- t.test(total_pr ~ group_var, data = data_subset, var.equal = TRUE)

p_value <- t_test_results$p.value
