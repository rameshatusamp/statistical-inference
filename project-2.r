
library("knitr")
library("stringr")
library("ggplot2")
require("ggplot2")
library(grid)
library(data.table)

# ToothGrowth dataset is the length of odontoblasts (teeth) in each of 10 guinea pigs at each of 
# three dose levels of Vitamin C (0.5, 1, and 2 mg) with each of two delivery methods (orange juice or ascorbic acid). 

# Now, let's load the `ToothGrowth` data

toothGrowthDF <- data.table(ToothGrowth)
head(toothGrowthDF)

# Basic Exploratory Data Analysis

# Review the internal structure of the `ToothGrowth` dataset:
str(toothGrowthDF)

# The numeric variable `dose` is an experimental factor with three discrete levels (0.5, 1 and 2 milligrams), 
# so we will convert it to a factor variable.
ToothGrowth$dose <- factor(ToothGrowth$dose)
table(ToothGrowth$dose)

# Review the summary of `ToothGrowth` dataset:
summary(toothGrowthDF)


# Frequency table to verify the number of observations

toothGrowthFreqTable <- table( ToothGrowth$supp, ToothGrowth$dose)
toothGrowthFreqTable

# Plot length of teeth v/s supplement (VC or OJ)
plot( len ~ supp, data=ToothGrowth)

# Plot length of teeth v/s dosage
plot( len ~ dose, data=ToothGrowth)

# From the above two plots we notice that as the dosage increases then length of the tooth increases. 
# However we are unable to determine which of the two supplements (VC or OJ), is contributing to the growth of the teeth. 

# Confidence Interval and Null Hypothesis Tests

# In the subsequent steps we will use `Confidence Interval and Null Hypothesis Tests` to 
# compare tooth growth by supplement and dosage.

# Comparative Analysis of Dosages

# Null Hypothesis: `Increasing the dosages of VC or OJ, DOES NOT increase the length of the teeth`

# Let's begin by creating three vectors of data, one for each dosage level (0.5mg, 1mg and 2mg)
subset0.5mg <- subset(toothGrowthDF,dose=='0.5')$len
subset1.0mg <- subset(toothGrowthDF,dose=='1')$len
subset2.0mg <- subset(toothGrowthDF,dose=='2')$len

# Working towards our null hypothesis we will start comparing dosage in their increasing order by 
# using t-tests to see if there are differences between the two dosage groups.

# Case 1: Increase the dosage from 0.5mg to 1.0mg:

# Performing t-tests:
tTest0.5mgTo1.0mg <- t.test(subset0.5mg, subset1.0mg, paired=FALSE,var.equal=FALSE)

# Get the 95% `confidence interval` for the mean appropriate to the specified alternative hypothesis.
tTest0.5mgTo1.0mg$conf.int[1:2]

# When we increase the dosage from 0.5mg to 1.0mg, we found that the confidence intervals do not contain zero (0)

# Case 2: Increase the dosage from 1.0mg to 2.0mg:

# Performing t-tests:
tTest1.0mgTo2.0mg <- t.test(subset1.0mg, subset2.0mg, paired=FALSE,var.equal=FALSE)

# Get the confidence interval for the mean appropriate to the specified alternative hypothesis.
tTest1.0mgTo2.0mg$conf.int[1:2]

# When we increase the dosage from 1.0mg to 2.0mg, we found that the confidence intervals do not contain zero (0)

# Conclusion from Case 1 and Case 2: 
# In both the cases, confidence intervals do not contain zero (0) we can reject the null hypothesis to 
# conclude that `Increasing the dose DOES increase the length of the teeth`

# Comparative Analysis of Supplements

# Null Hypothesis: `Increasing vitamin C supplements alone, DOES NOT increase the length of the teeth`

# Let's being by creating two vectors of data, one for VC and other for OJ:

subsetVC <- subset(toothGrowthDF,supp=='OJ')$len
subsetOJ <- subset(toothGrowthDF,supp=='VC')$len

# Compare the two supplements:

# Performing t-tests:
tTestSupplements <- t.test(subsetVC, subsetOJ, paired=FALSE,var.equal=FALSE)

# Get the p-value and confidence interval for the mean appropriate to the specified alternative hypothesis.
tTestSupplements$p.value
tTestSupplements$conf.int[1:2]

# Conclusion
# We observe that p-value os 0.60 and the confidence interval contains 0 and therefore we `DO NOT REJECT` the null hypothesis and conclude that `Increasing vitamin C supplements alone, DOES NOT increase the length of the teeth`.

# Comparative Analysis of two supplements with each of the three dosages:

# Null Hypothesis: `Supplements with 0.5mg dosage of vitamin C DOES NOT affect tooth growth`
# Let's being by creating two vectors of data, one for VC and other for OJ for 0.5mg dosage:
  
subsetVC0.5mg <- subset(toothGrowthDF,supp=='VC' & dose == '0.5')$len
subsetOJ0.5mg <- subset(toothGrowthDF,supp=='OJ' & dose == '0.5')$len

# Compare the two supplements with 0.5mg dosage:

# Performing t-tests:
tTestSupplements0.5mg <- t.test(subsetVC0.5mg, subsetOJ0.5mg, paired=FALSE,var.equal=FALSE)

# Get the p-value and confidence interval for the mean appropriate to the specified alternative hypothesis.
tTestSupplements0.5mg$p.value
tTestSupplements0.5mg$conf.int[1:2]

# Conclusion
We observe that the confidence interval does not contains 0 and therefore we `REJECT` the null hypothesis.

# Null Hypothesis: `Supplements with 1.0mg dosage of vitamin C DOES NOT affect tooth growth`
# Let's being by creating two vectors of data, one for VC and other for OJ for 1.0mg dosage:

subsetVC1.0mg <- subset(toothGrowthDF,supp=='VC' & dose == '1')$len
subsetOJ1.0mg <- subset(toothGrowthDF,supp=='OJ' & dose == '1')$len

# Compare the two supplements with 1.0mg dosage:

# Performing t-tests:
tTestSupplements1.0mg <- t.test(subsetVC1.0mg, subsetOJ1.0mg, paired=FALSE,var.equal=FALSE)

# Get the p-value and confidence interval for the mean appropriate to the specified alternative hypothesis.
tTestSupplements1.0mg$p.value
tTestSupplements1.0mg$conf.int[1:2]

# Conclusion
# We observe that the confidence interval does not contains 0 and therefore we `REJECT` the null hypothesis.

# Null Hypothesis: `Supplements with 2.0mg dosage of vitamin C DOES NOT affect tooth growth`
# Let's being by creating two vectors of data, one for VC and other for OJ for 2.0mg dosage:
  
subsetVC2.0mg <- subset(toothGrowthDF,supp=='VC' & dose == '2')$len
subsetOJ2.0mg <- subset(toothGrowthDF,supp=='OJ' & dose == '2')$len

# Compare the two supplements with 2.0mg dosage:

# Performing t-tests:
tTestSupplements2.0mg <- t.test(subsetVC2.0mg, subsetOJ2.0mg, paired=FALSE,var.equal=FALSE)

# Get the p-value and confidence interval for the mean appropriate to the specified alternative hypothesis.
tTestSupplements2.0mg$p.value
tTestSupplements2.0mg$conf.int[1:2]

# Conclusion
# We observe that p-value is close to 1 and the confidence interval DOES have 0 and therefore we `ACCEPT` the null hypothesis and conclude that `vitamin C dosage of 2.0mg does not affect tooth growth. However, we cannot conclude which of the two supplements types has a greater impact.


