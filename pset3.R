# install packages
library(AER)
library(dplyr)
library(tidyr)
library(ggplot2)

# set up working directory
getwd
setwd("/users/nachtloe/Documents/GitHub/ECON-21020/")

# using data frame
caschool <- read_excel("caschool.xlsx")

# question 6 
# a How many observations do you have in the data set?
print(summary(caschool)) # min observation number:1; max observation number:420
# so we have 420 observations in the data set

# b Define a new variable, income, the variable avginc multiplied by 1000.
income <- 1000*caschool$avginc
# ii. The mean and standard deviation of avginc?
print(mean(caschool$avginc)) # gives 15.31659
print(sd(caschool$avginc)) # gives 7.22589

# iii. The mean and standard deviation of income? 
print(sd(income)) # gives 15316.59
print(mean(income)) # gives 7225.89

# c The mean math score across all districts?
print(mean(caschool$math_scr)) # gives 653.3426
# ii The fraction of districts with an average class size of 20 or fewer
fraction_fewer_than_20 <- count(caschool[caschool$str<=20, "str"])/420
print(fraction_fewer_than_20) # gives .5785714
# The mean math score of districts w/ an average class size of 20 or less
mean_fewer_than_20 <- mean(caschool
                           [caschool$str<=20, c("str", "math_scr")]$math_scr)
print(mean_fewer_than_20) # gives 655.7177 

# iii The fraction of districts with an average class size of more than 20
fraction_more_than_20 <- count(caschool[caschool$str>20, "str"])/420
print(fraction_more_than_20) # gives .4214286
# The mean math score of districts w/ an average class size of more than 20
mean_more_than_20 <- mean(caschool
                          [caschool$str>20,c("str", "math_scr")]$math_scr)
print(mean_more_than_20) # gives 650.0819

# v. Calculate a test at the 10% level of whether the mean math score in
# districts with average class size of 20 or fewer students is equal to the
# mean math score in districts with average class size greater than 20.
# 2-sample test (code credit to TA)
# 1 refers to districts with average class size of 20 or fewer
# 0 refers to districts with average class size of more than 20

n1 <- unlist(count (caschool[caschool$str<=20, "math_scr"])[1])
n0 <- unlist(count (caschool[caschool$str>20, "math_scr"])[1])
m1 <- mean_fewer_than_20
m0 <- mean_more_than_20
s1 <- sd(caschool
          [caschool$str<=20,c("str", "math_scr")]$math_scr)
s0 <- sd(caschool
         [caschool$str>20,c("str", "math_scr")]$math_scr)
ts <- (m1 - m0) / sqrt(s1^2/n1 + s0^2/n0) # test statistic
print(ts) # gives 3.121798
2 * pnorm(q = -abs(ts), mean=0, sd=1) # p-value; gives 0.0017975
# since p-value < .10, we reject the null

#vi. The covariance between avginc and mean math score 
cov_bw_avginc_math <- cov(caschool$avginc,caschool$math_scr)
print(cov_bw_avginc_math) # gives 94.7795
#covariance between income and mean math score 
cov_bw_income_math <- cov(income,caschool$math_scr)
print(cov_bw_income_math) # gives 94779.5
  
# vii. The correlation between avginc and mean math score
corr_avginc_math <- cor(caschool$avginc,caschool$math_scr)
print(corr_avginc_math) # gives .6994
# correlation between income and mean math score? 
corr_income_math <- cor(income,caschool$math_scr)
print(corr_income_math) # gives .6994


