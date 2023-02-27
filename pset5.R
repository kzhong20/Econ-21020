# install packages
library(AER)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# set up working directory
getwd
setwd("/users/nachtloe/Documents/GitHub/ECON-21020/")

# using data frame
data1 <- read_excel("cps04.xlsx")
data2 <- read_excel("fertility.xlsx")

summary(data1)

# Question 4
# (a) iii Estimate the model using OLS and construct a 95% confidence interval
# for the effect of being female on income.

lm4a <- lm(AHE ~ Female + College + HS, data=data1)
summary(lm4a) # this gives SE(b1)=.07375

# v. Test H0 : β2 = β3 versus H1 : β2 > β3 at the 5% significance level.
# Be careful to explain how you construct the test. Explain the null and
# alternative hypotheses in words.

# ols of y on a constant, X.1, X.2, and X.3
ls_fit <- lm(AHE ~ Female + College + HS, data=data1)
summary(ls_fit)

# for a one sided test (credit to TA)
c <- 0
r <- c(0, 0, 1, -1)
beta_hat <- ls_fit$coefficients
cov_mat <- vcovHC(ls_fit, type = "HC1")
T_n <- (t(r) %*% beta_hat - c) / sqrt(t(r) %*% cov_mat %*% r)
T_n

# (b) Use the dataset to estimate the following model,
# wage = γ0 + γ1fem + γ2college + γ3(hs + college) + U.
# To do this regression, you will have to create a new variable that is the sum
# of hs and college.

# ii. Test H0 : γ2 = 0 versus H1 : γ2 > 0 at the 5% significance level. How do
# the results of your test compare to what you found in part (v) of part
# (a) above? Explain briefly.
data1$HSC <- data1$HS + data1$College
ls_fit2 <- lm(AHE ~ Female + College + HSC, data=data1)
summary(ls_fit2)

c <- 0
r <- c(0, 0, 1, 0)
beta_hat <- ls_fit2$coefficients
cov_mat <- vcovHC(ls_fit2, type = "HC1")
T_n <- (t(r) %*% beta_hat - c) / sqrt(t(r) %*% cov_mat %*% r)
T_n #gives Tn=86.50059

# (c) Consider the following model of the determinants of income:
#   wage =
#   β0 + β1fem + β2college + β3fem ×college + β4hs + β5fem ×hs + U .
# Suppose that the regressors are not perfectly colinear.
# i. For the rest of part (c), suppose that U is uncorrelated with each of the
# regressors and that the fourth moments of the regressors and depen-
#   dent variable exist. Estimate the model using OLS. Consider the null
# hypothesis that there are no interactions between sex and education.
# Formally state the null and alternative hypotheses. Conduct the test at
# the 10% level.

# Fit a multiple linear regression model
data1$femC <- data1$Female * data1$College
data1$femH <- data1$Female * data1$HS

lm3 <- lm(AHE ~ Female + College + femC + HS + femH, data = data1)

# Test the hypothesis

c <- 0
r <- matrix(c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), nrow = 2) 
linearHypothesis(lm3, r, c, vcov = vcovHC(lm3, type = "HC1"),
                 test = "Chisq")
# this gives Tn = 39.671, p-value = 2.43e-09 

# 5. How does fertility affect labor supply? 
# (a) Regress weeksworked on the indicator variable morekids using OLS. On
# average, do women with more than two children work less than women with
# two children? How much less?
summary(data2)

lm5a <- lm(weeksm1 ~ morekids, data2)
summary(lm5a) # this gives coefficient: -5.38700

# (c) The data contain the variable samesex, which is equal to 1 if the first two
# children are of the same sex, and equal to 0 otherwise. Are couples whose
# first two children are of the same sex more likely to have a third child? Is
# the effect large? Is it statistically significant?

lm5c <- lm(morekids ~ samesex, data2)
summary(lm5c, alternative="greater")
# this gives coefficient: .067525, Tn=35.2

# (e) Estimate the IV regression of weeksworked on morekids using samesex as
# an instrument. How large is the fertility effect on labor supply?
formula <- weeksm1 ~ morekids | samesex

# Fit the instrumental variable regression model
lm5e <- ivreg(formula, data = data2)

# Print the results
summary(lm5e) # this gives coefficient: -6.314

# (f) 
formula2 <- weeksm1 ~ morekids + agem1 + black + hispan + othrace | samesex+ 
  agem1 + black + hispan + othrace
# Fit the instrumental variable regression model
lm5f <- ivreg(formula2, data = data2)

# Print the results
summary(lm5f) # this gives coefficient for more kids: -5.82105






