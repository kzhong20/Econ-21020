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
data1 <- read_excel("birthweight_smoking.xlsx")

# Question 7 
# (a) Regress Birthweight on Smoker. 
# What is the estimated difference in average birthweight for smokers vs. non-smokers?
summary(data1)
birthweight.smoker.lm <- lm(birthweight ~ smoker, data = data1)
summary(birthweight.smoker.lm) # which gives a beta1 = -253.23


# (b) Regress Birthweight on Smoker, Alcohol, and Nprevist.
birthweight.smoker.alc.N.lm <- lm(birthweight ~ smoker + alcohol + nprevist, 
                                data = data1)

summary(birthweight.smoker.alc.N.lm)
# which gives:           Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3051.249     34.016  89.701  < 2e-16 ***
#   smoker      -217.580     26.680  -8.155 5.07e-16 ***
#   alcohol      -30.491     76.234  -0.400    0.689    
# nprevist      34.070      2.855  11.933  < 2e-16 ***

# i. Explain why the exclusion of Alcohol and Nprevist could lead to omit-
#   ted variable bias if we wished to interpret the regression in (a) as causal.

# ii. Is the estimated effect of smoking on birth weight substantially different
# from the regression that excludes Alcohol and Nprevist? What would
# this suggest about the omitted variable bias?

# iii. Use your regression to predict the birth weight of a child whose mother
# smoked, did not drink alcohol, and had 8 prenatal care visits.
birthweight1 <- 3051.249-217.580+34.07*8
print(birthweight1) #this gives 3106.229

# iv. Compute R2 and adjusted R2. Why are they so similar?
summary(birthweight.smoker.alc.N.lm)
# this gives R2 = .07285, adjusted R2 = .07192

# v. Given that we are including Nprevist as a control variable, how should
# you interpret the coefficient on Nprevist? Does the coefficient measure
# a causal effect of prenatal visits on birth weight?

# (c) Explain how you could use this dataset to empirically verify the Frisch-
# Waugh Theorem 
# Conduct this empirical verification for the regression coefficient on
# Smoker from part (b).
# Step 1: Regress Smoker on Alcohol & NPrevist
lm1 <- lm (smoker ~ alcohol + nprevist, data = data1)
# Store Residual1
resid.1 <- resid(lm1)
# Step 2: Regress Birthweight on Alcohol & NPrevist
lm2 <- lm (birthweight ~ alcohol + nprevist, data = data1)
# Store Residual2
resid.2 <- resid(lm2)
# Step 3: Regress Residual2 on Residual1
lm3 <-lm (resid.2 ~ resid.1)
summary(lm3) #gives -217.6, approx the same as -217.580 from part b).

