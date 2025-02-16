library(tidyverse)
library(pscl)
library(MASS)
library(lme4)
library(effects)
library(glmmTMB)
library(AER)


# read data ---------------------------------------------------------------

d <- read_csv("../data/graphvar_summary_1980ff.csv")


# Poisson model:
m1 <- glm(errors ~ year + subject + gender, 
          family = "poisson", data = d)

summary(m1)
dispersiontest(m1)


m2 <- glm.nb(errors ~ year + subject + gender, 
              data = d)
summary(m2)


scale(d$year)









# errors:
d$errors_scaled <- scale(d$errors)
d$year_scaled <-  scale(d$year)

m3 <- glm.nb(errors ~ year_scaled + subject + gender, 
             data = d)
summary(m3)



# zeros?
any(d$errors==0)

# minimum and maximum
range(d$errors)


# fit a Poisson model
m <- glm(errors ~ year + subject + gender, 
    family = "poisson", data = d)
summary(m)

# test for overdispersion
pr <- resid(m, type = "pearson") # residuals
pchi2 <- sum(residuals(m, type="pearson")^2) #Pearson chi-squared
disp <- pchi2/m$df.residual # Pearson disperson statistic
pchi2; disp
dispersiontest(m)

# fit a negative binomial model
m2 <- glm.nb(errors ~ year + subject + gender, data = d)
summary(m2)

# also check for zero-inflation/-truncation:
# how many 0s does the model predict?
preds <- predict(m2, type = "response")
sum(dpois(x=0, lambda = preds)) %>% round
# 0, as in our data

# compare with poisson model
preds <- predict(m, type = "response")
sum(dpois(x=0, lambda = preds)) %>% round
preds
# also 0
