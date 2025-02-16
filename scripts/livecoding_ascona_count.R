library(tidyverse)
library(lme4)
library(MASS)
library(pscl)
library(AER)
library(glmmTMB)


# ice cream consumption
d <- read_csv("../data/ice_cream.csv")

# Poisson model
m <- glm(scoops ~ temperature + rain,
     family = "poisson", data = d)
summary(m)


# calculate Pearson chisquare & Pearson dispersion
# Pearson dispersion value of 0 indicates a "true"
# Poisson distribution. Higher values indicate
# overdisperson, lower values indicate under-
# dispersion.
pr <- resid(m, type = "pearson") # residuals
pchi2 <- sum(residuals(m, type="pearson")^2) #Pearson chi-squared
disp <- pchi2/m$df.residual # Pearson disperson statistic
pchi2; disp

dispersiontest(m)




# Nettle ------------------------------------------------------------------

nettle <- read_csv("../data/nettle_1999_climate.csv")

# Poisson model:
m1 <- glm(Langs ~ MGS, 
          family = "poisson", 
          data = nettle)
summary(m1)

m2 <- glm(Langs ~ MGS + offset(Area), 
          family = "poisson", 
          data = nettle)

summary(m2)
anova(m1, m2, test = "Chisq")

m0 <- glm(Langs ~ 1, 
          family = "poisson", 
          data = nettle)
anova(m0, m1)

dispersiontest(m2)

# negative binomial regression
m_negbin <- glm.nb(Langs ~ MGS + offset(Area), 
                   data = nettle)
summary(m_negbin)


