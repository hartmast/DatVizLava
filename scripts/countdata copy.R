########################
# Modeling count data  #
########################



# libraries ---------------------------------------------------------------

library(tidyverse)
library(glmmTMB)
library(MASS)
library(pscl)
library(effects)
library(vcd)
library(car)
library(readxl)


### OVERDISPERSION ###

# "Overdispersion describes the observation that variation is higher than 
# would be expected. Some distributions do not have a parameter to fit 
# variability of the observation. For example, the normal distribution 
# does that through the parameter $\sigma$ (i.e. the standard deviation of 
# the model), which is constant in a typical regression. 
# In contrast, the Poisson distribution has no such parameter, 
# and in fact the variance increases with the mean (i.e. the variance 
# and the mean have the same value)."
# (http://biometry.github.io/APES//LectureNotes/2016-JAGS/Overdispersion/OverdispersionJAGS.html)

# get 100 * 200 Poisson-distributed samples

set.seed(1985)
random_pois <- lapply(9:50, function(i) rpois(n = 200, lambda = 10))

# 


# fake data ---------------------------------------------------------------
# (adapted from https://aosmith.rbind.io/2019/03/06/lots-of-zeros/)

# simulate data with a single, continuous independent variable

set.seed(16)
x <- runif(200, 5, 10) # simulate explanatory variable
b0 <- 1 # set value of intercept
b1 <- 0.25 # set value of slope
means <- exp(b0 + b1*x) # calculate true means
theta <- 0.25 # true theta


# simulate data from the negative binomial distribution
y <- rnbinom(200, mu = means, size = theta)


# fit negative binomial model
fit1 <- glm.nb(y ~ x)

# check for excess zeros:
# how many zeros are in the data?
sum(y == 0)

# How many zeros does the model predict?
preds <- predict(fit1, type = "response") # estimated means
esttheta <- summary(fit1)$theta # estimated theta
prop0 <- dnbinom(x = 0, mu = preds, size = esttheta )
round(sum(prop0))

# fit Poisson model instead
fit2 <- glm(y ~ x, family = poisson)
sum(y == 0)

# how many zeros does the model predict?
round( sum( dpois(x = 0,
                  lambda = predict(fit2, type = "response") ) ) )  # 0!



# spelling errors data ----------------------------------------------------

dat <- read_xlsx("../data/errors_data.xlsx")

# improve column names
colnames(dat) <- gsub(" ", "_", colnames(dat))

# English names for categories
dat$Category <- case_when(dat$Kategorie_grob=="Stamm" ~ "Stem",
                            dat$Kategorie_grob=="Derivation" ~ "Derivation",
                            dat$Kategorie_grob=="Flexion" ~ "Inflection")

dat$Category <- factor(dat$Category, levels = c("Stem", "Derivation", "Inflection"))

# visualize
ggplot(dat, aes(x = Category)) + geom_bar()



# fake data ---------------------------------------------------------------

# from McElreath (2021: 378)

# define parameters
prob_drink <- 0.2 # 20% of days
rate_work <- 1 # average 1 manuscript per day

# sample one year of production
N <- 365

# simulate days on which the monks drink
set.seed(365)
drink <- rbinom(N, 1, prob_drink)

# simulate number of completed manuscripts
y <- (1-drink)*rpois(N, rate_work)

# visualize
hist(y)
table(y) %>% barplot() # many days on which the monks produce 0 manuscripts!

# zero-inflated Poisson model:

# dataframe 
monks <- tibble(drink = drink,
       manuscripts = y)

ggplot(monks, aes(x = manuscripts, fill = factor(drink), group = factor(drink))) + geom_bar(position = "dodge")

m_monks <- zeroinfl(manuscripts ~ factor(drink), data = monks)
summary(m_monks)

# data --------------------------------------------------------------------

d <- read_csv("../data/nettle_1999_climate.csv")




# visualization -----------------------------------------------------------

# visualize data to show
# distributions

ggplot(d, aes(x = MGS, y = Langs)) +
  geom_point() + geom_smooth(method = MASS::glm.nb)



# fit model
m <- glm.nb(Langs ~ MGS + offset(Area), data = d)

# model summary
summary(m)


# compare with model without MGS
m0 <- glm.nb(Langs ~ 1 + offset(Area), data = d)
anova(m, m0)

# model assumptions:
# NB models assume that
# the conditional means are not equal 
# to the conditional variances

m_pois <- glm(Langs ~ MGS + offset(Area), family = "poisson", data = d)

# likelihood ratio test comparing the Poisson
# model and the negative binomial model:

pchisq(2*(logLik(m) - logLik(m_pois)), df = 3, lower.tail = F)

# estimated chi-squared value:
2*(logLik(m) - logLik(m_pois))




# plot
ggplot(d, aes(x = MGS, y = Langs)) +
  geom_point() + geom_smooth(method = MASS::glm.nb)



# model diagnostics -------------------------------------------------------

# observed vs. fitted frequencies
fit <- goodfit(d$Langs, type = "nbinomial")
summary(fit)
rootogram(fit)

Ord_plot(d$Langs, type = "nbinomial")
anova(m, test = "Chisq")

# check for influential datapoints
influencePlot(m)
