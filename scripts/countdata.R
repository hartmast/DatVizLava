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
library(ggbeeswarm)
library(VGAM)
library(AER)


# fake data ---------------------------------------------------------------
set.seed(utf8ToInt("Ascona"))
counts <- rpois(100, lambda = 2)
counts2 <- rpois(100, lambda = 1)

# dataframe
df <- tibble(category = c(rep("A", 100), rep("B", 100)),
       count = c(counts, counts2))

# plot
ggplot(df, aes(x = category, y = count)) +
  geom_boxplot()

# model
m <- glm(count ~ category, family = "poisson", data = df)
summary(m)
plot(allEffects(m))


# check assumptions

# how many zeros does the model predict?

# The predict() function returns the means that
# the model estimates.
preds <- predict(m, type = "response")
preds


# the density distribution function (for Poisson: dpois)
# returns the probability that the observation
# is equal to a given value, given the current
# lambda value. 
sum(dpois(x=0, lambda = preds)) # ca. 50 zeros expected


# calculate Pearson chisquare & Pearson dispersion
# Pearson dispersion value of 0 indicates a "true"
# Poisson distribution. Higher values indicate
# overdisperson, lower values indicate under-
# dispersion.
pr <- resid(m, type = "pearson") # residuals
pchi2 <- sum(residuals(m, type="pearson")^2) #Pearson chi-squared
disp <- pchi2/m$df.residual # Pearson disperson statistic
pchi2; disp

# AER package has a test for over-/underdispersion:
dispersiontest(m, alt = "two.sided") # null hypothesis: equidispersion


# Nettle data -------------------------------------------------------------

d <- read_csv("../data/nettle_1999_climate.csv")

# simple Poisson model

m1 <- glm(Langs ~ MGS, data = d)

# plot
plot(allEffects(m1))

# Pearson dispersion to check for over-
# and underdispersion

pr <- resid(m1, type = "pearson") # residuals
pchi2 <- sum(residuals(m1, type="pearson")^2) #Pearson chi-squared
disp <- pchi2/m1$df.residual # Pearson disperson statistic
pchi2; disp #woah!


# also check for zero-inflation:
# how many 0s does the model predict?
preds <- predict(m1, type = "response")
sum(dpois(x=0, lambda = preds)) %>% round

# none - this is in line with our data,
# where we don't have 0 because of course
# we can't have 0 laguages anywhere...

# (This of course raises the theoretical question
# of whether the assumption is fulfilled that zero
# is in principle possible, or whether we should
# use a zero-truncated Poisson or
# negative binomial model instead!)

# zero-truncated (positive) negative binomial model:
m2 <- vglm(Langs ~ MGS + offset(Area), family = posnegbinomial(),
           data = d)
summary(m2) # Hauck-Donner effect: test statistic
# fails to increase monotonically as a function 
# of its distance from the null value;
# leads to extremely high standard errors




######################
### ZERO-INFLATION ###
######################

# fake data ---------------------------------------------------------------
# (adapted from https://aosmith.rbind.io/2019/03/06/lots-of-zeros/)

# simulate data with a single, continuous independent variable

set.seed(16)
x <- runif(200, 2, 10) # simulate explanatory variable
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
