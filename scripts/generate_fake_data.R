library(stats)
devtools::install_github("johannabertl/SelectionMix")
library(rnegbinmix)

pnegbin(125, 7, .5)

# get ice cream
icecream_poisson <- rpois(125, 20)
x <- tibble(day = 1:125,
            scoops = icecream_poisson)

x$temperature <- round(jitter(x$scoops+2, factor = 5))
x$scoops %>% hist
x$rain <- ifelse(x$scoops<=20 & x$temperature>25, "yes", "no")

tempover20 <- which(x$temperature>20)
myspl <- sample(1:68, 20)
x[tempover20[myspl],]$temperature <- sample(15:28, 1)

tempover20 <- which(x$temperature>20)
myspl <- sample(1:68, 20)
x[myspl,]$rain <- "yes"
x[which(x$rain=="yes"),]$scoops <- x[which(x$rain=="yes"),]$scoops - sample(2:5,1) 

# export
write_csv(x, "ice_cream.csv")


glm(scoops ~ temperature+rain, data = x, family = "poisson") %>% summary
