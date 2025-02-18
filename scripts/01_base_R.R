library(tidyverse)
library(beeswarm)
library(ggbeeswarm)
library(scales)
library(patchwork)

###################################
# Data visuazlization with base R #
###################################

# simple scatterplot
data("cars")

plot(x= cars$speed, y = cars$dist)

plot(x = cars$speed,    # x axis 
     y = cars$dist,     # y axis
     pch = 20,          # optional: change point type
     col = "blue",      # optional: change color
     xlab = "speed",    # x axis label
     ylab = "distance") # y axis label
abline(lm(cars$dist~cars$speed), # add regression line
       col = "grey",                # color
       lty = 2)                     # line type (2 = dashed) 


# alternative: lowess curve
# lines(lowess(cars$speed, cars$dist)) # add lowess curve

title("Cars")           # add title


# simple lineplot ---------------------------------------------------------

data("WWWusage")
plot(1:100, WWWusage, type = "l", xlab="Time",
     main = "WWW usage per minute")

# we can also combine dotplot and lineplot:
plot(1:100, # x axis
     WWWusage, # y axis
     type = "p", # p = points
     xlab="Time", # x axis label
     pch = 20, # change point type
     col = rgb(0,0,1,.25), # RGB: red, green, blue + alpha (= transparency)
     ylim = c(0,250),  # y axis limits
     main = "WWW usage per minute", cex=2, cex.axis=3, cex.main=3, cex.lab=3)
points(x=20,y=150, col="red")
text(x=80,y=200, "hi", cex=3)
lines(1:100, WWWusage, 
      type = "l", # l = line
      col = rgb(.256,.256,.256,.5), # RGB code for grey (+ alpha)
      lwd = 4)

# we can also add a grid:
grid(nx = 0, # no grid lines on x axis
     ny = NULL, # NULL here means that grid aligns with tick marks
     # (see vignette: ?grid)
     lty = 1, col = "grey90")

# simple barplot ----------------------------------------------------------

# set a seed for replicability
set.seed(utf8ToInt("lotwinterschool"))

# get a sample
spl <- LETTERS[round(rnorm(200, mean = 5, sd = 2))]


# barplot
table(spl) %>%  # tabulate
  barplot()

# we can also sort the table by frequency
# before plotting:
table(spl) %>% sort(decreasing = T) %>% barplot()

# we can also add absolute numbers:
tbl <- table(spl) %>% sort(decreasing = T) # save table as object
(bp <- barplot(tbl))
text(bp,    # x axis values taken from bp object (i.e. the barplot itself)
     tbl/2, # y axis value taken from tbl, divided by two
     # to be in the middle of the bars
     tbl,   # labels taken from the table 
     cex = .8) # decrease font size a bit
title("My beautiful barplot") # add title

# x and y labels can also be added using the title() command:
title(xlab = "Letter", ylab = "Frequency")


# stacked barplot
# possible in base R but MUCH (!) easier in 
# ggplot2, hence we will deal with this later.



# boxplot -----------------------------------------------------------------

# sample data
set.seed(42)
d <- tibble(x = rnorm(100, mean = 5, sd = 1),
            y = rnorm(100, mean = 7, sd = 3))


boxplot(d)

# Interpretation: the box shows the interquartile
# range, i.e. the area in which the middle 50% of
# the data fall. The whiskers show 1.5 times the
# interquartile range; data that fall out of this
# range are presented as outliers (individual dots).
quantile(d$y)
IQR(d$y)

# beeswarm plot -----------------------------------------------------------

beeswarm(d,                # x axis
         pch = 20,         # y axis
         col = "darkblue", # color of the points
         method = "hex")   # methods for distributing the dots
# (chosen here to avoid overlaps
# that we see with the default method)
boxplot(d, add = T, col = rgb(.256, .256, .256, .2))




# exporting base R plots --------------------------------------------------

# base R plots can be exported using the png function:
# wrap the plot in
# png(...)
# PLOT COMMANDS
# dev.off()
png("exampleplot.png",
    width = 6.5, height = 5, 
    un = "in", 
    res = 300)
beeswarm(d,                
         pch = 20,         
         col = "darkblue", 
         method = "hex")
boxplot(d, add = T, col = rgb(.256, .256, .256, .2))
dev.off()



# plot grids --------------------------------------------------------------

# Multiple plots can be exported by setting up a 
# grid of n rows x n columns using par(mfrow = x,y)
png("exampleplot02.png", width = 11.5, height = 5, un = "in", res = 300)
par(mfrow = c(1,2)) # 1 row, two columns
beeswarm(d,                
         pch = 20,         
         col = "darkblue", 
         method = "hex")
boxplot(d, col = rgb(.256, .256, .256, .2))
dev.off()
par(mfrow = c(1,1)) # restore default

