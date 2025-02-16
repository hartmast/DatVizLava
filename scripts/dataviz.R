####################################
# Data visualization with ggplot2  #
####################################



# libraries ---------------------------------------------------------------

library(tidyverse)
library(beeswarm)
library(ggbeeswarm)
library(scales)
library(patchwork)

# load example dataset
data(cars) # see ?cars for info


# simple scatterplot ------------------------------------------------------

ggplot(cars,          # the dataset
      aes(            # IMPORTANT: the aesthetic mappings, i.e. the
          x = speed,  # mappings between data and visual properties,
          y = dist         # have to be wrapped in aes()!
      )) +
  geom_point()       # a geom object that describes
                     # how to render the observation.


# The plot can be saved as an object and then be
# customized further by adding more geoms as well as
# so-called SCALES, which can be used to modify the
# appearance of each geom layer.

p1 <- ggplot(cars,          
       aes(           
         x = speed, 
         y = dist   
       )) +
  geom_point() 

# add a regression line:
(p1 <- p1 + 
  geom_smooth(method = "lm") )

# change appearance of x axis:
# (just for illustrative purposes, doesn't make sense here)
p1 + scale_x_continuous(breaks = seq(0,25,7))

# log-transform y axis (also just for demonstraion):
p1 + scale_y_continuous(trans = "log2")

# with the scale_ arguments, we can change the
# aesthetic mappings specified via aes(). Why does this
# not work?
p1 + scale_color_manual(values = "red")

# Because we haven't specified color in the aesthetic
# mappings. We could do so:
(p1 <- p1 + geom_point(aes(color = "bla")))
(p1 <- p1 + scale_color_manual(values = "red"))

# remove legend:
(p1 <- p1 + guides(col = "none"))

# we can also customize x and y labels by adding
# the respective layers
(p1 <- p1 + xlab("Speed") + ylab("Dist"))

# ... and add a title:
(p1 <- (p1 + ggtitle("Cars")))

# ... and customize the title, e.g.
# to make it boldface and center it:
(p1 <- p1 + theme(plot.title = element_text(face = "bold", 
                                     hjust = 0.5)))


# we can also change the theme:
p1 + theme_bw()
p1 + theme_minimal()

# or remove or cutomize the gridlines:
p1 + theme(panel.grid.minor = element_blank())
p1 + theme(panel.grid.major = element_blank())
p1 + theme(panel.grid = element_blank())
p1 + theme(panel.grid.major.x =  element_blank(),
           panel.grid.minor.x = element_blank())

# simple lineplot ---------------------------------------------------------

# example dataset: WWWusage
data("WWWusage")

# ggplot only accepts dataframes as input,
# so we first have to build a dataframe:
wwwusage <- tibble(time = 1:100,
       WWWusage = WWWusage)

# now try building the plot yourself!
# Hint: the relevant geom is geom_line().
ggplot(wwwusage, aes(x = time, y = WWWusage)) +
  geom_point() +
  geom_line(lwd=1.5)




# barplot -----------------------------------------------------------------

# fake dataset:
# set a seed for replicability
set.seed(utf8ToInt("ascona"))

# get a sample
spl <- LETTERS[round(rnorm(200, mean = 7, sd = 2))]

# again, remember that ggplot usually only accepts
# dataframes as input. One thing we can do is just
# creating a one-column dataframe with our spl data
# from above:
tibble(x = spl) %>% 
  ggplot(aes(x = x)) + 
  geom_bar() # barplot

# alternatively, we can use tabulated data
# with geom_col():
tbl <- table(spl) %>% sort(decreasing = T)
tbl <- as_tibble(tbl)

# now we can use this as input for geom_col:
colnames(tbl)
ggplot(tbl, aes(x = spl,  # x axis
                y = n)) + # y axis
geom_col() # same result as before.


# stacked barplot

# add a group variable to spl: letters from A-E vs. letters
# from F-K
spl01 <- tibble(letters = spl)
spl01$group <- ifelse(spl01$letters %in% LETTERS[1:5], "A-E", "F-K")

# plot:
spl01 %>% 
  ggplot(aes(x = group, fill = letters)) + 
  geom_bar() +
  scale_fill_viridis_d() # colorblind-friendly palette

# we can also add labels:
spl01 %>% 
  ggplot(aes(x = group, fill = letters)) + 
  geom_bar() +
  scale_fill_viridis_d() +
  geom_text(stat="count", # necessary to compute y axis position
                          # (horribly counterintuitive, I know!!)
            aes(label = letters), 
            position = position_stack(vjust = .5),
            col = c(rep("white", 5), rep("black", 6))) + # specify color of text
  guides(fill = "none") # remove legend
  

# using proportions:
(p <-spl01 %>% 
  ggplot(aes(x = group, fill = letters)) + 
  geom_bar(position = "fill") +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = percent) ) # requires package "scales"!

# add labels:
p + geom_text(stat="count", 
              aes(label = after_stat(count)), 
              position = position_fill(vjust = .5))


# we can also give the labels different colors
# by exploiting the color aesthetic:

p + geom_text(stat="count", 
              aes(label = after_stat(count),
                  color = letters), 
              position = position_fill(vjust = .5)) +
  scale_color_manual(values = c(rep("white", 4), rep("black", 7))) +
  guides(color = "none")
  


# beeswarm and boxplots ---------------------------------------------------

# create a dataframe with random data
set.seed(utf8ToInt("ascona"))
d <- data.frame(x = 1:10,
                y = sample(1:100, 10))


head(d)

# first try:
ggplot(d, aes(x = x,
              y = y)) + geom_beeswarm()

# Doesn't look quite right. Why not?
# Because x and y are different groups!
# We have to transform the data first:
# One column for the values, one for the groups.
# This can be done using pivot_longer:

d <- pivot_longer(d, cols = c("x", "y"))
d

# replace the names:
colnames(d) <- c("group", "value")


# now let's try again:
ggplot(d, aes(x = group, y = value)) +
  geom_beeswarm()

# looks much better!
# again, we can overlay other plots:

(p <- ggplot(d, aes(x = group, y = value)) +
    geom_beeswarm(col = "blue"))

# add boxplot:
p + geom_boxplot(fill = "grey", alpha = .3)

# or a violin plot:
p + geom_violin(alpha = .3)



# exporting ggplots -------------------------------------------------------

# We can use ggsave to save ggplots - uncomment ggsave below to use:
p + geom_violin(alpha = .3)
# ggsave("exampleggplot.png")



# multiple ggplots --------------------------------------------------------

# We can use the excellent patchwork package
# to group ggplots:

p | p1
# ggsave("exampleggplot2.png", width = 12, height = 5.5)


# It often helps to use bigger axis labels
# because the default ones tend to be hard to read
# in presentations or publications.
# In fact, I have saved a code snippet in RStudio
# (see Tools > Global Options > Code > Code Snippets)
# to have it always available:

p + theme(text = element_text(size = 18),
          title = element_text(size = 18))

# Note that you have to use it *after* changing
# the global theme. If you switch to e.g. theme_bw or
# theme_minimal afterwards, your changes to the theme
# will be overridden:

p + theme(text = element_text(size = 18),
          title = element_text(size = 18)) +
  theme_bw() # font size change is lost

# So the order in which you use the commands matters!




# extended example --------------------------------------------------------

# fake data: conference acceptance

# generate data
country <- c(rep("Germany", 25), rep("USA", 21), rep("UK", 40), 
             rep("China", 10), rep("Switzerland", 2), rep("Kazakhstan", 1),
             rep("South Africa", 1))
decision <- c(rep("paper", 20), rep("poster", 3), rep("reject", 2),
              rep("paper", 15), rep("poster", 4), rep("reject", 2),
              rep("paper", 32), rep("poster", 4), rep("reject", 4),
              rep("paper", 3), rep("poster", 3), rep("reject", 4),
              rep("paper", 1), rep("reject", 1),
              rep("paper", 1), rep("poster", 1))

# combine data in a dataframe
papers <- tibble(country = country, decision = decision)
papers$decision <- factor(papers$decision, levels = rev(c("reject", "poster", "paper")))


# plot data
ggplot(papers, aes(group = decision, x = country, fill = decision)) + 
  geom_bar(position = "fill") + scale_fill_viridis_d(begin = .8, end = .1) +
  ggtitle("Conference paper decisions by country") +
  geom_text(stat = "count", aes(label = after_stat(count), color = decision), position = position_fill(vjust = .5)) +
  scale_color_manual(values = c("black", "white", "white")) +
  guides(color = "none") + 
  guides(fill = guide_legend(title = "Decision")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  xlab("Country") + ylab("Relative Frequency") +
  scale_y_continuous(labels = percent)

# plot data in descending order of acceptance rate -------

# add column with acceptance rate - see ?count for more info
papers %>% mutate(decision_binary = ifelse(decision %in% c("paper", "poster"), "accept", "reject")) %>%
  group_by(country) %>% 
  add_count(decision_binary) %>%
  add_count(name = "n_all") %>%
  mutate(rel = n/n_all) %>% 
  ggplot(aes(group = decision, x = fct_reorder(country, rel), fill = decision)) + 
  geom_bar(position = "fill") + scale_fill_viridis_d(begin = .8, end = .1) +
  ggtitle("Conference paper decisions by country") +
  geom_text(stat = "count", aes(label = after_stat(count), color = decision), position = position_fill(vjust = .5)) +
  scale_color_manual(values = c("black", "white", "white")) +
  guides(color = "none") + 
  guides(fill = guide_legend(title = "Decision")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  xlab("Country") + ylab("Relative Frequency") +
  scale_y_continuous(labels = percent)



# Nettle (1998) data ------------------------------------------------------

nettle <- read_csv("../data/language_diversity.csv")


# pivot wider
nettle <- nettle %>% 
  pivot_wider(names_from = Measurement, values_from = Value)


# plot MGS and number of languages
ggplot(nettle, aes(x = MGS, y = Langs)) +
  geom_point() +
  # geom_smooth() +
   facet_wrap(~Continent, ncol = 4)



