library(tidyverse)
library(ggbeeswarm)
library(scales)

set.seed(utf8ToInt("Ascona"))
d <- tibble(x = sample(letters, 10),
       y = sample(1:100, 10),
       group = sample(c("A", "B"), 10, 
                      replace = T))

d$group <- factor(d$group)
str(d)

(p <- ggplot(d, aes(x = fct_infreq(x, y), y = y)) +
  geom_point() +
  xlab("Participant") + 
  ylab("Reaction time") +
  facet_wrap(~group, scales = "free_x"))

p + theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.title = element_text(size = 18)) +
  theme(axis.text = element_text(size = 18)) +
  theme(axis.title = element_text(size = 18)) +
  theme(strip.text = element_text(size = 18)) 


ggplot(d, aes(x = group, y = y)) +
  geom_beeswarm() +
  geom_boxplot(alpha = .2) +
  theme_minimal() +
  theme(panel.grid = element_blank())


# read Nettle 1999 data
nettle <- read_csv("../data/nettle_1999_climate.csv")


# visualize Nettle data
nettle$Country250 <- ifelse(nettle$Langs>250, nettle$Country, "")
nettle$above250 <- nettle$Langs>250

ggplot(nettle, aes(x = MGS, y = Langs,
                   label = Country250)) +
  geom_point(aes(color = above250)) +
  geom_text() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("black", "white")) +
  scale_x_continuous(breaks = seq(0, 15, 5)) +
  guides(color = "none") +
  theme_bw()



# barplot -----------------------------------------------------------------


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

ggplot(papers, aes(x = country, fill = decision)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  geom_text(stat = "count", 
            aes(label = after_stat(count),
                col = decision),
            position = position_fill(vjust = .5)) +
  scale_fill_viridis_d(begin = 1, end = 0) +
  scale_color_manual(values = c("black", "black", "white")) +
  guides(color = "none") +
  guides(fill = guide_legend(title = "Decision")) +
  theme(axis.text.x = element_text(angle=45, hjust=.9, size=12))
  












