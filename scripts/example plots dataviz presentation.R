library(tidyverse)
library(wizard)
library(patchwork)
library(scales)


# generate skewed data ----------------------------------------------------

set.seed(1985)
a1 <- rnorm(200, mean = 20, sd = 2)
a2 <- rpois(200, 25)
a3 <- rnorm(200, mean = 20, sd = 2)^3
a3 <- round(a3/400)


a <- tibble(a1 = a1, a2 = a2, a3 = a3)
a <- pivot_longer(a, cols = 1:3)

p1 <- ggplot(a, aes(x = name, y = value)) + geom_bar(stat = "summary", 
                                               fun = "mean", fill = "blue")


p2 <- qbeeswarm(a, name, value, box = F)

p1 + p2
ggsave("ex1.png", width = 12, height = 6)




# generate categorical data -----------------------------------------------

no <- c(1:100)
country <- c(rep("Germany", 25), rep("USA", 21), rep("UK", 40), 
             rep("China", 10), rep("Switzerland", 2), rep("Kazakhstan", 1),
             rep("South Africa", 1))
decision <- c(rep("paper", 20), rep("poster", 3), rep("reject", 2),
              rep("paper", 15), rep("poster", 4), rep("reject", 2),
              rep("paper", 32), rep("poster", 4), rep("reject", 4),
              rep("paper", 3), rep("poster", 3), rep("reject", 4),
              rep("paper", 1), rep("reject", 1),
              rep("paper", 1), rep("poster", 1))

papers <- tibble(country = country, decision = decision)
papers$decision <- factor(papers$decision, levels = rev(c("reject", "poster", "paper")))

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
ggsave("paper_decsions_example.png", width = 9, height = 4)  

ggplot(papers, aes(group = decision, x = country, fill = decision)) + 
  geom_bar(position = "fill") + scale_fill_viridis_d(begin = .8, end = .1) +
  ggtitle("Conference paper decisions by country") +
  # geom_text(stat = "count", aes(label = after_stat(count), color = decision), position = position_fill(vjust = .5)) +
  scale_color_manual(values = c("black", "white", "white")) +
  guides(color = "none") + 
  guides(fill = guide_legend(title = "Decision")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  xlab("Country") + ylab("Relative Frequency") +
  scale_y_continuous(labels = percent)
ggsave("paper_decsions_example0.png", width = 9, height = 4)  


