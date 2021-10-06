library("tidyverse")
library("vroom")
library("stringr")
library("dplyr")
library("tidytext")
library("ggplot2")
library("viridis")

setwd("~/Downloads/")

book_words <- read.csv("Corpus-tf-idf(2).csv", stringsAsFactors = FALSE)

book_words$token <- as.character(book_words$token)

book_words <- book_words %>%
  bind_tf_idf(token, periode, n)

book_words

library(ggstance)
library(ggthemes)
plot_austen <- book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))
ggplot(plot_austen[1:20,], aes(tf_idf, word, fill = book, alpha = tf_idf)) +
  geom_barh(stat = "identity") +
  labs(title = "Highest tf-idf words in Jane Austen's Novels",
       y = NULL, x = "tf-idf") +
  theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1), guide = FALSE) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_viridis(end = 0.85, discrete=TRUE) +
  theme(legend.title=element_blank()) +
  theme(legend.justification=c(1,0), legend.position=c(1,0))


plot_austen <- plot_austen %>% group_by(book) %>% top_n(15) %>% ungroup
ggplot(plot_austen, aes(tf_idf, word, fill = book, alpha = tf_idf)) +
  geom_barh(stat = "identity", show.legend = FALSE) +
  labs(title = "Highest tf-idf words in Jane Austen's Novels",
       y = NULL, x = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_viridis(end = 0.85, discrete=TRUE) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic"))