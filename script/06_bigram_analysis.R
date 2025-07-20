# Bigram Analysis
# Purpose: Analyze and visualize two-word phrases

# Load libraries (alphabetical order)
library(dplyr)    # For data manipulation
library(ggraph)   # For network plots
library(here)     # For file paths
library(igraph)   # For network analysis
library(tidytext) # For text processing
library(tidyr)    # For data tidying

# Load cleaned data
cleaned_feedback <- readr::read_csv(here::here("output", "cleaned_feedback.csv"))
view(cleaned_feedback)

# Extract bigrams
bigrams <- cleaned_feedback %>%
  unnest_tokens(bigram, Feedback, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

View(bigrams)

# Create bigram network
bigram_graph <- bigrams %>%
  filter(n > 5) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  graph_from_data_frame()

# Plot network
bigram_plot <- ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 4) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Bigram Network from NHS Feedback")
print(bigram_plot)

# Save plot
ggsave(here::here("output", "bigram_plot.png"), bigram_plot, width = 8, height = 6)
