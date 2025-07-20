# Topic Modeling and TF-IDF
# Purpose: Identify topics and key words in feedback

# Load libraries (alphabetical order)
library(dplyr)      # For data manipulation
library(ggplot2)    # For plotting
library(here)       # For file paths
library(tidytext)   # For text processing
library(topicmodels) # For LDA

# Load tokenized data
cleaned_tokenized <- readr::read_csv(here::here("output", "cleaned_tokenized.csv"))

# TF-IDF: Identify important words
tf_idf <- cleaned_tokenized %>%
  count(id, word, sort = TRUE) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))
view(tf_idf)
# Save TF-IDF results
write_csv(tf_idf, here::here("output", "tf_idf_feedback.csv"))

# Plot top TF-IDF words for a specific ID (e.g., ID 15)
tf_idf_plot <- tf_idf %>%
  filter(id == 15) %>%
  slice_max(tf_idf, n = 10) %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(x = word, y = tf_idf)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top TF-IDF Words for Feedback ID 15",
       x = "Word", y = "TF-IDF Score") +
  theme_minimal()
print(tf_idf_plot)

# Create document-term matrix for LDA
dtm_feedback <- cleaned_tokenized %>%
  count(id, word) %>%
  cast_dtm(document = id, term = word, value = n)

# Fit LDA model (3 topics)
lda_model <- LDA(dtm_feedback, k = 3, control = list(seed = 1234))

# Get top words per topic
top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

# Plot top words
topic_plot <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Words per Topic (LDA)",
       x = "Word", y = "Probability") +
  theme_minimal()
print(topic_plot)

# Save plots
ggsave(here::here("output", "tf_idf_plot.png"), tf_idf_plot, width = 6, height = 4)
ggsave(here::here("output", "topic_plot.png"), topic_plot, width = 8, height = 6)

