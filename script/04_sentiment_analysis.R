# Sentiment Analysis
# Purpose: Analyze feedback sentiment using Bing, AFINN, and NRC lexicons

# Load libraries (alphabetical order)
library(dplyr)    # For data manipulation
library(ggplot2)  # For plotting
library(here)     # For file paths
library(tidytext) # For text processing
library(tidyr)    # For data tidying

# Load tokenized data
cleaned_tokenized <- readr::read_csv(here::here("output", "cleaned_tokenized.csv"))

# Bing: Count positive/negative words
bing_sentiment <- cleaned_tokenized %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment, sort = TRUE)

# Plot Bing sentiment
bing_plot <- ggplot(bing_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Counts (Bing Lexicon)",
       x = "Sentiment", y = "Count") +
  theme_minimal()
print(bing_plot)

# AFINN: Calculate sentiment scores
afinn_sentiment <- cleaned_tokenized %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarise(score = sum(value, na.rm = TRUE)) %>%
  mutate(lexicon = "AFINN")

# Plot AFINN scores
afinn_plot <- ggplot(afinn_sentiment, aes(x = score)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Sentiment Scores (AFINN)",
       x = "Sentiment Score", y = "Count") +
  theme_minimal()
print(afinn_plot)

# NRC: Count emotions
nrc_sentiment <- cleaned_tokenized %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  count(sentiment, sort = TRUE)

# Plot NRC emotions
nrc_plot <- ggplot(nrc_sentiment, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Emotion Distribution (NRC Lexicon)",
       x = "Emotion", y = "Count") +
  theme_minimal()
print(nrc_plot)

# Combine sentiment scores by ID
bing_scores <- cleaned_tokenized %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(score = positive - negative, lexicon = "Bing") %>%
  select(id, score, lexicon)

nrc_scores <- cleaned_tokenized %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(score = positive - negative, lexicon = "NRC") %>%
  select(id, score, lexicon)

combined_scores <- bind_rows(afinn_sentiment, bing_scores, nrc_scores)
# Plot combined scores for first 20 IDs
combined_plot <- combined_scores %>%
  filter(id <= 20) %>%
  ggplot(aes(x = id, y = score, fill = lexicon)) +
  geom_col(position = "dodge") +
  labs(title = "Sentiment Scores by Feedback ID",
       x = "Feedback ID", y = "Score", fill = "Lexicon") +
  theme_minimal()
print(combined_plot)

# Save plots and data
ggsave(here::here("output", "bing_plot.png"), bing_plot, width = 6, height = 4)
ggsave(here::here("output", "afinn_plot.png"), afinn_plot, width = 6, height = 4)
ggsave(here::here("output", "nrc_plot.png"), nrc_plot, width = 6, height = 4)
ggsave(here::here("output", "combined_plot.png"), combined_plot, width = 6, height = 4)
write_csv(combined_scores, here::here("output", "sentiment_scores.csv"))

