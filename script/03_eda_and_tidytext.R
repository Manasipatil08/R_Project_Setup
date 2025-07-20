# Exploratory Data Analysis
# Purpose: Analyze and visualize word frequencies

# Load libraries (alphabetical order)
library(dplyr)    # For data manipulation
library(ggplot2)  # For plotting
library(here)     # For file paths
library(tidytext) # For text processing
library(wordcloud) # For word cloud visualization

# Load tokenized data
tokenized_feedback <- readr::read_csv(here::here("output", "tokenized_feedback.csv"))

# Calculate word frequencies
word_frequencies <- tokenized_feedback %>%
  count(word, sort = TRUE)

view(word_frequencies)

# Define list of unwanted words
unwanted_words <- c("ve", "s", "t", "m", "doesn", "fi", "wi", "don", "hasn")

# Filter out the unwanted words
cleaned_word_frequencies <- word_frequencies %>%
  filter(!word %in% unwanted_words)

# View the result
view(cleaned_word_frequencies)

# Create bar chart of top 20 words
top_words_plot <- cleaned_word_frequencies %>%
  slice_max(n, n = 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Frequent Words",
       x = "Word", y = "Count") +
  theme_minimal()

# Show plot
print(top_words_plot)

# Create word cloud
set.seed(1234)  # For reproducibility
png(here::here("output", "word_cloud.png"), width = 600, height = 400)
wordcloud(words = cleaned_word_frequencies$word, freq = word_frequencies$n, max.words = 100, colors = c("gray20", "gray80"))
dev.off()

# Save bar plot
ggsave(here::here("output", "top_words_plot.png"), top_words_plot, width = 6, height = 4)


#removing unwanted words from tokenized_feedback and rewriting the file in output folder
# Define list of unwanted words

# Filter out the unwanted words
cleaned_tokenized <- tokenized_feedback %>%
  filter(!word %in% unwanted_words)

view(cleaned_tokenized)
write_csv(cleaned_tokenized, here::here("output", "cleaned_tokenized.csv"))

