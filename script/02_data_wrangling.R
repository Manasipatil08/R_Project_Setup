# Data Wrangling
# Purpose: Tokenize feedback text and remove stop words

# Load libraries (alphabetical order)
library(dplyr)    # For data manipulation
library(here)     # For file paths
library(tidytext) # For text processing
library(tidyr)    # For data tidying

# Load cleaned data
cleaned_feedback <- readr::read_csv(here::here("output", "cleaned_feedback.csv"))

# Add ID column and tokenize
tokenized_feedback <- cleaned_feedback %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, Feedback)

# Remove stop words
cleaned_tokens <- tokenized_feedback %>%
  anti_join(get_stopwords(), by = "word")

# Save tokenized data
write_csv(cleaned_tokens, here::here("output", "tokenized_feedback.csv"))
View(tokenized_feedback)
