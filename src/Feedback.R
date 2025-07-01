Feedback <- read.csv("C:/Users/Manasi/Documents/Tidy Text/synthetic_app_feedback.csv")

write.csv(Feedback,"C:/Users/Manasi/Documents/Tidy Text/synthetic_app_feedback.csv")

View(Feedback)


distinct(Feedback)


#--------Working with tidy text -----------------#

library(tidytext)
library(dplyr)

# Unnest tokens and save to a new object #splitted the sentences in word in one column
Feedback_tokens <- Feedback %>%
  unnest_tokens(word, Feedback)

# Now view the tokenized data
View(Feedback_tokens)


# View the structure
str(Feedback_tokens)

# Summary stats
summary(Feedback_tokens)

# Frequency like janeaustenr
Feedback_word_counts <- Feedback_tokens %>%
  count(word, sort = TRUE)

# View most common words
head(Feedback_word_counts, 1000)

#Feedback_data_clean

Feedback_data_clean <- Feedback_tokens %>%
  anti_join(get_stopwords(), by = "word")

View(Feedback_data_clean)

#before removing stop words 6,714 entries Feedback_tokens after --> 4,001 Feedback_data_clean


#-----------Finding High Frequency of the words---------------#
Feedback_word_freq <- Feedback_data_clean %>%
  count(word, sort = TRUE)

View(Feedback_word_freq)


# Step 4: View the top words
head(Feedback_word_freq, 20)  # top 20 most frequent words


library(ggplot2)

# Plot top 20 words
Feedback_word_freq %>%
  top_n(30, n) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Frequent Words (After Removing Stop Words)",
       x = "Word", y = "Frequency") +
  theme_minimal()


#----------------Bing-----------------# Gives positive or negative sentiment
library(tidytext)
library(dplyr)

# Step 1: Join with the Bing sentiment lexicon  
Feedback_sentiment_data <- Feedback_data_clean %>%
  inner_join(get_sentiments("bing"), by = "word")


head(Feedback_sentiment_data)


Feedback_sentiment_summary <- Feedback_sentiment_data %>%
  count(sentiment, sort = TRUE)

print(Feedback_sentiment_summary)


library(ggplot2)

ggplot(Feedback_sentiment_summary, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Counts from NHS Feedback",
       x = "Sentiment", y = "Count") +
  theme_minimal()

#---------------Afinn--------------------- Show the score -5 to +5
# Step 1: Load AFINN lexicon and join with cleaned feedback tokens
Feedback_afinn_sentiment <- Feedback_data_clean %>%
  inner_join(get_sentiments("afinn"), by = "word")

View(Feedback_afinn_sentiment)
# Step 2: View a few rows to understand the structure
head(Feedback_afinn_sentiment)

# Step 3: Summarize the total sentiment score
Feedback_total_afinn_score <- sum(Feedback_afinn_sentiment$value)
print(paste("Total AFINN sentiment score:", Feedback_total_afinn_score))

# Step 5: Visualize distribution of sentiment scores
library(ggplot2)

ggplot(Feedback_afinn_sentiment, aes(x = Feedback_total_afinn_score)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Sentiment Scores (AFINN)",
       x = "Sentiment Score", y = "Count of Feedbacks") +
  theme_minimal()


#-----------------------NRC-----   count on sentiments for example sentiment ,negative,trust etc
Feedback_nrc_sentiment <- Feedback_data_clean %>%
  inner_join(get_sentiments("nrc"), by = "word")

# View first few words and emotions
head(Feedback_nrc_sentiment)

# Count emotions
Feedback_nrc_summary <- Feedback_nrc_sentiment %>%
  count(sentiment, sort = TRUE)

print(Feedback_nrc_summary)

library(ggplot2)

# Filter out only emotion categories (not just positive/negative)
Feedback_nrc_summary %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Emotion Distribution in Feedback (NRC)",
       x = "Emotion", y = "Count") +
  theme_minimal()



#Just to check which words falls under negative sentiments

Feedback_nrc_data <- Feedback_data_clean %>%
  inner_join(get_sentiments("nrc"), by = "word")

# Filter for negative sentiment
negative_words <- Feedback_nrc_data %>%
  filter(sentiment == "negative")
# View the first few rows
head(negative_words)

#filter for fear sentiment
fear_words <- Feedback_nrc_data %>%
  filter(sentiment == "fear")


# View the first few rows
head(fear_words)

# Count unique words associated with negative sentiment
num_unique_negative_words <- negative_words %>%
  distinct(word) %>%
  count()

print(num_unique_negative_words)
head(num_unique_negative_words)

#Comparing three scores together by doing inner join just using common columns from all the lexicons

Feedback_afinn_sentiment_C <- Feedback_data_clean %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(Gender) %>%
  summarise(score = sum(value, na.rm = TRUE)) %>%
  mutate(lexicon = "AFINN")

# Bing sentiment score (positive - negative)
Feedback_sentiment_data_C <- Feedback_data_clean %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(Gender, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(score = positive - negative,
         lexicon = "Bing") %>%
  select(Gender, score, lexicon)

# NRC sentiment score (positive - negative)
Feedback_nrc_sentiment_C <- Feedback_data_clean %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(Gender, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(score = positive - negative,
         lexicon = "NRC") %>%
  select(Gender, score, lexicon)


# Combine all sentiment scores
all_scores <- bind_rows(Feedback_afinn_sentiment_C, Feedback_sentiment_data_C, Feedback_nrc_sentiment_C)
View(all_scores)


View(Feedback_afinn_sentiment)
View(Feedback_sentiment_data)
View(Feedback_nrc_sentiment)

#Filter to first 20 feedbacks if too many
all_scores_C <- all_scores %>% filter( Gender <= 20)

library(ggplot2)

ggplot(all_scores, aes(x = Gender, y = score, fill = lexicon)) +
  geom_col(position = "dodge") +
  labs(title = "Sentiment Scores by Gender and Lexicon",
       x = "Gender",
       y = "Sentiment Score",
       fill = "Lexicon") +
  theme_minimal()

#----------------------------------------------------
View(Feedback_data_clean)

#--------Trying by adding new ID column to do the analysis---
Feedback_data_clean_with_ID <- Feedback_data_clean %>%
  mutate(ID = row_number())
View(Feedback_data_clean_with_ID)

#---              -----------               -------------------
Feedback_afinn_sentiment_C_id <- Feedback_data_clean_with_ID %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(ID) %>%
  summarise(score = sum(value, na.rm = TRUE)) %>%
  mutate(lexicon = "AFINN")

# Bing sentiment score (positive - negative)
Feedback_sentiment_data_C_id <- Feedback_data_clean_with_ID %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(ID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(score = positive - negative,
         lexicon = "Bing") %>%
  select(ID, score, lexicon)

# NRC sentiment score (positive - negative)
Feedback_nrc_sentiment_C_id <- Feedback_data_clean_with_ID %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(ID, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(score = positive - negative,
         lexicon = "NRC") %>%
  select(ID, score, lexicon)


# Combine all sentiment scores
all_scores_ID <- bind_rows(Feedback_afinn_sentiment_C_id, Feedback_sentiment_data_C_id, Feedback_nrc_sentiment_C_id)
View(all_scores_ID)


View(Feedback_afinn_sentiment_C_id)
View(Feedback_sentiment_data_C_id)
View(Feedback_nrc_sentiment_C_id)

#Filter to first 20 feedbacks if too many
all_scores_C_id_1 <- all_scores_ID %>% filter( ID <= 20)

library(ggplot2)

ggplot(all_scores_C_id_1, aes(x = ID, y = score, fill = lexicon)) +
  geom_col(position = "dodge") +
  labs(title = "Sentiment Scores by Gender and Lexicon",
       x = "Gender",
       y = "Sentiment Score",
       fill = "Lexicon") +
  theme_minimal()

#------Word Cloud-------------------------------------
library(wordcloud)

Feedback_tokens %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)

Feedback_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

#-----------------------------------------------------
#-------------Term Frequency - How often the word is used in the document-----
library(tidytext)
library(dplyr)
rm(Feedback_term_freq)

Feedback_term_freq <- Feedback_tokens %>%
  count(word, sort = TRUE)

head(Feedback_term_freq, 60)  # Top 60 most frequent words


View(Feedback_term_freq)
View(Feedback_tokens)

#--------Trying by adding new ID column to do the analysis---
Feedback_tokens_ID <- Feedback_tokens %>%
  mutate(ID = row_number())
View(Feedback_tokens_ID)

Feedback_term_freq_by_id <- Feedback_tokens_ID %>%
  count(ID, word, sort = TRUE)

head(Feedback_term_freq_by_id, 20)


#------------------------------------------------------------

library(ggplot2)

# Top 20 overall terms
Feedback_top_terms <- Feedback_term_freq_by_id %>%
  top_n(20, n) %>%
  mutate(word = reorder(word, n))

ggplot(Feedback_top_terms, aes(x = word, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Frequent words",
       x = "Word", y = "Frequency") +
  theme_minimal(100)

View(Feedback_top_terms)

#---Not grouped by ID
# Step 1: Get total word frequency (ignoring ID)
Feedback_top_terms <- Feedback_term_freq_by_id %>%
  group_by(word) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  slice_max(n, n = 50) %>%
  mutate(word = reorder(word, n))

# Step 2: Plot
ggplot(Feedback_top_terms, aes(x = word, y = n)) +
  geom_col(fill = "steelblue") +
  
  coord_flip() +
  labs(title = "Top 20 Frequent Words",
       x = "Word", y = "Frequency") +
  theme_minimal()
#----------------------------#


#-----------TF_IDF-----------------#
# Step 1: Compute TF-IDF 
# Term frequency how often a word appears in a single document 

#IDF (Inverse Document Frequency) = how rare that word is across all documents 

Feed_back_tfidf <- Feedback_tokens_ID %>%
  count(ID, word, sort = TRUE) %>%  # X is the document ID
  bind_tf_idf(word, ID, n) %>%
  arrange(desc(tf_idf))

# Step 2: View top TF-IDF words
head(Feed_back_tfidf, 50)

#-------------------------------------
#-------------Top words per feedback--------------

Feedback_top_tfidf_words <- Feed_back_tfidf %>%
  group_by(ID) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup()

head(Feedback_top_tfidf_words)


#----------
# For a specific feedback ID (example: ID 15)
Feed_back_tfidf %>%
  filter(ID == 350) %>%
  slice_max(tf_idf, n = 10) %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(x = word, y = tf_idf)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top TF-IDF words for Feedback ID 15",
       x = "Word", y = "TF-IDF Score") +
  theme_minimal()

#topic modelling used to combine similar topic related words together

library(topicmodels)
library(tidytext)
library(tm)

# 1. Create a Document-Term Matrix
dtm <- Feedback_tokens_ID %>%
  count(ID, word) %>%
  cast_dtm(document = ID, term = word, value = n)

View(dtm)

# 2. Fit the LDA Model (3 topics here, change k as needed)
lda_model <- LDA(dtm, k = 3, control = list(seed = 1234))
View(lda_model)
# 3. Tidy the model output
topics <- tidy(lda_model, matrix = "beta")  # beta = prob of word in topic
View(topics)
# 4. Get top 100 terms per topic
top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

View(top_terms)



#---------------
# 5. Visualize top words in each topic
library(ggplot2)
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Words in Each Topic", x = "Word", y = "Probability")
#---------Bigrams and Trigrams-----------------
#instead of looking at single word we look at 2 words or triplets to better understanding
View(Feedback)
# 1. Extract Bigrams
Feedback_bigrams <- Feedback %>%
  unnest_tokens(bigram, Feedback, token = "ngrams", n = 2)
View(Feedback_bigrams)

# 2. Split bigrams into word1 and word2
Feedback_bigrams_separated <- Feedback_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

View("Feedback_bigrams_separated")

# 3. Remove stop words(you can see reduced rows)
Feedback_bigrams_filtered <- Feedback_bigrams_separated %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word)

View(Feedback_bigrams_filtered)

# 4. Count cleaned bigrams
Feedback_bigram_counts <- Feedback_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

# 5. View top bigrams
head(Feedback_bigram_counts, 20)


#------------------------Visualization for Bigrams--------------------------------
library(igraph)
library(ggraph)

# Create a graph object
Feedback_bigram_graph <- Feedback_bigram_counts %>%
  filter(n > 5) %>%  # Keep only frequently occurring bigrams
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  graph_from_data_frame()

# Plot the network
ggraph(Feedback_bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 4) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  labs(title = "Bigram Network from NHS Feedback")

#-------------Compare word usage by group ie :gender and clinical

View(Feedback)
Feedback_words <- Feedback %>%
  unnest_tokens(word, Feedback) %>%
  anti_join(stop_words)

# Count word usage by Gender
word_gender <- Feedback_words %>%
  count(word, Gender) %>%
  group_by(word) %>%
  filter(sum(n) > 10) %>%  # Words used at least 10 times
  spread(Gender, n, fill = 0) %>%
  mutate(log_ratio = log2((M + 1) / (F + 1)))
View(word_gender)

# Visualize top gendered words
word_gender %>%
  arrange(desc(abs(log_ratio))) %>%
  top_n(10, abs(log_ratio)) %>%
  ggplot(aes(x = reorder(word, log_ratio), y = log_ratio, fill = log_ratio > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Words Associated with Each Gender",
       x = "Word", y = "Log Ratio (M / F)") +
  scale_fill_manual(values = c("pink", "skyblue"))

#-----------Trying for Clinical------------
# Count word usage by Gender
word_clinical <- Feedback_words %>%
  count(word, Clinical.Y.N) %>%
  group_by(word) %>%
  filter(sum(n) > 10) %>%  # Words used at least 10 times
  spread(Clinical.Y.N, n, fill = 0) %>%
  mutate(log_ratio = log2((Y + 1) / (N + 1)))
View(word_clinical)

# Visualize top gendered words
word_clinical %>%
  arrange(desc(abs(log_ratio))) %>%
  top_n(10, abs(log_ratio)) %>%
  ggplot(aes(x = reorder(word, log_ratio), y = log_ratio, fill = log_ratio > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Words Associated with Each Gender",
       x = "Word", y = "Log Ratio (Y/ N)") +
  scale_fill_manual(values = c("pink", "skyblue"))


#---------Topic Modeling with LDA-----------------#
library(tidytext)
library(topicmodels)
library(dplyr)

# Create Document-Term Matrix
dtm_1 <- Feedback_tokens_ID %>%
  count(ID, word) %>%
  cast_dtm(document = ID, term = word, value = n)
View(dtm_1)


# Fit the LDA model with k = 3 topics
lda_model_1 <- LDA(dtm, k = 3, control = list(seed = 1234))
View(lda_model_1)

# Tidy the LDA output
library(tidytext)

topics_1 <- tidy(lda_model, matrix = "beta")

# Get top 10 terms per topic
top_terms_1 <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

View(top_terms_1)


library(ggplot2)

top_terms_1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Words per Topic (LDA)",
       x = "Word", y = "Probability")


# Document-topic probabilities
document_topics_1 <- tidy(lda_model, matrix = "gamma")

# For each feedback, find the most probable topic
top_feedback_topics_1 <- document_topics_1 %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

View(top_feedback_topics_1)


#----------Formalise testing for afinn--------
# Merge sentiment scores (AFINN) with original Feedback data to get Clinical label
afinn_scores_with_clinical <- Feedback_afinn_sentiment_C_id %>%
  left_join(Feedback %>% mutate(ID = row_number()) %>% select(ID, Clinical.Y.N), by = "ID")

# View to confirm
View(afinn_scores_with_clinical)


str(afinn_scores_with_clinical)
table(afinn_scores_with_clinical$Clinical.Y.N)


#Checking normality for each clinical group
shapiro.test(afinn_scores_with_clinical$score[afinn_scores_with_clinical$Clinical.Y.N == "Y"])
shapiro.test(afinn_scores_with_clinical$score[afinn_scores_with_clinical$Clinical.Y.N == "N"])


library(ggplot2)

ggplot(afinn_scores_with_clinical, aes(x = Clinical.Y.N, y = score, fill = Clinical.Y.N)) +
  geom_boxplot() +
  labs(title = "Sentiment Score by Clinical Review Group (AFINN)",
       x = "Clinical Review (Y/N)",
       y = "AFINN Sentiment Score") +
  theme_minimal()


#QQ-plot
ggplot(afinn_scores_with_clinical, aes(sample = score)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Clinical.Y.N) +
  labs(title = "Q-Q Plot of AFINN Sentiment Score by Clinical Group",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()


#output : data:  afinn_scores_with_clinical$score[afinn_scores_with_clinical$Clinical.Y.N == "Y"]
#W = 0.82045, p-value = 0.0006486 -------------------> For Y

#data:  afinn_scores_with_clinical$score[afinn_scores_with_clinical$Clinical.Y.N == "N"]
#W = 0.81543, p-value = 0.01502 -----------------------> For N


#Since both groups fail the normality test (p < 0.05), you must use a non-parametric test:
wilcox.test(score ~ Clinical.Y.N, data = afinn_scores_with_clinical)


wilcox.test(score ~ Clinical.Y.N, data = afinn_scores_with_clinical, exact = FALSE)
#There is no significant difference in AFINN sentiment scores between Clinical = Y and Clinical = N groups as p-value < 0.05
#W = 119.5, p-value = 0.661

#--------------------------For Bing----------------------------------------
# Merge sentiment scores (AFINN) with original Feedback data to get Clinical label
bing_scores_with_clinical <- Feedback_sentiment_data_C_id %>%
  left_join(Feedback %>% mutate(ID = row_number()) %>% select(ID, Clinical.Y.N), by = "ID")

# View to confirm
View(bing_scores_with_clinical)


str(bing_scores_with_clinical)
table(bing_scores_with_clinical)


#Checking normality for each clinical group
shapiro.test(bing_scores_with_clinical$score[bing_scores_with_clinical$Clinical.Y.N == "Y"])
#W = 0.57309, p-value = 9.018e-09

shapiro.test(bing_scores_with_clinical$score[bing_scores_with_clinical$Clinical.Y.N == "N"])
#W = 0.48429, p-value = 1.575e-06

library(ggplot2)

ggplot(bing_scores_with_clinical, aes(x = Clinical.Y.N, y = score, fill = Clinical.Y.N)) +
  geom_boxplot() +
  labs(title = "Sentiment Score by Clinical Review Group (Bing)",
       x = "Clinical Review (Y/N)",
       y = "Bing Sentiment Score") +
  theme_minimal()


#QQ-plot
ggplot(bing_scores_with_clinical, aes(sample = score)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Clinical.Y.N) +
  labs(title = "Q-Q Plot of Bing Sentiment Score by Clinical Group",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#Since both p-values are < 0.05, the sentiment scores for both groups do not follow a normal distribution.
#using non-parametric test
wilcox.test(score ~ Clinical.Y.N, data = bing_scores_with_clinical)

wilcox.test(score ~ Clinical.Y.N, data = bing_scores_with_clinical, exact = FALSE)
#data:  score by Clinical.Y.N
#W = 243, p-value = 0.4354
#does not differ significantly as p value is not <0.05


#-----------------------------------NRC--------------------------------------
# Merge sentiment scores (AFINN) with original Feedback data to get Clinical label
nrc_scores_with_clinical <- Feedback_nrc_sentiment_C_id%>%
  left_join(Feedback %>% mutate(ID = row_number()) %>% select(ID, Clinical.Y.N), by = "ID")

# View to confirm
View(nrc_scores_with_clinical)


str(nrc_scores_with_clinical)
table(nrc_scores_with_clinical)


#Checking normality for each clinical group
shapiro.test(nrc_scores_with_clinical$score[nrc_scores_with_clinical$Clinical.Y.N == "Y"])
#W = 0.62249, p-value = 1.608e-06

shapiro.test(nrc_scores_with_clinical$score[nrc_scores_with_clinical$Clinical.Y.N == "N"])
#W = 0.64917, p-value = 0.0001052

library(ggplot2)

ggplot(nrc_scores_with_clinical, aes(x = Clinical.Y.N, y = score, fill = Clinical.Y.N)) +
  geom_boxplot() +
  labs(title = "Sentiment Score by Clinical Review Group (NRC)",
       x = "Clinical Review (Y/N)",
       y = "Bing Sentiment Score") +
  theme_minimal()


#QQ-plot
ggplot(nrc_scores_with_clinical, aes(sample = score)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Clinical.Y.N) +
  labs(title = "Q-Q Plot of NRC Sentiment Score by Clinical Group",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#Since both groups fail the normality test (p < 0.05), you must use a non-parametric test:
wilcox.test(score ~ Clinical.Y.N, data = nrc_scores_with_clinical, exact = FALSE)
#data:  score by Clinical.Y.N
#W = 134.5, p-value = 0.7461





#----------------------------Modelling-------------------------------
library(tidymodels)
library(textrecipes)
library(tidytext)

#As factor
feedback_modelling <- Feedback %>%
  mutate(Clinical.Y.N = as.factor(Clinical.Y.N))

View(feedback_modelling)


#train/test split
set.seed(123)
feedback_split <- initial_split(feedback_modelling, strata = Clinical.Y.N)
train_data <- training(feedback_split)
test_data <- testing(feedback_split)

View(train_data)
View(test_data)

#This converts text into tokens, removes stopwords, applies TF-IDF:

feedback_recipe <- recipe(Clinical.Y.N ~ Feedback, data = train_data) %>%
  step_tokenize(Feedback) %>%
  step_stopwords(Feedback) %>%
  step_tokenfilter(Feedback, max_tokens = 400) %>%
  step_tfidf(Feedback)
View(feedback_recipe)


#using logistic regression
log_model <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")
View(log_model)
#workflow
feedback_workflow <- workflow() %>%
  add_model(log_model) %>%
  add_recipe(feedback_recipe)
View(feedback_workflow)


#cross validation + tuning
set.seed(234)
feedback_folds <- vfold_cv(train_data, v = 5, strata = Clinical.Y.N)

View(feedback_folds)

install.packages("glmnet")
library(glmnet)
library(tidymodels)

feedback_tune <- tune_grid(
  feedback_workflow,
  resamples = feedback_folds,
  grid = 10,
  metrics = metric_set(accuracy, roc_auc)
)

View(feedback_tune)


#Select the best model

best_model <- select_best(feedback_tune, metric = "roc_auc")

final_workflow <- finalize_workflow(feedback_workflow, best_model)


#final model evaluation

final_fit <- last_fit(final_workflow, feedback_split)

collect_metrics(final_fit)
collect_predictions(final_fit) %>%
  conf_mat(truth = Clinical.Y.N, estimate = .pred_class)

