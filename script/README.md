# NHS App Feedback Analysis

This project analyzes NHS app feedback and predicts if a patient needs clinical review (Clinical.Y.N) based on their feedback text.

## Folders
- `data/`: Has `updated_feedback.csv` (input data).
- `output/`: Saves results (CSVs, plots like `word_cloud.png`, `model_predictions.csv`).
- `scripts/`:
  - `01_initial_data_cleaning.Rmd`: Cleans the dataset.
  - `02_data_wrangling.R`: Breaks feedback into words.
  - `03_eda_and_tidytext.R`: Shows frequent words and a word cloud.
  - `04_sentiment_analysis.R: Analyzes sentiment using Bing, AFINN, and NRC lexicons.
  -  05_topic_modeling.R: Applies LDA topic modeling and TF-IDF analysis.
  -  06_bigram_analysis.R: Analyzes and visualizes two-word phrases (bigrams).
  -  07_group_comparison.R: Compares word usage by Gender and Clinical.Y.N.
  -  08_statistical_testing.R: Tests sentiment score differences by Clinical.Y.N.

## Setup
1. Put `updated_feedback.csv` in `data/`.
2. Install R packages in RStudio:
   ```R
   install.packages(c("dplyr", "ggplot2", "here", "janitor", "stringr", "tidytext", "tidyr", "wordcloud", "glmnet"))
   
   
   
