library(tidyverse)
library(stringr)
library(tm)        # For text mining
library(gt)        # For table formatting
library(wordcloud)
library(viridis)

###################################################
# COUNT Adaptation and Mitigation, and both

count_ccm <- solutions_clean %>%
  filter(cc_mitigation == "Y" & !is.na(primary_rdd) & primary_rdd != "NA" & primary_rdd != "") %>%
  count()
count_ccm

count_ccm <- solutions_clean %>%
  filter(cc_adaptation == "Y" & !is.na(primary_rdd) & primary_rdd != "NA" & primary_rdd != "") %>%
  count()
count_ccm

count_ccm <- solutions_clean %>%
  filter(cc_adaptation == "Y", cc_mitigation == "Y" & !is.na(primary_rdd) & primary_rdd != "NA" & primary_rdd != "") %>%
  count()
count_ccm

###################################################
# Table of adaptation and mitigation together broken down by RDD

# Step 1: Filter data for entries where both mitigation and adaptation are "Y" and primary_rdd is valid
climate_change_summary <- solutions_clean %>%
  filter(cc_mitigation == "Y" & cc_adaptation == "Y" & 
           !is.na(primary_rdd) & primary_rdd != "NA" & primary_rdd != "") %>%
  count(primary_rdd) %>%
  rename(factor = primary_rdd, count = n)

# Step 2: Create a complete set of factors (in case some factors have zero count)
all_factors <- data.frame(factor = unique(solutions_clean$primary_rdd[!is.na(solutions_clean$primary_rdd) & 
                                                                        solutions_clean$primary_rdd != "NA" & 
                                                                        solutions_clean$primary_rdd != ""]))

# Step 3: Join with the summary to ensure all factors are represented
climate_change_complete <- all_factors %>%
  left_join(climate_change_summary, by = "factor") %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  arrange(desc(count))  # Sort by count in descending order

# Step 4: Display the results in a nicely formatted table using gt
climate_change_complete %>%
  gt() %>%
  tab_header(
    title = html("<strong>Table 1:</strong> No. of solutions addressing climate change mitigation and adaptation"),
  ) %>%
  cols_label(
    factor = "Primary RDD",
    count = "Count"
  ) %>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold",
    table.align = "left"
  )


###################################################
### Word frequencies of adaptation and mitigation
# Define keywords to search for, including variations
keywords <- c("mitigation[s]?", "mitigate[s]?", "mitigating", "adaptation[s]?", "adapt[s]?", "adapting")

# Function to preprocess (stem) text using tm
preprocess_text <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))    # Convert to lowercase
  corpus <- tm_map(corpus, removePunctuation)               # Remove punctuation
  corpus <- tm_map(corpus, removeNumbers)                   # Remove numbers
  corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove stopwords
  corpus <- tm_map(corpus, stemDocument)                    # Stem the text
  return(sapply(corpus, as.character))                      # Return preprocessed text
}

# Preprocess the keywords
stemmed_keywords <- preprocess_text(keywords)

# Function to count total keyword occurrences using stemmed keywords
count_total_keywords <- function(text) {
  text <- preprocess_text(text) # Preprocess the text
  total_count <- sum(sapply(stemmed_keywords, function(keyword) {
    str_count(text, regex(keyword))
  }))
  return(total_count)
}

# Combine 'aim' and 'story' columns
solutions_clean$combined_text <- paste(solutions_clean$aim, solutions_clean$story, sep = " ")

# Apply the function to the combined text
keyword_counts <- solutions_clean %>%
  mutate(total_count = map_dbl(combined_text, count_total_keywords)) %>%
  select(primary_rdd, total_count)

# Group by primary_rdd and sum the total keyword counts
result <- keyword_counts %>%
  filter(!is.na(primary_rdd)) %>%
  group_by(primary_rdd) %>%
  summarise(total_keyword_count = sum(total_count)) %>% 
  arrange(desc(total_keyword_count))  # Sort by count in descending order

# Create a nice table using gt
result %>%
  gt() %>%
  tab_header(
    title = html("<strong>Table 2:</strong> Frequency of keywords 'mitigation' and 'adaptation' in solutions"),
  ) %>%
  cols_label(
    primary_rdd = "Primary RDD",
    total_keyword_count = "Total Keyword Count"
  ) %>%
  fmt_number(
    columns = vars(total_keyword_count),
    decimals = 0
  ) %>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold",
    table.align = "left"
  )

############################################################
############################################################
############################################################
# Define keywords to search for, including variations
keywords_mitigation <- c("mitigation[s]?", "mitigate[s]?", "mitigating")
keywords_adaptation <- c("adaptation[s]?", "adapt[s]?", "adapting")

# Function to preprocess (stem) text using tm
preprocess_text <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))    # Convert to lowercase
  corpus <- tm_map(corpus, removePunctuation)               # Remove punctuation
  corpus <- tm_map(corpus, removeNumbers)                   # Remove numbers
  corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove stopwords
  corpus <- tm_map(corpus, stemDocument)                    # Stem the text
  return(sapply(corpus, as.character))                      # Return preprocessed text
}

# Function to count total keyword occurrences using stemmed keywords
count_total_keywords <- function(text, keywords) {
  text <- preprocess_text(text) # Preprocess the text
  total_count <- sum(sapply(keywords, function(keyword) {
    str_count(text, regex(keyword))
  }))
  return(total_count)
}

# Combine 'aim' and 'story' columns
solutions_clean$combined_text <- paste(solutions_clean$aim, solutions_clean$story, sep = " ")

###################################################
### Analysis for Climate Change Adaptation Related Words where Mitigation = "Y"
adaptation_mitigation_data <- solutions_clean %>%
  filter(cc_mitigation == "Y")  # Filter for entries with climate change mitigation

# Apply the function to the combined text
adaptation_mitigation_keyword_counts <- adaptation_mitigation_data %>%
  mutate(total_count = map_dbl(combined_text, ~ count_total_keywords(.x, keywords_adaptation))) %>%
  select(primary_rdd, total_count)

# Group by primary_rdd and sum the total keyword counts
adaptation_mitigation_result <- adaptation_mitigation_keyword_counts %>%
  filter(!is.na(primary_rdd)) %>%
  group_by(primary_rdd) %>%
  summarise(total_keyword_count = sum(total_count)) %>% 
  arrange(desc(total_keyword_count))  # Sort by count in descending order

# Create a nice table for adaptation related to mitigation using gt
adaptation_mitigation_result %>%
  gt() %>%
  tab_header(
    title = html("<strong>Table 3:</strong> Count of 'Adaptation' Keywords where Mitigation Addressed as CCP"),
  ) %>%
  cols_label(
    primary_rdd = "Primary RDD",
    total_keyword_count = "Total Keyword Count"
  ) %>%
  fmt_number(
    columns = vars(total_keyword_count),
    decimals = 0
  ) %>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold",
    table.align = "left"
  )

###################################################
### Analysis for Climate Change Mitigation Related Words where Adaptation = "Y"
mitigation_adaptation_data <- solutions_clean %>%
  filter(cc_adaptation == "Y")  # Filter for entries with climate change adaptation

# Apply the function to the combined text
mitigation_adaptation_keyword_counts <- mitigation_adaptation_data %>%
  mutate(total_count = map_dbl(combined_text, ~ count_total_keywords(.x, keywords_mitigation))) %>%
  select(primary_rdd, total_count)

# Group by primary_rdd and sum the total keyword counts
mitigation_adaptation_result <- mitigation_adaptation_keyword_counts %>%
  filter(!is.na(primary_rdd)) %>%
  group_by(primary_rdd) %>%
  summarise(total_keyword_count = sum(total_count)) %>% 
  arrange(desc(total_keyword_count))  # Sort by count in descending order

# Create a nice table for mitigation related to adaptation using gt
mitigation_adaptation_result %>%
  gt() %>%
  tab_header(
    title = "Total Keyword Frequency by Primary RDD",
    subtitle = "Count of 'Mitigation' Keywords where Adaptation = 'Y'"
  ) %>%
  cols_label(
    primary_rdd = "Primary RDD",
    total_keyword_count = "Total Keyword Count"
  ) %>%
  fmt_number(
    columns = vars(total_keyword_count),
    decimals = 0
  ) %>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold",
    table.align = "left"
  )



###################################################
### Wordclouds for Climate Change Mitigation and Climate Change Adaptation

variables <- c("cc_adaptation", "cc_mitigation")

# Function to create and display a word cloud
create_wordcloud <- function(variable, data) {
  # Filter data for the current variable
  var_filtered <- data %>% filter(!!sym(variable) == "Y")
  
  if (nrow(var_filtered) == 0) {
    cat("No data for variable:", variable, ".\n")
    return(NULL)
  }
  
  # Combine aim and story texts for analysis
  text_data <- paste(var_filtered$aim, var_filtered$story, sep = "\n")
  
  # Create and preprocess the corpus
  corpus <- Corpus(VectorSource(text_data)) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace)
  
  # Remove empty documents
  corpus <- corpus[!sapply(corpus, is.null)]
  
  # Create document-term matrix and process it
  dtm <- TermDocumentMatrix(corpus)
  words <- sort(rowSums(as.matrix(dtm)), decreasing = TRUE)
  df_words <- data.frame(word = names(words), freq = words)
  
  # Create the word cloud
  if (nrow(df_words) > 0) {
    wordcloud(words = df_words$word, freq = df_words$freq, min.freq = 1,
              max.words = 50, random.order = FALSE, rot.per = 0.35,
              colors = viridis::viridis(7))  # Use viridis color palette
  } else {
    cat("No words found for variable:", variable, ".\n")
  }
}

# Generate and display word clouds for adaptation and mitigation
create_wordcloud("cc_adaptation", solutions_clean)
create_wordcloud("cc_mitigation", solutions_clean)











# Load necessary libraries
library(dplyr)
library(gt)

###################################################
# Table of adaptation and mitigation together broken down by RDD

# Step 1: Filter data for entries where both mitigation and adaptation are "Y" and primary_rdd is valid
climate_change_summary <- solutions_clean %>%
  filter(cc_mitigation == "Y" & cc_adaptation == "Y" & 
           !is.na(primary_rdd) & primary_rdd != "NA" & primary_rdd != "") %>%
  count(primary_rdd) %>%
  rename(factor = primary_rdd, count = n)

# Step 2: Create a complete set of factors (in case some factors have zero count)
all_factors <- data.frame(factor = unique(solutions_clean$primary_rdd[!is.na(solutions_clean$primary_rdd) & 
                                                                        solutions_clean$primary_rdd != "NA" & 
                                                                        solutions_clean$primary_rdd != ""]))

# Step 3: Join with the summary to ensure all factors are represented
climate_change_complete <- all_factors %>%
  left_join(climate_change_summary, by = "factor") %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  arrange(desc(count))  # Sort by count in descending order

# Step 4: Display the results in a nicely formatted table using gt
climate_change_complete %>%
  gt() %>%
  tab_header(
    title = "Count of Entries with Both Climate Change Mitigation and Adaptation",
    subtitle = "Grouped by Primary RDD | This table presents the counts of solutions that demonstrate both climate change mitigation and adaptation, categorized by the primary Rural Development Drivers (RDD)."
  ) %>%
  cols_label(
    factor = "Primary RDD",
    count = "Count"
  ) %>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold",
    table.align = "left"
  )


