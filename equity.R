# Count Social Justice CCP x RDD1,2,3
library(viridis)

factors <- c("Sustainable multimodal mobility",
             "Energy transition and climate neutrality",
             "Sustainable agrifood systems and ecosystem management",
             "Nature-based and cultural tourism",
             "Culture and cultural innovation",
             "Local services, health and wellbeing")

# Summarize social justice data by RDD type and factor
social_justice_summary <- solutions_clean %>%
  filter(social_justice == "Y") %>%
  pivot_longer(cols = c("primary_rdd", "secondary_rdd", "tertiary_rdd"),
               names_to = "rdd_type", values_to = "rdd_value") %>%
  filter(rdd_value %in% factors) %>%  # Keep only matching factors
  count(factor = rdd_value, rdd_type)  # Count occurrences

# Create a full set of factor x RDD type combinations
social_justice_complete <- expand.grid(factor = factors,
                                       rdd_type = unique(social_justice_summary$rdd_type)) %>%
  left_join(social_justice_summary, by = c("factor", "rdd_type")) %>%
  replace_na(list(n = 0))  # Replace NAs with 0 counts

social_justice_graph <- ggplot(social_justice_complete, aes(x = factor, y = n, fill = rdd_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Social Justice & Inclusion Priority by Rural Development Driver (RDD)",
       x = "RDD", y = "No. of Solutions",
       fill = "RDD Type") +
  scale_fill_viridis_d(option = "C") +  # Use viridis color scale
  theme_classic() +
  scale_x_discrete(labels = rdd_abbreviations) +  
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +  # Remove space above the x-axis
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis labels
    axis.title.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add right margin to y-axis title
    legend.position = "right",
    text = element_text(size = 14)  # Increase the size of all text elements
  )  

figure_label_sj <- "**Figure 6:** Dodged bar chart of solutions prioritising social justice and inclusion as a cross-cutting priority across Rural Development Drivers (RDDs), categorized by primary, secondary, and tertiary RDD types"

sj_final <- social_justice_graph +
  labs(caption = figure_label_sj) +  # Add the caption
  theme(
    plot.caption = element_textbox_simple(
      family = "Times New Roman",          # Font family
      size = 12,                           # Font size
      lineheight = 1.2,                    # Adjust line spacing for better readability
      margin = margin(t = 20),     # Add some space above and below the caption
      width = unit(0.97, "npc"),            # Set width to 90% of the plot width
    ),
    plot.margin = margin(t = 20, b = 20, l = 10, r = 10), # Adjust margins for title, plot area, and caption
  )

print(sj_final)


################################################
### Occurence of inclusion related terms

# Load necessary libraries
library(tidyverse)
library(stringr)
library(tm)        # For text mining

# Define keywords to search for, including variations
keywords <- c(
  "Equity", "Equalities",
  "Equality",
  "Social justice",
  "Inclusion", "Inclusive",
  "Diversity", "Diversities",
  "Non-discrimination", "Non-discriminations", "Discrimination", "Discriminations",
  "Community engagement",
  "Participatory decision-making", "Participatory decisions",
  "Participatory processes",
  "Stakeholder collaboration",
  "Local capacity building", "Capacity building",
  "Grassroots leadership",
  "Multistakeholder involvement",
  "Inclusive education",
  "Healthcare equity",
  "Indigenous inclusion",
  "Gender equality",
  "Disability inclusion", "Disability",
  "Youth empowerment",
  "Elderly care", "Elder care",
  "Minority rights",
  "Inclusive economies",
  "Sustainable livelihoods",
  "Collective action",
  "Inclusive governance",
  "Policy advocacy", "Advocacy",
  "Legal empowerment",
  "Social protection",
  "Climate justice",
  "Ecological stewardship",
  "Cultural sensitivity"
)

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
#### GRAPH 3 - Removing duplicates

stemmed_keywords <- preprocess_text(keywords)

# Function to count unique matched keywords
count_total_keywords <- function(text) {
  text <- preprocess_text(text)  # Preprocess the text
  matched_keywords <- sapply(stemmed_keywords, function(keyword) {
    matches <- str_extract_all(text, regex(keyword))  # Find all matches
    return(unlist(matches))  # Return the matched words
  })
  
  # Filter out empty matches and store identified words
  matched_keywords <- unique(unlist(matched_keywords[lengths(matched_keywords) > 0]))  # Get unique matches
  total_count <- length(matched_keywords)  # Count total unique matches
  
  return(list(total_count = total_count, matched_words = matched_keywords))
}

# Combine 'aim' and 'story' columns
solutions_clean$combined_text <- paste(solutions_clean$aim, solutions_clean$story, sep = " ")

# Apply the function to the combined text and store the results
keyword_counts <- solutions_clean %>%
  mutate(keyword_data = map(combined_text, count_total_keywords)) %>%
  mutate(total_count = map_dbl(keyword_data, "total_count"),
         matched_words = map(keyword_data, "matched_words")) %>%
  select(primary_rdd, total_count, matched_words)

# Group by primary_rdd and sum the total keyword counts
result <- keyword_counts %>%
  filter(!is.na(primary_rdd)) %>%
  group_by(primary_rdd) %>%
  summarise(total_keyword_count = sum(total_count),
            matched_words_list = list(unique(unlist(matched_words))))  # Get unique words per group

# Print the result
print(result)

# Check which words were identified for quality control
print(result$matched_words_list)

# Plot the result
ggplot(result, aes(x = primary_rdd, y = total_keyword_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Primary RDD", y = "Total Keyword Count", 
       title = "Total Keyword Frequency by Primary RDD") +
  geom_text(aes(label = total_keyword_count), vjust = -0.5)


### 3. Tabulating Percentage

# Group by primary_rdd and calculate the total keyword counts and percentage
result <- keyword_counts %>%
  filter(!is.na(primary_rdd)) %>%
  group_by(primary_rdd) %>%
  summarise(
    total_keyword_count = sum(total_count), 
    total_solutions = n(),  # Count total solutions for each primary_rdd
    matched_words_list = list(unique(unlist(matched_words)))
  ) %>%
  mutate(
    percentage_keyword_count = (total_keyword_count / total_solutions)  # Calculate percentage of keyword count per RDD
  ) %>%
  ungroup()  # Un-group to avoid issues with future operations

# Debug: Print intermediate results for verification
print(result)

# Create a formatted table using gt
table_output <- result %>%
  select(primary_rdd, total_solutions, total_keyword_count, percentage_keyword_count) %>%
  gt() %>%
  tab_header(
    title = html("<strong>Table 5:</strong> Count and Percentage of Primary RDDs Addressing Social Justice and Inclusion"),
  ) %>%
  fmt_number(
    columns = vars(total_keyword_count, total_solutions),
    decimals = 0  # Display whole numbers
  ) %>%
  fmt_percent(
    columns = vars(percentage_keyword_count),
    decimals = 1  # Show one decimal point for percentages
  ) %>%
  cols_label(
    primary_rdd = "Primary RDD",
    total_solutions = "Total Solutions",
    total_keyword_count = "Total Keyword Count",
    percentage_keyword_count = "Percentage of Keyword Count"
  ) %>%
  tab_spanner(
    label = "Keyword Data",
    columns = vars(total_keyword_count, percentage_keyword_count)
  )

# Display the table
table_output
