# Install packages
# install.packages(c("tidyverse", "readr","janitor", "patchwork", "ggtext", "grid", "tm", "wordcloud", "quanteda", "stringr"))

# Load required libraries
library(tidyverse)
library(readr)
library(janitor)
library(patchwork)
library(ggtext)
library(stringr) 
library(grid) 

# Read in the dataset
solutions_catalogue <- read_csv("RURACTIVE Solutions Catalogue_DataSet_240730_v1 copy.csv", locale=locale(encoding="latin1"))
view(solutions_catalogue)

# Adjusting column names
solutions_colnames <- solutions_catalogue %>%
  set_names(make.unique(as.character(.[1,])))
# view(solutions_colnames)

# Adjusting columns
solutions_clean <- solutions_colnames[-1, ] %>% 
  drop_na("RuractiveID") %>%
  select(c("RuractiveID", 
           contains("English title*"), 
           contains("where the solution has been implemented, including the village/town (where appropriate), the region and the country. - Country"), 
           contains("The story of the solution"), 
           contains("briefly describe the aim"), 
           "Primary RDD", 
           "Secondary RDD (optional)", 
           "Tertiary RDD (optional)", 
           "Climate change adaptation", 
           "Climate change mitigation", 
           "Protecting biodiversity", 
           "Social justice and inclusion",
           "What are the key territorial features of the territory where the solution is applied?",
           "The solution is/was designed and/or implemented through a participatory process")) %>% 
  rename(solution_ID = "RuractiveID",
        solution_title = contains("English title*"),
        country = contains("where the solution has been implemented, including the village/town (where appropriate), the region and the country. - Country"),
        story = contains("The story of the solution"),
        aim = contains("briefly describe the aim"),
        secondary_RDD = "Secondary RDD (optional)",
        tertiary_RDD = "Tertiary RDD (optional)",
        cc_adaptation = "Climate change adaptation",
        cc_mitigation = "Climate change mitigation",
        biodiversity = "Protecting biodiversity",
        social_justice = "Social justice and inclusion",
        territorial_features = "What are the key territorial features of the territory where the solution is applied?",
        participatory_process = "The solution is/was designed and/or implemented through a participatory process") %>%
  clean_names() %>% 
  filter(!is.na(primary_rdd)) %>% 
  mutate(across(everything(), 
                ~case_when(
                  str_detect(., "^\\s*$|Â") ~ NA_character_,
                  TRUE ~ str_trim(as.character(.))
                )))

view(solutions_clean)
str(solutions_clean)

########################################################################
# GRAPH 1: Solutions x RDD
rdd_counts <- solutions_clean %>%
  filter(!is.na(primary_rdd)) %>%
  count(primary_rdd)
print(rdd_counts)

rdd_abbreviations <- c( 
  "Sustainable multimodal mobility" = "SMM",
  "Energy transition and climate neutrality" = "ETCN",
  "Sustainable agrifood systems and ecosystem management" = "SASEM", 
  "Nature-based and cultural tourism" = "NBCT", 
  "Culture and cultural innovation" = "CCI",
  "Local services, health and wellbeing" = "LSHW"
)

# Define the RDD factors of interest with abbreviations for the x-axis labels
factors <- c("Sustainable multimodal mobility",
             "Energy transition and climate neutrality",
             "Sustainable agrifood systems and ecosystem management", 
             "Nature-based and cultural tourism",
             "Culture and cultural innovation",
             "Local services, health and wellbeing")

g1_primary_rdd <- solutions_clean %>%
  filter(!is.na(primary_rdd)) %>%
  mutate(primary_rdd = factor(primary_rdd, levels = factors)) %>% 
  ggplot(aes(x = primary_rdd)) +
  geom_bar() +
  labs(title = "Distribution of Primary Rural Development Drivers",
       x = "Primary Rural Development Driver",
       y = "No. of Solutions") +
  theme_classic() +
  scale_x_discrete(labels = rdd_abbreviations) +  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Remove space above the x-axis
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis labels
    axis.title.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add right margin to y-axis title
    legend.position = "right",
    text = element_text(size = 14)  # Increase the size of all text elements
  ) 

figure_label_1 <- "**Figure 1:** Distribution of solutions among primary Rural Development Drivers (RDDs). This bar chart displays the count of solutions categorised by six primary RDDs: Sustainable multimodal mobility (SMM), Energy transition and climate neutrality (ETCN), Sustainable agrifood systems and ecosystem management (SASEM), Nature-based and cultural tourism (NBCT), Culture and cultural innovation (CCI), and Local services, health and wellbeing (LSHW)."

primary_rdd_final <- g1_primary_rdd +
  labs(caption = figure_label_1) +  # Add the caption
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

print(primary_rdd_final)

########################################################################
# GRAPH 2: Solutions x RDD (stacked primary, secondary, tertiary)

g2_stacked_all_rdds <- solutions_clean %>%
  pivot_longer(cols = c(primary_rdd, secondary_rdd, tertiary_rdd), 
               names_to = "rdd_type", 
               values_to = "rdd_value") %>%
  filter(!is.na(rdd_value)) %>%
  mutate(rdd_type = factor(rdd_type, levels = c("primary_rdd", "secondary_rdd", "tertiary_rdd"),
                           labels = c("Primary", "Secondary", "Tertiary")),
         rdd_value = factor(rdd_value, levels = factors)) %>%  # Ensure the same factor order
  ggplot(aes(x = rdd_value, fill = rdd_type)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of all Rural Development Driver (RDD) Levels",
       x = "Rural Development Driver",
       y = "No. of Solutions",
       fill = "RDD Level") +
  theme_classic() +
  scale_fill_viridis_d(option = "C") +  
  scale_x_discrete(labels = rdd_abbreviations) +  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Remove space above the x-axis
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis labels
    axis.title.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add right margin to y-axis title
    legend.position = "right",
    text = element_text(size = 14)  # Increase the size of all text elements
  )

figure_label_2 <- "**Figure 2:** Stacked bar chart illustrating the distribution of solutions across Rural Development Drivers split by primary, secondary, and tertiary levels."

# Add caption and formatting
stacked_rdd_final <- g2_stacked_all_rdds +
  labs(caption = figure_label_2) +  # Add the caption
  theme(
    plot.caption = element_textbox_simple(
      family = "Times New Roman",          # Font family
      size = 12,                           # Font size
      lineheight = 1.2,                    # Adjust line spacing for better readability
      margin = margin(t = 20),     # Add some space above and below the caption
      width = unit(1, "npc"),            # Set width to 90% of the plot width
    ),
    plot.margin = margin(t = 20, b = 20, l = 10, r = 10), # Adjust margins for title, plot area, and caption
  )

print(stacked_rdd_final)

########################################################################
# GRAPH 3: Count Cross Cutting Priorities

counts_ccp <- solutions_clean %>% 
  pivot_longer(cols = c("cc_adaptation", "cc_mitigation", "biodiversity", "social_justice"), 
               names_to = "variable", 
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  count(variable, value) %>%
  group_by(variable) %>%
  summarize(count = sum(n))
counts_ccp

g3_ccp <- solutions_clean %>% 
  pivot_longer(cols = c("cc_adaptation", "cc_mitigation", "biodiversity", "social_justice"), 
               names_to = "variable", 
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  count(variable, value) %>%
  ggplot(aes(x = fct_reorder(variable, n, sum), y = n)) +
  geom_col() +
  labs(title = "Prevalence of Cross-Cutting Priorities in Solutions",
       x = "Cross Cutting Priority",
       y = "No. of Solutions") +
  scale_x_discrete(labels = rdd_abbreviations) +  # Apply abbreviations to x-axis (Primary RDDs)
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Remove space above the x-axis
  theme_classic() +
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis labels
    axis.title.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add right margin to y-axis title
    legend.position = "right",
    text = element_text(size = 14)  # Increase the size of all text elements
  ) 

g3_ccp

figure_label_3 <- "**Figure 3:** Bar chart displaying the count of solutions that specify the incorporation of a Cross-Cutting Priority (cc = climate change)" 

# Add the caption and formatting
ccp_final <- g3_ccp +
  labs(caption = figure_label_3) +  # Add the caption
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

# Display the final plot with the label
print(ccp_final)














