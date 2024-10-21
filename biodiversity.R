library(viridis)
library(gt)
library(stringr)

# PART 1
# Define the RDD factors with abbreviations for the x-axis labels
factors <- c("Sustainable multimodal mobility",
             "Energy transition and climate neutrality",
             "Sustainable agrifood systems and ecosystem management", 
             "Nature-based and cultural tourism",
             "Culture and cultural innovation",
             "Local services, health and wellbeing")

abbreviations <- c( 
  "Sustainable multimodal mobility" = "SMM",
  "Energy transition and climate neutrality" = "ETCN",
  "Sustainable agrifood systems and ecosystem management" = "SASEM", 
  "Nature-based and cultural tourism" = "NBCT", 
  "Culture and cultural innovation" = "CCI",
  "Local services, health and wellbeing" = "LSHW"
)

# Pivot the RDDs (primary, secondary, tertiary) into a long format and filter for biodiversity == "Y"
biodiversity_rdd_dodged <- solutions_clean %>%
  filter(biodiversity == "Y") %>%  
  pivot_longer(cols = c(primary_rdd, secondary_rdd, tertiary_rdd),   
               names_to = "rdd_type", 
               values_to = "rdd_value") %>%
  filter(!is.na(rdd_value),  
         rdd_value %in% factors) %>%  
  mutate(rdd_value = factor(rdd_value, levels = factors),  # This enforces the custom order
         rdd_type = factor(rdd_type,  
                           levels = c("primary_rdd", "secondary_rdd", "tertiary_rdd"),  
                           labels = c("Primary", "Secondary", "Tertiary"))) %>% 
  ggplot(aes(x = rdd_value, fill = rdd_type)) +  
  geom_bar(position = position_dodge(width = 0.8), width = 0.7) +  
  labs(title = "Biodiversity Priority by Rural Development Driver (RDD)",  
       x = "Rural Development Driver",
       y = "No. of Solutions",
       fill = "RDD Type") +
  scale_fill_viridis_d(option = "C") +  
  scale_x_discrete(labels = rdd_abbreviations) +  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Remove space above the x-axis
  theme_classic() +  
  theme(
    axis.text.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis labels
    axis.title.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add right margin to y-axis title
    legend.position = "right",
    text = element_text(size = 14)  # Increase the size of all text elements
  )  

figure_label_bd <- "**Figure 5:** Dodged bar chart of solutions prioritising social justice and inclusion as a cross-cutting priority across Rural Development Drivers (RDDs), categorized by primary, secondary, and tertiary RDD types"

# Add caption and formatting
bd_final <- biodiversity_rdd_dodged +
  labs(caption = figure_label_bd) +  # Add the caption
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

print(bd_final)

#########################################
# PART 2

# Define biodiversity-related keywords
biodiversity_keywords <- c(
  "biodiversity",
  "ecosystem",
  "conservation",
  "species",
  "habitat",
  "ecology",
  "wildlife"
)

# Filter solutions related to all six RDDs
rdd_agri_food <- solutions_clean %>%
  filter(primary_rdd == "Sustainable agrifood systems and ecosystem management")

rdd_tourism <- solutions_clean %>%
  filter(primary_rdd == "Nature-based and cultural tourism")

rdd_multimodal <- solutions_clean %>%
  filter(primary_rdd == "Sustainable multimodal mobility")

rdd_energy <- solutions_clean %>%
  filter(primary_rdd == "Energy transition and climate neutrality")

rdd_culture <- solutions_clean %>%
  filter(primary_rdd == "Culture and cultural innovation")

rdd_services <- solutions_clean %>%
  filter(primary_rdd == "Local services, health and wellbeing")

# Output the count of solutions for each RDD
cat("Number of solutions for Sustainable Agri-food Systems and Ecosystem Management:", nrow(rdd_agri_food), "\n")
cat("Number of solutions for Nature-based and Cultural Tourism:", nrow(rdd_tourism), "\n")
cat("Number of solutions for Sustainable Multimodal Mobility:", nrow(rdd_multimodal), "\n")
cat("Number of solutions for Energy Transition and Climate Neutrality:", nrow(rdd_energy), "\n")
cat("Number of solutions for Culture and Cultural Innovation:", nrow(rdd_culture), "\n")
cat("Number of solutions for Local Services, Health and Wellbeing:", nrow(rdd_services), "\n")

# Combine aim and story fields for analysis
rdd_agri_food$text_data <- paste(rdd_agri_food$aim, rdd_agri_food$story, sep = "\n")
rdd_tourism$text_data <- paste(rdd_tourism$aim, rdd_tourism$story, sep = "\n")
rdd_multimodal$text_data <- paste(rdd_multimodal$aim, rdd_multimodal$story, sep = "\n")
rdd_energy$text_data <- paste(rdd_energy$aim, rdd_energy$story, sep = "\n")
rdd_culture$text_data <- paste(rdd_culture$aim, rdd_culture$story, sep = "\n")
rdd_services$text_data <- paste(rdd_services$aim, rdd_services$story, sep = "\n")

# Create a function to count keyword frequency
count_keyword_frequency <- function(text_data, keywords) {
  keyword_count <- sapply(keywords, function(keyword) {
    sum(str_count(tolower(text_data), keyword))
  })
  return(keyword_count)
}

# Count biodiversity-related keyword frequency for each RDD
biodiversity_agri_food <- count_keyword_frequency(rdd_agri_food$text_data, biodiversity_keywords)
biodiversity_tourism <- count_keyword_frequency(rdd_tourism$text_data, biodiversity_keywords)
biodiversity_multimodal <- count_keyword_frequency(rdd_multimodal$text_data, biodiversity_keywords)
biodiversity_energy <- count_keyword_frequency(rdd_energy$text_data, biodiversity_keywords)
biodiversity_culture <- count_keyword_frequency(rdd_culture$text_data, biodiversity_keywords)
biodiversity_services <- count_keyword_frequency(rdd_services$text_data, biodiversity_keywords)

# Total number of solutions for each RDD
total_agri_food_solutions <- nrow(rdd_agri_food)
total_tourism_solutions <- nrow(rdd_tourism)
total_multimodal_solutions <- nrow(rdd_multimodal)
total_energy_solutions <- nrow(rdd_energy)
total_culture_solutions <- nrow(rdd_culture)
total_services_solutions <- nrow(rdd_services)

# Create a data frame to summarize the results
df_biodiversity_freq <- data.frame(
  Keyword = biodiversity_keywords,
  Frequency_AgriFood = biodiversity_agri_food,
  Frequency_Tourism = biodiversity_tourism,
  Frequency_Multimodal = biodiversity_multimodal,
  Frequency_Energy = biodiversity_energy,
  Frequency_Culture = biodiversity_culture,
  Frequency_Services = biodiversity_services
)

# Calculate percentages
df_biodiversity_freq$Percentage_AgriFood <- (df_biodiversity_freq$Frequency_AgriFood / total_agri_food_solutions) * 100
df_biodiversity_freq$Percentage_Tourism <- (df_biodiversity_freq$Frequency_Tourism / total_tourism_solutions) * 100
df_biodiversity_freq$Percentage_Multimodal <- (df_biodiversity_freq$Frequency_Multimodal / total_multimodal_solutions) * 100
df_biodiversity_freq$Percentage_Energy <- (df_biodiversity_freq$Frequency_Energy / total_energy_solutions) * 100
df_biodiversity_freq$Percentage_Culture <- (df_biodiversity_freq$Frequency_Culture / total_culture_solutions) * 100
df_biodiversity_freq$Percentage_Services <- (df_biodiversity_freq$Frequency_Services / total_services_solutions) * 100

# Ctable
df_biodiversity_percentages <- df_biodiversity_freq %>%
  select(Keyword, 
         Percentage_Multimodal, 
         Percentage_Energy, 
         Percentage_AgriFood, 
         Percentage_Tourism, 
         Percentage_Culture, 
         Percentage_Services)

df_biodiversity_percentages %>%
  gt() %>%
  tab_header(
    title = html("<strong>Table 4:</strong> Biodiversity-Related Keyword Percentages for each Primary RDD"),
  ) %>%
  cols_label(
    Keyword = "Biodiversity Keyword",
    Percentage_Multimodal = "Percentage in SMM (%)",
    Percentage_Energy = "Percentage in ETCN (%)",
    Percentage_AgriFood = "Percentage in SASEM (%)",
    Percentage_Tourism = "Percentage in NBCT (%)",
    Percentage_Culture = "Percentage in CCI (%)",
    Percentage_Services = "Percentage in LSHW (%)"
  ) %>%
  fmt_number(
    columns = vars(Percentage_Multimodal, Percentage_Energy, Percentage_AgriFood, Percentage_Tourism, Percentage_Culture, Percentage_Services),
    decimals = 2  # Change this to 0 if you prefer no decimal places
  ) %>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold",
    table.align = "left"
  )
