# Define categories for territorial features
correct_categories <- c(
  "Intermediate region, close to a city",
  "Intermediate, remote region",
  "Predominantly rural region, close to a city",
  "Predominantly rural, remote region"
)

# Clean and find the closest territorial match
find_closest_match <- function(x, categories) {
  if (is.na(x) || nchar(trimws(x)) == 0) return(NA)
  
  # Calculate string distances
  distances <- stringdist::stringdist(tolower(x), tolower(categories), method = "lv")
  closest_category <- categories[which.min(distances)]
  
  if (min(distances) > 5) return(NA)
  
  return(closest_category)
}

# Clean territorial_features column in the data
territorial_features_clean <- solutions_clean %>%
  select(solution_id, primary_rdd, territorial_features) %>%
  separate_rows(territorial_features, sep = ";") %>%
  mutate(
    territorial_features = str_trim(territorial_features),
    territorial_features = str_replace_all(territorial_features, "\\s+", " "),
    territorial_features = map_chr(territorial_features, ~find_closest_match(., correct_categories))
  ) %>%
  filter(!is.na(territorial_features))  # Filter out rows where no match is found

# Count the occurrences of eaach level of rurality
rurality_rdd <- territorial_features_clean %>%
  group_by(territorial_features) %>%
  summarise(total_count = n(), .groups = 'drop')  # Summarize the count and drop grouping
print(rurality_rdd)

# Count the occurrences of RDDs by territorial features
rdd_by_territorial_features <- territorial_features_clean %>%
  group_by(territorial_features, primary_rdd) %>%
  summarise(count = n()) %>%
  ungroup()  # Ensure data is ungrouped for plotting
view(rdd_by_territorial_features)


factors <- c("Sustainable multimodal mobility",
             "Energy transition and climate neutrality",
             "Sustainable agrifood systems and ecosystem management", 
             "Nature-based and cultural tourism",
             "Culture and cultural innovation",
             "Local services, health and wellbeing")

# Define RDD abbreviations
rdd_abbreviations <- c( 
  "Sustainable multimodal mobility" = "SMM",
  "Energy transition and climate neutrality" = "ETCN",
  "Sustainable agrifood systems and ecosystem management" = "SASEM", 
  "Nature-based and cultural tourism" = "NBCT", 
  "Culture and cultural innovation" = "CCI",
  "Local services, health and wellbeing" = "LSHW"
)

rdd_by_territorial_features <- rdd_by_territorial_features %>%
  mutate(primary_rdd = factor(primary_rdd, levels = factors))  # Apply custom order


# Faceted plot
rural_gradient <- ggplot(rdd_by_territorial_features, aes(x = primary_rdd, y = count, fill = primary_rdd)) +
  geom_bar(position = position_dodge(width = 0.8), width = 0.7, stat = "identity") +  
  labs(
    title = "Count of Primary RDDs by Territorial Features",
    x = "Primary RDD",
    y = "No. of Solutions",
    fill = "Primary RDD"
  ) +
  scale_fill_viridis_d(option = "C") +  # Use Viridis color palette for the Primary RDDs
  scale_x_discrete(labels = rdd_abbreviations) +  # Apply abbreviations to x-axis (Primary RDDs)
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Remove space above the x-axis
  facet_wrap(~territorial_features) +  # Create separate graph for each territorial feature
  theme_classic() +  # Classic theme
  theme(
    axis.text.x = element_text(margin = margin(t = 10), angle = 45, hjust = 1),  # Tilt x-axis labels
    axis.title.x = element_text(margin = margin(t = 10)),  # Add top margin to x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),  # Add right margin to y-axis title
    legend.position = "none",  # Remove legend since each graph is for one feature
    text = element_text(size = 14)  # Increase text size for readability
  )

print(rural_gradient)

figure_label <- "**Figure 4:** Distribution of primary Rural Development Drivers categorised by territorial features. The figure illustrates the count of solutions associated with each primary RDD across four territorial categories: (1) Intermediate region, close to a city; (2) Intermediate, remote region; (3) Predominantly rural regions, close to a city; and (4) Predominantly rural, remote regions."

rural_gradient_final <- rural_gradient +
  labs(caption = figure_label) +  # Add the caption
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

print(rural_gradient_final)

