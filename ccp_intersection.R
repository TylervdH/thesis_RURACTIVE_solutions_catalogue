# Step 1: Inspect unique values (already done, but keeping for clarity)
unique(solutions_clean$cc_adaptation)
unique(solutions_clean$cc_mitigation)
unique(solutions_clean$biodiversity)
unique(solutions_clean$social_justice)

# Step 2: Clean the columns by filtering out invalid rows
# Only keep rows where values are "Y" or NA for cross-cutting priorities
# Only keep rows where values are "Y" or NA for cross-cutting priorities
ccp_filtered <- solutions_clean %>%
  filter(
    (cc_adaptation == "Y" | is.na(cc_adaptation)) &
      (cc_mitigation == "Y" | is.na(cc_mitigation)) &
      (biodiversity == "Y" | is.na(biodiversity)) &
      (social_justice == "Y" | is.na(social_justice))
  )

# Step 3: Convert the cross-cutting priorities into binary (1 or 0) format for analysis
ccp_intersection <- ccp_filtered %>%
  mutate(
    cc_adaptation = as.numeric(cc_adaptation == "Y"),
    cc_mitigation = as.numeric(cc_mitigation == "Y"),
    biodiversity = as.numeric(biodiversity == "Y"),
    social_justice = as.numeric(social_justice == "Y")
  )

# Check the conversion
print(head(ccp_intersection))

# Recalculate counts
# Total number of solutions
total_solutions <- nrow(ccp_intersection)

# Calculate the number of solutions for each cross-cutting priority (individual counts)
single_counts <- ccp_intersection %>%
  summarise(
    CCM = sum(cc_mitigation, na.rm = TRUE),
    CCA = sum(cc_adaptation, na.rm = TRUE),
    BD = sum(biodiversity, na.rm = TRUE),
    SJI = sum(social_justice, na.rm = TRUE)
  )

# Calculate the number of solutions for each pair of cross-cutting priorities
pair_counts <- ccp_intersection %>%
  summarise(
    CCM_CCA = sum(cc_mitigation & cc_adaptation, na.rm = TRUE),
    CCM_BD = sum(cc_mitigation & biodiversity, na.rm = TRUE),
    CCM_SJI = sum(cc_mitigation & social_justice, na.rm = TRUE),
    CCA_BD = sum(cc_adaptation & biodiversity, na.rm = TRUE),
    CCA_SJI = sum(cc_adaptation & social_justice, na.rm = TRUE),
    BD_SJI = sum(biodiversity & social_justice, na.rm = TRUE)
  )

# Calculate the number of solutions for each triple combination of cross-cutting priorities
triple_counts <- ccp_intersection %>%
  summarise(
    CCM_CCA_BD = sum(cc_mitigation & cc_adaptation & biodiversity, na.rm = TRUE),
    CCM_CCA_SJI = sum(cc_mitigation & cc_adaptation & social_justice, na.rm = TRUE),
    CCM_BD_SJI = sum(cc_mitigation & biodiversity & social_justice, na.rm = TRUE),
    CCA_BD_SJI = sum(cc_adaptation & biodiversity & social_justice, na.rm = TRUE)
  )

# Calculate the number of solutions that include all four cross-cutting priorities
quadruple_count <- ccp_intersection %>%
  summarise(
    All_4 = sum(cc_mitigation & cc_adaptation & biodiversity & social_justice, na.rm = TRUE)
  )

# Print the results to check
print(single_counts)
print(pair_counts)
print(triple_counts)
print(quadruple_count)

# Step 5: Display the counts in a table format
library(tidyr)
library(knitr)

# Reshape the data
intersection_long <- bind_rows(
 # single_counts %>% pivot_longer(everything(), names_to = "Priority", values_to = "Count") %>% mutate(Type = "Single"),
  pair_counts %>% pivot_longer(everything(), names_to = "Priority", values_to = "Count") %>% mutate(Type = "Pairs"),
  triple_counts %>% pivot_longer(everything(), names_to = "Priority", values_to = "Count") %>% mutate(Type = "Triples"),
  quadruple_count %>% pivot_longer(everything(), names_to = "Priority", values_to = "Count") %>% mutate(Type = "All Four")
)
# 
# kable(intersection_long, format = "markdown", col.names = c("Priority", "Count", "Type"))

# library(ggplot2)
# library(dplyr)
# library(forcats)
# library(viridis)

# First, let's create a custom order for the Type factor
type_order <- c("Pairs", "Triples", "All Four")

# Now, let's modify the data to create a grouping variable and order the priorities
intersection_long_modified <- intersection_long %>%
  mutate(
    Type = factor(Type, levels = type_order),
    Priority = fct_reorder(Priority, Count, .desc = F)
  ) %>%
  arrange(Type, desc(Count))

# PLOT

ccp_intersection_graph <- ggplot(intersection_long_modified, aes(x = Priority, y = Count, fill = Type)) +
  geom_col(position = position_dodge(width = 0.9)) +
  coord_flip() +
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Intersection of Cross-Cutting Priorities",
    x = "Combination of Cross-cutting Priorities",
    y = "No. of Solutions"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )

figure_label_ccp_intersection <- "**Figure 7:** Bar chart visualising the various combinations of intersecting cross-cutting priorities addressed in the solutions catalogue. Differentiations are made between double, triple, and quadruple intersections of: climate change mitigation (CCM), climate change adaptation (CCA), biodiversity (BD), and social justice and inclusion (SJI)."

ccp_intersection <- ccp_intersection_graph +
  labs(caption = figure_label_ccp_intersection) +  # Add the caption
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

print(ccp_intersection)


####################################################
####################################################
# Step 1: Create a column to represent each triple and the quadruple intersection

# Mutate the dataset to convert the cross-cutting priorities to binary and create columns for each intersection type
ccp_intersections <- ccp_filtered %>%
  mutate(
    cc_adaptation = as.numeric(cc_adaptation == "Y"),
    cc_mitigation = as.numeric(cc_mitigation == "Y"),
    biodiversity = as.numeric(biodiversity == "Y"),
    social_justice = as.numeric(social_justice == "Y"),
    # Create a new column for each intersection type
    CCM_CCA_BD = cc_mitigation & cc_adaptation & biodiversity,
    CCM_CCA_SJI = cc_mitigation & cc_adaptation & social_justice,
    CCM_BD_SJI = cc_mitigation & biodiversity & social_justice,
    CCA_BD_SJI = cc_adaptation & biodiversity & social_justice,
    All_4 = cc_mitigation & cc_adaptation & biodiversity & social_justice
  )

# Step 2: Create subsets for each triple and the quadruple

ccp_CCM_CCA_BD <- ccp_intersections %>% filter(CCM_CCA_BD == 1)
ccp_CCM_CCA_SJI <- ccp_intersections %>% filter(CCM_CCA_SJI == 1)
ccp_CCM_BD_SJI <- ccp_intersections %>% filter(CCM_BD_SJI == 1)
ccp_CCA_BD_SJI <- ccp_intersections %>% filter(CCA_BD_SJI == 1)
ccp_All_4 <- ccp_intersections %>% filter(All_4 == 1)

# Step 3: Create a mapping of RDDs to abbreviations
rdd_abbreviations <- c( 
  "Sustainable multimodal mobility" = "SMM",
  "Energy transition and climate neutrality" = "ETCN",
  "Sustainable agrifood systems and ecosystem management" = "SASEM", 
  "Nature-based and cultural tourism" = "NBCT", 
  "Culture and cultural innovation" = "CCI",
  "Local services, health and wellbeing" = "LSHW"
)

# Step 4: Create a function to plot the bar chart for a given subset using abbreviations
plot_rdd_count <- function(data, title) {
  # Count the number of solutions per primary_rdd
  data %>%
    group_by(primary_rdd) %>%
    summarise(solution_count = n()) %>%
    mutate(
      primary_rdd_abbr = recode(primary_rdd, !!!rdd_abbreviations)  # Recode RDD to abbreviations
    ) %>%
    ggplot(aes(x = primary_rdd_abbr, y = solution_count, fill = primary_rdd_abbr)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +
    labs(
      title = title,
      x = "Primary RDD",
      y = "Number of Solutions"
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
          text = element_text(size = 12),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),)
  
}

# Step 5: Create the five plots
plot_CCM_CCA_BD <- plot_rdd_count(ccp_CCM_CCA_BD, "CCM + CCA + BD")
plot_CCM_CCA_SJI <- plot_rdd_count(ccp_CCM_CCA_SJI, "CCM + CCA + SJI")
plot_CCM_BD_SJI <- plot_rdd_count(ccp_CCM_BD_SJI, "CCM + BD + SJI")
plot_CCA_BD_SJI <- plot_rdd_count(ccp_CCA_BD_SJI, "CCA + BD + SJI")
plot_all_4 <- plot_rdd_count(ccp_All_4, "CCM + CCA + BD + SJI")



triples_graph <- (plot_CCM_CCA_BD / plot_CCM_CCA_SJI / plot_CCM_BD_SJI / plot_CCA_BD_SJI) +
  plot_layout(ncol = 2)

figure_label_ccp_triples <- "**Figure 8:** Bar chart visualising the triple intersectinos of cross-cutting priorities addressed, broken down by primary rural development driver"

ccp_triples <- triples_graph +
  labs(caption = figure_label_ccp_triples) +  # Add the caption
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

print(ccp_triples)



ccp_all_graph <- plot_all_4

figure_label_ccp_all <- "**Figure 9:** Bar chart visualising the solutions that address all four cross-cutting priorities, broken down by primary rural development driver"

ccp_all_final <- ccp_all_graph +
  labs(caption = figure_label_ccp_all) +  # Add the caption
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

print(ccp_all_final)








# Arrange the plots in a grid with 2 columns, and set guides = "collect" to manage legends uniformly
triples_graph <- (plot_CCM_CCA_BD / plot_CCM_CCA_SJI / plot_CCM_BD_SJI / plot_CCA_BD_SJI) +
  plot_layout(ncol = 2, guides = "collect")

# Add the main title and combine the graph and caption into a single layout, placing the caption beneath all plots
ccp_triples <- triples_graph + 
  plot_annotation(
    title = "Triple Intersections of Cross-Cutting Priorities Across Rural Development Drivers",  # Main title
    caption = figure_label_ccp_triples,
    theme = theme(
      plot.title = element_text(
        family = "Times New Roman",  # Font family
        size = 16,                   # Font size for title
        face = "bold",               # Bold the title
        hjust = 0.5,                 # Center the title
        margin = margin(b = 15)      # Add space below the title
      ),
      plot.caption = element_textbox_simple(
        family = "Times New Roman",  # Font family
        size = 12,                   # Font size for caption
        lineheight = 1.2,            # Adjust line spacing for readability
        margin = margin(t = 20),     # Add space above the caption
        width = unit(0.97, "npc")    # Set width to 97% of plot width
      ),
      plot.margin = margin(t = 20, b = 20, l = 10, r = 10)  # Adjust margins for title, plot area, and caption
    )
  )

print(ccp_triples)


