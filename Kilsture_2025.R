setwd('C:/dev/code/Petra')

library(dplyr)
library(ggplot2)
library(ggrepel)

#exploring the Kilsture data

AWI = read.csv('../../data/AWI.csv')
species_df = read.csv('../../data/compiled_plant_matrix.csv')
wca_df = read.csv('../../data/compiled_WCA.csv')

species.found = colnames(species_df[,-c(1:3)])
AWI_spp = as.vector(AWI$Species) # make a vector to find intersection
species.found = gsub("_"," ", species.found)
AWI_found = intersect(AWI_spp, species.found)

#number of habitats
habitat_counts = wca_df %>% count(Habitat)

habitat_counts <- habitat_counts %>%
  arrange(desc(Habitat)) %>%
  mutate(
    prop = n / sum(n),
    ypos = cumsum(prop) - 0.5 * prop,
    label = paste0(Habitat, " (", n, ")")  # e.g., Woodland (24)
  )

# 3. Create pie chart with labels outside
ggplot(habitat_counts, aes(x = "", y = n, fill = Habitat)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label_repel(
    aes(
      y = ypos * sum(n),
      label = label
    ),
    nudge_x = 1,       # pushes labels outside
    show.legend = FALSE,
    segment.color = "grey50"
  ) +
  theme_void() +
  theme(legend.position = "none")+
  labs(title = "Habitat Types",
       fill = "Habitat")


#richness bu plot types, but divided by number of plots in the group
richness_by_habitat <- wca_df %>%
  group_by(Habitat) %>%
  summarise(
    TotalRichness = sum(Richness, na.rm = TRUE),
    NumPlots = n(),
    RichnessPerPlot = TotalRichness / NumPlots
  )

richness_by_habitat <- richness_by_habitat %>%
  arrange(desc(RichnessPerPlot)) %>%
  mutate(Habitat = factor(Habitat, levels = Habitat))

ggplot(richness_by_habitat, aes(x = Habitat, y = RichnessPerPlot)) +
  geom_bar(stat = "identity", fill = "darkgreen", width = 0.5) +
  theme_minimal() +
  labs(title = "Average plot richness by habitat",
       x = "Habitat",
       y = "Richness per Plot") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

#do the same thing for Ellenbergs
light_by_habitat <- wca_df %>%
  group_by(Habitat) %>%
  summarise(
    TotalLight = sum(Light, na.rm = TRUE),
    NumPlots = n(),
    LightPerPlot = TotalLight / NumPlots
  )

light_by_habitat <- light_by_habitat %>%
  arrange(desc(LightPerPlot)) %>%
  mutate(Habitat = factor(Habitat, levels = Habitat))


moisture_by_habitat <- wca_df %>%
  group_by(Habitat) %>%
  summarise(
    TotalMoisture = sum(Moisture, na.rm = TRUE),
    NumPlots = n(),
    MoisturePerPlot = TotalMoisture / NumPlots
  )

acidity_by_habitat = wca_df %>%
  group_by(Habitat) %>%
  summarise(
    TotalAcidity = sum(Acidity, na.rm = TRUE),
    NumPlots = n(),
    AcidityPerPlot = TotalAcidity / NumPlots
  )

nitrogen_by_habitat = wca_df %>%
  group_by(Habitat) %>%
  summarise(
    TotalNitrogen = sum(Nitrogen, na.rm = TRUE),
    NumPlots = n(),
    NitrogenPerPlot = TotalNitrogen / NumPlots
  )

ellenbergs_all_df = cbind.data.frame(light_by_habitat$Habitat, 
                                     light_by_habitat$LightPerPlot, 
                                     moisture_by_habitat$MoisturePerPlot,
                                     acidity_by_habitat$AcidityPerPlot, 
                                     nitrogen_by_habitat$NitrogenPerPlot)
colnames(ellenbergs_all_df) = c('Habitat','Light','Moisture','Acidity','Nitrogen')

library(tidyr)

long_df <- ellenbergs_all_df %>%
  pivot_longer(cols = c(Light, Moisture, Acidity, Nitrogen),
               names_to = "Variable",
               values_to = "Value")

# 3. Plot stacked bar chart
ggplot(long_df, aes(x = Habitat, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  labs(title = "Stacked Bar by Habitat",
       x = "Habitat",
       y = "Ellenberg per plot") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

#add richness and do correlation??
richness_ellenbergs = cbind.data.frame(ellenbergs_all_df, richness_by_habitat$RichnessPerPlot)
names(richness_ellenbergs)[names(richness_ellenbergs) == 'richness_by_habitat$RichnessPerPlot'] = 'Richness'


df_long <- richness_ellenbergs %>%
  pivot_longer(cols = c(Light, Moisture, Acidity, Nitrogen),
               names_to = "Variable",
               values_to = "Value")

# Plot Richness vs each variable with a smooth trend line, faceted
ggplot(df_long, aes(x = Value, y = Richness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Richness vs Environmental Variables",
       x = "Environmental Variable",
       y = "Richness")

library(ggpubr)       

ggplot(df_long, aes(x = Value, y = Richness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x.npc = "left", label.y.npc = 0.9, size = 4) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Richness vs Ellenberg Values",
       x = "Ellenberg Values per plot",
       y = "Richness")
