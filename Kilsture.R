#make a bipartite plot of the emf to the tree species - to see
#how many associations of each tree to emf there are

setwd('C:/dev/code/Petra')
library(igraph)
library(ggraph)
library(tidyverse)
library(ggrepel)


data = read.csv('../../data/kilsture_habs_areas.csv')


#pie
df_long <- pivot_longer(data, cols = everything(), names_to = "Category", values_to = "Value")

# Add labels (you can customize this)
data$Label <- paste0(data$Habitat, "\n", round(100 * data$Area_ha / sum(data$Area_ha), 1), "%")

# Arrange and calculate cumulative values
data <- data %>%
  arrange(desc(Habitat))

# Basic pie chart with legend
ggplot(data, aes(x = "", y = Area_ha, fill = Habitat)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(
    legend.position = "right",  # or "bottom" or "top"
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  guides(fill = guide_legend(title = "Habitat Type"))