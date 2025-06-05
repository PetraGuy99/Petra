#make a bipartite plot of the emf to the tree species - to see
#how many associations of each tree to emf there are

setwd('C:/dev/code/Petra')
library(igraph)
library(ggraph)
library(tidyverse)
library(ggrepel)


data = read.csv('../../data/sitesummary.csv')

#make bar chart of number of tree spp 
#take the final 11 col names and the col sums = row 18

df = data[18,14:23]

df_long <- pivot_longer(df, cols = everything(), names_to = "Category", values_to = "Value")

ggplot(df_long, aes(x = Category, y = Value)) +
  geom_bar(stat = "identity", fill = "dark green", width = 0.5) +  # narrower bars, green fill
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Number of sites") +
  xlab("Tree spp")


#pie

df_long <- pivot_longer(df, cols = everything(), names_to = "Category", values_to = "Value")

# Add label and position columns
df_long <- df_long %>%
  mutate(Value = as.numeric(Value)) %>%  # Convert to numeric
  arrange(desc(Category)) %>%
  mutate(
    Percentage = Value / sum(Value) * 100,
    Label = paste0(Category, "\n", round(Percentage, 1), "%"),
    ypos = cumsum(Value) - 0.5 * Value
  )

# Pie chart with labels
ggplot(df_long, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label_repel(
    aes(y = ypos, label = Label),
    nudge_x = 1,
    show.legend = FALSE,
    segment.color = "grey30",
    direction = "y"
  ) +
  theme_void() +
  theme(
    text = element_text(size = 16),
    legend.position = "none"  # removes legend
  )


#now repeat for the emf 
df = data[18,6:13]

df_long <- pivot_longer(df, cols = everything(), names_to = "Category", values_to = "Value")
df_long$Value = as.numeric(df_long$Value)

ggplot(df_long, aes(x = Category, y = Value)) +
  geom_bar(stat = "identity", fill = "brown", width = 0.5) +
  theme_minimal(base_size = 16) +
  ylab("Number of sites") +
  xlab("EMF spp") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = 1:10)




#pie
df_long <- pivot_longer(df, cols = everything(), names_to = "Category", values_to = "Value")

# Add label and position columns
df_long <- df_long %>%
  mutate(Value = as.numeric(Value)) %>%  # Convert to numeric
  arrange(desc(Category)) %>%
  mutate(
    Percentage = Value / sum(Value) * 100,
    Label = paste0(Category, "\n", round(Percentage, 1), "%"),
    ypos = cumsum(Value) - 0.5 * Value
  )

# Pie chart with labels
ggplot(df_long, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label_repel(
    aes(y = ypos, label = Label),
    nudge_x = 1,
    show.legend = FALSE,
    segment.color = "grey30",
    direction = "y"
  ) +
  theme_void() +
  theme(
    text = element_text(size = 16),
    legend.position = "none"  # removes legend
  )

###bipaetite plot


df = read.csv('../../data/bipartitedata.csv')

#delete unnecessary cols

df = df[,25:26]
# Create edges
edges <- df %>%
  select(emf, host) %>%
  group_by(emf, host) %>%
  summarise(weight = n(), .groups = "drop") %>%
  rename(from = emf, to = host)

# Create a list of unique nodes
nodes <- data.frame(name = unique(c(edges$from, edges$to)))
nodes$type <- nodes$name %in% edges$to

# Define the type: TRUE if it's a tree, FALSE if it's a fungus
nodes$type <- nodes$name %in% edges$to

# Create the graph
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)


ggraph(g, layout = 'bipartite') +
  geom_edge_link(aes(width = weight), alpha = 0.6, color = "gray40") +
  geom_node_point(aes(color = type), size = 6) +
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 6,               # 🔺 Increase text size here
    family = "serif"
  ) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_color_manual(values = c("forestgreen", "sienna3")) +
  theme_void(base_size = 18) +  # 🔺 Increase base font size here too
  theme(
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "white", color = NA)
  )
