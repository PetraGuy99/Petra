setwd('C:/dev/code/Petra')

library(dplyr)
library(ggplot2)
library(ggrepel)

library(tidyr)

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

#merge wet dead ash with wet, open space with open ash die back


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

library(purrr)
dfs = list(richness_by_habitat, nitrogen_by_habitat, moisture_by_habitat,
           light_by_habitat, acidity_by_habitat)

merged_df <- dfs %>%
  map(~ select(.x, -`NumPlots`)) %>%
  reduce(full_join, by = "Habitat")

merged_df_clean = merged_df[,-c(2,4,6,8,10)]

colnames(merged_df_clean) = c('Habitat','Richness','Nitrogen','Moisture','Light','Acidity')

library(tidyr)

long_df <- merged_df_clean %>%
  pivot_longer(cols = c(Light, Moisture, Acidity, Nitrogen),
               names_to = "Variable",
               values_to = "Value")



# Plot Richness vs each variable with a smooth trend line, faceted
ggplot(long_df, aes(x = Value, y = Richness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Richness vs Environmental Variables",
       x = "Environmental Variable",
       y = "Richness")


# add R2 and p
library(ggpubr)       

ggplot(long_df, aes(x = Value, y = Richness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x.npc = "left", label.y.npc = 0.9, size = 4) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Richness vs Ellenberg Values",
       x = "Ellenberg Values per plot",
       y = "Richness")


ggplot(long_df, aes(x = Value, y = Richness, colour = Habitat)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(y = "Mean Plot Richness") +
  theme_bw()


###NOW DO SOMETHING WITH DEAD WOOD, TREE SAPLINGS ETC AND BARE GROUND####
neat_df <- wca_df %>%
  pivot_longer(
    cols = c(Seedlings, Saplings, Young_trees, Semi.mature, Mature),
    names_to = "Stage",
    values_to = "Count"
  ) %>%
  group_by(Habitat, Stage) %>%
  summarise(
    Total = sum(Count, na.rm = TRUE),
    NumPlots = n(),
    PerPlot = Total / NumPlots,
    .groups = "drop"
  )

neat_df$Stage <- factor(
  neat_df$Stage,
  levels = c("Seedlings", "Saplings", "Young_trees", "Semi.mature", "Mature")
)

# Reorder habitats by total trees per plot (descending)
neat_df <- neat_df %>%
  group_by(Habitat) %>%
  mutate(TotalPerPlot = sum(PerPlot, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Habitat = reorder(Habitat, -TotalPerPlot))

# Nature-inspired palette
stage_cols <- c(
  "Seedlings"    = "#FFFFE0" , #LightYellow
  "Saplings"     = "#74C476", # medium green
  "Young_trees"  = "#31A354", # dark green
  "Semi.mature"  = "#8C6BB1", # muted purple
  "Mature"       = "#54278F"  # deep purple
)

# Plot
ggplot(neat_df, aes(x = Habitat, y = PerPlot, fill = Stage)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", linewidth = 0.2) +
  scale_fill_manual(values = stage_cols) +
  labs(
    x = "Habitat",
    y = "Trees per Plot",
    fill = "Tree Stage",
    title = "Tree Stage Composition per Habitat"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

#repeat without Open because it scales everything weird
neat_df$Stage <- factor(
  neat_df$Stage,
  levels = c("Seedlings", "Saplings", "Young_trees", "Semi.mature", "Mature")
)

# Filter out "Open" and reorder habitats by total trees per plot
neat_df_filtered <- neat_df %>%
  filter(Habitat != "O") %>%
  group_by(Habitat) %>%
  mutate(TotalPerPlot = sum(PerPlot, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Habitat = reorder(Habitat, -TotalPerPlot))

# Nature-inspired palette
stage_cols <- c(
  "Seedlings"    = "#FFFFE0" , #LightYellow
  "Saplings"     = "#74C476",
  "Young_trees"  = "#31A354",
  "Semi.mature"  = "#8C6BB1",
  "Mature"       = "#54278F"
)

# Plot
ggplot(neat_df_filtered, aes(x = Habitat, y = PerPlot, fill = Stage)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", linewidth = 0.2) +
  scale_fill_manual(values = stage_cols) +
  labs(
    x = "Habitat",
    y = "Trees per Plot",
    fill = "Tree Stage",
    title = "Tree Stage Composition per Habitat"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
    
  )
    

#########################################################################
 #redo the ellenbergs charts, but dont group by habitat, just use plot values

make_plot <- function(xvar, yvar = "Richness", data) {
  ggscatter(
    data, x = xvar, y = yvar,
    add = "reg.line",
    conf.int = TRUE,
    color = "blue",
    shape = 19
  ) +
    stat_cor(
      method = "pearson",
      label.x = min(data[[xvar]]) * 1.05,
      label.y = max(data[[yvar]]) * 0.95,
      aes(label = paste0("R^2 == ", signif(..r..^2, 3), 
                         "*', p = '*", signif(..p.., 3))),
      parse = TRUE,
      size = 6
    ) +
    labs(x = xvar, y = yvar) +
    theme_minimal()+
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),  # x-axis title
      axis.title.y = element_text(size = 20, face = "bold"),  # y-axis title
      axis.text = element_text(size = 14)                      # axis tick labels
    )
}


# Make the 4 plots
p1 <- make_plot("Moisture", data = wca_df)
p2 <- make_plot("Light", data = wca_df)
p3 <- make_plot("Nitrogen", data = wca_df)
p4 <- make_plot("Acidity", data = wca_df)

# Arrange them in a grid
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

#redo this but remove the zero richness because its dominating the data?
df = wca_df %>% filter(Richness != 0)

# Make the 4 plots
p1 <- make_plot("Moisture", data = df)
p2 <- make_plot("Light", data = df)
p3 <- make_plot("Nitrogen", data = df)
p4 <- make_plot("Acidity", data = df)

# Arrange them in a grid
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


#outliers???######################
make_plot_with_outliers <- function(xvar, yvar = "Richness", data) {
  model <- lm(as.formula(paste(yvar, "~", xvar)), data = data)
  res <- rstudent(model)
  data$outlier <- abs(res) > 3
  
  # Stats
  r_val <- cor(data[[xvar]], data[[yvar]], use = "complete.obs")
  r2_val <- signif(r_val^2, 3)
  p_val <- signif(summary(model)$coefficients[2,4], 3)
  
  ggscatter(
    data, x = xvar, y = yvar,
    color = "outlier",
    palette = c("FALSE" = "blue", "TRUE" = "red"),
    shape = 19,
    add = "reg.line",
    conf.int = TRUE
  ) +
    annotate("text", 
             x = min(data[[xvar]]) * 1.05, 
             y = max(data[[yvar]]) * 0.95,
             label = paste0("R² = ", r2_val, ", p = ", p_val),
             hjust = 0) +
    labs(x = xvar, y = yvar) +
    theme_minimal()
}
df = wca_df
# Make the 4 plots
p1 <- make_plot_with_outliers("Moisture", data = df)
p2 <- make_plot_with_outliers("Light", data = df)
p3 <- make_plot_with_outliers("Nitrogen", data = df)
p4 <- make_plot_with_outliers("Acidity", data = df)

# Arrange in a 2x2 grid
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


####dead wood
neat_df <- wca_df %>%
  pivot_longer(
    cols = c(Logs,Standing,Branches),
    names_to = "Form",
    values_to = "Count"
  ) %>%
  group_by(Habitat, Form) %>%
  summarise(
    Total = sum(Count, na.rm = TRUE),
    NumPlots = n(),
    PerPlot = Total / NumPlots,
    .groups = "drop"
  )

neat_df$Stage <- factor(
  neat_df$Form,
  levels = c("Logs", "Standing","Branch")
)

# Reorder habitats by total trees per plot (descending)
neat_df <- neat_df %>%
  group_by(Habitat) %>%
  mutate(TotalPerPlot = sum(PerPlot, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Habitat = reorder(Habitat, -TotalPerPlot))

# Nature-inspired palette
stage_cols <- c(
  "Logs"    = "#FFFFE0" , #LightYellow
   "Branches"  = "#31A354", # dark green
    "Standing"       = "#54278F"  # deep purple
)

# Plot
ggplot(neat_df, aes(x = Habitat, y = PerPlot, fill = Form)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", linewidth = 0.2) +
  scale_fill_manual(values = stage_cols) +
  labs(
    x = "Habitat",
    y = "Dead Wood",
    fill = "Form",
    title = "Dead Wood Composition per Habitat"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

##################################

df_long <- melt(wca_df, id.vars = "Light",
                measure.vars = c("Seedlings", "Saplings", "Semi.mature", "Mature"),
                variable.name = "Stage",
                value.name = "Count")

# Scatter plot with regression lines for each stage
ggplot(df_long, aes(x = Light, y = Count, color = Stage)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +   # linear regression with confidence interval
  theme_minimal() +
  labs(x = "Light", y = "Count", color = "Stage") +
  theme(
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


# Function to make one scatter plot with regression line, R² and p-value
make_stage_plot <- function(stage, xvar = "Light", data) {
  # Fit linear model
  model <- lm(as.formula(paste(stage, "~", xvar)), data = data)
  
  # Calculate stats
  r_val <- cor(data[[xvar]], data[[stage]], use = "complete.obs")
  r2_val <- signif(r_val^2, 3)
  p_val <- signif(summary(model)$coefficients[2,4], 3)
  
  ggplot(data, aes_string(x = xvar, y = stage)) +
    geom_point(color = "blue", size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "red", size = 1) +
    annotate("text", x = min(data[[xvar]]) * 1.05, 
             y = max(data[[stage]]) * 0.95,
             label = paste0("R² = ", r2_val, ", p = ", p_val),
             hjust = 0, size = 5) +
    labs(x = "Light", y = stage) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 12)
    )
}

# Make the four plots
p1 <- make_stage_plot("Seedlings", data = df)
p2 <- make_stage_plot("Saplings", data = df)
p3 <- make_stage_plot("Semi.mature", data = df)
p4 <- make_stage_plot("Mature", data = df)

# Arrange them in a 2x2 grid
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


#by habitat

avg_df <- wca_df %>%
  group_by(Habitat) %>%
  summarise(
    Light = mean(Light, na.rm = TRUE),
    Seedlings = mean(Seedlings, na.rm = TRUE),
    Saplings = mean(Saplings, na.rm = TRUE),
    Semi.mature = mean(Semi.mature, na.rm = TRUE),
    Mature = mean(Mature, na.rm = TRUE)
  )

# Step 2: Reshape to long format
long_df <- melt(avg_df,
                id.vars = c("Habitat", "Light"),
                measure.vars = c("Seedlings", "Saplings", "Semi.mature", "Mature"),
                variable.name = "Stage",
                value.name = "Value")

# Step 3: Plot
ggplot(long_df, aes(x = Light, y = Value, colour = Habitat)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~ Stage, scales = "free_y") +
  theme_bw() +
  labs(x = "Light", y = "Mean number of trees") +
  theme(
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

#STACKED BAR CHARTS FOR THE CONDITIONS SCORES 

condition = read.csv('../../data/condition.csv')

neat_df <- condition %>%
  pivot_longer(
    cols = c(Deadwood, AWI,Age_class),
    names_to = "Section",
    values_to = "Count"
  ) %>%
  group_by(Habitat, Section) %>%
  summarise(
    Total = sum(Count, na.rm = TRUE),
    NumPlots = n(),
    PerPlot = Total / NumPlots,
    .groups = "drop"
  )

neat_df$Stage <- factor(
  neat_df$Section,
  levels = c("Deadwood", "AWI","Age_class")
)

# Reorder habitats by total trees per plot (descending)
neat_df <- neat_df %>%
  group_by(Habitat) %>%
  mutate(TotalPerPlot = sum(PerPlot, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Habitat = reorder(Habitat, -TotalPerPlot))

# Nature-inspired palette
stage_cols <- c(
  "AWI"    = "#FFFFE0" , #LightYellow
  "Deadwood"  = "#31A354", # dark green
  "Age_class"       = "#54278F"  # deep purple
)

# Plot
ggplot(neat_df, aes(x = Habitat, y = PerPlot, fill = Section)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", linewidth = 0.2) +
  scale_fill_manual(values = stage_cols) +
  labs(
    x = "Habitat",
    y = "Total Score",
    fill = "Section",
    title = ""
  ) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )
