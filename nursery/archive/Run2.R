#Looking at Elsoms nuresry run 2 data

setwd('C:/dev/code/Petra')


library(readxl)
library(dplyr)
library(tidyr)
library(ggpubr) 
library(ggplot2)

#oak data
oak = read_xlsx('../../data/nursery/Run2.xlsx', sheet = 'Oak', col_names = T)

# 1️⃣ Filter data - bench 2 away from the door
plot_data <- oak %>%
  filter(Drought == "Non", !is.na(Delta_H))

# 2️⃣ Summary table for medians
oak_summary <- plot_data %>%
  group_by(Fungi) %>%
  summarise(
    mean_Delta_H   = mean(Delta_H, na.rm = TRUE),
    median_Delta_H = median(Delta_H, na.rm = TRUE),
    n_non_missing  = sum(!is.na(Delta_H)),
    .groups = "drop"
  )

# 3️⃣ Desired order of Fungi for comparisons
fungi_order <- c("heb", "lac", "lact", "pax", "scl", "sui")

# 4️⃣ Compute t-test p-values vs Control
pvals <- lapply(fungi_order, function(f) {
  t_res <- t.test(
    Delta_H ~ Fungi,
    data = plot_data %>% filter(Fungi %in% c("control", f))
  )
  data.frame(
    Fungi = f,
    p     = t_res$p.value
  )
}) %>% bind_rows()

# 5️⃣ Format p-values as thresholds
pvals <- pvals %>%
  mutate(
    label = case_when(
      p < 0.001 ~ "<0.001",
      p < 0.01  ~ "<0.01",
      p < 0.05  ~ "<0.05",
      p < 0.1   ~ "<0.1",
      TRUE      ~ "ns"
    )
  )

# 6️⃣ Automatically calculate y-position above each box
y_max <- plot_data %>% group_by(Fungi) %>% summarise(y_max = max(Delta_H, na.rm = TRUE))
pvals <- pvals %>% left_join(y_max, by = "Fungi") %>%
  mutate(y.position = y_max + 0.05 * (max(plot_data$Delta_H, na.rm = TRUE) - min(plot_data$Delta_H, na.rm = TRUE)))

# 7️⃣ Plot boxplot with medians and formatted p-values
ggplot(plot_data, aes(x = Fungi, y = Delta_H)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(data = oak_summary, aes(x = Fungi, y = median_Delta_H),
             color = "red", size = 3) +
  geom_text(data = pvals, aes(x = Fungi, y = y.position, label = label),
            color = "blue", size = 4) +
  labs(title = "Delta_H Bench 2",
       x = "Fungi",
       y = "Delta_H") +
  theme_minimal()

#============================================
# 1️⃣ Filter data - bench, next to the dor
plot_data <- oak %>%
  filter(Drought == "Drought", !is.na(Delta_H))

# 2️⃣ Summary table for medians
oak_summary <- plot_data %>%
  group_by(Fungi) %>%
  summarise(
    mean_Delta_H   = mean(Delta_H, na.rm = TRUE),
    median_Delta_H = median(Delta_H, na.rm = TRUE),
    n_non_missing  = sum(!is.na(Delta_H)),
    .groups = "drop"
  )

# 3️⃣ Desired order of Fungi for comparisons
fungi_order <- c("heb", "lac", "lact", "pax", "scl", "sui")

# 4️⃣ Compute t-test p-values vs Control
pvals <- lapply(fungi_order, function(f) {
  t_res <- t.test(
    Delta_H ~ Fungi,
    data = plot_data %>% filter(Fungi %in% c("control", f))
  )
  data.frame(
    Fungi = f,
    p     = t_res$p.value
  )
}) %>% bind_rows()

# 5️⃣ Format p-values as thresholds
pvals <- pvals %>%
  mutate(
    label = case_when(
      p < 0.001 ~ "<0.001",
      p < 0.01  ~ "<0.01",
      p < 0.05  ~ "<0.05",
      p < 0.1   ~ "<0.1",
      TRUE      ~ "ns"
    )
  )

# 6️⃣ Automatically calculate y-position above each box
y_max <- plot_data %>% group_by(Fungi) %>% summarise(y_max = max(Delta_H, na.rm = TRUE))
pvals <- pvals %>% left_join(y_max, by = "Fungi") %>%
  mutate(y.position = y_max + 0.05 * (max(plot_data$Delta_H, na.rm = TRUE) - min(plot_data$Delta_H, na.rm = TRUE)))

# 7️⃣ Plot boxplot with medians and formatted p-values
ggplot(plot_data, aes(x = Fungi, y = Delta_H)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(data = oak_summary, aes(x = Fungi, y = median_Delta_H),
             color = "red", size = 3) +
  geom_text(data = pvals, aes(x = Fungi, y = y.position, label = label),
            color = "blue", size = 4) +
  labs(title = "Delta_H Bench 1",
       x = "Fungi",
       y = "Delta_H") +
  theme_minimal()

#========================================================

#plot the two benches next to each other

# Desired x-axis order
fungi_order <- c("control", "heb", "lac", "lact", "pax", "scl", "sui")

# Prepare data
plot_data <- oak %>%
  filter(!is.na(Delta_H)) %>%
  mutate(
    Fungi = factor(Fungi, levels = fungi_order),
    # Combine Fungi type and Drought for coloring
    FungiTreatment = case_when(
      Fungi == "control" & Drought == "Drought" ~ "control_D",
      Fungi == "control" & Drought == "Non"     ~ "control_N",
      Fungi != "control" & Drought == "Drought" ~ "pellet_D",
      Fungi != "control" & Drought == "Non"     ~ "pellet_N"
    )
  )

# Define colors
colors <- c(
  "control_D" = "#E69F00", # dark orange
  "control_N" = "#FDD585", # light orange
  "pellet_D"   = "#009E73", # dark green
  "pellet_N"   = "#66CDAA"  # light green
)

# 1️⃣ Compute p-values vs Control
pvals <- lapply(unique(plot_data$Drought), function(d) {
  lapply(fungi_order[-1], function(f) {  # skip "control"
    subset_data <- plot_data %>%
      filter(Drought == d, Fungi %in% c("control", f))
    
    # Only run t-test if both groups are present
    if(length(unique(subset_data$Fungi)) == 2){
      t_res <- t.test(Delta_H ~ Fungi, data = subset_data)
      data.frame(
        Fungi = f,
        Drought = d,
        p = t_res$p.value
      )
    } else {
      data.frame(Fungi = f, Drought = d, p = NA)
    }
  }) %>% bind_rows()
}) %>% bind_rows() %>%
  mutate(
    # Convert p-values to labels
    label = case_when(
      p < 0.001 ~ "<0.001",
      p < 0.01  ~ "<0.01",
      p < 0.05  ~ "<0.05",
      p < 0.1   ~ "<0.1",
      TRUE      ~ "ns"
    ),
    # Create FungiTreatment column for dodge alignment
    FungiTreatment = case_when(
      Drought == "Drought" ~ "D",
      Drought == "Non"     ~ "N"
    )
  )

# Compute max y for labels
y_max <- max(plot_data$Delta_H, na.rm = TRUE)

# 2️⃣ Plot boxplots with custom colors and p-values
ggplot(plot_data, aes(x = Fungi, y = Delta_H, fill = FungiTreatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_text(
    data = pvals,
    aes(x = Fungi, y = y_max + 0.05*y_max, label = label, group = Drought),
    position = position_dodge(width = 0.8),
    vjust = 0
  ) +
  labs(x = "Fungi", y = "Delta_H") +
  theme_minimal() +
  scale_fill_manual(values = colors)

#==================================================================

#combine both sets of trays - a large group 0f 80 plants splits across 2 benches
# 1️⃣ Filter data - bench 2 away from the door
# Prepare data
library(dplyr)
library(ggplot2)

# Desired x-axis order
fungi_order <- c("control", "heb", "lac", "lact", "pax", "scl", "sui")

# Prepare data
plot_data <- oak %>%
  filter(!is.na(Delta_H)) %>%
  mutate(
    Fungi = factor(Fungi, levels = fungi_order),
    Treatment = ifelse(Fungi == "control", "control", "pellet")
  )

# Define colors
colors <- c(
  "control" = "#E69F00",  # orange
  "pellet"   = "#009E73"   # green
)

# Compute max y for positioning p-values
y_max <- max(plot_data$Delta_H, na.rm = TRUE)

# Compute p-values vs control (all data, ignoring Drought)
pvals <- lapply(fungi_order[-1], function(f) {  # skip "control"
  subset_data <- plot_data %>% filter(Fungi %in% c("control", f))
  if(length(unique(subset_data$Fungi)) == 2){
    t_res <- t.test(Delta_H ~ Fungi, data = subset_data)
    data.frame(Fungi = f, p = t_res$p.value)
  } else {
    data.frame(Fungi = f, p = NA)
  }
}) %>% bind_rows() %>%
  mutate(
    label = case_when(
      p < 0.001 ~ "<0.001",
      p < 0.01  ~ "<0.01",
      p < 0.05  ~ "<0.05",
      p < 0.1   ~ "<0.1",
      TRUE      ~ "ns"
    )
  )

ggplot(plot_data, aes(x = Fungi, y = Delta_H, fill = Treatment)) +
  geom_boxplot(width = 0.3) +
  geom_text(
    data = pvals,
    aes(x = Fungi, y = y_max + 0.05*y_max, label = label),
    vjust = 0,
    inherit.aes = FALSE   # important!
  ) +
  labs(title = "Change in height for Quercus robur",x = "Treatment", y = "Change in height (cm)") +
  theme_minimal() +
  scale_fill_manual(values = colors)

#bar chart??

ggplot(plot_data, aes(x = Fungi, y = Delta_H, fill = Treatment)) +
  stat_summary(fun = mean, geom = "col", width = 0.3, position = position_dodge()) +  # bars for means
  #stat_summary(fun.data = mean_se, geom = "errorbar", 
              # width = 0.2, position = position_dodge(0.6)) +  # error bars for SE

  labs(title = "Change in height for Quercus robur",
       x = "Treatment",
       y = "Change in height (cm)") +
  theme_minimal() +
  scale_fill_manual(values = colors)


#with points
ggplot(plot_data, aes(x = Fungi, y = Delta_H, fill = Treatment)) +
  stat_summary(fun = mean, geom = "col", width = 0.6, position = position_dodge()) +  # bars for means
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(0.6)) +  # standard error bars
  geom_jitter(color = "black",  # make points dark
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6),
              size = 1.5, alpha = 0.8) +  # individual points
  #geom_text(
   # data = pvals,
    #aes(x = Fungi, y = y_max + 0.05*y_max, label = label),
    #vjust = 0,
   # inherit.aes = FALSE
  #) +
  labs(title = "Average change in height for Quercus robur",
       x = "Treatment",
       y = "Change in height (cm)") +
  theme_minimal() +
  scale_fill_manual(values = colors)
#==================
