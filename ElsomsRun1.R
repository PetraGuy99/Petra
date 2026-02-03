setwd('C:/dev/code/Petra')


library(readxl)
library(dplyr)
library(tidyr)
library(ggpubr) 
library(ggplot2)
#Exploration of nursery Run 1

#read the inital heights raw data
initial = read_xlsx('../../data/nursery/NurseryRun1.xlsx', sheet = 'Rdata_i', col_names = FALSE)
final = read_xlsx('../../data/nursery/NurseryRun1.xlsx', sheet = 'Rdata_f',col_names = FALSE)

#decompress the data frame to get the heights out of the groups
#take the body of the data frame - not the first three rows for treatment labels
body = initial[-c(1:3),]


# Make row 1 the column names
colnames(body) <- as.character(body[1, ])

# Remove the first row since it's now the header
body <- body[-1, ]

body <- data.frame(lapply(body, function(x) as.numeric(as.character(x))))

# Optionally reset row names
rownames(body) <- NULL


# heights column
heights <- body$NAME

# treatment columns
treatments <- body[, -1]

# replace NA with 0
treatments[is.na(treatments)] <- 0

# round counts to integers (rep requires integers)
treatments <- round(treatments)

# expand each treatment
expanded_list <- lapply(treatments, function(col) {
  # keep only positive counts
  pos_idx <- which(col > 0)
  rep(heights[pos_idx], times = col[pos_idx])
})

# Because each treatment may have different number of trees, pad with NA
max_len <- max(sapply(expanded_list, length))
expanded_list_padded <- lapply(expanded_list, function(x) c(x, rep(NA, max_len - length(x))))

# combine into dataframe
expanded_df <- as.data.frame(expanded_list_padded)
colnames(expanded_df) <- colnames(treatments)

#add the initial rows back on which list the tree species and treatment long
row12 = as.data.frame(initial[c(1,2,3),-1])
colnames(row12) =  colnames(expanded_df)
initial_wide = rbind(row12, expanded_df)

#make long
# Extract header info
species         <- as.character(initial_wide[1, ])
fertiliser_lvl  <- as.character(initial_wide[2, ])
treatment       <- as.character(initial_wide[3, ])

# Extract the actual data (tree heights)
data_rows <- initial_wide[-(1:3), ]

colnames(data_rows) <- paste0("V", seq_len(ncol(data_rows)))


# Add header info as columns for later use
metadata <- data.frame(
  Species = species,
  Fertiliser = fertiliser_lvl,
  Treatment = treatment
)

long_df_i <- data_rows %>%
  mutate(TreeID = row_number()) %>%    # optional: row number as ID
  pivot_longer(
    cols = -TreeID,
    names_to = "Column",
    values_to = "Height"
  ) %>%
  # Add metadata by matching Column
  mutate(
    Species = species[as.numeric(gsub("V", "", Column))],
    Fertiliser = fertiliser_lvl[as.numeric(gsub("V", "", Column))],
    Treatment = treatment[as.numeric(gsub("V", "", Column))]
  ) %>%
  select(TreeID, Species, Fertiliser, Treatment, Height)
###############################################################################

#now make the final height data long, that was measured per tree, so dont need to decompress
colnames(final) = as.character(final[4,])
final = final[-4,]

# Extract header info
species         <- as.character(final[1, ])
fertiliser_lvl  <- as.character(final[2, ])
treatment       <- as.character(final[3, ])

# Extract the actual data (tree heights)
data_rows <- final[-(1:3), ]

colnames(data_rows) <- paste0("V", seq_len(ncol(data_rows)))


# Add header info as columns for later use
metadata <- data.frame(
  Species = species,
  Fertiliser = fertiliser_lvl,
  Treatment = treatment
)

long_df_f <- data_rows %>%
  mutate(TreeID = row_number()) %>%    # optional: row number as ID
  pivot_longer(
    cols = -TreeID,
    names_to = "Column",
    values_to = "Height"
  ) %>%
  # Add metadata by matching Column
  mutate(
    Species = species[as.numeric(gsub("V", "", Column))],
    Fertiliser = fertiliser_lvl[as.numeric(gsub("V", "", Column))],
    Treatment = treatment[as.numeric(gsub("V", "", Column))]
  ) %>%
  select(TreeID, Species, Fertiliser, Treatment, Height)
###############################################################################

#above has munged the data sheets into long dfs for final and initial heights
#merge these so can do change in height


avg_i <- long_df_i %>%
  group_by(Species, Fertiliser, Treatment) %>%
  summarise(InitialHeight = mean(as.numeric(Height), na.rm = TRUE), .groups = "drop")

avg_f <- long_df_f %>%
  group_by(Species, Fertiliser, Treatment) %>%
  summarise(FinalHeight = mean(as.numeric(Height), na.rm = TRUE), .groups = "drop")

# Convert heights to numeric and remove non-numeric entries
long_df_i <- long_df_i %>%
  mutate(Height = as.numeric(as.character(Height))) %>%
  filter(!is.na(Height))

long_df_f <- long_df_f %>%
  mutate(Height = as.numeric(as.character(Height))) %>%
  filter(!is.na(Height))  # removes dead trees

# Compute averages and merge
merged_df <- long_df_i %>%
  group_by(Species, Fertiliser, Treatment) %>%
  summarise(InitialHeight = mean(Height, na.rm = TRUE), .groups = "drop") %>%
  full_join(
    long_df_f %>%
      group_by(Species, Fertiliser, Treatment) %>%
      summarise(FinalHeight = mean(Height, na.rm = TRUE), .groups = "drop"),
    by = c("Species", "Fertiliser", "Treatment")
  ) %>%
  mutate(HeightChange = FinalHeight - InitialHeight)

merged_df <- merged_df %>%
  mutate(Fertiliser = factor(Fertiliser, levels = c("0.0", "25.0", "50.0", "75.0", "100.0")))
###############################################################################

# Add a column for time point
long_df_i2 <- long_df_i %>% mutate(Time = "Initial")
long_df_f2 <- long_df_f %>% mutate(Time = "Final")

# Combine
combined_df <- bind_rows(long_df_i2, long_df_f2)

# Convert Height to numeric
combined_df <- combined_df %>%
  mutate(
    Height = as.numeric(as.character(Height)),
    Fertiliser = factor(Fertiliser, levels = c("0","25","50","75","100")) # ensure correct order
  )

##################################################################################

# Boxplot by Fertiliser, split by Time
ggplot(combined_df, aes(x = Fertiliser, y = Height, fill = Time)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +  # side-by-side boxes
  facet_grid(Species ~ Treatment) +
  theme_bw() +
  labs(title = "Tree Heights by Fertiliser, Initial vs Final",
       y = "Height",
       x = "Fertiliser Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 1: BP + PS
combined_df %>%
  filter(Species %in% c("BP", "PS")) %>%
  ggplot(aes(x = Fertiliser, y = Height, fill = Time)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_grid(Species ~ Treatment) +
  theme_bw() +
  labs(title = "Tree Heights by Fertiliser (BP & PS)",
       y = "Height",
       x = "Fertiliser Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: SITKA
combined_df %>%
  filter(Species == "SITKA") %>%
  ggplot(aes(x = Fertiliser, y = Height, fill = Time)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_grid(Species ~ Treatment) +
  theme_bw() +
  labs(title = "Tree Heights by Fertiliser (SITKA)",
       y = "Height",
       x = "Fertiliser Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################################################

#explore the above with lm to see if there is a correlation
merged_df <- merged_df %>%
  mutate(
    Fertiliser = as.numeric(as.character(Fertiliser)),
    HeightChange = as.numeric(HeightChange)
  )

# BP + PS
# Compute lm, R2, p for each Species C Treatment
library(purrr)

lm_stats <- merged_df %>%
  group_by(Species, Treatment) %>%
  summarise(
    lm_fit = list(lm(HeightChange ~ Fertiliser, data = cur_data())),
    .groups = "drop"
  ) %>%
  mutate(
    r2 = map_dbl(lm_fit, ~ summary(.x)$r.squared),
    pval = map_dbl(lm_fit, ~ summary(.x)$coefficients[2,4]),
    eq_label = paste0("RB2=", round(r2,2), ", p=", signif(pval,3))
  ) %>%
  select(-lm_fit, -r2, -pval)

plot_df <- merged_df %>%
  left_join(lm_stats, by = c("Species","Treatment"))

# Compute maximum HeightChange per Species C Treatment for label placement
label_positions <- merged_df %>%
  group_by(Species, Treatment) %>%
  summarise(label_y = max(HeightChange, na.rm = TRUE), .groups = "drop") %>%
  left_join(lm_stats, by = c("Species", "Treatment"))

# Merge labels with data
plot_df <- merged_df %>%
  left_join(label_positions, by = c("Species", "Treatment"))

# Plot
ggplot(plot_df, aes(x = Fertiliser, y = HeightChange)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, color = "blue") +
  geom_text(aes(x = 20, y = label_y, label = eq_label),
            inherit.aes = FALSE, hjust = 0) +
  facet_grid(Species ~ Treatment) +
  theme_bw() +
  labs(
    title = "Height Change vs Fertiliser",
    x = "Fertiliser Level",
    y = "Height Change"
  )


#############################################################################

#table of height changes
# Merge initial and final averages
height_change_df <- avg_i %>%
  inner_join(
    avg_f,
    by = c("Species", "Fertiliser", "Treatment")
  ) %>%
  mutate(
    HeightChange = FinalHeight - InitialHeight
  ) %>%
  arrange(Species, Treatment, Fertiliser)

# View the resulting table
height_change_df

####

bp_df <- height_change_df %>% filter(Species == "BP")

# Compute relative change compared to control
# We assume 'co' is the main control; 'cb' is another control you might also compare to
bp_relative <- bp_df %>%
  group_by(Fertiliser) %>%
  mutate(
    ControlCO = HeightChange[Treatment == "co"],        # change in co
    ControlCB = HeightChange[Treatment == "cb"],        # change in cb (if you want)
    Change_vs_CO = HeightChange - ControlCO,
    Change_vs_CB = HeightChange - ControlCB
  ) %>%
  ungroup()

# Keep only the treatments of interest (excluding co and cb if desired)
# Ensure Fertiliser is a factor in the right order
bp_relative <- bp_relative %>%
  mutate(Fertiliser = factor(Fertiliser, levels = c("0.0","25.0","50.0","75.0","100.0")))

# Line plot: change relative to co
ggplot(bp_relative, aes(x = Fertiliser, y = Change_vs_CO, color = Treatment, group = Treatment)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_bw() +
  labs(
    title = "BP Height Change Compared to Control (co)",
    x = "Fertiliser Level",
    y = "Height Change vs co",
    color = "Treatment"
  ) +
  scale_color_manual(values = c("heb" = "blue", "pax" = "green", "cb" = "red")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") 

################################################################################


#pine
# Filter PS and SUI treatment
ps_df <- height_change_df %>%
  filter(Species == "PS", Treatment %in% c("sui","cb","co")) %>%
  mutate(Fertiliser = factor(Fertiliser, levels = c("0.0","25.0","50.0","75.0","100.0")))

# Compute relative change vs co
ps_relative <- ps_df %>%
  group_by(Fertiliser) %>%
  mutate(
    ControlCO = HeightChange[Treatment == "co"],  # control baseline
    Change_vs_CO = HeightChange - ControlCO
  ) %>%
  ungroup() %>%
  filter(Treatment != "co")  # exclude co itself from plot

ggplot(ps_relative, aes(x = Fertiliser, y = Change_vs_CO, color = Treatment, group = Treatment)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_bw() +
  labs(
    title = "PS Height Change Compared to Control (co) - SUI & CB",
    x = "Fertiliser Level",
    y = "Height Change vs co",
    color = "Treatment"
  ) +
  scale_color_manual(values = c("sui" = "red", "cb" = "blue")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  

#sitka
sitka_df <- height_change_df %>%
  filter(Species == "SITKA", Treatment %in% c("heb","pax","sui","cb","co")) %>%
  mutate(Fertiliser = factor(Fertiliser, levels = c("0.0","25.0","50.0","75.0","100.0")))

# Compute relative change vs co
sitka_relative <- sitka_df %>%
  group_by(Fertiliser) %>%
  mutate(
    ControlCO = HeightChange[Treatment == "co"],  # control baseline
    Change_vs_CO = HeightChange - ControlCO
  ) %>%
  ungroup() %>%
  filter(Treatment != "co")  # exclude co itself from plot

ggplot(sitka_relative, aes(x = Fertiliser, y = Change_vs_CO, color = Treatment, group = Treatment)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_bw() +
  labs(
    title = "SITKA Height Change Compared to Control (co)",
    x = "Fertiliser Level",
    y = "Height Change vs co",
    color = "Treatment"
  ) +
  scale_color_manual(values = c("heb" = "blue", "pax" = "green", "sui" = "red", "cb" = "purple")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # baseline at control

################################################################################
#combine all the fertiliser levels as if they are all one treatment


# Add Species C Treatment C Fertiliser info
# Merge avg_i and avg_f if needed to get HeightChange
height_change_df <- height_change_df %>%
  mutate(
    Fertiliser = factor(Fertiliser, levels = c("0.0","25.0","50.0","75.0","100.0"))
  )

# Function to compute average HeightChange ignoring fertilizer
average_across_fert <- height_change_df %>%
  group_by(Species, Treatment) %>%
  summarise(
    AvgHeightChange = mean(HeightChange, na.rm = TRUE),
    .groups = "drop"
  )
control_df <- average_across_fert %>%
  filter(Treatment == "co") %>%
  select(Species, ControlChange = AvgHeightChange)

# Join to get relative change
relative_df <- average_across_fert %>%
  filter(Treatment != "co") %>%  # only treatments to compare
  left_join(control_df, by = "Species") %>%
  mutate(Change_vs_CO = AvgHeightChange - ControlChange)

plot_df <- relative_df %>%
  filter(
    (Species == "BP" & Treatment %in% c("heb","pax")) |
      (Species == "PS" & Treatment == "sui") |
      (Species == "SITKA" & Treatment %in% c("heb","pax","sui"))
  )
ggplot(plot_df, aes(x = Treatment, y = Change_vs_CO, fill = Treatment)) +
  geom_col(position = "dodge") +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Average Height Change Compared to Control Across Fertiliser Levels",
    x = "Treatment",
    y = "Height Change vs co",
    fill = "Treatment"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

#################################################################################
#since there appears to be no correlation with fertilizer, which we expected because the
#adding back in process was unlikely to give an even addition

#have already computed the height_change_df
#lets look at birch
birch_height_change = height_change_df %>% filter(Species == 'BP')
names(birch_height_change)[names(birch_height_change) == "Fertiliser"] <- "Rep"
#view as boxplot with fertiliser as reps

ggplot(birch_height_change, aes(x = Treatment, y = HeightChange)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.shape = NA) +
  geom_jitter(aes(color = factor(Rep)), width = 0.2, size = 2) +
  theme_bw() +
  labs(
    x = "Treatment",
    y = "Height Change",
    color = "Rep",
    title = "Birch Height Change by Treatment"
  )

#do a t test for birch against co
subset_heb <- subset(birch_height_change, Treatment %in% c("co", "heb"))

t_test_heb <- t.test(
  HeightChange ~ Treatment,
  data = subset_heb
)

t_test_heb

#do a t test for birch against co
subset_pax <- subset(birch_height_change, Treatment %in% c("co", "pax"))

t_test_pax <- t.test(
  HeightChange ~ Treatment,
  data = subset_pax
)

t_test_pax

####### pine #####################

pine_height_change = height_change_df %>% filter(Species == 'PS')
names(pine_height_change)[names(pine_height_change) == "Fertiliser"] <- "Rep"
#view as boxplot with fertiliser as reps

ggplot(pine_height_change, aes(x = Treatment, y = HeightChange)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.shape = NA) +
  geom_jitter(aes(color = factor(Rep)), width = 0.2, size = 2) +
  theme_bw() +
  labs(
    x = "Treatment",
    y = "Height Change",
    color = "Rep",
    title = "Pine Height Change by Treatment"
  )

#do a t test for birch against co
subset_sui <- subset(pine_height_change, Treatment %in% c("co", "sui"))

t_test_sui <- t.test(
  HeightChange ~ Treatment,
  data = subset_sui
)

t_test_sui

#no point doing sui against cb, its not significant against co

#### sitka ####

sitka_height_change = height_change_df %>% filter(Species == 'SITKA')
names(sitka_height_change)[names(sitka_height_change) == "Fertiliser"] <- "Rep"
#view as boxplot with fertiliser as reps

ggplot(sitka_height_change, aes(x = Treatment, y = HeightChange)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.shape = NA) +
  geom_jitter(aes(color = factor(Rep)), width = 0.2, size = 2) +
  theme_bw() +
  labs(
    x = "Treatment",
    y = "Height Change",
    color = "Rep",
    title = "Sitka Height Change by Treatment"
  )

#do a t test for birch against co
subset_sui <- subset(sitka_height_change, Treatment %in% c("co", "sui"))

t_test_sui <- t.test(
  HeightChange ~ Treatment,
  data = subset_sui
)

t_test_sui


#also lets do change in ht on a tree by tree basis to check we get the same story
#Note that the trees were graded and counted in batches for initial hts, and planted in oder
#so should line up final counting order with initial and subtract - but I was working in freezing
#weather in Jan on my own, and I am not totally confident the trees always went in in the 
#right order. Not sure it should make much difference tho - as so many were the same ht#


#use tree by tree from londf_f and long_df i

birch_i = long_df_i %>% filter(Species == 'BP')
birch_f = long_df_f %>% filter(Species == 'BP')

merged_df <- merge(birch_i, birch_f, by = c("TreeID", "Fertiliser", "Treatment"), suffixes = c("_start", "_end"))

#delete the spurious cols
merged_df =  merged_df[,-c(4,6)]

#put the change in ht on

merged_df$HeightChange = merged_df$Height_end - merged_df$Height_start

#plot this
merged_df <- merged_df %>%
  mutate(
    Fertiliser = factor(Fertiliser, levels = c("0.0", "25.0", "50.0", "75.0", "100.0")),
    Treatment = factor(Treatment, levels = c("co", "cb", "heb", "pax"))
  )

# Side-by-side boxplot
ggplot(merged_df, aes(x = Fertiliser, y = HeightChange, fill = Treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,        # diamond shape
    size = 3,
    color = "black",
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = "Fertiliser Level",
    y = "Height Change",
    title = "Height Change by Treatment at Each Fertiliser Level, BP"
  ) +
  theme_bw() +
  scale_fill_brewer(palette = "Set2")

##PS##########

#use tree by tree from londf_f and long_df i

pine_i = long_df_i %>% filter(Species == 'PS')
pine_f = long_df_f %>% filter(Species == 'PS')

merged_df <- merge(pine_i, pine_f, by = c("TreeID", "Fertiliser", "Treatment"), suffixes = c("_start", "_end"))

#delete the spurious cols
merged_df =  merged_df[,-c(4,6)]

#put the change in ht on

merged_df$HeightChange = merged_df$Height_end - merged_df$Height_start

#plot this
merged_df <- merged_df %>%
  mutate(
    Fertiliser = factor(Fertiliser, levels = c("0.0", "25.0", "50.0", "75.0", "100.0")),
    Treatment = factor(Treatment, levels = c("co", "cb", "sui"))
  )

# Side-by-side boxplot
ggplot(merged_df, aes(x = Fertiliser, y = HeightChange, fill = Treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,        # diamond shape
    size = 3,
    color = "black",
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = "Fertiliser Level",
    y = "Height Change",
    title = "Height Change by Treatment at Each Fertiliser Level, PS"
  ) +
  theme_bw() +
  scale_fill_brewer(palette = "Set2")


###sitka##########

#use tree by tree from londf_f and long_df i

sitka_i = long_df_i %>% filter(Species == 'SITKA')
sitka_f = long_df_f %>% filter(Species == 'SITKA')

merged_df <- merge(sitka_i, sitka_f, by = c("TreeID", "Fertiliser", "Treatment"), suffixes = c("_start", "_end"))

#delete the spurious cols
merged_df =  merged_df[,-c(4,6)]

#put the change in ht on

merged_df$HeightChange = merged_df$Height_end - merged_df$Height_start

#plot this
merged_df <- merged_df %>%
  mutate(
    Fertiliser = factor(Fertiliser, levels = c("0.0", "25.0", "50.0", "75.0", "100.0")),
    Treatment = factor(Treatment, levels = c("co", "cb", "sui","pax","heb"))
  )

# Side-by-side boxplot
ggplot(merged_df, aes(x = Fertiliser, y = HeightChange, fill = Treatment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,        # diamond shape
    size = 3,
    color = "black",
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = "Fertiliser Level",
    y = "Height Change",
    title = "Height Change by Treatment at Each Fertiliser Level, Sitka"
  ) +
  theme_bw() +
  scale_fill_brewer(palette = "Set2")


