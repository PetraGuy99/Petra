etwd('C:/dev/code/Petra')

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

tilhill = read_excel('../../data/TilhillWorkingCopy.xlsx', sheet = 'data')

#get the plot averages for time points 0,1,2 for control,treatment for oak,birch

#plot 1,2 are treatment/fertiliser, plot 3 is control

group_means <- tilhill %>%
  group_by(Treatment, timepoint) %>%
  summarize(Mean_Height = mean(height, na.rm = TRUE), .groups = "drop")

mean_change = unlist(c(group_means[1,3]-group_means[1,3],
                group_means[2,3]-group_means[1,3],
                group_means[3,3]-group_means[1,3],
                group_means[4,3]-group_means[4,3],
                group_means[5,3]-group_means[4,3],
                group_means[6,3]-group_means[4,3]))
treatment = c(rep('control',3), rep('pellet',3))
timepoint = rep(c(0,11,17),2)
df = cbind.data.frame(mean_change,treatment,timepoint)


ggplot(df, aes(x = timepoint, y = mean_change, color = treatment)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Change in average plot height for Picea sitchensis at Tilhill",
    x = "Months since planting",
    y = "Change in average plot height (cm)"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet" = "darkgreen")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )


##### relative height
relative_ht_change = unlist(c(group_means[1,3]-group_means[1,3],
                       (group_means[2,3]-group_means[1,3])/group_means[1,3],
                       (group_means[3,3]-group_means[1,3])/group_means[1,3],
                       group_means[4,3]-group_means[4,3],
                       (group_means[5,3]-group_means[4,3])/group_means[4,3],
                       (group_means[6,3]-group_means[4,3])/group_means[4,3]))
treatment = c(rep('control',3), rep('pellet',3))
timepoint = rep(c(0,11,17),2)
df = cbind.data.frame(relative_ht_change,treatment,timepoint)


ggplot(df, aes(x = timepoint, y = relative_ht_change, color = treatment)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Relative plot height for Picea sitchensis at Tilhill",
    x = "Months since planting",
    y = "Relative plot height change"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet" = "darkgreen")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )

