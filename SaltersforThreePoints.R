setwd('C:/dev/code/Petra')

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

saltersford= read_excel('../../data/Saltersford_WorkingSheet.xlsx', sheet = 'Combined')

#get the plot averages for time points 0,1,2 for control,treatment for oak,birch

group_means <- saltersford %>%
  group_by(Treatment, Tree_Species, timepoint) %>%
  summarize(Mean_Height = mean(Height_2024, na.rm = TRUE), .groups = "drop")

#calculate growth as average for the plot - initial average
#subtract initial to get a zero start point

bp_control0 = group_means[1,4] - group_means[1,4]
bp_control1 = group_means[2,4] - group_means[1,4]
bp_control2 = group_means[3,4] - group_means[1,4]

bp_pellet0 = group_means[8,4] - group_means[8,4]
bp_pellet1 = group_means[9,4] - group_means[8,4]
bp_pellet2 = group_means[10,4] - group_means[8,4]

qr_control0 = group_means[4,4] - group_means[4,4]
qr_control1 = group_means[5,4] - group_means[4,4]
qr_control2 = group_means[6,4] - group_means[4,4]

qr_pellet0 = group_means[11,4] - group_means[11,4]
qr_pellet1 = group_means[12,4] - group_means[11,4]
qr_pellet2 = group_means[13,4] - group_means[11,4]

#make these into a df

df = rbind.data.frame(bp_control0,bp_control1,bp_control2,
                      bp_pellet0,bp_pellet1,bp_pellet2,
                      qr_control0,qr_control1,qr_control2,
                      qr_pellet0, qr_pellet1, qr_pellet2 )
df$treat = c('control','control','control',
             'pellet','pellet','pellet',
             'control','control','control',
             'pellet','pellet','pellet')
df$tree = c(rep('birch',6), rep('oak',6))
df$Year = rep(c(0,11,17),4)

library(ggplot2)

# Create the plot
ggplot(df, aes(x = Year, y = Mean_Height, color = treat, linetype = tree)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Change in average plot height for Betula pendula and Quercus robur at CWT Saltersford ",
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

bp_control0 = group_means[1,4] - group_means[1,4]
bp_control1 = group_means[2,4] - group_means[1,4]
bp_control2 = group_means[3,4] - group_means[1,4]

bp_pellet0 = group_means[8,4] - group_means[8,4]
bp_pellet1 = group_means[9,4] - group_means[8,4]
bp_pellet2 = group_means[10,4] - group_means[8,4]

qr_control0 = group_means[4,4] - group_means[4,4]
qr_control1 = group_means[5,4] - group_means[4,4]
qr_control2 = group_means[6,4] - group_means[4,4]

qr_pellet0 = group_means[11,4] - group_means[11,4]
qr_pellet1 = group_means[12,4] - group_means[11,4]
qr_pellet2 = group_means[13,4] - group_means[11,4]

#make these into a df

df = rbind.data.frame(bp_control0,bp_control1,bp_control2,
                      bp_pellet0,bp_pellet1,bp_pellet2,
                      qr_control0,qr_control1,qr_control2,
                      qr_pellet0, qr_pellet1, qr_pellet2 )
df$treat = c('control','control','control',
             'pellet','pellet','pellet',
             'control','control','control',
             'pellet','pellet','pellet')
df$tree = c(rep('birch',6), rep('oak',6))
df$Year = rep(c(0,11,17),4)

library(ggplot2)

# Create the plot
ggplot(df, aes(x = Year, y = Mean_Height, color = treat, linetype = tree)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Change in average plot height for Betula pendula and Quercus robur at CWT Saltersford ",
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
##relative change plots######################################


bp_control0 = group_means[1,4] - group_means[1,4]
bp_control1 = (group_means[2,4] - group_means[1,4])/group_means[1,4]
bp_control2 = (group_means[3,4] - group_means[1,4])/group_means[1,4]

bp_pellet0 = group_means[8,4] - group_means[8,4]
bp_pellet1 = (group_means[9,4] - group_means[8,4])/group_means[8,4]
bp_pellet2 = (group_means[10,4] - group_means[8,4])/group_means[8,4]

qr_control0 = group_means[4,4] - group_means[4,4]
qr_control1 = (group_means[5,4] - group_means[4,4])/group_means[4,4]
qr_control2 = (group_means[6,4] - group_means[4,4])/group_means[4,4]

qr_pellet0 = group_means[11,4] - group_means[11,4]
qr_pellet1 = (group_means[12,4] - group_means[11,4])/group_means[11,4]
qr_pellet2 = (group_means[13,4] - group_means[11,4])/group_means[11,4]

#make these into a df

df = rbind.data.frame(bp_control0,bp_control1,bp_control2,
                      bp_pellet0,bp_pellet1,bp_pellet2,
                      qr_control0,qr_control1,qr_control2,
                      qr_pellet0, qr_pellet1, qr_pellet2 )
df$treat = c('control','control','control',
             'pellet','pellet','pellet',
             'control','control','control',
             'pellet','pellet','pellet')
df$tree = c(rep('birch',6), rep('oak',6))
df$Year = rep(c(0,11,17),4)

library(ggplot2)

# Create the plot
ggplot(df, aes(x = Year, y = Mean_Height, color = treat, linetype = tree)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Relative change in average plot height for Betula pendula and Quercus robur at CWT Saltersford ",
    x = "Months since planting",
    y = "Relative change in average plot height"
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





