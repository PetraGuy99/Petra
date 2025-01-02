setwd('C:/dev/code/Petra')


library(dplyr)
library(ggplot2)
library(readxl)

p_abies_T = read_excel('../../data/Forrest_working_Copy.xlsx', sheet = 'P_abies_T')
p_abies_C =read_excel('../../data/Forrest_working_Copy.xlsx', sheet = 'P_abies_C')
p_sitch_T = read_excel('../../data/Forrest_working_Copy.xlsx', sheet = 'P_sitch_T')
p_sitch_C = read_excel('../../data/Forrest_working_Copy.xlsx', sheet = 'P_sitch_C')
p_sylv_T1 = read_excel('../../data/Forrest_working_Copy.xlsx', sheet = 'P_sylv_T1')
p_sylv_T2 = read_excel('../../data/Forrest_working_Copy.xlsx', sheet = 'P_sylv_T2')
p_sylv_C = read_excel('../../data/Forrest_working_Copy.xlsx', sheet = 'P_sylv_C')

p_abies_T.mean = mean(p_abies_T$Height_2024)
p_abies_C.mean = mean(p_abies_C$Height_2024)

p_sitch_T.mean = mean(p_sitch_T$Height_2024)
p_sitch_C.mean = mean(p_sitch_C$Height_2024)

p_sylv_T1.mean = mean(p_sylv_T1$Height_2024)
P_sylv_T2.mean = mean(p_sylv_T2$Height_2024)
p_sylv_C.mean = mean(p_sylv_C$Height_2024)

df <- data.frame(
  Value = c(p_abies_T.mean,p_abies_C.mean,p_sylv_T1.mean,P_sylv_T2.mean,p_sylv_C.mean),
  Treatment =c('pellet','control','pellet','pellet','control'),
  Label = c('P abies', 'P abies','P sylvestris','P sylvestris, plot 1','P sylvestris, plot 2'), # Labels for bars
  row.names = c('p_abies_T.mean','p_abies_C.mean','p_sylv_T1.mean','P_sylv_T2.mean','p_sylv_C.mean')
)


df$Key <- rownames(data)
df$zeroed = df$Value - 40

ggplot(df, aes(x = Key, y = Value, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  scale_fill_manual(
    values = c("control" = "orange", "pellet" = "darkgreen")
  ) +
  scale_x_discrete(labels = df$Label) + # Replace x-axis labels with Label column
  labs(
    title = "Average plot height after one growing season",
    x = "Tree species",
    y = "Mean plot height (cm)",
    fill = "Treatment"
  )


ggplot(df, aes(x = Key, y = zeroed, fill = Treatment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(
    values = c("control" = "orange", "pellet" = "darkgreen")
  ) +
  scale_x_discrete(labels = df$Label) + 
  scale_y_continuous(
    limits = c(0, 20), # Match the range of the actual data
    breaks = c(0, 5, 10, 15, 20), # Actual data breakpoints
    labels = c(40, 45, 50, 55, 60) # Custom labels for the y-axis
  ) + #Replace x-axis labels with Label column
  labs(
    title = "Average plot height after one growing season",
    x = "Tree species",
    y = "Mean plot height (cm)",
    fill = "Treatment"
  )

#narrow bars


ggplot(df, aes(x = Key, y = zeroed, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  scale_fill_manual(
    values = c("control" = "orange", "pellet" = "darkgreen")
  ) +
  scale_x_discrete(labels = df$Label) + 
  scale_y_continuous(
    limits = c(0, 20), # Match the range of the actual data
    breaks = c(0, 5, 10, 15, 20), # Actual data breakpoints
    labels = c(40, 45, 50, 55, 60) # Custom labels for the y-axis
  ) + #Replace x-axis labels with Label column
  labs(
    title = "Average plot height after one growing season",
    x = "Tree species",
    y = "Mean plot height (cm)",
    fill = "Treatment"
  )

#add the sitka data

df2 <- data.frame(
  Value = c(p_abies_T.mean,p_abies_C.mean,p_sylv_T1.mean,P_sylv_T2.mean,p_sylv_C.mean, p_sitch_T.mean, p_sitch_C.mean),
  Treatment =c('pellet','control','pellet','pellet','control','pellet', 'control'),
  Label = c('P abies', 'P abies','P sylvestris','P sylvestris plot 1','P sylvestris plot 2','P sitchensis','P sitchensis'), # Labels for bars
  row.names = c('p_abies_T.mean','p_abies_C.mean','p_sylv_T1.mean','P_sylv_T2.mean','p_sylv_C.mean', 'p_sitch_T.mean','p_sitch_C.mean')
)

df2$Key <- factor(rownames(df2), levels = rownames(df2))
df2$zeroed = df2$Value - 40

ggplot(df2, aes(x = Key, y = zeroed, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  scale_fill_manual(
    values = c("control" = "orange", "pellet" = "darkgreen")
  ) +
  scale_x_discrete(labels = df2$Label) + 
  scale_y_continuous(
    limits = c(0, 25), # Match the range of the actual data
    breaks = c(0, 5, 10, 15, 20,25), # Actual data breakpoints
    labels = c(40, 45, 50, 55, 60,65) # Custom labels for the y-axis
  ) + #Replace x-axis labels with Label column
  labs(
    title = "Average plot height after one growing season",
    x = "Tree species",
    y = "Mean plot height (cm)",
    fill = "Treatment"
  )+
theme(axis.text.x = element_text(angle = 45, hjust = 1) )


#just pines

df3 <- data.frame(
  Value = c(p_sylv_T1.mean,P_sylv_T2.mean,p_sylv_C.mean),
  Treatment =c('pellet','pellet','control'),
  Label = c('treatment plot 1','treatment plot 2','control plot'), # Labels for bars
  row.names = c('p_sylv_T1.mean','P_sylv_T2.mean','p_sylv_C.mean')
)

df3$Key <- factor(rownames(df3), levels = rownames(df3))
df3$zeroed = df3$Value - 40

ggplot(df3, aes(x = Key, y = zeroed, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.3) +
  theme_minimal() +
  scale_fill_manual(
    values = c("control" = "orange", "pellet" = "darkgreen")
  ) +
  scale_x_discrete(labels = df3$Label) + 
  scale_y_continuous(
    limits = c(0, 6), # Match the range of the actual data
    breaks = c(0,1,2,3,4,5,6), # Actual data breakpoints
    labels = c(40,41,42,43,44,45,46) # Custom labels for the y-axis
  ) + #Replace x-axis labels with Label column
  labs(
    title = "Average plot height after one growing season of Pinus sylvestris",
    x = "Tree species",
    y = "Mean plot height (cm)",
    fill = "Treatment"
  )+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels by 45 degrees
    plot.margin = unit(c(1, 1, 1, 1), "cm") # Reduce plot margin to further compress the area
  )
  