setwd('C:/dev/code/Petra')


library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

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


df$Key <- rownames(df)
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
    limits = c(0, 30), # Match the range of the actual data
    breaks = c(0, 5, 10, 15, 20,25,30), # Actual data breakpoints
    labels = c(40, 45, 50, 55, 60,65,70) # Custom labels for the y-axis
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


############################################################################
# start again and check with updated spread sheet

data = read_excel('../../data/Copy of Forrest Estate Statistics SHeet.xlsx', sheet = 'Combined')
  
#tidy some colnames
data = data%>% 
  rename(
    height = Height_cm_0,
    treatment = Treatment,
    tree =  Tree_Species,
    rcd = Rcd_mm_0
    )
#tidy column text
data$treatment <- ifelse(data$treatment == "Fungi_Treatment", "Pellet", 
                         ifelse(data$treatment == "Fungi_Control", "Control", data$treatment))




#delete NA rows
data = data %>% drop_na(height)

#look at quick summary
mean_heights <- data %>%
  group_by(treatment, tree) %>%
  summarize(
    mean_height = mean(height), 
    mean_rcd = mean(rcd), 
    .groups = "drop"
  )
#change tree species to look neater on chart
mean_heights$tree = c('Norway','Sitka','Scots pine','Norway','Sitka','Scots pine')
print(mean_heights)

#boxplots of all the points....heights
ggplot(data, aes(x =  tree, y = height, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Heights at end of first growing season",
       x = "Tree species",
       y = "Height(cm)",
       fill = "Treatment") +
  scale_fill_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )+
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )

##Bar charts
ggplot(mean_heights, aes(x = tree, y = mean_height, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge for side-by-side bars
  labs(x = "Tree Species", y = "Mean Height (cm)", fill = "Treatment Group") +
  theme_minimal() +  # Cleaner theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) 


#...rcd
ggplot(data, aes(x =  tree, y = rcd, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Root collar diameter at end of first growing season",
       x = "Tree species",
       y = "Root collar diameter(mm)",
       fill = "Treatment") +
  scale_fill_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) )+
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16),
      )

ggplot(mean_heights, aes(x = tree, y = mean_rcd, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +  # Dodge for side-by-side bars
  labs(x = "Tree Species", y = "Mean Root Collar Diameter (mm)", fill = "Treatment Group") +
  theme_minimal() +  # Cleaner theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)
  ) +
  scale_fill_manual(values = c("Control" = "orange", "Pellet" = "darkgreen")) 

#survival

survival = 
  data = read_excel('../../data/Copy of Forrest Estate Statistics SHeet.xlsx', sheet = 'survival')

#munge the data - remove cols 2-5
 survival.select = survival[-c(2,3,4,5,6)]
 
 #pivot for gg plot
 long_data <- survival.select %>%
   pivot_longer(cols = c(norm_density, norm_ht, norm_rcd), 
                names_to = "Measurement", 
                values_to = "Value")

 # Create the bar chart
 ggplot(long_data, aes(x = Measurement, y = Value, fill = treatment)) +
   geom_bar(stat = "identity", position = "dodge") +
   scale_fill_manual(values = c("pellet" = "darkgreen", "control" = "orange")) +
   labs(x = "", y = "Normalised values", fill = "Treatment") +
   scale_x_discrete(labels = c("norm_density" = "density", "norm_ht" = "height", "norm_rcd" = "rcd")) +
   theme(
     legend.position = "right", # Adjust legend position
     axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
     axis.text.y = element_text(size = 16),
     axis.title.y = element_text(size = 16),
     axis.title.x = element_text(size = 16),
     panel.background = element_rect(fill = "white", colour = NA), # White panel background
     plot.background = element_rect(fill = "white", colour = NA),  # White plot background
     panel.grid.major = element_blank(), # Remove major grid lines
     panel.grid.minor = element_blank()  # Remove minor grid lines
   )
 
 