setwd('C:/dev/code/Petra')

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

dundreggan = read_excel('../../data/Dundreggan_Workingheet.xlsx', sheet = 'Combined all')

#get the plot averages for time points 0,1,2 for control,treatment for oak,birch

#plot 1,2 are treatment/fertiliser, plot 3 is control

group_means <- dundreggan %>%
  group_by(Treatment, timepoint, Site) %>%
  summarize(Mean_Height = mean(Height, na.rm = TRUE), .groups = "drop")

#calculate growth as average for the plot - initial average
#subtract initial to get a zero start point

control0= group_means[7,4] - group_means[7,4]
control1 = group_means[8,4] - group_means[7,4]
control2 = group_means[9,4] - group_means[7,4]

plot1_pellet0 = group_means[10,4] - group_means[10,4]
plot1_pellet1 = group_means[12,4] - group_means[10,4]
plot1_pellet2 = group_means[14,4] - group_means[10,4]

plot2_pellet0 = group_means[11,4] - group_means[11,4]
plot2_pellet1 = group_means[13,4] - group_means[11,4]
plot2_pellet3 = group_means[15,4] - group_means[11,4]

plot1_fert0= group_means[1,4] - group_means[1,4]
plot1_fert1 = group_means[3,4] - group_means[1,4]
plot1_fert2 = group_means[5,4] - group_means[1,4]

plot2_fert0= group_means[2,4] - group_means[2,4]
plot2_fert1 = group_means[4,4] - group_means[2,4]
plot2_fert2 = group_means[6,4] - group_means[2,4]

#make these into a df

df = rbind.data.frame(control0,control1,control2,
                      plot1_pellet0,plot1_pellet1,plot1_pellet2,
                      plot2_pellet0,plot2_pellet1,plot2_pellet3,
                      plot1_fert0, plot1_fert1, plot1_fert2,
                      plot2_fert0, plot2_fert1, plot2_fert2)
df$treat = c('control','control','control',
             'pellet1','pellet1','pellet1',
             'pellet2','pellet2','pellet2',
             'fert1','fert1','fert1',
             'fert2','fert2','fert2'
                          )

df$Year = rep(c(0,9,15),5)
df$plot = c(rep('c',3), rep(1,3), rep(2,3), rep(1,3), rep(2,3))

library(ggplot2)

# Create the plot
ggplot(df, aes(x = Year, y = Mean_Height, color = treat, linetype = plot)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Change in average plot height for Betula pubescens at Trees for Life",
    x = "Months since planting",
    y = "Change in average plot height (cm)"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet1" = "darkgreen", "pellet2" = "darkgreen", "fert1" = "red", "fert2" = "red")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )
####DO relative change, i.e. final - initial/ initial

control0= group_means[7,4] - group_means[7,4]
control1 = (group_means[8,4] - group_means[7,4])/group_means[7,4]
control2 = (group_means[9,4] - group_means[7,4])/group_means[7,4]

plot1_pellet0 = group_means[10,4] - group_means[10,4]
plot1_pellet1 = (group_means[12,4] - group_means[10,4])/group_means[10,4]
plot1_pellet2 = (group_means[14,4] - group_means[10,4])/group_means[10,4]

plot2_pellet0 = group_means[11,4] - group_means[11,4]
plot2_pellet1 = (group_means[13,4] - group_means[11,4])/group_means[11,4]
plot2_pellet3 = (group_means[15,4] - group_means[11,4])/group_means[11,4]

plot1_fert0= group_means[1,4] - group_means[1,4]
plot1_fert1 = (group_means[3,4] - group_means[1,4])/group_means[1,4]
plot1_fert2 = (group_means[5,4] - group_means[1,4])/group_means[1,4]

plot2_fert0= group_means[2,4] - group_means[2,4]
plot2_fert1 = (group_means[4,4] - group_means[2,4])/group_means[2,4]
plot2_fert2 = (group_means[6,4] - group_means[2,4])/group_means[2,4]

#make these into a df

df = rbind.data.frame(control0,control1,control2,
                      plot1_pellet0,plot1_pellet1,plot1_pellet2,
                      plot2_pellet0,plot2_pellet1,plot2_pellet3,
                      plot1_fert0, plot1_fert1, plot1_fert2,
                      plot2_fert0, plot2_fert1, plot2_fert2)
df$treat = c('control','control','control',
             'pellet1','pellet1','pellet1',
             'pellet2','pellet2','pellet2',
             'fert1','fert1','fert1',
             'fert2','fert2','fert2'
)

df$Year = rep(c(0,9,15),5)
df$plot = c(rep('c',3), rep(1,3), rep(2,3), rep(1,3), rep(2,3))

library(ggplot2)

# Create the plot
ggplot(df, aes(x = Year, y = Mean_Height, color = treat, linetype = plot)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Relative change in average plot height for Betula pubescens at Trees for Life",
    x = "Months since planting",
    y = "Relative change in average plot height"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet1" = "darkgreen", "pellet2" = "darkgreen", "fert1" = "red", "fert2" = "red")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )


######Average across the two plots
#take site_1 and site_2 and sum all the data for pellet and fert

pellet = filter(dundreggan, Site %in%  c('Site_1', 'Site_2') & Treatment == 'Fungi_Treatment')
fert = filter(dundreggan, Site %in%  c('Site_1', 'Site_2') & Treatment == 'Fertiliser')
control = dundreggan %>% filter(Treatment == 'Fungi_Control')


pellet_means = pellet %>%  group_by(timepoint) %>%
  summarize(Mean_Height = mean(Height, na.rm = TRUE), .groups = "drop")
fert_means =  fert %>% group_by(timepoint) %>%
  summarize(Mean_Height = mean(Height, na.rm = TRUE), .groups = "drop")
control_means = control %>% group_by(timepoint) %>%
  summarize(Mean_Height = mean(Height, na.rm = TRUE), .groups = "drop")

df = rbind.data.frame(pellet_means, fert_means, control_means)
df$treatment = c(rep('pellet',3), rep('fertiliser',3), rep('control',3))

df$mean_change = c(pellet_means[1,2] - pellet_means[1,2],
                   pellet_means[2,2] - pellet_means[1,2],
                   pellet_means[3,2] - pellet_means[1,2],
                   fert_means[1,2] - fert_means[1,2],
                   fert_means[2,2] - fert_means[1,2],
                   fert_means[3,2] - fert_means[1,2],
                   control_means[1,2] - control_means[1,2],
                   control_means[2,2] - control_means[1,2],
                   control_means[3,2] - control_means[1,2])

df$relmeanchange = c(pellet_means[1,2] - pellet_means[1,2],
                     (pellet_means[2,2] - pellet_means[1,2])/pellet_means[1,2],
                     (pellet_means[3,2] - pellet_means[1,2])/pellet_means[1,2],
                     fert_means[1,2] - fert_means[1,2],
                     (fert_means[2,2] - fert_means[1,2])/fert_means[1,2],
                     (fert_means[3,2] - fert_means[1,2])/fert_means[1,2],
                     control_means[1,2] - control_means[1,2],
                     (control_means[2,2] - control_means[1,2])/control_means[1,2],
                     (control_means[3,2] - control_means[1,2])/control_means[1,2])
df$relmeanchange = unlist(df$relmeanchange)
df$mean_change = unlist(df$mean_change)

ggplot(df, aes(x = timepoint, y = relmeanchange, color = treatment)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Relative change in average plot height for Betula pubescens at Trees for Life",
    x = "Months since planting",
    y = "Relative change in average plot height"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet" = "darkgreen", "fertiliser" = "red", "fert2" = "red")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )

ggplot(df, aes(x = timepoint, y = mean_change, color = treatment)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) + # Add points for better visualization
  labs(
    title = "Mean change in average plot height for Betula pubescens at Trees for Life",
    x = "Months since planting",
    y = "Mean change in average plot height (cm)"
  ) +
  scale_color_manual(
    values = c("control" = "orange", "pellet" = "darkgreen", "fertiliser" = "red", "fert2" = "red")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", # Adjust legend position
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 16) ,
    axis.title.x = element_text(size = 16) # Adjust x-axis labels
  )
