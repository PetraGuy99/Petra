setwd('C:/dev/code/Petra')

#boxplots of AS polytunnel experiment
#but only the data for the PAX_302 has been
#extracted here - to possibly use for the TPIF talk
#Pax was applied to birch in a 'trail' and a 'main' experiment
#different fertiliser levels were used
#this is the final heights

library(readxl)
library(ggplot2)
library(dplyr)

birch_main = read_excel('../../data/polytunneltrial.xlsx', sheet = 'Data_BM')
birch_trial = read_excel('../../data/polytunneltrial.xlsx', sheet = 'Data_BT')


#there is a separate file for survival of main trial - for the trial data, it is 
#in the 'Alive column'
survival_main =  read.csv('../../data/birch_survival_main.csv')

#plot survival of main and trial###

#extract survival from trial
birch_trial_survival = birch_trial_PAX302 %>% select(Treatment, N_level, Alive)

#tidy the trial survival to match the cols
birch_main_survival = survival_main %>% select(Treatment,N_level, Alive)

#Look at overall number of Alive in control and treatment groups in each rep
birch_trial_survival %>%
  filter(Alive == "Yes") %>%
  group_by(Treatment) %>%
  summarise(Count_Alive = n())

birch_main_survival %>%
  filter(Alive == "Yes") %>%
  group_by(Treatment) %>%
  summarise(Count_Alive = n())

#some survival plots - trial run
alive_percent <- birch_trial_survival %>%
  group_by(Treatment) %>%
  summarise(
    Total = n(),
    Alive_Yes = sum(Alive == "Yes")
  ) %>%
  mutate(Percent_Alive = (Alive_Yes / Total) * 100)

# Plot as a bar chart
ggplot(alive_percent, aes(x = Treatment, y = Percent_Alive, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_fill_manual(values = c("Control" = "orange", "PAX-302-009" = "darkgreen")) +
  labs(title = "",
       x = "",
       y = "% survival") +
  theme_minimal(base_size = 24)


# repeat for 'main'
alive_percent <- birch_main_survival %>%
  group_by(Treatment) %>%
  summarise(
    Total = n(),
    Alive_Yes = sum(Alive == "Yes")
  ) %>%
  mutate(Percent_Alive = (Alive_Yes / Total) * 100)

# Plot as a bar chart
ggplot(alive_percent, aes(x = Treatment, y = Percent_Alive, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.5)+
  scale_fill_manual(values = c("Control" = "orange", "PAX-302-008" = "darkgreen", "PAX-201-001" = "green")) +
  labs(title = "",
       x = "",
       y = "% survival") +
  theme_minimal(base_size = 24)

data$N_level <- as.factor(data$N_level)

ggplot(data, aes(x = N_level, y = change_in_ht, fill = Treatment)) +
  geom_boxplot() +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +  # Optional: nice color palette
  labs(
    x = "Nitrogen level in fertiliser",
    y = "Change in height",
    fill = "Treatment"
  )

#add FLS Damside because I need chart for TPIF

df <- data.frame(
  Treatment = c("Control", "Pax"),
  Survival = c(80, 97)
)

ggplot(df, aes(x = Treatment, y = Survival, fill = Treatment)) +
  geom_bar(stat = "identity", width = 0.4)+
  scale_fill_manual(values = c("Control" = "orange", "Pax" = "darkgreen")) +
  labs(title = "FLS Damside",
       x = "",
       y = "% survival") +
  theme_minimal(base_size = 24)
