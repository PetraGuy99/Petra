setwd('C:/dev/code/Petra')


library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

#This is Woodstock

Woodstock = read.csv('../../data/Woodstock.csv')

#This is the data for time point 0 = spring and timepoint 1 = October 2022
#For Mondrem farm and Woodstock where agar wedges used. Both sites now abandoned
#because agar and not pellets
#Tree spp were oak and birch, emf were Lac and Pax
#blocks can be ignored it was all same area
#2 timepoints and change in ht per tree 


#remove the dead
Woodstock = Woodstock %>% filter(Notes!='Dead')
ggplot(Woodstock, aes(x = Tree_species, y = Height.change, fill = Strain)) +
  geom_boxplot() +
  labs(title = "Woodstock change in height",
       x = "Treatment",
       y = "Height") +
  theme_minimal()

Mondrem =  read.csv('../../data/Mondrem.csv')
#remove the dead
Mondrem = Mondrem %>% filter(Notes!='Dead')
ggplot(Mondrem, aes(x = Tree_species, y = Height.change, fill = Strain)) +
  geom_boxplot() +
  labs(title = "Mondrem Farm",
       x = "Treatment",
       y = "Height") +
  theme_minimal()

#t test the oak at Woodstock
oak_control = Woodstock %>% filter(Tree_species == 'Oak'& Strain == 'Control')
oak_lac = Woodstock %>% filter(Tree_species == 'Oak'& Strain == 'LAC_302_031')
oak_pax = Woodstock %>% filter(Tree_species == 'Oak'& Strain == 'PAX_302_007')

t.test(oak_control$Height.change, oak_lac$Height.change)
t.test(oak_control$Height.change, oak_pax$Height.change)

#t.test hazel at Mondrem
hazel_control =  Mondrem %>% filter (Tree_species == 'Hazel' & Strain == 'Control')
hazel_pax1 = Mondrem %>% filter (Tree_species == 'Hazel' & Strain == 'PAX_302_007')
hazel_pax2 = Mondrem %>% filter (Tree_species == 'Hazel' & Strain == 'PAX_302_002')

t.test(hazel_control$Height.change, hazel_pax1$Height.change)
