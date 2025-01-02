setwd('C:/dev/code/Petra')

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)



#######################################
#get the mean of the hts column for 100 random selecctions 
#create df for the 100 means each time a random selection is made
getmeandistribution =  function(data) {
  set.seed(123)
  n = nrow(data)
  means = numeric(100) 
  #data&Height_2024 = as.numeric(data$Height_2024)
  
  # Repeat sampling and calculate the mean 100 times
  for (i in 1:100) {
    sampled_rows = data[sample(1:n, size = 5, replace = T), ] # Sample rows
    means[i] = mean(sampled_rows$Height_2024, na.rm = TRUE) # Calculate mean and store it
  }
  
  means_df = data.frame(mean_ht = means)
  return(means_df)
}

#################Tilhill########################################################

#get the sheet in with the first 2 time points
tilhill.yr1_2 = read_excel('../../data/Tilhill_WorkingSheet.xlsx', sheet = 'point1and2')



#get the hts for first time point
point1 = tilhill.yr1_2 %>% filter(timepoint == 1) %>% select(c(Treatment,Height_2024))
point1.pellet = point1 %>% filter(Treatment == 'Fungi_Treatment')
point1.control = point1 %>% filter(Treatment == 'Fungi_Control')

point2 = tilhill.yr1_2 %>% filter(timepoint == 2) %>% select(c(Treatment, Height_2024))
point2.pellet = point2 %>% filter(Treatment == 'Fungi_Treatment')
point2.control = point2 %>% filter(Treatment == 'Fungi_Control')

means1.control = getmeandistribution(point1.control)
means1.pellet = getmeandistribution(point1.pellet)
means2.control =  getmeandistribution(point2.control)
means2.pellet = getmeandistribution(point2.pellet)



###Saltersford#################################################################

#read the data
saltersford= read_excel('../../data/Saltersford_WorkingSheet.xlsx', sheet = 'Combined')

#select on control oaks from first time point - there were 239 of these

oaks_1 = saltersford %>% filter(Tree_Species == 'Quercus_robur' & timepoint == 1 & 
                                  Treatment == 'Fungi_Control') %>% select(c(Treatment,Height_2024))
#generate the 100 random samples
salt_oak_control = getmeandistribution(oaks_1)           

#######################################
##Coilte###
#read the data
coilte = read_excel('../../data/Coillte_WorkingSheet.xlsx', sheet = 'Combined')



#there were 203 treatment oaks in year 0 and year 1
coilte_oaks1 = coilte %>% filter(Tree_Species == 'Quercus'& Treatment == 'Fungi_Treatment') %>% select(c(Treatment,Height_0))
coilte_oaks2 = coilte %>% filter(Tree_Species == 'Quercus'& Treatment == 'Fungi_Treatment') %>% select(c(Treatment,Height_1))


#rename the ht column because function needs Height_2024
coilte_oaks1 <- coilte_oaks1 %>%
  rename(Height_2024 = Height_0)

coilte_oaks2 <- coilte_oaks2 %>%
  rename(Height_2024 = Height_1)

coilte_oaks2$Height_2024 =  as.numeric(coilte_oaks2$Height_2024)

#height in this df is char - filter out nas and try and convert to numeric
#coilte_oaks2 <- coilte_oaks2[!is.na(df$Height_2024), ]

#generate the 100 random samples
coillte1 = getmeandistribution(coilte_oaks1)

#remove NAs
coillte2 = getmeandistribution(coilte_oaks2)
###############################################################################
                    
means.df = cbind.data.frame(means1.pellet, means1.control, means2.pellet, means2.control, salt_oak_control, coillte1, coillte2)
colnames(means.df) = c('Tilpellet1','Tilcontrol1','Tilpellet2','Tilcontrol2', 'Saltoak1','CoillteOak1','CilteOak2')

means_long = pivot_longer(means.df, cols = everything(), names_to = 'Variable', values_to = 'Value')

ggplot(means_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Spread of means of 100 random resamples of 100 trees",
    x = "Columns",
    y = "Values"
  ) +
  theme(legend.position = "none") 


sd(means1.control$mean_ht)/sqrt(length(means1.control))
sd(means2.control$mean_ht)/sqrt(length(means1.control))
sd(means1.pellet$mean_ht)/sqrt(length(means1.control))
sd(means2.pellet$mean_ht)/sqrt(length(means1.control))
sd(coillte1$mean_ht)/sqrt(length(coillte1))
sd(coillte2$mean_ht)/sqrt(length(coillte1))
sd(salt_oak_control$mean_ht)/sqrt(length(salt_oak_control))

