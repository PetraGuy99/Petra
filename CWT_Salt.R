setwd('C:/dev/code')


library(dplyr)
library(ggplot2)


CWT_Salt = read.csv('../data/CWT_Salt.csv', colClasses =c(
  'integer',
  'integer',
  'integer',
  'character',
  'character',
  'integer',
  'integer',
  'character',
  'integer'
))

#there are different numbers of treatment and control across the 2 years, by separating the groups
#we can do a delta h for the trees which match up
#note that some trees in 2023 do not match to those in 2024 - e.g. tree 67 might have been measured
#in 2023 but not 2024, so need to merge on tree_id contained in both df
#prob not worth matching trees here

################################################################################

getheights = function(t, yr, treat){
  data = CWT_Salt%>% filter(treatment == treat)%>%
    filter(year == yr) %>%
    filter(tree == t)
  heights = na.omit(as.numeric(data$value)) # just the heights
  return(heights)
}

#get control initial heights - input required tree, 'Q_robur' or 'B_pendula'
controli = getheights('B_pendula',2023,0)
controlf = getheights('B_pendula',2024, 0)

#get treatment average plot heights
pelleti = getheights('B_pendula',2023,1)
pelletf = getheights('B_pendula',2024,1)

data = data.frame(
  value = c(controli, controlf, pelleti, pelletf),
  group = factor(rep(c('controli','controlf','pelleti','pelletf'),
                     times = c(length(controli), length(controlf),
                               length(pelleti), length(pelletf))))
)
summary_stat = data %>%
  group_by(group) %>%
  summarize(
    mean = mean(value),
    se = sd(value)/sqrt(n())
  )

getrelchange = function(initial,final){ # data is the summary stat for the values
  change = ((mean(final)-mean(initial))/mean(initial))*100
  return(change)
}

control.relchange = getrelchange(controli, controlf)
treat.change = getrelchange(pelleti, pelletf)

summary_stat$se2 = (summary_stat$se)^2

term1 = summary_stat[1,2]/summary_stat[2,2]
term2 = summary_stat[1,4]/summary_stat[1,2]
term3 = summary_stat[2,4]/summary_stat[2,2]

controlchange.se = unlist(term1*sqrt(term2+term3))

term1 = summary_stat[3,2]/summary_stat[4,2]
term2 = summary_stat[3,4]/summary_stat[3,2]
term3 = summary_stat[4,4]/summary_stat[4,2]

pelletchange.se = unlist(term1*sqrt(term2+term3))

data = data.frame(
  group = c('control','pellet'),
  value = c(control.relchange, treat.change),
  se = c(controlchange.se, pelletchange.se )
)

ggplot(data, aes(x = group, y = value, fill = group))+
  geom_point(stat = 'identity', colour = 'black', position = position_dodge(0.9), show.legend = F)+
  geom_errorbar(aes(ymin = value-se, ymax = value+se), width = 0.2, position = position_dodge((0.9)))+
  labs(title = 'Relative difference in plot mean heights between 2023 and 2024',
       subtitle = 'Betula pendula', #input tree here for graph
       x = 'treatment',
       y =  'relative change in height')+
  theme(text=element_text(size = 20))






#Welches t and box plot for delta h of oak or birch
#choose B_pendula of Q_robur

#run function in terminal with either B_pendula or Q_robur

treespp = 'B_pendula'
getdeltah = function(treespp){
tree_2023_01 = CWT_Salt %>% filter(tree == treespp & year == '2023'& treatment == '1')
tree_2023_00 = CWT_Salt %>% filter(tree == treespp & year == '2023'& treatment == '0')

tree_2024_01 = CWT_Salt %>% filter(tree == treespp& year == '2024' & treatment == '1') 
tree_2024_00 = CWT_Salt %>% filter(tree == treespp& year == '2024' & treatment == '0') 

#now merge the df to get those which appear in both years so we can have a delta h

tree_00 = merge(tree_2023_00, tree_2024_00, by = 'tree_id') 
tree_01 = merge(tree_2023_01, tree_2024_01, by = 'tree_id')

# add a delta h column - this is rate, but could be delta - is just a constant
tree_00$deltah = (as.numeric(tree_00$value.y)- as.numeric(tree_00$value.x))/as.numeric(tree_00$value.x)
tree_01$deltah = (as.numeric(tree_01$value.y) - as.numeric(tree_01$value.x))/as.numeric(tree_01$value.x)

#some missing values in 2024 because trees dead or missing, remove those for the
#sake of the dh calulations

#take out the detla h for neatness
dh0 = na.omit(tree_00$deltah)
dh1 = na.omit(tree_01$deltah)

#remove negatives
dh0 = dh0[dh0 >=0]
dh1 = dh1[dh1>= 0]

welcest = t.test(dh0, dh1)
print(welcest)
#make a df for plot
data = data.frame(
  value = c(dh1,dh2),
  group =  factor(rep(c('control', 'pellet'), times = c(length(dh1), length(dh2))))
)

ggplot(data, aes(x = group, y = value, fill = group)) +
  geom_boxplot() + 
  labs(title = 'relative height change(cm) at CWT_Saltersford, 2023 to 2024',
       x = 'treatment',
       y = 'relative change in height (cm)')+
  theme_minimal()
}
############################################################################

#ignoring the tree ids - compare the averages across all the control and pellet
#treatments by tree species
#now we cant do a change in ht because we are not matching trees, we can 
#look for difference in mean ht between treatment and control - in 2024
#omit 2023 because things only just planted

treespp = 'Q_robur'
treespp = 'B_pendula'

getmeanh(treespp)

getmeanh = function(treespp){
tree_control = CWT_Salt %>% filter(tree == treespp & treatment == 0 & year == 2024)
control_h = na.omit(as.numeric(tree_control$value)) # just the heights
tree_treatment = CWT_Salt %>% filter(tree == treespp & treatment == 1 & year == 2024)
treatment_h = na.omit(as.numeric(tree_treatment$value))

data = data.frame(
  value = c(control_h, treatment_h),
  group = factor(rep(c('control', 'pellet'), times = c(length(control_h), length(treatment_h))))
)

anova_result = aov(value ~ group, data = data)
summary(anova_result)

#make a plot
summary_stat = data %>%
  group_by(group) %>%
  summarize(
    mean = mean(value),
    se = sd(value)/sqrt(n())
  )

ggplot(summary_stat, aes(x = group, y = mean, fill = group))+
  geom_point(stat = 'identity', colour = 'black', position = position_dodge(0.9), show.legend = F)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, position = position_dodge((0.9)))+
  labs(title = 'Differences in mean height for treatment and control 2024',
       subtitle = treespp,
       x = 'treatment',
       y =  'mean height')+
  theme(text=element_text(size = 20))
}


#########################################

#survival or mortality rate
#number dead = number NAs in 2024
#Mortality = number trees which died between 2023 and 2024/ number trees
# not sure we can do this

treespp = 'B_pendula'

tree_2023_01 = CWT_Salt %>% filter(tree == treespp & year == '2023'& treatment == '1')
tree_2023_00 = CWT_Salt %>% filter(tree == treespp & year == '2023'& treatment == '0')

tree_2024_01 = CWT_Salt %>% filter(tree == treespp& year == '2024' & treatment == '1') 
tree_2024_00 = CWT_Salt %>% filter(tree == treespp& year == '2024' & treatment == '0') 

mortality_00 = sum(is.na(tree_2024_00$value))/length(tree_2024_00$value)
mortality_01 = sum(is.na(tree_2024_01$value))/length(tree_2024_01$value)

#Z test for proportions
#nas in control
sum(is.na(tree_2024_00$value))
#values in control
length(tree_2024_00$value)

#nas in treatment
sum(is.na(tree_2024_01$value))
#values in treatment
length(tree_2024_01$value)

prop.test(c(3, 5), c(53, 60))

#calculate relative change in height across 2023 and 2024. Get mean height 2023,
#rel change (new-old)/old
treespp = 'Q_robur'
treespp = 'B_pendula'

tree_control = CWT_Salt %>% filter(tree == treespp & treatment == 0 & year == 2023)
control_hi = na.omit(as.numeric(tree_control$value)) # just the heights
tree_treatment = CWT_Salt %>% filter(tree == treespp & treatment == 1 & year == 2023)
treatment_hi = na.omit(as.numeric(tree_treatment$value))

tree_control = CWT_Salt %>% filter(tree == treespp & treatment == 0 & year == 2024)
control_hf = na.omit(as.numeric(tree_control$value)) # just the heights
tree_treatment = CWT_Salt %>% filter(tree == treespp & treatment == 1 & year == 2024)
treatment_hf = na.omit(as.numeric(tree_treatment$value))

mean0_i = mean(control_hi)
mean0_f = mean(control_hf)

mean1_i = mean(treatment_hi)
mean1_f = mean(treatment_hf)

relchange_0 = (mean0_f - mean0_i)/mean0_i
relchange_1 = (mean1_f - mean1_i)/mean1_i
############################################################################

#mortality - just do count na in 2024/total count in 2024
#birch
birch2024control = CWT_Salt%>% filter(year == 2024 & tree == 'B_pendula'& treatment == 0)
mortality.control = sum(is.na(birch2024control$value))/length(birch2024control$value)

birch2024treat = CWT_Salt%>% filter(year == 2024 & tree == 'B_pendula'& treatment == 1)
mortality.treatment = sum(is.na(birch2024treat$value))/length(birch2024treat$value)
