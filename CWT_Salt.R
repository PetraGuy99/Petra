setwd('C:/dev/code/Petra')


library(dplyr)
library(ggplot2)


CWT_Salt = read.csv('../../data/CWT_Salt.csv', colClasses =c(
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

#tree numbers did not match in 2023/2024 data sets, so only plt mean height possible for 
#first two data points
# 100 trees of each species retagged autumn 2024 - and thereafter change in tree ht
#is possible.


################################################################################

#this to get change in PLOT MEAN HtS for first two data points when no tree numbers

treespp = 'B_pendula'


getheights = function(t, yr, treat){
  data = CWT_Salt%>% filter(treatment == treat)%>%
    filter(year == yr) %>%
    filter(tree == t)
  heights = na.omit(as.numeric(data$value)) # just the heights
  return(heights)
}

#get control initial heights - input required tree, 'Q_robur' or 'B_pendula'
controli = getheights(treespp,2023,0)
controlf = getheights(treespp,2024, 0)

#get treatment average plot heights
pelleti = getheights(treespp,2023,1)
pelletf = getheights(treespp,2024,1)

data = data.frame(
  value = c(controli, controlf, pelleti, pelletf),
  group = factor(rep(c('controli','controlf','pelleti','pelletf'),
                     times = c(length(controli), length(controlf),
                               length(pelleti), length(pelletf))))
)
summary_stat = data %>%   #work out the mean and se of the means
  group_by(group) %>%
  summarize(
    mean = mean(value),
    se = sd(value)/sqrt(n())
  )

#the average plot change
getrelchange = function(initial,final){ # calculate the mean change in ht
  change = ((mean(final)-mean(initial))/mean(initial))*100
  return(change)
}

control.relchange = getrelchange(controli, controlf)
treat.change = getrelchange(pelleti, pelletf)


#this to plot the error bars

summary_stat$se2 = (summary_stat$se)^2

term1 = summary_stat[1,2]/summary_stat[2,2]
term2 = summary_stat[1,4]/summary_stat[1,2]
term3 = summary_stat[2,4]/summary_stat[2,2]

controlchange.se = unlist(term1*sqrt(term2+term3))

term1 = summary_stat[3,2]/summary_stat[4,2]
term2 = summary_stat[3,4]/summary_stat[3,2]
term3 = summary_stat[4,4]/summary_stat[4,2]

pelletchange.se = unlist(term1*sqrt(term2+term3))

#new df of means and errors
data = data.frame(
  group = c('control','pellet'),
  value = c(control.relchange, treat.change),
  se = c(controlchange.se, pelletchange.se )
  
  
)

ggplot(data, aes(x = group, y = value)) +
  geom_point(aes(fill = group), size = 5, shape = 21, colour = 'black', 
             position = position_dodge(0.9), show.legend = FALSE) +
  geom_errorbar(aes(ymin = value-se, ymax = value+se), width = 0.2, position = position_dodge((0.9)))+
  scale_fill_manual(values = c("orange", "green")) +  # Set specific fill colors for points
  scale_color_manual(values = c("orange", "green")) + # Apply color to ensure consistency
  labs(
    title = 'Differences in plot mean height for treatment and control 2023 - 2024',
    subtitle = treespp,
    x = 'Treatment',
    y = 'Mean Height'
  ) +
  theme_minimal() +
  theme(text = element_text(size = 15))


#############################################################################
#### now add in the third data point and plot a time series
#note that this is bit wrong because third mean is random resample of 100 trees
#first two points sampled more trees

#read the new 2024 data
SALT_end2024 = read.csv('../../data/Saltersford_tagging_2024_2.csv')

treespp = 'Quercus_robur'


qr_control_ht = SALT_end2024%>% filter(Treatment == 'control')%>% filter(Tree_Species == treespp) %>%
  select(Height_cm)

qr_treat_ht = SALT_end2024%>% filter(Treatment == 'treatment')%>% filter(Tree_Species == treespp) %>%
  select(Height_cm)

qr_control_meanht = mean(qr_control_ht$Height_cm)
qr_treat_meanht = mean(qr_treat_ht$Height_cm)

treespp = 'Betula_pendula'

bp_control_ht = SALT_end2024%>% filter(Treatment == 'control')%>% filter(Tree_Species == treespp) %>%
  select(Height_cm)
bp_treat_ht  = SALT_end2024%>% filter(Treatment == 'treatment')%>% filter(Tree_Species == treespp) %>%
  select(Height_cm)

bp_control_meanht = mean(bp_control_ht$Height_cm)
bp_treat_mean_ht = mean(bp_treat_ht$Height_cm)


#create df for the 3 points in the time series

salt = data.frame(
  tree = c(rep('Betula_pendula'),3, rep('Quercus_robur'), 3),
  time = c('0','1','2','0','1','2'),
  treatment = c(rep('control',3), rep('treatment',3)),
  deltah = c()







#get control initial heights - input required tree, 'Q_robur' or 'B_pendula'
controli = getheights(treespp,0)
controlf = getheights(treespp,2024, 0)

#get treatment average plot heights
pelleti = getheights(treespp,2023,1)
pelletf = getheights(treespp,2024,1)

#get the averages for 2023, 2024 beginning and 2024 end.

qr_pellet_control = 




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

ggplot(summary_stat, aes(x = group, y = mean)) +
  geom_point(aes(fill = group), size = 5, shape = 21, colour = 'black', 
             position = position_dodge(0.9), show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                width = 0.2, colour = 'black', position = position_dodge(0.9)) +
  scale_fill_manual(values = c("orange", "green")) +  # Set specific fill colors for points
  scale_color_manual(values = c("orange", "green")) + # Apply color to ensure consistency
  labs(
    title = 'plot mean height for treatment and control 2024',
    subtitle = treespp,
    x = 'Treatment',
    y = 'Mean Height'
  ) +
  theme_minimal() +
  theme(text = element_text(size = 15))
}


treespp = 'Q_robur'
treespp = 'B_pendula'

getmeanh(treespp)
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
