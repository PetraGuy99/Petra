setwd('C:/dev/code')


library(dplyr)
library(ggplot2)
library(tidyverse)

Cloughton= read.csv('../data/Cloughton.csv', colClasses =c(
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
#just look at site 1 = plot 7, because Alison raised some questions about site 2
# lots of drought?


########CHANGE IN HEIGHTS PER TREE######################

Cloughton = Cloughton %>% filter(plot ==7) #repeat for plot 8 for Cloughton 2
#but at Clouton 2 tree numbers dont correspond - s skip first section

getheights = function(yr, treat){
  data = Cloughton %>% filter(treatment == treat)%>%
    filter(year == yr) %>%
    select(tree_id, value, year)
      return(data)
}


controli = getheights(2023, 0)
controlf = getheights(2024,0)

control = as.data.frame(cbind(controlf,controli))
colnames(control) = c('tree_id2','value2','year2','tree_id1','value1','year1')
control$deltah = control$value2 - control$value1

pelleti = getheights(2023,1)
pelletf = getheights(2024,1)

#remove the extra rows from year2

pelletf= pelletf %>% filter(row_number() <= n()-5)

pellet = cbind(pelletf, pelleti)
colnames(pellet) = c('tree_id2','value2','year2','tree_id1','value1','year1')
pellet$deltah = pellet$value2 - pellet$value1


##format for plot with se
#remove the NA rows

pellet = pellet %>% drop_na(deltah)
control = control %>% drop_na(deltah)


data = data.frame(
  value = c(control$deltah, pellet$deltah),
  group = factor(rep(c('control', 'pellet'), times = c(length(control$deltah), length(pellet$deltah))))
  )


write.csv(data, 'Tilhill1_BD.csv')
anova_result = aov(value ~ group, data = data)
summary(anova_result)

#make a plot
summary_stat = data %>%
  group_by(group) %>%
  summarize(
    mean = mean(value),
    se = sd(value)/sqrt(n())
  )

summary_stat$color = c('orange','green')


ggplot(summary_stat, aes(x = group, y = mean))+
  geom_point(stat = 'identity',position = position_dodge(0.9), show.legend = F,
             size = 5, color = summary_stat$color)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, position = position_dodge((0.9)))+
  labs(title = 'Mean change in tree height ',
       x = 'treatment',
       y =  'change in mean tree height(cm)')+
  theme(text=element_text(size = 20))+
theme_bw()

t.test(pellet$deltah, control$deltah)
####################################################################
###MORTALITY

control = Cloughton %>% filter(plot==  8 & year == 2024 & treatment == 0)
mortality.control = sum(is.na(control$value))/length(control$value) #8, 227


pellet = Cloughton %>% filter(plot== 8& year == 2024 & treatment == 1)
mortality.pellet = sum(is.na(pellet$value))/length(pellet$value) #3, 486

# (NAs in contrl/treatment), (num values in control/treatment)

prop.test(c(sum(is.na(control$value)), sum(is.na(pellet$value))), c(length(control$value), length(pellet$value)))

#######################################################################


###############################################################################
###############################################################################
#plot change values

#remember Cloughtn 1 and 2 have been set already at top of code

getheights = function(yr, treat){
  data = Cloughton%>% filter(treatment == treat)%>%
    filter(year == yr)
    heights = na.omit(as.numeric(data$value)) # just the heights
  return(heights)
}

#get control initial heights - input required tree, 'Q_robur' or 'B_pendula'

controli = getheights( 2023,0)
controlf = getheights(2024, 0)

#get treatment average plot heights
pelleti = getheights(2023,1)
pelletf = getheights(2024,1)

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
data$fillcolor = c('orange','green')

ggplot(data, aes(x = group, y = value), color = fillcolor)+
  geom_point(stat = 'identity',  position = position_dodge(0.9), show.legend = F, colour = data$fillcolor, size  = 5)+
  geom_errorbar(aes(ymin = value-se, ymax = value+se), width = 0.2, position = position_dodge((0.9)))+
  labs(title = 'Relative difference in plot mean heights',
       x = 'treatment',
       y =  'relative change in height %')+
  theme_bw()+
  theme(text=element_text(size = 20))

#############################################################################
###############################################################################
####ATCHIVE################################################################

tree_control = Cloughton %>% filter(treatment == 0 & year == 2024)
control_h = na.omit(as.numeric(tree_control$value)) # just the heights
tree_treatment = Cloughton %>% filter(treatment == 1 & year == 2024)
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
       x = 'treatment',
       y =  'mean height')+
  theme(text=element_text(size = 20))
  theme_minimal()


control = data %>% filter(group == 'control') 
pellet = data %>% filter(group == 'pellet')

ht_0= control$value
ht_1 = pellet$value

welcest = t.test(ht_0, ht_1)
print(welcest)


#for mortality - assuming the tree numbers do match - merge the two years because 
#additional control trees sampled in year 2 - these need to be filtered out

#for the treatment trees - 880 in 2023, 885 in 2024, so just count NAs
#for 2024 - remove control trees with tree id >550 because I think they are the extras
# then count NAs

#for control, 628 in 2024 and 377 in 2023, so remove the 2024 extras

tree_2024_01 = Cloughton %>% filter(year == '2024' & treatment == '1') 
tree_2024_00 = Cloughton %>% filter(year == '2024' & treatment == '0') 

mortality_01 = sum(is.na(tree_2024_01$value))/length(tree_2024_01$value)

tree_2024_0_subset = tree_2024_00 %>% filter (tree_id <= 550)
mortality_0 = sum(is.na(tree_2024_0_subset$value))/length(tree_2024_0_subset$value)

#Z-test for proportions
prop.test(c(70, 73), c(377, 885))



