setwd('C:/dev/code/Petra')


library(dplyr)
library(ggplot2)

Coilte= read.csv('../data/Coilte.csv', colClasses =c(
  'integer',
  'character',
  'numeric',
  'character',
  'character',
  'numeric',
  'integer',
  'character',
  'integer'
))

#several tree spp here
# trees do correspl=ond so do the delta h type plots#################
#trees = Salix_spp, Q_spp, Alnus_spp, C_avellana, P_sylvestris, B_pendula

t = 'Q_spp'

getheights = function(yr, treat, tree){
  data = Coilte%>% filter(treatment == treat)%>%
    filter(year == yr) %>%
    filter(tree == t) %>%
    select(tree_id, value, year)
  return(data)
}


controli = getheights(2023, 0, t)
controlf = getheights(2024,0, t)

control = as.data.frame(cbind(controlf,controli))
colnames(control) = c('tree_id2','value2','year2','tree_id1','value1','year1')
control$deltah = control$value2 - control$value1

pelleti = getheights(2023,1,t)
pelletf = getheights(2024,1,t)


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


write.csv(data, 'Q_spp_BD.csv')

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
       subtitle = t,
       x = 'treatment',
       y =  'change in mean tree height(cm)')+
  theme(text=element_text(size = 20))+
  theme_bw()

t.test(pellet$deltah, control$deltah)


########################################
control = Coilte %>% filter(tree == t & year == 2024 & treatment == 0)
mortality.control = sum(is.na(control$value))/length(control$value)


pellet = Coilte %>% filter(tree == t & year == 2024 & treatment == 1)
mortality.pellet = sum(is.na(pellet$value))/length(pellet$value) 

# (NAs in contrl/treatment), (num values in control/treatment)

prop.test(c(sum(is.na(control$value)), sum(is.na(pellet$value))), 
          c(length(control$value), length(pellet$value)))


###ARCHIVE##################################

treespp = 'Salix_spp'

getmeanh(treespp = 'Salix_spp')
getmeanh = function(treespp){
  tree_control = Coilte %>% filter(tree == treespp & treatment == 0 & year == 2024)
  control_h = na.omit(as.numeric(tree_control$value)) # just the heights
  tree_treatment = Coilte %>% filter(tree == treespp & treatment == 1 & year == 2024)
  treatment_h = na.omit(as.numeric(tree_treatment$value))
  
  data = data.frame(
    value = c(control_h, treatment_h),
    group = factor(rep(c('control', 'pellet'), times = c(length(control_h), length(treatment_h))))
  )
  
  anova_result = aov(value ~ group, data = data)
  summary(anova_result)
  
  control = data %>% filter(group == 'control') 
  pellet = data %>% filter(group == 'pellet')
  
  ht_0= control$value
  ht_1 = pellet$value
  
  welcest = t.test(ht_0, ht_1)
  print(welcest)
  
  
  #make a plot
  summary_stat = data %>%
    group_by(group) %>%
    summarize(
      mean = mean(value),
      se = sd(value)/sqrt(n())
    )
  subtitle = treespp
  ggplot(summary_stat, aes(x = group, y = mean, fill = group))+
    geom_point(stat = 'identity', colour = 'black', position = position_dodge(0.9))+
    geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, position = position_dodge((0.9)))+
    labs(title = 'Differences in mean height for treatment and control 2024',
         subtitle = treespp,
         x = 'treatment',
         y =  'mean height')+
    theme_minimal()
}