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

#tree numbers do not correspond, so chngae in height per tree cannot be caclculated
#different numbers of trees measured each year to due lack of time, so plot averages
#are across different sized datasets

###############################################################################
#this calculates the average plot height when we are working at plot level, not tree level.
getheights = function(t, yr, treat){
  data = CWT_Salt%>% filter(treatment == treat)%>%
    filter(year == yr) %>%
    filter(tree == t)
  heights = na.omit(as.numeric(data$value)) # just the heights
  return(heights)
}

#get control initial heights - input required tree, 'Q_robur' or 'B_pendula'

treespp = 'B_pendula'
controli = getheights(treespp, 2023,0)
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
       subtitle = treespp, 
       x = 'treatment',
       y =  'relative change in height %')+
  theme_bw()+
  theme(text=element_text(size = 20))

###############################################################################
#mortality here is number of unmeasured trees in 2024/number trees counted in 2024
#trees with no value in 2024 are dead - but these don't correspond to trees in 2023
#so this isn't the ideal metric, its saying - of 59 trees selected - how many were dead

treespp = 'Q_robur' # choose tree species

#select all treatment trees for 2024
tree_2024_01 = CWT_Salt %>% filter(tree == treespp& year == '2024' & treatment == '1') 

#select all control trees for 2024
tree_2024_00 = CWT_Salt %>% filter(tree == treespp& year == '2024' & treatment == '0') 

mortality_00 = sum(is.na(tree_2024_00$value))/length(tree_2024_00$value)
mortality_01 = sum(is.na(tree_2024_01$value))/length(tree_2024_01$value)

#Z test for proportions

# (NAs in contrl/treatment), (num values in control/treatment)
prop.test(c(sum(is.na(tree_2024_00$value)), length(tree_2024_00$value)), 
          c(sum(is.na(tree_2024_01$value)), length(tree_2024_01$value))) 

#############################################################################

#box plot for distributions
treespp = 'Q_robur'
controli = getheights(treespp, 2023,0)
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

colours = c('pelleti' = 'green', 'pelletf'= 'green', 
            'controli' = 'orange', 'controlf' = 'orange')

ggplot(data , (aes(x = group, y = value, fill = group)))+
  geom_boxplot()+
  scale_fill_manual(values = colours)+
 theme_bw()

#######################################################################
#normality test

shapiro.test(pelleti)
