setwd('C:/dev/code/Petra')


library(dplyr)
library(ggplot2)

Dundreggan = read.csv('../../data/Dundreggan.csv', colClasses =c(
  'integer',
  'integer',
  'character',
  'character',
  'integer',
  'integer',
  'character',
  'integer'
))

#different numbers of trees in each treatment
#there are 3 treatments here 0 = no pellet, 1 - pellet, 2 = fertiliser
#all trees Betula pubescens
#trees not identified so we can do average heigh differences across treatment

#plot 3 is control only
#plot 1 is fert and pellet
#plot 2 is fert and pellet

################################################################################

############################################################################

#ignoring the tree ids - compare the averages across all the control and pellet
#treatments by tree species
#now we cant do a change in ht because we are not matching trees, we can 
#look for difference in mean ht between treatment and control - in 2024
#omit 2023 because things only just planted

# split by plot because they are slightly different

getheights = function(p, yr, treat){
  data = Dundreggan %>% filter(treatment == treat)%>%
    filter(year == yr) %>%
    filter(plot == p)
  heights = na.omit(as.numeric(data$value)) # just the heights
  return(heights)
}

#plot 1

tp1i = getheights(1,2023,1) #pellet, plot 1 , 2023 
tp1f = getheights(1,2024,1) # pellet, plot1, 2024

fp1i = getheights(1,2023,2) # fert, plot 1 , 2023
fp1f = getheights(1,2024,2) #fert, plot 1, 2024

# plot 2
tp2i = getheights(2,2023,1) #pellet, plot 2 , 2023 
tp2f = getheights(2,2024,1) # pellet, plot2, 2024

fp2i = getheights(2,2023,2) # fert, plot 2 , 2023
fp2f = getheights(2,2024,2) #fert, plot 2, 2024

#plot 3 - control only
cp3i = getheights(3, 2023, 0)
cp3f = getheights(3,2024, 0)

###make data frame to get the SE for year 1 and 2
data = data.frame(
  value = c(cp3i, cp3f, fp1i, fp1f, fp2i, fp2f, tp1i, tp1f, tp2i, tp2f),
  group = factor(rep(c('ci','cf','fp1i','fp1f','fp2i','fp2f','tp1i','tp1f','tp2i','tp2f'),
                     times = c(length(cp3i), length(cp3f),
                               length(fp1i),length(fp1f),
                               length(fp2i),length(fp2f),
                               length(tp1i), length(tp1f),
                               length(tp2i), length(tp2f))))
)


summary_stat = data %>%
  group_by(group) %>%
  summarize(
    mean = mean(value),
    se = sd(value)/sqrt(n())
  )


#now calculate percentage change for each group
 
getrelchange = function(initial,final){ # data is the summary stat for the values
  change = ((mean(final)-mean(initial))/mean(initial))*100
  return(change)
}

control.relchange = getrelchange(cp3i,cp3f)
treat1.change = getrelchange(tp1i, tp1f)
treat2.change = getrelchange(tp2i, tp2f)
fert1.change = getrelchange(fp1i, fp1f)
fert2.change = getrelchange(fp2i, fp2f)

#now need to use the se for each treatment to calculate se for each change
#add se2 col to summary stat
summary_stat$se2 = (summary_stat$se)^2

#control
term1 = summary_stat[1,2]/summary_stat[2,2]
term2 = summary_stat[1,4]/summary_stat[1,2]
term3 = summary_stat[2,4]/summary_stat[2,2]

controlchange.se = term1*sqrt(term2+term3)

#fertiliser plot 1
term1 = summary_stat[3,2]/summary_stat[4,2]
term2 = summary_stat[3,4]/summary_stat[3,2]
term3 = summary_stat[4,4]/summary_stat[4,2]
fert1change.se = term1*sqrt(term2+term3)

#fertiliser plot 2 
term1 = summary_stat[5,2]/summary_stat[6,2]
term2 = summary_stat[5,4]/summary_stat[5,2]
term3 = summary_stat[6,4]/summary_stat[6,2]
fert2change.se = term1*sqrt(term2+term3)

#treatment plot 1
term1 = summary_stat[7,2]/summary_stat[8,2]
term2 = summary_stat[7,4]/summary_stat[7,2]
term3 = summary_stat[8,4]/summary_stat[8,2]
treat1change.se = term1*sqrt(term2+term3)

#treatment plot 2
term1 = summary_stat[9,2]/summary_stat[10,2]
term2 = summary_stat[9,4]/summary_stat[9,2]
term3 = summary_stat[10,4]/summary_stat[10,2]
treat2change.se = term1*sqrt(term2+term3)

#now make data frame of changes with se


data = data.frame(
  group = c('control','fertiliser1','fertiliser2','pellet1','pellet2'),
  value = c(control.relchange, fert1.change, fert2.change, treat1.change, treat2.change))
  change.se = as.vector(c(controlchange.se, fert1change.se, fert2change.se, treat1change.se, treat2change.se))
data$se = unlist(change.se)


data$fillcolor = c('orange','yellow','yellow','green','green')

ggplot(data, aes(x = group, y = value), color = fillcolor)+
  geom_point(stat = 'identity',  position = position_dodge(0.9), show.legend = F, colour = data$fillcolor, size  = 5)+
  geom_errorbar(aes(ymin = value-se, ymax = value+se), width = 0.2, position = position_dodge((0.9)))+
  labs(title = 'Relative difference in plot mean heights',
       #subtitle = treespp, 
       x = 'treatment',
       y =  'relative change in height %')+
  theme_bw()+
  theme(text=element_text(size = 20))
############################################################################

#mortality
#there are 3 treatments here 0 = no pellet, 1 - pellet, 2 = fertiliser
#plot 1 = fert and pellet, treat 1 and 2
#plot 2 = fert and pellet, treat 1 and 2
#plot 3 = control only

#select all treatment trees for 2024
control = Dundreggan %>% filter(year == 2024 & treatment == 0) 
fert1 =  Dundreggan %>% filter(year == 2024 & treatment == 2 & plot == 1) 
fert2 = Dundreggan %>% filter(year == 2024 & treatment == 2 & plot ==2)
pellet1 = Dundreggan %>% filter (year == 2024 & treatment == 1 & plot == 1)
pellet2 = Dundreggan %>% filter(year == 2024 & treatment == 1 & plot == 2)

 

mortality_c = sum(is.na(control$value))/length(control$value)
mortality_fert1 = sum(is.na(fert1$value))/length(fert1$value)
mortality_fert2 = sum(is.na(fert2$value))/length(fert2$value)
mortality_pellet1= sum(is.na(pellet1$value))/length(pellet1$value)


#Z test for proportions

# (NAs in contrl/treatment), (num values in control/treatment)
prop.test(c(sum(is.na(tree_2024_00$value)), length(tree_2024_00$value)), 
          c(sum(is.na(tree_2024_01$value)), length(tree_2024_01$value))) 

#############################################################################  
  treatment1 = Dundreggan %>% filter(treatment == 1 & year == 2024 & plot == 1)
  treatment1_h = na.omit(as.numeric(treatment1$value))
  
  fertiliser1 = Dundreggan %>% filter(treatment == 2 & year == 2024 & plot == 1)
  fertiliser1_h = na.omit(as.numeric(fertiliser1$value))
  
  
  treatment2 = Dundreggan %>% filter(treatment == 1 & year == 2024 & plot == 2)
  treatment2_h = na.omit(as.numeric(treatment2$value))
  
  fertiliser2 = Dundreggan %>% filter(treatment == 2 & year == 2024 & plot == 2)
  fertiliser2_h = na.omit(as.numeric(fertiliser2$value))
  
  data = data.frame(
    value = c(control_h, fertiliser1_h, treatment1_h, fertiliser2_h, treatment2_h),
    group = factor(rep(c('1_control', '1_fertilser', '1_treatement','2_fertilser', '2_treatment'), 
                       times = c(length(control_h), 
                                 length(fertiliser1_h),length(treatment1_h),
                                 length(fertiliser2_h),
                                 length(treatment2_h) )))
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
    labs(title = 'Differences in mean height for treatment, fertilised and plots control 2024',
         x = 'treatment',
         y =  'mean height (cm)')+
        theme_minimal()



#########################################

#survival or mortality rate
#number dead = number NAs in 2024
#Mortality = number trees which died between 2023 and 2024/ number trees

tree_2023_02 = Dundreggan %>% filter(year == '2023'& treatment == '2')
tree_2023_01 = Dundreggan %>% filter(year == '2023'& treatment == '1')
tree_2023_00 = Dundreggan%>% filter(year == '2023'& treatment == '0')

tree_2024_02 = Dundreggan %>% filter(year == '2024' & treatment == '2') 
tree_2024_01 = Dundreggan %>% filter(year == '2024' & treatment == '1') 
tree_2024_00 = Dundreggan %>% filter(year == '2024' & treatment == '0') 

mortality_00 = sum(length(tree_2023_00$value))/length(tree_2024_00$value)
mortality_01 = sum(length(tree_2023_01$value))/length(tree_2024_01$value)
mortality_02 = sum(length(tree_2023_02$value))/length(tree_2024_02$value)

######mortality rates
#mortality as count nas in 2024
control2024 = Dundreggan%>%filter(year==2024&plot==3)
mortality.control = sum(is.na(control2024$value))/length(control2024$value)

fert22024 = Dundreggan%>%filter(year==2024&plot==2&treatment==2)
mortality.fert2 = sum(is.na(fert22024$value))/length(fert22024$value)


fert12024 = Dundreggan%>%filter(year==2024&plot==1&treatment==2)
mortality.fert1 = sum(is.na(fert12024$value))/length(fert12024$value)

#get mean hts each year

# do this by plot
#plot 1 - fertilizer and pellet

#year 1

treespp = 'B_pubescens'
#plot = 1
treat_i = Dundreggan %>% filter(tree == treespp & treatment == 1 & year == 2023 & plot == 1)
fert_i = Dundreggan %>% filter(tree == treespp & treatment == 2 & year == 2023 & plot ==1)
treat_i = na.omit(as.numeric(treat_i$value))
fert_i = na.omit(as.numeric(fert_i$value))

#year 2
treat_f = Dundreggan %>% filter(tree == treespp  & treatment == 1 & year == 2024 & plot == 1)
fert_f = Dundreggan %>% filter(tree == treespp  & treatment == 2 & year == 2024 & plot ==1)
treat_f = na.omit(as.numeric(treat_f$value))
fert_f = na.omit(as.numeric(fert_f$value))


meantreat1_i = mean(treat_i)
meantreat1_f = mean(treat_f)

meanfert1_i = mean(fert_i)
meanfert1_f = mean(fert_f)


relchange_treat1 = ((meantreat1_f - meantreat1_i)/meantreat1_i)*100
relchange_fert1 = ((meanfert1_f - meanfert1_i)/meanfert1_i)*100

treat1_max = (((meantreat1_f+1.13)-(meantreat1_i-1.13))/(meantreat1_i-1.13))*100
treat1_min= (((meantreat1_f-1.13)-(meantreat1_i+1.13))/(meantreat1_i+1.13))*100

fert1_max = (((meanfert1_f+1.32)-(meanfert1_i-1.32))/(meanfert1_i-1.32))*100
fert1_min= (((meanfert1_f-1.32)-(meanfert1_i+1.32))/(meanfert1_i+1.32))*100
#################################################################################



#plot = 2
treat_i = Dundreggan %>% filter(tree == treespp & treatment == 1 & year == 2023 & plot == 2)
fert_i = Dundreggan %>% filter(tree == treespp & treatment == 2 & year == 2023 & plot ==2)
treat_i = na.omit(as.numeric(treat_i$value))
fert_i = na.omit(as.numeric(fert_i$value))

#year 2
treat_f = Dundreggan %>% filter(tree == treespp  & treatment == 1 & year == 2024 & plot == 2)
fert_f = Dundreggan %>% filter(tree == treespp  & treatment == 2 & year == 2024 & plot ==2)
treat_f = na.omit(as.numeric(treat_f$value))
fert_f = na.omit(as.numeric(fert_f$value))


meantreat2_i = mean(treat_i)
meantreat2_f = mean(treat_f)

meanfert2_i = mean(fert_i)
meanfert2_f = mean(fert_f)

relchange_treat2 = ((meantreat2_f - meantreat2_i)/meantreat2_i)*100
relchange_fert2 = ((meanfert2_f - meanfert2_i)/meanfert2_i)*100

treat2_max = (((meantreat2_f+1.06)-(meantreat2_i-1.06))/(meantreat2_i-1.06))*100
treat2_min= (((meantreat2_f-1.06)-(meantreat2_i+1.06))/(meantreat2_i+1.06))*100

fert2_max = (((meanfert2_f+0.84)-(meanfert2_i-0.84))/(meanfert2_i-0.84))*100
fert2_min= (((meanfert2_f-0.84)-(meanfert2_i+0.84))/(meanfert2_i+0.84))*100
#########################################

#plot 3 - control only

reespp = 'B_pubescens'
plot = '3' #put plot in

#year 1
cont_i = Dundreggan %>% filter(tree == treespp & treatment == 0 & year == 2023 & plot == 3)
cont_i= na.omit(as.numeric(cont_i$value))

#year 2
cont_f = Dundreggan %>% filter(tree == treespp & treatment == 0 & year == 2024 & plot == 3)
cont_f = na.omit(as.numeric(cont_f$value))



meantcont_i = mean(cont_i)
meantcont_f = mean(cont_f)



relchange1_cont = ((meantcont_f - meantcont_i)/meantcont_i)*100

#eeror bars using max and min possible values
#se values from summary stats above



data = data.frame(
  value = c(relchange1_cont,relchange_fert1,relchange_fert2, relchange_treat1, relchange_treat2),
  group = c('control','fertiliser_1','fertiliser_2','treatment_1','treatment_2'),
  se = c(2.84,3.8,3.72,3.83,4.9))

ggplot(data, aes(x = group, y = value))+
  geom_point(stat = 'identity', colour = 'black', position = position_dodge(0.9), show.legend = F)+
  geom_errorbar(aes(ymin = value-se, ymax = value+se), width = 0.2, position = position_dodge((0.9)))+
  labs(title = 'Differences in mean height for treatment, fertilised and plots control 2024',
       x = 'treatment',
       y =  'mean height (cm)')+
  theme_minimal()

ggplot(data = data, aes(x = group, y = value))+
  geom_point(size = 5)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = 0.2, position = position_dodge((0.9)))+
  labs(title = 'relative change in tree height between 2023 and 2024',
       y = 'percentage change')

#########################################


