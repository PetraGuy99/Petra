##quick calculation and plots of means over the three time points

setwd('C:/dev/code/Petra')


library(dplyr)
library(ggplot2)


CWT_Salt = read.csv('../../data/CWT_Salt_ht.csv', colClasses =c(
 'character',
 'numeric',
 'integer',
 'character',
 'integer'
 
))

#get inital and final heights for each species and treatment

getheights = function(t, yr, treat){
  data = CWT_Salt%>% filter(treatment == treat)%>%
    filter(year == yr) %>%
    filter(tree == t)
  heights = na.omit(as.numeric(data$value)) # just the heights
  mean = mean(heights)
  return(mean)
}

#get control initial heights - input required tree, 'Q_robur' or 'B_pendula'
bp_control_0 = getheights('B_pendula', 0, 0)
bp_control_1 = getheights('B_pendula',1,0)
bp_control_2 = getheights('B_pendula',2,0)

bp_treat_0 = getheights('B_pendula', 0, 1)
bp_treat_1 = getheights('B_pendula',1,1)
bp_treat_2 = getheights('B_pendula',2,1)

qr_control_0 = getheights('Q_robur', 0, 0)
qr_control_1 = getheights('Q_robur',1,0)
qr_control_2 = getheights('Q_robur',2,0)

qr_treat_0 = getheights('Q_robur', 0, 1)
qr_treat_1 = getheights('Q_robur',1,1)
qr_treat_2 = getheights('Q_robur',2,1)


#convert to change in heights relative to year 0 - relative growth rate
bp_control_change_0 = 0
bp_control_change_1 = 100*(bp_control_1 - bp_control_0)/bp_control_0
bp_control_change_2 = 100*(bp_control_2 - bp_control_0)/bp_control_0

bp_treat_change_0 = 0
bp_treat_change_1 = 100*(bp_treat_1 - bp_treat_0)/bp_treat_0
bp_treat_change_2 = 100*(bp_treat_2 - bp_treat_0)/bp_treat_0

qr_control_change_0 = 0
qr_control_change_1 = 100*(qr_control_1 - qr_control_0)/qr_control_0
qr_control_change_2 = 100*(qr_control_2 - qr_control_0)/qr_control_0

qr_treat_change_0 = 0
qr_treat_change_1 = 100*(qr_treat_1 - qr_treat_0)/qr_treat_0
qr_treat_change_2 = 100*(qr_treat_2 - qr_treat_0)/qr_treat_0

data = data.frame(
  deltah = c(bp_control_change_0, bp_control_change_1, bp_control_change_2, 
             bp_treat_change_0, bp_treat_change_1, bp_treat_change_2,
             qr_control_change_0, qr_control_change_1, qr_control_change_2,
             qr_treat_change_0, qr_treat_change_1, qr_treat_change_2),
  tree = c(rep('B_pendula',6), rep('Q_robur',6)),
  treatment = rep(c(rep('control',3), rep('treatment',3)),2),
  time = rep(c(1,2,3),4)
)

ggplot(data, aes(x = time, y = deltah, colour = treatment, shape = tree))+
  geom_point(size = 5)+
  geom_line()+
  scale_color_manual(values = c("control" = "orange", "treatment" = "green")) +
  labs(title = 'Relative growth rate over three measurement points',
       y= 'Relative growth rate %',
       x = 'time point')+
  theme_classic()+
  theme(
    text = element_text(size = 16),          # Increases all text
    plot.title = element_text(size = 20),    # Larger title
    axis.title = element_text(size = 18),    # Larger axis titles
    axis.text = element_text(size = 14),     # Larger axis tick labels
    legend.title = element_text(size = 16),  # Larger legend title
    legend.text = element_text(size = 14)    # Larger legend text
  )+
  scale_x_continuous(breaks = c(1,2,3))
  
  

##look at rcd data
CWT_Salt_rcd =  read.csv('../../data/CWT_Salt_rcd.csv', colClasses =c(
    'character',
    'integer',
    'numeric'
    
  ))

bp_control = CWT_Salt_rcd %>% filter(Tree_Species == 'Betula_pendula' & Treatment == 0)
bp_treatment = CWT_Salt_rcd%>% filter(Tree_Species == 'Betula_pendula' & Treatment == 1)
bp_rcd_mean_0 = mean(bp_control$Rcd_mm)
bp_rcd_mean_1 = mean(bp_treatment$Rcd_mm)

qr_control = CWT_Salt_rcd %>% filter (Tree_Species == 'Quercus_robur' & Treatment == 0)  
qr_treatment = CWT_Salt_rcd %>% filter (Tree_Species == 'Quercus_robur' & Treatment == 1)
qr_rcd_mean_0 = mean(qr_control$Rcd_mm)  
qr_rcd_mean_1 = mean(qr_treatment$Rcd_mm)
  
  
  