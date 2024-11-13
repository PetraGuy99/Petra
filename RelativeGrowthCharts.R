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

#change in hts for the CWT Salt Birch
control.relchange.Salt.bp = summary_stat[1,2] -  summary_stat[2,2]
treat.change.Salt.bp = summary_stat[3,2] -  summary_stat[4,2]

####Dundreggan

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

Dund.control.relchange = getrelchange(cp3i,cp3f)
Dund.treat1.change = getrelchange(tp1i, tp1f)
Dund.treat2.change = getrelchange(tp2i, tp2f)
Dund.fert1.change = getrelchange(fp1i, fp1f)
Dund.fert2.change = getrelchange(fp2i, fp2f)

##Cloughton
Cloughton= read.csv('../../data/Cloughton.csv', colClasses =c(
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

Cloughton.pellet = pellet %>% drop_na(deltah)
Cloughton.control = control %>% drop_na(deltah)


data = data.frame(
  value = c(control$deltah, pellet$deltah),
  group = factor(rep(c('control', 'pellet'), times = c(length(control$deltah), length(pellet$deltah))))
)

Cloughton.pellet.change = mean(Cloughton.pellet$deltah)
Cloughton.control.change = mean(Cloughton.control$deltah)

##########################################
#make data fram of the changes for the three sites


CWT_Salt.growth.control = c(control.relchange.Salt.bp, 'Cheshire Wildlife Trust site 1, ex-pasture birch', 'Control')
CWT.Salt.growth.teat = c(treat.change.Salt.bp, 'Cheshire Wildlife Trust site 1, ex-pasture birch','Rhizopellet')
Cloughton.control.line = c(Cloughton.control.change, 'Tilhill Scarborough site 1, ex-pasture Sitka','Control')
Cloughton.treat.line = c(Cloughton.pellet.change, 'Tilhill Scarborough site 1, ex-pasture Sitka', 'Rhizopellet')
dund.control.line = c(Dund.control.relchange, 'Trees for Life, upland moor birch','Control')
dund.fert.line = c(Dund.fert1.change, 'Trees for Life, upland moor birch', 'Fertiliser')
dund.treat.line = c(Dund.treat1.change,'Trees for Life, upland moor birch','Rhizopellet')

df = rbind.data.frame(CWT_Salt.growth.control, CWT.Salt.growth.teat, 
                      Cloughton.control.line, Cloughton.treat.line,
                      dund.treat.line, dund.control.line, dund.fert.line)
colnames(df) = c('Growth','Site','Treatment')

df$Growth = round(as.numeric(df$Growth), 2)

data <- df

library(stringr)

# Create a grouped horizontal bar chart
relgrowth = ggplot(data, aes(x = Site, y = Growth, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5) +
  scale_fill_manual(values = c("Control" = "orange", "Rhizopellet" = 'darkgreen', "Fertiliser" = 'grey')) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
   coord_flip() +
  theme_minimal() +
  labs(title = "Relative growth rate 2023-2024",
       x = "Site", y = "Relative Growth %") +
  theme(legend.position = "top")

ggsave("../../results/relgrowth.png", plot = relgrowth, width = 6, height = 4, dpi = 300,bg = 'white')
