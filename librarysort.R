setwd('C:/dev/code/Petra')
library(readxl)
library(dplyr)
library(tidyr)

library = read.csv('../../data/ectos.csv')
traits = read.csv('../../data/FunctionalTraits.csv')

#not sure why I have col 2 or 8
library = library[,-c(2,8)]
colnames(library) = c('code','species','seq','prov','host','trait')

#create the genus col so can merge
library$GENUS <- sub("_.*", "", library$species)

#now merge
library <- library %>%
  left_join(traits %>% select(GENUS, Ectomycorrhiza_exploration_type_template),
            by = "GENUS")
names(library)[names(library) == "Ectomycorrhiza_exploration_type_template"] <- "exploration_type"

#take ectos only

ectos = library %>% filter(trait == 'Ectomycorrhizal')

short <- ectos %>%
  filter(exploration_type %in% c("contact", "short", "short-distance_delicate"))

long = ectos %>% 
  filter(exploration_type %in% c("long-distance"))

medium = ectos %>%
  filter(exploration_type %in% c("medium-distance_fringe", "medium-distance_smooth"))
