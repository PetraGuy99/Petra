#looking at nursery species
setwd('C:/dev/code/Petra')
library(readxl)
library(dplyr)
library(tidyr)

set1 =  read_excel('../../data/seq_data_11478.xlsx', sheet = 'spp_site')
set2 = read_excel('../../data/seq_data_12900.xlsx', sheet = 'spp_site')
set3 = read_excel('../../data/seq_data_13478.xlsx', sheet = 'spp_site')

#split the species columns so it can be merged with Functional Traits
set1.split = set1 %>% separate(Taxonomy, into = c('GENUS','species'), 
                               sep = '_', extra = 'merge', fill = 'right')
set2.split = set2 %>% separate(Taxonomy, into = c('GENUS','species'), 
                               sep = '_', extra = 'merge', fill = 'right')
set3.split = set3 %>% separate(Taxonomy, into = c('GENUS','species'), 
                               sep = '_', extra = 'merge', fill = 'right')

FunctionalTraits =  read.csv('../../data/FunctionalTraits.csv')

#merge with Functional traits
set1.merge = left_join(set1.split, FunctionalTraits, by = 'GENUS')
set2.merge = left_join(set2.split, FunctionalTraits, by = 'GENUS')
set3.merge = left_join(set3.split, FunctionalTraits, by = 'GENUS')


cheviot_cell = set1.merge %>% select(starts_with("H"))
cheviot_cell_bp = cbind.data.frame(set1.merge[,c(1:2,286)],cheviot_cell[,c(1:20)])
cheviot_cell_qr = cbind.data.frame(set1.merge[,c(1:2,286)],cheviot_cell[c(21:40)])

maelor_bare = set2 %>% select(starts_with(("J")))
maelor_bare_bp = cbind.data.frame(set2.merge[,c(1:2,70)],maelor_bare[,c(1:20)])
maelor_bare_qr = cbind.data.frame(set2.merge[,c(1:2,70)],maelor_bare[,c(21:40)])

alba = set3 %>% select(starts_with("K"))
alba_cell_bp = cbind.data.frame(set3.merge[,c(1:2,73)],alba[,c(1:20)])
alba_cell_qr = cbind.data.frame(set3.merge[,c(1:2,73)],alba[,c(21:40)])
alba_bare_bp = cbind.data.frame(set3.merge[,c(1:2,73)],alba[,c(41:60)])


#filter the emf only bt remove zero rows as well 
remove_zero_rows <- function(df) {
  df[rowSums(df[, -c(1:3), drop = FALSE]) != 0, ]
}
process_occurrence <- function(df) {
  df %>%
    mutate(across(-(1:3), ~ ifelse(. > 0, 1, .))) %>%
    rowwise() %>%
    mutate(occurrence = sum(c_across(-(1:3))) / 20) %>%
    ungroup()
}

alba_bare_bp.emf = alba_bare_bp %>% filter(primary_lifestyle == 'ectomycorrhizal') %>%  
  filter(rowSums(across(-(1:3))) != 0) %>% process_occurrence() 

alba_cell_bp.emf = alba_cell_bp %>% filter(primary_lifestyle == 'ectomycorrhizal') %>%
  filter(rowSums(across(-(1:3))) != 0) %>% process_occurrence()

alba_cell_qr.emf = alba_cell_qr %>% filter(primary_lifestyle == 'ectomycorrhizal') %>%
  filter(rowSums(across(-(1:3))) != 0) %>% process_occurrence()

cheviot_cell_bp.emf = cheviot_cell_bp %>% filter(primary_lifestyle == 'ectomycorrhizal') %>% 
  filter(rowSums(across(-(1:3))) != 0) %>% process_occurrence()

cheviot_cell_qr.emf = cheviot_cell_qr %>% filter(primary_lifestyle == 'ectomycorrhizal') %>%
  filter(rowSums(across(-(1:3))) != 0) %>% process_occurrence()

maelor_bare_bp.emf = maelor_bare_bp %>% filter(primary_lifestyle == 'ectomycorrhizal') %>%
  filter(rowSums(across(-(1:3))) != 0) %>% process_occurrence()

maelor_bare_qr.emf = maelor_bare_qr %>% filter(primary_lifestyle == 'ectomycorrhizal') %>%
  filter(rowSums(across(-(1:3))) != 0) %>% process_occurrence()

#create df of cols = emf, method (bare/cell), nursery (alba/cheviot/maelor), tree (bp/qr), occ (out of 20)
alba_bare_bp.summary = alba_bare_bp.emf[,c(1,2,24)] %>% mutate(nursery = "alba", method = "bare", tree = 'bp')
alba_cell_bp.summary = alba_cell_bp.emf[,c(1,2,24)] %>% mutate(nursery = "alba", method = "cell", tree = 'bp')
alba_cell_qr.summary = alba_cell_qr.emf[,c(1,2,24)] %>% mutate(nursery = "alba", method = "cell", tree = 'qr')
cheviot_cell_bp.summary = cheviot_cell_bp.emf[,c(1,2,24)] %>% mutate(nursery = "cheviot", method = "cell", tree = 'bp')
cheviot_cell_qr.summary = cheviot_cell_qr.emf[,c(1,2,24)] %>% mutate(nursery = "cheviot", method = "cell", tree = 'qr')
maelor_bare_bp.summary = maelor_bare_bp.emf[,c(1,2,24)] %>% mutate(nursery = "maelor", method = "bare", tree = 'bp')
maelor_bare_qr.summary = maelor_bare_qr.emf[,c(1,2,24)] %>% mutate(nursery = "maelor", method = "bare", tree = 'qr')

summary = rbind.data.frame(alba_bare_bp.summary, alba_cell_bp.summary, cheviot_cell_bp.summary, cheviot_cell_qr.summary,
                           maelor_bare_bp.summary, maelor_bare_qr.summary)
summary$emf = paste(summary$GENUS, summary$species)

library(ggplot2)
ggplot(summary, aes(x = emf, y = occurrence, fill = interaction(nursery, method, tree))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Fungal Species (EMF)", y = "Occurrence", fill = "Nursery-Method-Tree") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#how many trees had nothing at each
colSums(alba_bare_bp.emf[,-c(1,2,3,24)])
colSums(alba_cell_bp.emf[,-c(1,2,3,24)])
colSums(cheviot_cell_bp.emf[,-c(1,2,3,24)])
colSums(cheviot_cell_qr.emf[,-c(1,2,3,24)])
colSums(maelor_bare_bp.emf[,-c(1,2,3,24)])
colSums(maelor_bare_qr.emf[,-c(1,2,3,24)])
    