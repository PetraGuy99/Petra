setwd('C:/dev/code/Petra')
library(tidyr)
library(dplyr)

proportions =  read.csv('../../data/Fordie_proportions.csv')

toprow = colnames(proportions[,-1])

tidy1 <- sub("^s__", "", toprow) # remove the s_

#put this back as the top row
newtoprow= c('sample_mame', tidy1)
colnames(proportions) =  newtoprow


#save as df and split out genus and species
species_df = as.data.frame(tidy1)
species_df.split = separate(species_df, tidy1, into = c("GENUS", "species"), sep = "_")

#delete rows where second column is empty or says sp
species_df_specieslevel = species_df.split %>%
  filter(species != "" & species != "sp" & !is.na(species))

#now extract emf
functionaltraits = read.csv('../../data/FunctionalTraits.csv')
allemf = functionaltraits %>% filter(primary_lifestyle == 'ectomycorrhizal')

Fordie.emf = species_df_specieslevel %>% inner_join(allemf, by = 'GENUS') %>%
  select(GENUS, species)

genus = c('Elaphomyces', 'Imleria', 'Thelephora','Polyozellus')

emf_proportions = proportions %>%
  select(sample_mame, which(sapply(colnames(proportions), function(col) {
    any(sapply(genus, function(sub) grepl(sub, col)))
  })))

emf_proportions <- emf_proportions %>%
  mutate(row_sums = rowSums(select(., -sample_mame))) 

non_zero_samples = emf_proportions %>% filter(row_sums != 0)
         