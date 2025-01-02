setwd('C:/dev/code/Petra')
library(tidyr)
library(dplyr)
library(purrr)
library(writexl)
library(readxl)
# Load the species and functional traits data
species <- read_xlsx('../../data/Coilte_eDNA_Data.xlsx', sheet = 'Coilte_spp')
functionaltraits <- read.csv('../../data/FunctionalTraits.csv')

# Clean the species data: remove rows with empty species or certain values and rename 'genus' to 'GENUS'
#this removes all rows where OTU is at genus level or above
species_df_specieslevel <- species %>%
  filter(species != "" & species != "sp" & species != "unclassified" & !is.na(species)) 

# Merge species data with functional traits, keeping all species from species_df_specieslevel
Coilte.functionaltraits <- species_df_specieslevel %>%
  left_join(functionaltraits, by = 'GENUS') %>%
  select(GENUS, species, primary_lifestyle)

# Load proportions data and convert to presence-absence
proportions <-  read_xlsx('../../data/Coilte_eDNA_Data.xlsx', sheet = 'proportion-Coilte')
pres_abs <- proportions %>%
  mutate(across(everything(), ~ ifelse(. > 0, 1, 0)))


# Clean column names by removing 's__', 'o_' and 'g_ prefix and update column headers
colnames(pres_abs)[-1] <- sub("^s__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^g__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^o__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^f__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^k__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^p__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^c__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[1] <- 'sample_name'

#take first column from proportions (the site name), and stick it back on to the pres abs 
pres_abs = pres_abs[,-1] # remove the site column which has become 0s
site = c(12:36)# rename in simpler format
pres_abs = cbind.data.frame(site, pres_abs) #stick it back on
names(pres_abs)[names(pres_abs) == "...1"] <- "site" #its called 1, rename


# Prepare the working fungi species list (genus_species)
#this is the emf that occur in Coilte data
emf <- Coilte.functionaltraits %>% filter(primary_lifestyle == 'ectomycorrhizal')

#select the emf at each 'site' by matching emf to the pres-abs
#remove columns where column name is not found in emf df
#join genus_species to make cols in the right format, then filter

emf_vector = paste(emf$GENUS, emf$species, sep = '_')
emf_pres_abs = pres_abs %>% select(all_of(emf_vector))
emf_pres_abs = cbind.data.frame(site, emf_pres_abs)

#select the emf found in trial site pre-planting
#this is samples 12 - 18
open = emf_pres_abs %>% filter (site >= 12 & site <=19) %>% select(where(~sum(.) !=0))
trees = emf_pres_abs %>% filter(site >= 31 & site <= 36)%>% select(where(~sum(.) !=0))

spruce1 = emf_pres_abs %>% filter(site == 19 ) %>% select(where(~sum(.) !=0))
spruce2 = emf_pres_abs %>% filter(site == 20 ) %>% select(where(~sum(.) !=0))
spruce3 = emf_pres_abs %>% filter(site == 21 ) %>% select(where(~sum(.) !=0))
spruce3 = emf_pres_abs %>% filter(site == 22 ) %>% select(where(~sum(.) !=0))
spruce4 = emf_pres_abs %>% filter(site == 23 ) %>% select(where(~sum(.) !=0))
spruce5 = emf_pres_abs %>% filter(site == 24 ) %>% select(where(~sum(.) !=0))

pine1 = emf_pres_abs %>% filter(site == 26) %>% select(where(~sum(.) !=0))
pine2 = emf_pres_abs %>% filter(site == 27) %>% select(where(~sum(.) !=0))
pine3 = emf_pres_abs %>% filter(site == 28) %>% select(where(~sum(.) !=0))
pine4 = emf_pres_abs %>% filter(site == 29) %>% select(where(~sum(.) !=0))
pine5 = emf_pres_abs %>% filter(site == 30) %>% select(where(~sum(.) !=0))



