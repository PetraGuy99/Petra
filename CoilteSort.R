setwd('C:/dev/code/Petra')
library(tidyr)
library(dplyr)
library(purrr)
library(writexl)
# Load the species and functional traits data
species <- read.csv('../../data/Coite_spp.csv')
functionaltraits <- read.csv('../../data/FunctionalTraits.csv')

# Clean the species data: remove rows with empty species or certain values and rename 'genus' to 'GENUS'
#this removes all rows where OTU is at genus level or above
species_df_specieslevel <- species %>%
  filter(species != "" & species != "sp" & species != "unclassified" & !is.na(species)) 

# Merge species data with functional traits, keeping all species from species_df_specieslevel
Coite.functionaltraits <- species_df_specieslevel %>%
  left_join(functionaltraits, by = 'GENUS') %>%
  select(GENUS, species, primary_lifestyle)

# Load proportions data and convert to presence-absence
proportions <- read.csv('../../data/Coilte_proportions.csv')
pres_abs <- proportions %>%
  mutate(across(-X, ~ ifelse(. != 0, 1, 0))) %>%
  rename(location = X)

# Clean column names by removing 's__', 'o_' and 'g_ prefix and update column headers
colnames(pres_abs)[-1] <- sub("^s__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^g__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^o__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^f__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^k__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^p__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[-1] <- sub("^c__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[1] <- 'sample_name'

# Prepare the working fungi species list (genus_species)
#this is all the fungi that occur in the Coilte data
working_fungi <- paste(Coite.functionaltraits$GENUS, Coite.functionaltraits$species, sep = '_')

#delete this

# Rename specific columns in presence-absence data to match the required format
pres_abs <- pres_abs %>%
  rename(
    'Ilyonectria_mors-panacis' = 'Ilyonectria_mors.panacis',
    'Nadsonia_starkeyi-henricii' = 'Nadsonia_starkeyi.henricii',
    'Spirosphaera_carici-graminis'= 'Spirosphaera_carici.graminis'
  )

# Create working fungi presence-absence data frame
pres_abs.workingfungi <- pres_abs %>%
  select(sample_name, all_of(working_fungi)) %>%
  rename(samplename = sample_name)

# Function to process each row of the data
process_row <- function(row) {
  # Clean the row by removing columns with value 0
  row_cleaned <- row[, row[1, ] != 0]
  
  # Transpose the cleaned row and set column names
  row_cleaned <- as.data.frame(t(rbind(colnames(row_cleaned), row_cleaned)), row.names = FALSE)
  colnames(row_cleaned) <- as.character(row_cleaned[1, ])
  row_cleaned <- row_cleaned[-1, ]  # Remove the first row (which is now the column names)
  
  # Split the 'samplename' column into 'GENUS' and 'species'
  site <- row_cleaned %>%
    separate(samplename, into = c("GENUS", "species"), sep = "_")
  
  # Merge with functional traits data and select necessary columns
  site_traits <- left_join(site, functionaltraits, by = "GENUS") %>%
    select(GENUS, species, primary_lifestyle)
  
  return(site_traits)
}

# Apply the processing function across all rows in the presence-absence dataframe
result_list <- pres_abs.workingfungi %>%
  pmap(~ process_row(data.frame(...)))


write_xlsx(result_list, path = '../../results/Fordiespecies.xlsx')
