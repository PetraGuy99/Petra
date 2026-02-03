setwd('C:/dev/code/Petra')


library(readxl)
library(dplyr)
library(tidyr)
library(ggpubr) 
library(ggplot2)

data = read.csv('../../data/occurrencerecords.csv')

#tidy dont need all cols
namelocation = data[,c(5,12,21)]

# keep only full species names (contain a space)
namelocation_clean <- namelocation %>%
  filter(grepl(" ", Scientific.name))

species_counts <- namelocation_clean %>%
  count(Scientific.name, name = "occurrences")  # <- explicitly call it 'occurrences'

# now filter numeric values
species_counts_small <- species_counts %>%
  filter(occurrences <= 100)

# histogram of species frequencies for 'rare' species
ggplot(species_counts_small, aes(x = occurrences)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Frequency distribution of species occurrences (≤100)",
    x = "Number of records per species",
    y = "Number of species"
  ) +
  theme_minimal()

# define bins
breaks <- c(0, 5, 10, 20, 50, 100, 500, Inf)
labels <- c("1-5", "6-10", "11-20", "21-50", "51-100", "101-500", ">500")

# cut occurrences into bins
species_counts <- species_counts %>%
  mutate(count_bin = cut(occurrences, breaks = breaks, labels = labels, right = TRUE))

# summarize frequency table
freq_table <- species_counts %>%
  group_by(count_bin) %>%
  summarise(num_species = n(), .groups = "drop")

freq_table


###############################################################################

library(rgbif)
library(dplyr)
#querying this on GBIF - not downloading locally
#have reduced the number of classes by filtering via Functional Traits and selecting just ecto
# and sapro. Then further quick look at all the classes to see which are microscopic or
#water based for example and deleting those
#idea being to only be querying a subset of relevant GBIF data, otherwise its too big

classes = c('Agaricomycetes','Atractiellomycetes','Calcarisporiellomycetes',
  'Cystobasidiomycetes','Dacrymycetes', 'Dothideomycetes','Eurotiomycetes', 
  'Geoglossomycetes','Kickxellomycetes','Mortierellomycetes',
  'Pezizomycetes','Polychytriomycetes','Saccharomycetes',
  'Sordariomycetes','Tremellomycetes')

# 2️⃣ Get species keys for all classes
all_species_keys <- c()

for(cls in classes) {
  class_info <- tryCatch(name_backbone(name = cls, rank = "class"), error = function(e) NULL)
  if(is.null(class_info) || is.null(class_info$usageKey)) next
  class_key <- class_info$usageKey
  
  species_list <- tryCatch(
    name_lookup(higherTaxonKey = class_key, rank = "species", limit = 10000),
    error = function(e) NULL
  )
  
  if(is.null(species_list$data) || nrow(species_list$data) == 0) next
  
  # detect correct key column
  key_col <- intersect(c("usageKey","key","taxonKey"), colnames(species_list$data))[1]
  
  species_data <- species_list$data %>% filter(!is.na(.data[[key_col]]))
  all_species_keys <- c(all_species_keys, species_data[[key_col]])
}

all_species_keys <- unique(all_species_keys)
length(all_species_keys)  # check how many species we have

# 3️⃣ Build the download query
# We convert species keys into GBIF query string
species_query <- paste0("taxonKey IN (", paste(all_species_keys, collapse = ","), ")")
filters <- paste0(
  species_query,
  " AND country = 'GB'",
  " AND basisOfRecord = 'HUMAN_OBSERVATION'",
  " AND eventDate >= '2000-01-01'",
  " AND occurrenceStatus = 'PRESENT'"
)

# 4️⃣ Submit the download request
# You must provide your GBIF username, password, and email
download_key <- occ_download(
  filters,
  user = "pguy",
  pwd = "a6khyNNU3PnSspx",
  email = "petra.guy.uk@gmail.com"
)

download_key


##another way?

library(rgbif)
library(dplyr)


class_keys <- vapply(classes, function(cls) {
  info <- tryCatch(name_backbone(name = cls, rank = "class"), error = function(e) NULL)
  if(!is.null(info) && "usageKey" %in% names(info)) info$usageKey else NA_integer_
}, FUN.VALUE = integer(1))  

class_keys <- class_keys[!is.na(class_keys)]

# 3️⃣ Build GBIF download predicates
preds <- c(
  pred_in("taxonKey", class_keys),                 # all species under the classes
  pred_eq("country", "GB"),                        # UK only
  pred_eq("basisOfRecord", "HUMAN_OBSERVATION"),  # human observations only
  pred_between("eventDate", "2000-01-01", "2100-12-31"), # after 2000
  pred_eq("occurrenceStatus", "PRESENT")          # must be present
)

# 4️⃣ Submit the download request
# Replace with your GBIF credentials
download_key <- occ_download(
  filters = filters,
  user = "pguy",
  pwd = "a6khyNNU3PnSspx",
  email = "petra.guy.uk@gmail.com"
)

download_key 

