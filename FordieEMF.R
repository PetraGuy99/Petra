setwd('C:/dev/code/Petra')
library(tidyr)
library(dplyr)
library(purrr)
library(writexl)
library(readxl)
# Load the species and functional traits data
species <- read.csv('../../data/Fordie_eDNA_Data_species.csv')
functionaltraits <- read.csv('../../data/FunctionalTraits.csv')

# Clean the species data: remove rows with empty species or certain values and rename 'genus' to 'GENUS'
species_df_specieslevel <- species %>%
  filter(species != "" & species != "sp" & species != "unclassified" & !is.na(species)) %>%
  rename(GENUS = genus)

# Merge species data with functional traits, keeping all species from species_df_specieslevel
Fordie.functionaltraits <- species_df_specieslevel %>%
  left_join(functionaltraits, by = 'GENUS') %>%
  select(GENUS, species, primary_lifestyle)

# Load proportions data and convert to presence-absence
proportions <- read.csv('../../data/Fordie_proportions.csv')
pres_abs <- proportions %>%
  mutate(across(-X, ~ ifelse(. != 0, 1, 0))) %>%
  rename(location = X)

# Clean column names by removing 's__' prefix and update column headers
colnames(pres_abs)[-1] <- sub("^s__", "", colnames(pres_abs)[-1])
colnames(pres_abs)[1] <- 'sample_name'

# Prepare the working fungi species list (genus_species)
#these are all the OTUs to species level
working_fungi <- paste(Fordie.functionaltraits$GENUS, Fordie.functionaltraits$species, sep = '_')

# Rename specific columns in presence-absence data to match the required format
pres_abs <- pres_abs %>%
  rename(
    'Ilyonectria_mors-panacis' = 'Ilyonectria_mors.panacis',
    'Nadsonia_starkeyi-henricii' = 'Nadsonia_starkeyi.henricii'
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


#################################################################
#checking the OTUs and species names against my BLAST.
#Copy in just the OTU sheeet, filter this and get OTU numbers for the
#species so I can check those against my BLAST

#read the OTU table - this is the table with ALL the OTUs and the species hypotheses
#for each one
OTUs =  read.csv('../../data/Fordie_OTU_spp.csv')

#tidy
# Function to extract the last two non-NA entries
get_last_two_before_empty <- function(row) {
  row[row == ""] <- NA  # Convert empty strings to NA
  first_empty_idx <- which(is.na(row) | row == "")  # Find first empty cell
  
  if (length(first_empty_idx) > 0) {
    stop_idx <- first_empty_idx[1] - 1  # The last valid entry before an empty one
  } else {
    stop_idx <- length(row)  # If no empty, use full row
  }
  
  if (stop_idx >= 2) {
    return(c(row[stop_idx - 1], row[stop_idx]))  # Get last two valid entries
  } else if (stop_idx == 1) {
    return(c(NA, row[stop_idx]))  # Only one valid entry
  } else {
    return(c(NA, NA))  # No valid entries
  }
}
# Apply function to get last two entries for each row
last_two_entries <- as.data.frame(t(apply(OTUs, 1, get_last_two_before_empty)))


#tidy version of OTU table with just OTU, genus, species
OTUs_new = cbind.data.frame(OTUs$OTU, last_two_entries)

#remove all rows where last column is 'unclassified' or 'sp'
#need to remove, each Alnicola sp(98) - each final entry has the 
#match percentage - but we dont want to remove say alnicola sphaer...
#new table is just species level hypotheses, no unclassified or genus level only
OTUs_filtered = OTUs_new[!grepl("unclassified|sp\\(\\d+\\)", OTUs_new$V2), ]

#now to get the high matched. MBI data has the match in brackets after the spp
#remove all rows where match <97
# Function to check if 'Last' contains any text followed by '(n)' where n < 97
is_n_below_97 <- function(entry) {
  match <- regmatches(entry, regexpr(".*\\((\\d+)\\)", entry))  # Extract text(n)
  if (length(match) > 0) {
    n_value <- as.numeric(sub(".*\\((\\d+)\\)", "\\1", match))  # Extract numeric value
    return(n_value < 97)  # Remove if n < 97
  }
  return(FALSE)  # Keep other values
}

#this is now table of all the high % matches
OTUs.over96 <- OTUs_filtered[!sapply(OTUs_filtered$V2, is_n_below_97), ]

#now check reads table for singletons - these may  have already been removed - 
#open reads table, change to pres absence, sum each column - any col with colsum = 1
#would be removed because this OTU occurs in one location only.
#read table has reads for all OTUs = 4829, not just those presented by MBI after their filtering
reads =  read.csv('../../data/FordieReads.csv')

#tidy - delete cols 1 and 3 - introduced nonsense
reads = reads[,-c(1,3)]

#reads is all 4829 OTS, without the above filtering of spp >=97%
#select filtered OTUs from OTUs.over97 - then select only those cols
#from reads to give 661 in the reads table

keep_cols = c('Group',OTUs.over96$`OTUs$OTU`) #add the first coll on tho

#this is all reads matched to my filtering - spp level >=97%, but still contains low
#occurrence and singletons
reads_filtered <- reads[, colnames(reads) %in% keep_cols]

################################################################################
# Convert to presence-absence (1 if >1, else keep as is)
singleton.check <- reads_filtered[,-1] %>%
  mutate(across(everything(), ~ ifelse(. > 1, 1, .))) 

# Remove OTUs that occur only once (i.e., column sum == 1)
no_singletons <- singleton.check %>%
  select(where(~ sum(.) > 1))

# Get the remaining OTUs
filtered.OTU <- colnames(no_singletons)

# Filter reads data to retain only the selected OTUs
reads_filtered.2 <- reads %>%
  select(all_of(filtered.OTU))

reads_filtered.2 =  cbind.data.frame(reads$Group, reads_filtered.2) # no singletons - but DOES have low occurrence


###########################################################################

#now check colSums to see if there are low occurrence OTUs
colsums = colSums(reads_filtered.2)
names = colnames(reads_filtered.2)

occurrence = cbind.data.frame(names, colsums)

#count the OTUs left as you remove colsums <2,<3 etc

count_remaining_rows <- function(df, threshold) {
  df_filtered <- df %>%
    filter(colsums >= threshold)  # Keep rows where ColSum >= threshold
  return(nrow(df_filtered))  # Return the number of remaining rows
}
thresholds <- 2:500

remaining_counts <- sapply(thresholds, function(thresh) count_remaining_rows(occurrence, thresh))

plot_data <- data.frame(
  Threshold = thresholds,
  RemainingRows = remaining_counts
)
library(ggplot2)

ggplot(plot_data, aes(x = Threshold, y = RemainingRows)) +
  geom_line() +
  geom_point() +
  labs(title = "Remaining Rows vs. Threshold",
       x = "Threshold",
       y = "Remaining Rows") +
  theme_minimal()

####choose 200 as threshold and delete entries where read abundance < 200
reads_hi_occurrence.A <- cbind(reads_filtered.2[, 1, drop = FALSE], 
                               as.data.frame(lapply(reads_filtered.2[, -1], function(x) ifelse(x >= 200, 1, 0))))


#process these to a list of species at each location - using code above
#already have pres/absence df = reads_hi_occurrence, but it has OTUs rather than spp names
#replace OTU num with the matching spp names

#start with the OTUs.over96 df and modify
#remove the brackeys in the sp column and merge the genus spp to a single column

#craete dummy df to work on
OTUs.tidy = as.data.frame(OTUs.over96)

OTU.tidy$V2 =  gsub("\\(\\d+\\);", "", OTUs.tidy$V2)

###############################################################################
###############################################################################

#reverse enginear this question - we have 399 OTUs at this point, MI had 382 - which
#17 did they loose?
#attach these 399 OTUs back to a SH to wotk out what we have left
#select the OTUs with SH from the OTU_new above

#1. check the >97%, the 661 OTUs against the 382 final OTU in the site/spp table, and see whats missing

#MBI - final spp list = this is just the species from the Fordie eDNA spreadsheet
MBIspecies = read.csv('../../data/MBIspecies.csv')

#stick OTU nums on these so can be compared
#rename the OTUs.over 96 cols so can merge
colnames(OTUs.over96) = c('OTU','Genus','species')

#look at of MBI final 144 species, which ones of OTU_filtered are missing
#remove the (100); etc so i can merge on species, genus

OTUs.over96$species <- gsub("\\(\\d+\\)|;", "", OTUs.over96$species)

#which OTU ARE NOT in OTUs.over96 i.e. what were the additional filtering stages
missing_OTUs <- anti_join(OTUs.over96, MBIspecies, by = c("Genus", "species"))


#looks like a lot are singletons - do go the the no.singletons set

#2 compare the final MBI species set against my no singletons filtered read table

OTUs.nosingletons = as.data.frame(colnames(no_singletons))
colnames(OTUs.nosingletons) = 'OTU'

#of the 399 OTUs identified as spp level, >96, not a singleton - which of thes
#ARE NOT in the MBI final data set
missing_OTUs <- anti_join(OTUs.nosingletons, MBIspecies, by = c("Genus", "species"))

###############################################################################
#PLAN A
#running out of time for this reverse engineering - although it does look like
#MBI have deleted singletons, but some OTUs missing were not singletons, e.g. OTU0018, with reads 6627 and 19568
#OTU0035 had reads 17807, 11,1,1 - so if you consider <50 read abundance = 0, then it becomes a singleton
#OTU184 was deleted, with read abundance 890,69,929, OTU0193 read abundance 60,1349,371.


#start from reads_filtered = spp level >=97%, change any cell <200 to 0, keep all others
reads_hi_occurrence.A <- cbind(reads_filtered[, 1, drop = FALSE], 
                       as.data.frame(lapply(reads_filtered[, -1], function(x) ifelse(x >= 200, 1, 0))))

#these are the OTUs we then plan to keep based on our filters

#check how many of these are singletons...
#get colsums then check these to see if any are 1 - then that OTU only occurred once
#are any row sums == 1??
#make a new df of OTUs where row 64 - the row sums. == 1
#remove singletons - this is where low reads removed then you create singletons, then remove
no_singletons.A <- reads_hi_occurrence.A[,-1] %>%
  select(where(~ sum(.) > 1))

#or - remove singletons first, then remove low reads
#this is reads_filtered.2 ]#the difference here is that say you have 
#OTU001, reads = 0,1,5,60000,
#OTU002, reads = 0,0,200,0
#if you delete low reads first you create a singleton, which you then delete
#so OTU001 and OTU002 are lost
#If you delete singletons first then low reads, you loose OTU002, butOTU001 becomes rare - the low reads are now = 0

###PLAN B
##delete strict singletons - then delte low occurrence
#reads are now reads_filtered.2

#then remove low occurrence reads
no_singletons.B <- cbind(reads_filtered.2[, 1, drop = FALSE], 
                             as.data.frame(lapply(reads_filtered.2[, -1], function(x) ifelse(x >= 200, 1, 0))))


################################################################################
###############################################################################
###############################################################################

#MBI DATA USED GENBANK NOT UNITE - SO STARTING THE FILTERING AGAIN - USING OUR BLAST
#AND THEN MERGING INTO THEIR READ ABUNDANCE. 
#START FROPM THEIR READ ABUNDANCE INSTEAD OF THEIR FILTERED PROPORTIONS TABLE
#BECAUSE I DONT KNOW WHAT FILTERING THEY DID

#FIRST STEP IS TO MUNGE THE BLAST DATA INTO A NICE CSV
 
#read in first blast sheet
blast = read_excel('../../data/blast.xlsx', sheet = 'blast5', col_names = F)

#1. remove rows starting 'Length','Score','Identities','Strand','Sbjct','Query:'  
filter1 =blast[!grepl("^(Length|Score|Identities|Strand|Sbjct|Query:)", blast$...1), ]

#remove rows where all cols are NA - these were various text lines, unless they are the OTU number rows
filter2 <- filter1[rowSums(is.na(filter1[, 2:13])) < ncol(filter1[, 2:13]) | grepl("^Query \\d+ of \\d+: OTU_", filter1[[1]]), ]

#adda column for the OTU number
filter2$OTU = NA

otu_values <- NA  # Temporary variable to store current OTU_x
for (i in 1:nrow(filter2)) {
  match <- regmatches(filter2[i, 1], regexpr("^Query \\d+ of \\d+: (OTU_\\d+)", filter2[i, 1]))
  if (length(match) > 0) {
    otu_values <- match  # Update OTU_x value when a new query row is found
  }
  filter2$OTU[i] <- otu_values  # Assign the current OTU_x value to the row
}

#delete the rest of the text so it just says OTU_0003 or whatever
filter2$OTU <- sub("^.*?(OTU_\\d+)$", "\\1", filter2$OTU)

#now reove the rest of the rows with NAs, these are the OTU headers before the set of matches
filter3 <- filter2[rowSums(is.na(filter2[, 2:13])) < ncol(filter2[, 2:13]), ]

#remove the rows which start 'reference'
filter4 <- filter3[!grepl("^Reference", filter3[[1]]), ]

#only retain useful cols, SH, names and % match
filter5 = filter4[,c(1,2,5,8,14)]

#add some nice colnames
colnames(filter5) = c('Reference','SH','Taxon','Percent','OTU')

#split name into genus and spp
filter5$species <- sapply(strsplit(filter5$Taxon, " "), function(x) ifelse(length(x) > 1, x[2], NA))  # Assign the second word to Species if it exists

#remove rows where spp col is NA
filter6 <- filter5[!is.na(filter5$species), ]


#remove rows <97%
filter7 = filter6 %>% filter(Percent>=97)

#save this for reference
write.csv(filter7, '../../results/blast597.csv')

 #check the OTUs where there are multiple matches
library(dplyr)

#these are the OTUs where there are multiple spp matches - but NOT at the same %
#so that the top % match can be chosen
#NB - here multiple spp level matches of Laccaria say, at the same % are lost.
#So if you had L.laccata and L.amethystina both at 99% - they are lost
#Here we are extracting multiple genus level, of different genus
df_filtered <- filter7 %>%
  group_by(OTU) %>%
  filter(n_distinct(Taxon) > 1) %>%  # Keep only OTUs where Taxon name values DO NOT match
  filter(n_distinct(Percent) > 1) %>%  # Keep only OTUs where there is variation in Percent
  ungroup()

#now all take the top percent match of the multiple matches
df_top_percent <- df_filtered %>%
  group_by(OTU) %>%
  filter(Percent == max(Percent)) %>%  # Select all rows with the highest Percent per OTU
  ungroup()

#now - IF within those same % values filtered out - all the Taxon names are the same - keep, 
#else save to a different df to peruse later
df_top_percent_clean <- df_top_percent %>%
  group_by(OTU) %>%
  filter(n_distinct(Taxon) == 1) %>%  # Keep only OTUs where all Taxon values match
  ungroup()

#now there are still multiple rows per OTU, we need one row per OTU
df_top_item <- df_top_percent_clean %>%
  group_by(OTU) %>%
  slice_max(order_by = Percent, n = 1, with_ties = FALSE) %>%  # Keep only the highest Percent per OTU
  ungroup()
#the lost OTUs where multiple spp have same %
df_multiple_taxa <- df_top_percent %>%
  group_by(OTU) %>%
  filter(n_distinct(Taxon) > 1) %>%  # Keep OTUs where Taxon varies
  ungroup()

#then the OTUs which didnt have multiple matches - were all the same species for each OTU
#doesnt matter if same spp same % or same spp different %, just take top row
df_same_spp <- filter7 %>%
  group_by(OTU) %>%
  filter(n_distinct(Taxon) == 1) %>%  # Keep OTUs where all Taxon values are the same
  ungroup()

#and then take the first row
df_first_value <- df_same_spp %>%
  group_by(OTU) %>%
  slice_head(n = 1) %>%  # Select the first row for each OTU
  ungroup()


blast_OTUs = rbind.data.frame(df_first_value, df_top_item)
write.csv(blast_OTUs, '../../results/blast5_OTUs.csv')

#get the OTUs where taxon names was different - but percent match was the same - leave these out
#but what were they
df_filtered_out <- filter7 %>%
  group_by(OTU) %>%
  filter(n_distinct(Taxon) > 1) %>%  # Keep only OTUs where taxon name is different
  filter(n_distinct(Percent) == 1) %>%  # Keep only OTUs where Percent has no variation
  ungroup()

#all lost OTU = 
blast_OTUs_lost = rbind.data.frame(df_filtered_out, df_multiple_taxa)
write.csv(df_filtered_out,'../../results/blast5_OTUs_lost.csv')


#Now to combine all the OTUs into one vector and merge with the reads table - because the
#reads table has locations for each OTU. The matched OTUs we have selected to be >97% etc
#will be a subset of all OTUs contained in the reads table.
#Therefor - select the columns of the reads table where the OTU appears in our OTU set

#first collect all our matched OTUs
#path to excel data
file_path <- "../../data/blast.xlsx"

# Generate sheet names dynamically
sheet_names <- paste0("blast", 1:5, "_matches")

# Read all sheets into a list of data frames
blast_data <- lapply(sheet_names, read_excel, path = file_path, col_names = TRUE)

#combine into single df
blast_combined <- do.call(rbind, blast_data)

#read in the reads table
reads = read_xlsx('../../data/Fordie_eDNA_Data.xlsx',sheet = 'reads', col_names = T)

#tidy
reads =reads[,-c(1,3)]

#check - there are no cols where colsum = 1, i.e. no singletons (those entries with 1 read)
# Compute column sums
col_sums <- colSums(reads[,-1], na.rm = TRUE)

# Check if any column sum equals 1
any(col_sums == 1) #FALSE, i.e. yes - there are no singletons   

#checking low read occurrences
#what do reads per sample look like
#add col of total reads per sample
RowSum <- rowSums(reads[, -1], na.rm = TRUE)
#row 2 and 3 have unusually low reads

#examine row 2
row3_data <- as.numeric(reads[3, -1])  

row_data <- row3_data[row3_data != 0]

# Plot histogram
hist(row_data, breaks = breaks, main = "Histogram of Values at location S001Z0008", 
     xlab = "Value", ylab = "Frequency", col = "blue", border = "black", 
     xaxt = "n")  # Disable default x-axis ticks

# Add custom x-axis ticks
axis(1, at = 1:38, labels = 1:38)

#Site S001Z0007 has a low total read abundance, with one entry of 38 - look at this OTU
column_index <- which(reads[2, -1] == 38)

# Get the column name
column_name <- colnames(reads[column_index + 1] ) # Adding 1 to account for excluding the first column
column_name #+ OTU1843, this is a common sapro, no reason to think its weird, so leaving this in

#examine row 3

row_data <- row2_data[row2_data != 0]

breaks <- seq(1, 38, by = 1)

# Plot histogram
hist(row_data, breaks = breaks, main = "Histogram of Values at location S001Z0007", 
     xlab = "Value", ylab = "Frequency", col = "blue", border = "black", 
     xaxt = "n")  # Disable default x-axis ticks


#Site S001Z0007 has a low total read abunance, with one entry of 38 - look at this OTU
column_index <- which(reads[2, -1] == 1)

# Get the column name
column_name <- colnames(reads[column_index + 1] ) # Adding 1 to account for excluding the first column
column_name #OTU0002,0013 and 0071 - none of these in our matched OTUs, so this sample will
#fall out of data set anyway


#######end of checking low read abundance etc#########################

# Load functional traits data
functionaltraits <- read.csv('../../data/FunctionalTraits.csv')

#split the taxon column into genus and species so that it can be merged with functional traits
# Split Taxon column by space
split_taxon <- strsplit(as.character(blast_combined$Taxon), " ")

# Extract Genus (first element) and Species (second element, if present)
blast_combined$GENUS <- sapply(split_taxon, `[`, 1)
blast_combined$Species <- sapply(split_taxon, `[`, 2)


# Merge species data with functional traits, keeping all species from species_df_species level
blast_functional_traits <- blast_combined %>%
  left_join(functionaltraits %>% select(GENUS, primary_lifestyle), by = "GENUS")

#Make changes to funcitonal traits where some species arent in the database
#see data processing file for details
#1. 
# Update primary_lifestyle where Taxon is 'Podila humilis'
blast_functional_traits <- blast_functional_traits %>%
  mutate(primary_lifestyle = ifelse(Taxon == "Podila humilis", "soil_saprotroph", primary_lifestyle))

#2.
#Update primary_lifestye where Taxon is 'Entomortierella jenkinii'
blast_functional_traits <- blast_functional_traits %>%
  mutate(primary_lifestyle = ifelse(Taxon == 'Entomortierella jenkinii', "soil_saprotroph", primary_lifestyle))

#3.
#Update primary_lifestye where Taxon is 'Pseudoschizothecium atropurpureum
blast_functional_traits <- blast_functional_traits %>%
  mutate(primary_lifestyle = ifelse(Taxon == 'Pseudoschizothecium atropurpureum', "soil_saprotroph", primary_lifestyle))

#4.
#Update primary_lifestye where Taxon is 'Dissoderma galerinicola
blast_functional_traits <- blast_functional_traits %>%
  mutate(primary_lifestyle = ifelse(Taxon == 'Dissoderma galerinicola', "unspecified_saprotroph", primary_lifestyle))

#5
#delete the unknowns and weird
# Define OTUs to remove
otus_to_remove <- c("OTU_632", "OTU_2511", "OTU_1942", "OTU_2183",'OTU_4340')

# Remove rows where OTU is in otus_to_remove
blast_functional_traits <- blast_functional_traits[!blast_functional_traits$OTU %in% otus_to_remove, ]

#blast_functional_traits now has all the OTUs we want to work with for this analysis
###################################################################################


#get OTUs from reads table to match in format to the blast OTU column
otu_blast = blast_functional_traits$OTU
otu_blast_fixed1 = sub('OTU_', 'Otu', otu_blast) #remove _ and change case
otu_blast_fixed2 <- sprintf("Otu%04d", as.numeric(sub("Otu", "", otu_blast_fixed1))) #add the zeros

#now otu_blast_fixed is in the same text format as the colnames in the reads table 
#can now select only the cols of the reads table which have otus in our blast match
reads_subset <- reads %>% select(all_of(otu_blast_fixed2))

#stick the locations col back on
reads_subset = cbind.data.frame(reads[,1], reads_subset)

# Load proportions data and convert to presence-absence
reads_pres_abs <- reads_subset %>%
  mutate(across(-1, ~ ifelse(. > 0, 1, 0)))  # Apply transformation to all columns except the first

#Change from Otu to species hypothesis in the reads subset table for final analysis
#oops change the col names match functional traits
#bit circular here :)
# Rename columns in reads_subset to match OTU format in blast_functional_traits
colnames(reads_pres_abs)[-1] <- gsub("Otu0*", "OTU_", gsub("Otu", "OTU_", colnames(reads_pres_abs)[-1]))

# Remove any remaining leading zeros in the numeric part
colnames(reads_pres_abs)[-1] <- gsub("OTU_0*", "OTU_", colnames(reads_pres_abs)[-1])

#change to taxa
otu_to_taxon <- setNames(blast_functional_traits$Taxon, blast_functional_traits$OTU)

# Rename matching columns in reads_pres_abs
colnames(reads_pres_abs)[-1] <- ifelse(colnames(reads_pres_abs)[-1] %in% names(otu_to_taxon),
                                       otu_to_taxon[colnames(reads_pres_abs)[-1]],
                                       colnames(reads_pres_abs)[-1])

###
#duplicated colnames - because several OTUs are the same spp there are duplicated 
#cols in the reads_pres_abs df
# Assuming your dataframe is called df
duplicated_columns <- colnames(reads_pres_abs)[duplicated(colnames(reads_pres_abs))]

# Check the result
duplicated_columns

#these might take care of themselves - but we will have a many to many match below
#in the merge which we will need to adjust

#check for all zero cols
# Identify columns that contain only zeros
all_zero_cols <- colSums(reads_pres_abs[-1] == 0) == nrow(reads_pres_abs)

# Print column names that are all zeros
names(all_zero_cols[all_zero_cols])
#none - i.e. all the species hypotheses we have occur at at least one site

#now create list by site#######################

#need to only select the unique taxa from blast_functional_traits otherwise the merge 
#is confused - i.e. the blast_funtional_traits has a row for every OTU, where multiple
#OTU map to the same tax

unique_blast_functional <- blast_functional_traits %>%
  distinct(Taxon, .keep_all = TRUE)

# Function to process each row of the data
process_row <- function(row) {
  # Clean the row by removing columns with value 0
  row_cleaned <- row[, row[1, ] != 0]
  
  # Transpose the cleaned row and set column names
  row_cleaned <- as.data.frame(t(rbind(colnames(row_cleaned), row_cleaned)), row.names = FALSE)
  colnames(row_cleaned) <- as.character(row_cleaned[1, ])
  row_cleaned <- row_cleaned[-1, ]  # Remove the first row (which is now the column names)
  
  #there will be multiple entreies here for the OTUs where there is many to one relationship -
  #eg, Podila humilis which is multiple OTUs.
  #The merge will then create Podila humils, Podilia humilis.1 etc if both OTUs occur
  #If both occur - we only need the first instance because they are the same spp -
  #so delete any .
  row_cleaned_duplicates <- row_cleaned[!grepl("\\.\\d+$", row_cleaned$Group), ]
 
  colnames(row_cleaned_duplicates)[1] = 'Taxon' #change for merge
  
  # Merge with functional traits data and select necessary columns
  # Assuming your dataframe is called df_cleaned_duplicates and unique_blast_functional
  merged_df <- merge(row_cleaned_duplicates, unique_blast_functional[, c("Taxon", "primary_lifestyle")], 
                     by = "Taxon", 
                     all.x = TRUE)  
  
  return(merged_df)
}

species_by_site <- apply(reads_pres_abs, 1, function(process_row) {
  
  # Identify species (column names) present at the site (i.e., where value is 1, not NA)
  species_present <- colnames(reads_pres_abs)[process_row == 1]
  
  # For each present species, get its primary_lifestyle from unique_blast_functional
  species_lifestyle <- unique_blast_functional[unique_blast_functional$Taxon %in% species_present, c("Taxon", "primary_lifestyle")]
  
  # Create a data frame of species and primary_lifestyle for this site
  result <- data.frame(Species = species_lifestyle$Taxon, 
                       Primary_Lifestyle = species_lifestyle$primary_lifestyle)
  
  
  
  return(result)
})

  
site_names <- reads_pres_abs$Group  # Assuming 'Group' is the name of the column for site names

# Convert the resulting list of dataframes into a named list of dataframes
species_by_site_named <- setNames(species_by_site, site_names)

write_xlsx(species_by_site_named, path = '../../results/Fordiespecies2.xlsx')




