setwd('C:/dev/code/Petra')
install.packages("BiocManager")

# Install Biostrings package from Bioconductor
BiocManager::install("Biostrings")

# Load Biostrings package
library(Biostrings)

fasta_file <- "../../data/MicrobiomeMasterData.fasta"
fasta_sequences <- readDNAStringSet(fasta_file)

#sequences are list item 1, and outs are 1 to...
length(fasta_sequences) #= 4829

# Read the raw FASTA file
raw_lines <- readLines(fasta_file)

# Filter out lines that do not start with '>'
sequences_only <- raw_lines[!grepl("^>", raw_lines)]


#construct OTU label
otu_lables = paste('OTU', 1:4829, sep = '_')
 
#make sequences df
sequences_df = cbind(otu_lables, sequences_only)

write.csv(sequences_df, '../../data/MicrbiomeMasterData_sequences.csv')


