setwd('C:/dev/code/Petra')
#a
install.packages("BiocManager")

# Install Biostrings package from Bioconductor
BiocManager::install("Biostrings")

# Load Biostrings package
library(Biostrings)

#fastafile = "../../data/MicrobiomeMasterData.fasta"
fasta_file = "../../data/fordie_fasta.fasta"

fasta_sequences <- readDNAStringSet(fasta_file)


# Read the raw FASTA file
raw_lines <- readLines(fasta_file)

# Filter out lines that do not start with '>'
sequences_only <- raw_lines[!grepl("^>", raw_lines)]


#construct OTU label
otu_lables = paste('OTU', 1:length(fasta_sequences), sep = '_')
 
#make sequences df
sequences_df <- data.frame(
  ID = otu_lables,
  Sequence = sequences_only,
  stringsAsFactors = FALSE
)
dna_sequences <- DNAStringSet(sequences_df$Sequence)
names(dna_sequences) <- sequences_df$ID 

#save as csv and as modified fasta - i.e. just OTU and sequence
write.csv(sequences_df, '../../data/Fordie_sequences.csv')

writeXStringSet(dna_sequences, "../../results/fordie_modified.fasta")

###############################################################
#for plutoF need to split into sections of 1000 otus per upload.
library(seqinr)


# Function to split FASTA file into smaller files
split_fasta <- function(input_file, output_prefix, max_otus = 5) {
  # Read the input FASTA file
  seqs <- read.fasta(input_file)
  
  # Ensure sequences are in uppercase
  seqs <- lapply(seqs, toupper)
  # Calculate the number of chunks needed
  num_chunks <- ceiling(length(seqs) / max_otus)
  
  # Split the sequences into smaller chunks
  for (i in 1:num_chunks) {
    start_index <- (i - 1) * max_otus + 1
    end_index <- min(i * max_otus, length(seqs))
    chunk_seqs <- seqs[start_index:end_index]
    
    # Create output file for each chunk
    output_file <- paste0(output_prefix, "_", i, ".fasta")
    write.fasta(sequences = chunk_seqs, names = names(chunk_seqs), file.out = output_file)
    cat("Written:", output_file, "\n")
  }
}


split_fasta("../../results/fordie_modified.fasta", "../../results/split_fasta", 5)

#make small test fasta
split_fasta('../../resultssplit_fasta_1','../../results/smal_test_fast')
