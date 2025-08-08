get_species_repeats <- function(df, col1, col2, col3) {
  # Convert column names to symbols if given as strings
  col1 <- rlang::ensym(col1)
  col2 <- rlang::ensym(col2)
  col3 <- rlang::ensym(col3)
  
  # Extract and binarise presence-absence data
  data <- cbind.data.frame(
    col1 = as.integer(df[[as.character(col1)]] > 0),
    col2 = as.integer(df[[as.character(col2)]] > 0),
    col3 = as.integer(df[[as.character(col3)]] > 0)
  )
  
  # Identify unique species at each step
  species_1 <- which(data$col1 == 1)
  species_2_new <- which(data$col2 == 1 & data$col1 == 0)
  species_3_new <- which(data$col3 == 1 & data$col1 == 0 & data$col2 == 0)
  
  # Cumulative counts
  count1 <- length(species_1)
  count2 <- count1 + length(species_2_new)
  count3 <- count2 + length(species_3_new)
  
  # Return vector
  df_out <- c(count1, count2, count3)
  return(df_out)
}

repeats = get_species_repeats(allfungi, A12, A121, A122)
singles1.test = get_species_repeats(allfungi,A12, A13, A20)
