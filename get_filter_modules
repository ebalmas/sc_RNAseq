# Load necessary library
library(dplyr)

# Function to convert a column (module) to percentages for every cell.
convert_to_percentage <- function(column) {
  rng <- range(column)
  if (rng[2] > rng[1]) {
    # Scale the values such that the min becomes 0 and the max becomes 100.
    (column - rng[1]) / (rng[2] - rng[1]) * 100
  } else {
    # If all values are the same, return 0 for every cell
    rep(0, length(column))
  }
}

# Load necessary library
library(dplyr)

# Function to convert a column (module) to percentages for every cell
convert_to_percentage <- function(column) {
  rng <- range(column)
  if (rng[2] > rng[1]) {
    # Scale the values such that the min becomes 0 and the max becomes 100
    (column - rng[1]) / (rng[2] - rng[1]) * 100
  } else {
    # If all values are the same, return 0 for every cell
    rep(0, length(column))
  }
}

# Function to filter columns based on the percentage of cells with values over a specified threshold
filter_columns_by_percentage <- function(a, percentage_threshold = 0.10, value_threshold = 65) {
  # Calculate number of cells (rows)
  num_cells <- nrow(a)
  # Define the threshold for a specified percentage of cells
  threshold <- ceiling(percentage_threshold * num_cells)
  
  # Initialize an empty dataframe to store the filtered columns
  filtered_df <- data.frame(row.names = rownames(a))
  
  # Loop through each column in the dataframe
  for (colname in colnames(a)) {
    column <- a[[colname]]
    
    # Calculate the number of cells with values over the specified threshold
    num_high_values <- sum(column > value_threshold)
    
    # If more than the specified percentage of cells have values over the threshold, keep this column
    if (num_high_values >= threshold) {
      filtered_df[[colname]] <- column
    }
  }
  
  return(filtered_df)
}

# Main function to get filter_modules vector
get_filter_modules <- function(df, percentage_threshold = 0.10, value_threshold = 65) {
  colnames(df) <- paste0("module_", 1:93)
  
  a <- data.frame(row.names = rownames(df))
  
  # Apply the function to each module (column) from module_1 to module_93
  for (i in 1:93) {
    module_name <- paste0("module_", i)
    a[[module_name]] <- convert_to_percentage(df[[module_name]])
  }
  
  # Apply the filter columns function
  filtered_a <- filter_columns_by_percentage(a, percentage_threshold, value_threshold)
  
  # Get the column names of the filtered dataframe
  filter_modules <- colnames(filtered_a)
  
  return(filter_modules)
}

# Example usage
# df <- read.csv("path_to_your_data.csv") # Load your data
# filter_modules <- get_filter_modules(df, percentage_threshold = 0.15, value_threshold = 70)
# print(filter_modules)
