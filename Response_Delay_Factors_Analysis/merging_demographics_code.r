# This R script reads multiple CSV files from a specified directory, combines them into a single data frame,
# and then writes the combined data to a CSV file. It utilizes the 'tidyverse' and 'xlsx' packages for data
# manipulation and file handling. The combined data frame is created by row-binding the individual data frames
# read from each CSV file. Finally, the resulting combined data is exported to a CSV file.


library(tidyverse)

# Set the working directory
setwd("C:/Users/20174715/OneDrive - Higher Education Commission/TUe/CSCW-2021/Data-Set/demographics/demo_combine")

# List all CSV files in the directory
temp <- list.files(pattern = "*.csv")

# Read all CSV files into a list of data frames
myfiles <- lapply(temp, read.delim)

# Combine all data frames into a single large data frame
big_data <- do.call(rbind, myfiles)

# View the first few rows of the combined data frame
head(big_data)

# Install and load the 'xlsx' package if not already installed
if (!requireNamespace("xlsx", quietly = TRUE)) {
  install.packages("xlsx")
}
library(xlsx)

# Write the combined data to a CSV file
write.csv(big_data, file = "C:/Users/20174715/OneDrive - Higher Education Commission/TUe/CSCW-2021/Data-Set/demographics/demo_combine/combined_data.csv", row.names = TRUE)
