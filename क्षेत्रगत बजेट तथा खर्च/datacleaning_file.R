#===========================================
library(readxl)

setwd("C:/Users/s222385015/OneDrive - Tribhuvan University/Master Dataset/LG Fiscal Data/क्षेत्रगत बजेट तथा खर्च/Sector LG")


# List all Excel files in the folder
excel_files <- list.files(path = file_path, pattern = "\\.xlsx$", full.names = TRUE)

# Loop through each file
for (file in excel_files) {
  # Extract the file name without extension
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the Excel file
  df <- read_excel(file, skip = 7)
  
  # Assign the dataframe to a variable named after the file
  assign(file_name, df)
  
  # Print a message to confirm
  print(paste("File", file_name, "has been read and assigned to", file_name))
}




lgexp1 <- read_xlsx("220003501(1).xlsx", skip = 6)


# Read the headers separately (first three rows)
headers_raw <- read_excel("220003501(1).xlsx", range = "A4:CK7")  # Adjust range if needed

library(tidyverse)
library(tidyr)

# Convert to tibble & assign temporary column names
headers_raw <- headers_raw %>% as_tibble(.name_repair = "minimal")
colnames(headers_raw) <- paste0("V", seq_along(headers_raw))

# Transpose and convert to tibble
headers_cleaned <- headers_raw %>% 
  t() %>% 
  as_tibble(.name_repair = "minimal") 

# Replace empty names with placeholders
colnames(headers_cleaned) <- paste0("col_", seq_along(headers_cleaned))

# Fill missing values downwards
headers_filled <- headers_cleaned %>%
  fill(everything(), .direction = "down") %>% 
  unite("new_colnames", everything(), sep = "_", na.rm = TRUE)  # Combine into one column name





# Set the filled headers back into the data (this only changes the column names)
colnames(lgexp1) <- headers_filled$new_colnames

lgexp1 <- lgexp1[-nrow(lgexp1), ]    


lgexp1 <- lgexp1 %>%
  mutate(
    across(1:2, as.integer),   # Convert first two columns to integer
    across(3, as.character),   # Convert third column to character
    across(4:ncol(lgexp1), as.numeric) # Convert the rest to numeric
  )
