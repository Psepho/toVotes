library(tidyverse)
raw_results_file <- "http://www.elections.on.ca/content/dam/NGW/sitecontent/2017/results/Poll%20by%20Poll%20Results%20-%20Excel.zip"

zip_file <- "data-raw/Poll%20by%20Poll%20Results%20-%20Excel.zip"
if(file.exists(zip_file)) { # Only download the data once
  # File exists, so nothing to do
}  else {
  download.file(raw_results_file,
                destfile = zip_file)
  unzip(zip_file, exdir="data-raw") # Extract the data into data-raw
}
file.rename("data-raw/GE Results - 2014 (unconverted)", "data-raw/pollresults")

file_pattern <- "*_[[:digit:]]{3}.xls" # Can use this to filter down to specific files
poll_data <- list.files(path = "data-raw/pollresults/", pattern = file_pattern, full.names = TRUE) %>% # Find all files that match the pattern
  set_names() %>%
  purrr::map_df(readxl::read_excel, sheet = 1, col_types = "text", .id = "file") %>%   # Import each file and merge into a dataframe
  # Specifying sheet = 1 just to be clear we're ignoring the rest of the sheets
  # Declare col_types since there are duplicate surnames and map_df can't recast column types in the rbind
  dplyr::select(-starts_with("X__")) %>% # Drop all of the blank columns
  dplyr::select(1:2,4:8,15:dim(.)[2]) %>% # Reorganize a bit and drop unneeded columns
  dplyr::rename(poll_number = `POLL NO.`) %>%
  tidyr::gather(candidate, votes, -file, -poll_number) %>% # Convert to a long table
  dplyr::filter(!is.na(votes),
                poll_number != "Totals") %>%
  dplyr::mutate(electoral_district = stringr::str_extract(file, "[[:digit:]]{3}"),
                votes = as.numeric(votes)) %>%
  dplyr::select(-file)
poll_data
#TODO: match names to political parties
#TODO: find electoral district names
#       https://www3.elections.on.ca/internetapp/FYED_Error.aspx?lang=en-ca div:ddlElectoralDistricts

#bell 063 014
