library(tidyverse)
library(magrittr)
raw_results_file <- "http://www.elections.on.ca/content/dam/NGW/sitecontent/2017/results/Poll%20by%20Poll%20Results%20-%20Excel.zip"

zip_file <- "data-raw/Poll%20by%20Poll%20Results%20-%20Excel.zip"
if(file.exists(zip_file)) { # Only download the data once
  # File exists, so nothing to do
}  else {
  download.file(raw_results_file,
                destfile = zip_file)
  unzip(zip_file, exdir="data-raw") # Extract the data into data-raw
  file.rename("data-raw/GE Results - 2014 (unconverted)", "data-raw/pollresults")
}

# Electoral districts -----------------------------------------------------

# Pull the names of the electoral districts from the "Find My Electoral District" website
# Didn't find any relevant resources on the Elections Ontario website

ed_webpage <- "https://www3.elections.on.ca/internetapp/FYED_Error.aspx?lang=en-ca"
ed_xpath <- "//*[(@id = \"ddlElectoralDistricts\")]" # Use an xpath selector to get the drop down list by ID

electoral_districts <- xml2::read_html(ed_webpage) %>%
  rvest::html_node(xpath = ed_xpath) %>%
  rvest::html_nodes("option") %>%
  rvest::html_text() %>%
  .[-1] %>% # Drop the first item on the list
  tibble::as.tibble() %>% # Convert to a data frame and split into ID number and name
  tidyr::separate(value, c("electoral_district", "electoral_district_name"),
                  sep = " ",
                  extra = "merge") %>%
  # Clean up district names for later matching and presentation
  dplyr::mutate(electoral_district_name = utf8::as_utf8(stringr::str_to_title(
    stringr::str_replace_all(electoral_district_name, "--", "—"))))

# Extract votes -----------------------------------------------------------

file_pattern <- "*_[[:digit:]]{3}.xls" # Can use this to filter down to specific files
poll_data <- list.files(path = "data-raw/pollresults", pattern = file_pattern, full.names = TRUE) %>% # Find all files that match the pattern
  purrr::set_names() %>%
  purrr::map_df(readxl::read_excel, sheet = 1, col_types = "text", .id = "file") %>%   # Import each file and merge into a dataframe
  # Specifying sheet = 1 just to be clear we're ignoring the rest of the sheets
  # Declare col_types since there are duplicate surnames and map_df can't recast column types in the rbind
  # For example, Bell is in both district 014 and 063
  dplyr::select(-starts_with("X__")) %>% # Drop all of the blank columns
  dplyr::select(1:2,4:8,15:dim(.)[2]) %>% # Reorganize a bit and drop unneeded columns
  dplyr::rename(poll_number = `POLL NO.`) %>%
  tidyr::gather(candidate, votes, -file, -poll_number) %>% # Convert to a long table
  dplyr::filter(!is.na(votes),
                poll_number != "Totals") %>%
  dplyr::mutate(electoral_district = stringr::str_extract(file, "[[:digit:]]{3}"),
                votes = as.numeric(votes)) %>%
  dplyr::select(-file) %>%
  dplyr::left_join(electoral_districts)

# Candidate parties -------------------------------------------------------

candidate_webpage <- "https://en.wikipedia.org/wiki/Ontario_general_election,_2014#Candidates_by_region"
candidate_tables <- "table" # Use an xpath selector to get the drop down list by ID

candidates <- xml2::read_html(candidate_webpage) %>%
  rvest::html_nodes(candidate_tables) %>% # Pull tables from the wikipedia entry
  .[13:25] %>% # Drop unecessary tables
  rvest::html_table(fill = TRUE)

# Setup empty dataframe to store results
candidate_parties <- tibble::as_tibble(
  electoral_district_name = NULL,
  party = NULL,
  candidate = NULL
)

for(i in seq_along(1:length(candidates))) { # Messy, but works
  this <- candidates[[i]]
  # The header spans mess up the header row, so renaming
  names(this) <- c(this[1,-c(3,4)], "NA", "Incumbent")
  # Get rid of the blank spacer columns
  this <- this[-1, ]
  # Drop the NA columns by keeping only odd columns
  this <- this[,seq(from = 1, to = dim(this)[2], by = 2)]
  this %<>%
    tidyr::gather(party, candidate, -`Electoral District`) %>%
    dplyr::rename(electoral_district_name = `Electoral District`) %>%
    dplyr::filter(party != "Incumbent")
  candidate_parties <- dplyr::bind_rows(candidate_parties, this)
}
rm(this, i)

# Join electoral district numbers into the candidate and parties data

candidate_parties %<>%
  # These three lines are cleaning up hyphens and dashes, seems overly complicated
  dplyr::mutate(electoral_district_name = stringr::str_replace_all(electoral_district_name, "—\n", "—")) %>%
  dplyr::mutate(electoral_district_name = stringr::str_replace_all(electoral_district_name,
                                                                   "Chatham-Kent—Essex",
                                                                   "Chatham—Kent—Essex")) %>%
  dplyr::mutate(electoral_district_name = utf8::as_utf8(stringr::str_to_title(electoral_district_name))) %>%
  dplyr::left_join(electoral_districts) %>%
  dplyr::filter(!candidate == "") %>%
  tidyr::separate(candidate, into = c("first","candidate"), extra = "merge", remove = TRUE)

poll_data_party_match_table <- poll_data %>%
  group_by(candidate, electoral_district_name) %>%
  summarise() %>%
  fuzzyjoin::stringdist_left_join(candidate_parties,
                                  ignore_case = TRUE) %>%
  dplyr::select(candidate = candidate.x,
                # electoral_district_name = electoral_district_name.x,
                party = party,
                electoral_district = electoral_district) %>%
  dplyr::filter(!is.na(party))

poll_data %<>%
  dplyr::left_join(poll_data_party_match_table)

