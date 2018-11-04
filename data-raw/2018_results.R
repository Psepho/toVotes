library(magrittr)
# Download results --------------------------------------------------------

if(file.exists("data-raw/2018_Results.zip")) {
  # Nothing to do
}  else {
  download.file("https://www.toronto.ca/ext/open_data/catalog/data_set_files/2018_Results.zip", destfile = "data-raw/2018_Results.zip")
  unzip("data-raw/2018_Results.zip", exdir="data-raw")
}

# Convert to data frame ---------------------------------------------------

mayor_file <- "data-raw/2018_Toronto_Poll_By_Poll_Mayor.xlsx"
results <- mayor_file %>%
  readxl::excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map_df(~ readxl::read_excel(path = mayor_file, sheet = .x, skip = 1), .id = "sheet") %>%
  dplyr::filter(Subdivision != "Mayor") %>%
  dplyr::filter(!grepl('Totals', Subdivision)) %>%
  dplyr::rename(ward = sheet, candidate = Subdivision) %>%
  dplyr::mutate(ward = as.integer(stringr::str_remove(ward, "Ward "))) %>%
  tidyr::gather(area, votes, -ward, -candidate) %>%
  dplyr::filter(!is.na(votes)) %>%
  dplyr::mutate(year = as.factor(2018), type = "Mayor",
                area = as.integer(area),
                candidate = as.factor(candidate)) %>%
  dplyr::filter(!is.na(area)) # Removes the totals for each ward
data("toVotes")
toVotes <- dplyr::bind_rows(toVotes, results)
usethis::use_data(toVotes, overwrite = TRUE)
